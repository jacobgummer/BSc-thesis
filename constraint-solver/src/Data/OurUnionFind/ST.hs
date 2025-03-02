{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.OurUnionFind.ST where

import Type 

import Control.Monad ( when )
import Control.Monad.ST
import Data.STRef

newtype TypeNode s = Node (STRef s (Link s)) deriving Eq

data Link s
    = Repr {-# UNPACK #-} !(STRef s Info)
      -- ^ This is the representative of the equivalence class.
    | Link {-# UNPACK #-} !(TypeNode s)
      -- ^ Pointer to some other element of the equivalence class.
     deriving Eq

data Info = MkInfo
  { weight :: {-# UNPACK #-} !Int
    -- ^ The size of the equivalence class, used by 'union'.
  , descr  :: Type
  } deriving Eq

-- | /O(1)/. Create a fresh node and return it.  A fresh node is in
-- the equivalence class that contains only itself.
makeSet :: Type -> ST s (TypeNode s)
makeSet t = do
  info <- newSTRef (MkInfo { weight = 1, descr = t })
  l <- newSTRef (Repr info)
  return (Node l)

-- | /O(1)/. @find node@ returns the representative node of
-- @node@'s equivalence class.
--
-- This method performs the path compresssion.
find :: TypeNode s -> ST s (TypeNode s)
find node@(Node l) = do
  link <- readSTRef l
  case link of
    Repr _ -> return node
    Link node'@(Node l') -> do
      node'' <- find node'
      when (node' /= node'') $ do
        -- At this node we know that @node'@ is not the representative
        -- element of @node@'s equivalent class.  Therefore @node'@'s
        -- link must be of the form @Link r@.  We write this same
        -- value into @node@'s link reference and thereby perform
        -- path compression.
        link' <- readSTRef l'
        writeSTRef l link'
      return node''

-- | Return the reference to the node's equivalence class's
-- getDescriptor.
descrRef :: TypeNode s -> ST s (STRef s Info) 
descrRef node@(Node link_ref) = do
  link <- readSTRef link_ref
  case link of
    Repr info -> return info
    Link (Node link'_ref) -> do
      link' <- readSTRef link'_ref
      case link' of
        Repr info -> return info
        _ -> descrRef =<< find node

-- | /O(1)/. Return the decriptor associated with argument node's
-- equivalence class.
getDescriptor :: TypeNode s -> ST s Type
getDescriptor node = do
  descr <$> (readSTRef =<< descrRef node)

-- | /O(1)/. Replace the descriptor of the node's equivalence class
-- with the second argument.
setDescriptor :: TypeNode s -> Type -> ST s ()
setDescriptor node new_descr = do
  r <- descrRef node
  modifySTRef r $ \i -> i { descr = new_descr }

modifyDescriptor :: TypeNode s -> (Type -> Type) -> ST s ()
modifyDescriptor node f = do
  r <- descrRef node
  modifySTRef r $ \i -> i { descr = f (descr i) }

-- | /O(1)/. Join the equivalence classes of the nodes. If both or none
-- of the nodes are in equivalence classes with a type variable as a
-- representative, the resulting equivalence class will get the descriptor
-- of the second argument; otherwise, the descriptor will be the node that
-- doesn't represent a type variable.
union :: TypeNode s -> TypeNode s -> ST s ()
union n1 n2 = do
  (node1@(Node link_ref1), node2@(Node link_ref2)) <- preprocess n1 n2
  -- Ensure that nodes aren't in the same equivalence class. 
  when (node1 /= node2) $ do
    repr1 <- readSTRef link_ref1
    repr2 <- readSTRef link_ref2
    case (repr1, repr2) of
      (Repr info_ref1, Repr info_ref2) -> do
        (MkInfo w1 _) <- readSTRef info_ref1
        (MkInfo w2 d2) <- readSTRef info_ref2
        if w1 >= w2 then do
          -- Make n1('s parent) the parent of n2.
          writeSTRef link_ref2 (Link n1)
          writeSTRef info_ref1 (MkInfo (w1 + w2) d2)
        else do
          -- Make n2('s parent) the parent of n1.
          writeSTRef link_ref1 (Link node2)
          writeSTRef info_ref2 (MkInfo (w1 + w2) d2)
      _ -> error "'find' somehow didn't return a Repr"
    where
      preprocess :: TypeNode s -> TypeNode s -> ST s (TypeNode s, TypeNode s) 
      preprocess n1' n2' = do
        -- Find representatives of each node's equivalence class.
        r1 <- find n1'
        r2 <- find n2'
        -- Check if representatives represent type variables.
        r1IsTVar <- isTVar r1
        r2IsTVar <- isTVar r2
        case (r1IsTVar, r2IsTVar) of 
          (False, True) -> return (r2, r1)
          _             -> return (r1, r2)

      isTVar :: TypeNode s -> ST s Bool
      isTVar node = do
        t <- getDescriptor node
        case t of
          TVar _ -> return True
          _      -> return False      

-- | /O(1)/. Return @True@ if both nodes belong to the same
-- | equivalence class.
equivalent :: TypeNode s -> TypeNode s -> ST s Bool
equivalent n1 n2 = (==) <$> find n1 <*> find n2
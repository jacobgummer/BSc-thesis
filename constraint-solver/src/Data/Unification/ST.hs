{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.Unification.ST where

import Type 

import Control.Monad ( when )
import Control.Monad.ST
import Data.STRef

newtype VarNode s = Node (STRef s (Link s)) deriving Eq

data Link s
    = Repr {-# UNPACK #-} !(STRef s Info)
      -- ^ This is the representative of the equivalence class.
    | Link {-# UNPACK #-} !(VarNode s)
      -- ^ Pointer to some other element of the equivalence class.
     deriving Eq

data Info = MkInfo
  { weight :: {-# UNPACK #-} !Int
    -- ^ The size of the equivalence class, used by 'union'.
  , descr  :: Maybe Type
  } deriving Eq

-- | /O(1)/. Create a fresh node and return it.  A fresh node is in
-- the equivalence class that contains only itself.
makeSet :: Maybe Type -> ST s (VarNode s)
makeSet t = do
  info <- newSTRef (MkInfo { weight = 1, descr = t })
  l <- newSTRef (Repr info)
  return (Node l)

-- | /O(1)/. @find node@ returns the representative node of
-- @node@'s equivalence class.
--
-- This method performs the path compresssion.
find :: VarNode s -> ST s (VarNode s)
find node@(Node l) = do
  link <- readSTRef l
  case link of
    -- Input node is representative.
    Repr _ -> return node

    -- Input node's parent is another node.
    Link node'@(Node l') -> do
      node'' <- find node'
      when (node' /= node'') $ do
        -- Input node's parent isn't representative;
        -- performing path compression.
        link' <- readSTRef l'
        writeSTRef l link'
      return node''

-- | Return the reference to the node's equivalence class's
-- descriptor.
descrRef :: VarNode s -> ST s (STRef s Info) 
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
getDescriptor :: VarNode s -> ST s (Maybe Type)
getDescriptor node = do
  descr <$> (readSTRef =<< descrRef node)

-- | /O(1)/. Replace the descriptor of the node's equivalence class
-- with the second argument.
setDescriptor :: VarNode s -> Maybe Type -> ST s ()
setDescriptor node new_descr = do
  r <- descrRef node
  modifySTRef r $ \i -> i { descr = new_descr }

-- modifyDescriptor :: VarNode s -> (Type -> Type) -> ST s ()
-- modifyDescriptor node f = do
--   r <- descrRef node
--   modifySTRef r $ \i -> i { descr = f (descr i) }

-- | /O(1)/. Join the equivalence classes of the nodes. If both or none
-- of the nodes are in equivalence classes with a type variable as a
-- representative, the resulting equivalence class will get the descriptor
-- of the second argument; otherwise, the new descriptor will be from the 
-- node that doesn't represent a type variable.
union :: VarNode s -> VarNode s -> ST s ()
union n1 n2 = do
  (node1@(Node link_ref1), node2@(Node link_ref2)) <- preprocess n1 n2

  -- Ensure that nodes aren't in the same equivalence class. 
  when (node1 /= node2) $ do
    link1 <- readSTRef link_ref1
    link2 <- readSTRef link_ref2
    case (link1, link2) of
      (Repr info_ref1, Repr info_ref2) -> do
        (MkInfo w1 _) <- readSTRef info_ref1
        (MkInfo w2 d2) <- readSTRef info_ref2
        if w1 >= w2 then do
          writeSTRef link_ref2 (Link n1)
          writeSTRef info_ref1 (MkInfo (w1 + w2) d2)
        else do
          writeSTRef link_ref1 (Link node2)
          writeSTRef info_ref2 (MkInfo (w1 + w2) d2)

      -- This shouldn't be possible.       
      _ -> error "'find' somehow didn't return a Repr" 

      where
        -- TODO: Maybe handle if (different) roots both have assigned values?
        preprocess :: VarNode s -> VarNode s -> ST s (VarNode s, VarNode s) 
        preprocess n1' n2' = do
          r1 <- find n1'
          r2 <- find n2'
          -- Checking if n1 points to root with type assigned to it.
          d1 <- getDescriptor r1
          case d1 of
            Just _  -> return (r2, r1)
            Nothing -> return (r1, r2)    

-- | /O(1)/. Return @True@ if both nodes belong to the same
-- | equivalence class.
equivalent :: VarNode s -> VarNode s -> ST s Bool
equivalent n1 n2 = (==) <$> find n1 <*> find n2
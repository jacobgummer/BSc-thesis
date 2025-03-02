-- | An implementation of Tarjan's UNION-FIND algorithm.  (Robert E
-- Tarjan. \"Efficiency of a Good But Not Linear Set Union Algorithm\", JACM
-- 22(2), 1975)
--
-- The algorithm implements three operations efficiently (all amortised
-- @O(1)@):
--
--  1. Check whether two elements are in the same equivalence class.
--
--  2. Create a union of two equivalence classes.
--
--  3. Look up the descriptor of the equivalence class.
-- 
-- The implementation is based on mutable references.  Each
-- equivalence class has exactly one member that serves as its
-- representative element.  Every element either is the representative
-- element of its equivalence class or points to another element in
-- the same equivalence class.  Equivalence testing thus consists of
-- following the pointers to the representative elements and then
-- comparing these for identity.
--
-- The algorithm performs lazy path compression.  That is, whenever we
-- walk along a path greater than length 1 we automatically update the
-- pointers along the path to directly point to the representative
-- element.  Consequently future lookups will be have a path length of
-- at most 1.
--
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.OurUnionFind.ST where

import Type 

import Control.Monad ( when )
import Control.Monad.ST
import Data.STRef

newtype TypeNode s = Pt (STRef s (Link s)) deriving Eq

data Link s
    = Repr {-# UNPACK #-} !(STRef s TypeInfo)
      -- ^ This is the descriptive element of the equivalence class.
    | Link {-# UNPACK #-} !(TypeNode s)
      -- ^ Pointer to some other element of the equivalence class.
     deriving Eq

data TypeInfo = MkInfo
  { weight :: {-# UNPACK #-} !Int
    -- ^ The size of the equivalence class, used by 'union'.
  , descr  :: Type
  } deriving Eq

-- | /O(1)/. Create a fresh point and return it.  A fresh point is in
-- the equivalence class that contains only itself.
makeSet :: Type -> ST s (TypeNode s)
makeSet t = do
  info <- newSTRef (MkInfo { weight = 1, descr = t })
  l <- newSTRef (Repr info)
  return (Pt l)

-- | /O(1)/. @find point@ returns the representative point of
-- @point@'s equivalence class.
--
-- This method performs the path compresssion.
find :: TypeNode s -> ST s (TypeNode s)
find point@(Pt l) = do
  link <- readSTRef l
  case link of
    Repr _ -> return point
    Link pt'@(Pt l') -> do
      pt'' <- find pt'
      when (pt'' /= pt') $ do
        -- At this point we know that @pt'@ is not the representative
        -- element of @point@'s equivalent class.  Therefore @pt'@'s
        -- link must be of the form @Link r@.  We write this same
        -- value into @point@'s link reference and thereby perform
        -- path compression.
        link' <- readSTRef l'
        writeSTRef l link'
      return pt''

-- | Return the reference to the point's equivalence class's
-- descriptor.
descrRef :: TypeNode s -> ST s (STRef s TypeInfo) 
descrRef point@(Pt link_ref) = do
  link <- readSTRef link_ref
  case link of
    Repr info -> return info
    Link (Pt link'_ref) -> do
      link' <- readSTRef link'_ref
      case link' of
        Repr info -> return info
        _ -> descrRef =<< find point

-- | /O(1)/. Return the descriptor associated with argument point's
-- equivalence class.
descriptor :: TypeNode s -> ST s Type
descriptor point = do
  descr <$> (readSTRef =<< descrRef point)

-- | /O(1)/. Replace the descriptor of the point's equivalence class
-- with the second argument.
setDescriptor :: TypeNode s -> Type -> ST s ()
setDescriptor point new_descr = do
  r <- descrRef point
  modifySTRef r $ \i -> i { descr = new_descr }

modifyDescriptor :: TypeNode s -> (Type -> Type) -> ST s ()
modifyDescriptor point f = do
  r <- descrRef point
  modifySTRef r $ \i -> i { descr = f (descr i) }

-- | /O(1)/. Join the equivalence classes of the points (which must be
-- distinct).  The resulting equivalence class will get the descriptor
-- of the second argument.
union :: TypeNode s -> TypeNode s -> ST s ()
union p1 p2 = union' p1 p2 (\_ d2 -> return d2)

-- | Like 'union', but sets the descriptor returned from the callback.
-- 
-- The intention is to keep the descriptor of the second argument to
-- the callback, but the callback might adjust the information of the
-- descriptor or perform side effects.
union' :: TypeNode s -> TypeNode s -> (Type -> Type -> ST s Type) -> ST s ()
union' p1 p2 update = do
  point1@(Pt link_ref1) <- find p1
  point2@(Pt link_ref2) <- find p2
  when (point1 /= point2) $ do
    info1 <- readSTRef link_ref1
    info2 <- readSTRef link_ref2
    case (info1, info2) of
      (Repr info_ref1, Repr info_ref2) -> do
        (MkInfo w1 d1) <- readSTRef info_ref1
        (MkInfo w2 d2) <- readSTRef info_ref2
        d2' <- update d1 d2
        if w1 >= w2 then do
          writeSTRef link_ref2 (Link point1)
          writeSTRef info_ref1 (MkInfo (w1 + w2) d2')
        else do
          writeSTRef link_ref1 (Link point2)
          writeSTRef info_ref2 (MkInfo (w1 + w2) d2')
      _ -> error "Unexpected value in link_ref"

-- | /O(1)/. Return @True@ if both points belong to the same
-- | equivalence class.
equivalent :: TypeNode s -> TypeNode s -> ST s Bool
equivalent p1 p2 = (==) <$> find p1 <*> find p2
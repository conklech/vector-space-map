{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
module Data.Map.Vector (MapVector(..)) where

import Prelude hiding (foldr)
import Data.Foldable
import Data.Traversable
import Data.Data
import Control.Applicative
import Control.Arrow
import Data.AdditiveGroup
import Data.VectorSpace
import Data.Basis
import Data.Map (Map)
import qualified Data.Map as Map

-- TODO: Clean up overlong >>> examples.

-- | Note: '<*>' in the 'Applicative' instance operates under /intersection/.  i.e.:
--
-- >>> (MapVector $ Map.fromList [("x", id)]) <*> (MapVector $ Map.fromList [("y", 3)])
-- MapVector (Map.fromList [])
--  
-- '*' in the 'Num' instance performs elementwise multiplication.  It is defined in terms of
-- '<*>' and therefore also operates under intersection:
--   
-- >>> (MapVector $ Map.fromList [("x", 2), ("y", 3)]) * (MapVector $ Map.fromList [("x", 5),("y", 7)])
-- MapVector (Map.fromList [("x", 10), ("y", 21)])
--   
-- >>> (MapVector $ Map.fromList [("x", 2), ("y", 3)]) * (MapVector $ Map.fromList [("y", 7)])
-- MapVector (Map.fromList [("y", 21)])
--
-- '*^' in the 'VectorSpace' instance multiplies by the scalar of v.  Nesting MapVectors preserves
-- the scalar type, e.g. @Scalar (MapVector k (MapVector k' v))@ = @Scalar v@.
--
-- >>> 2 *^ (ConstantMap $ MapVector $ Map.fromList [("x", 3 :: Int), ("y", 5)])
-- ConstantMap (MapVector (fromList [("x",6),("y",10)]))
--
-- Finally, '<.>' in 'InnerSpace' is the dot-product operator.  Again, it operates under intersection.
--
-- >>> (MapVector $ Map.fromList [("x", 2 :: Int), ("y", 3)]) <.> (MapVector $ Map.fromList [("x", 5),("y", 7)])
-- 31
--
-- >>> (pure . MapVector $ Map.fromList [("x", 2 :: Int), ("y", 3)]) <.> (MapVector $ Map.fromList [("a", pure (5::Int))])
-- 25
--
-- Addition, using either '+' or '^+^', operates under union.

data MapVector k v = 
      MapVector (Map k v) 
    | ConstantMap v -- ^ An infinite-dimensional vector with the same value on all dimensions
    deriving (Eq, Functor, Show, Read, Foldable, Traversable, Typeable, Data)

instance (Ord k) => Applicative (MapVector k) where 
    pure = ConstantMap
    (ConstantMap f) <*> (ConstantMap v) = ConstantMap $ f v
    (ConstantMap f) <*> (MapVector vs)  = MapVector   $ f     <$> vs
    (MapVector fs)  <*> (ConstantMap v) = MapVector   $ ($ v) <$> fs
    (MapVector fs)  <*> (MapVector vs)  = MapVector   $ Map.intersectionWith ($) fs vs
    {-# INLINABLE (<*>) #-}

instance (AdditiveGroup v, Ord k) => AdditiveGroup (MapVector k v) where
    zeroV = MapVector Map.empty
    negateV = fmap negateV
    (ConstantMap v) ^+^ (ConstantMap v') = ConstantMap $ v ^+^ v'
    (ConstantMap v) ^+^ (MapVector vs)   = MapVector   $ (v ^+^) <$> vs
    (MapVector vs)  ^+^ (ConstantMap v)  = MapVector   $ (^+^ v) <$> vs
    (MapVector vs)  ^+^ (MapVector vs')  = MapVector   $ Map.unionWith (^+^) vs vs'
    {-# INLINABLE (^+^) #-}
    
instance (Ord k, VectorSpace v) => VectorSpace (MapVector k v) where
    type Scalar (MapVector k v) = Scalar v  -- therefore Scalar (MapVector k (Mapvector k' v))
                                            --   = Scalar v
    s *^ v  = (s *^) <$> v 
    {-# INLINABLE (*^) #-}

instance (Ord k, VectorSpace v, InnerSpace v, AdditiveGroup (Scalar v)) => InnerSpace (MapVector k v) where
    (ConstantMap v) <.> (ConstantMap v') =                      v <.> v'
    (ConstantMap v) <.> (MapVector vs)   = foldl' (^+^) zeroV $ (v <.>) <$> vs
    (MapVector vs)  <.> (ConstantMap v)  = foldl' (^+^) zeroV $ (<.> v) <$> vs
    (MapVector vs)  <.> (MapVector vs')  = foldl' (^+^) zeroV $ Map.intersectionWith (<.>) vs vs'
    {-# INLINABLE (<.>) #-}

instance (Ord k, HasBasis v, AdditiveGroup (Scalar v)) => HasBasis (MapVector k v) where
    type Basis (MapVector k v) = (k, Basis v)

    basisValue (k, v) = MapVector $ Map.fromList $ (k, basisValue v):[]
    
    decompose (MapVector vs)
      = Map.toList (decompose<$>vs) >>= \(k,vs) -> first(k,)<$>vs
    decompose (ConstantMap _) = error "decompose: not defined for ConstantMap.\
          \ Use decompose', which works properly on infinite-dimensional spaces."
       -- Note that it would be possible to properly implement 'decompose' under
       -- the additional constraint that the keys are enumerable. See e.g. the
       -- @universe-base@ package.

    decompose' (MapVector vs) (k,bv) = case Map.lookup k vs of
         Nothing -> zeroV
         Just v  -> decompose' v bv
    decompose' (ConstantMap c) (_,bv) = decompose' c bv
    
    

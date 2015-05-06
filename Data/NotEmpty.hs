{-# LANGUAGE DeriveGeneric #-}
module Data.NotEmpty
  ( NE()
  , notEmpty
  , original
  , NotEmptyException(..)
  ) where

-- {{{ Imports
import           Control.Monad.Catch

import           Data.Hashable
import           Data.Semigroup

import           GHC.Generics

import           Test.QuickCheck
-- }}}

-- | For a monoid @s@, @NE s@ is @s@ without its neutral element 'mempty'.
newtype NE s = NE s deriving(Generic)

instance (Eq s) => Eq (NE s) where
  (NE a) == (NE b) = a == b

-- | @NE s@ is the semigroup derived from the monoid type @s@.
instance (Monoid s) => Semigroup (NE s) where
  (NE a) <> (NE b) = NE (a `mappend` b)

instance (Show s) => Show (NE s) where
  show (NE s) = show s

instance (Eq s, Monoid s, Arbitrary s) => Arbitrary (NE s) where
  arbitrary = NE <$> arbitrary `suchThat` (/= mempty)

instance (Hashable s) => Hashable (NE s)

-- | Smart constructor.
notEmpty :: (MonadThrow m, Eq s, Monoid s) => s -> m (NE s)
notEmpty s | s == mempty = throwM EmptyValue
           | otherwise   = return $ NE s

-- | Unwrap the underlying value.
original :: NE s -> s
original (NE s) = s


data NotEmptyException = EmptyValue deriving(Eq, Show)
instance Exception NotEmptyException

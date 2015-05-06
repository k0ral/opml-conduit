{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | External 'Arbitrary' instances used by OPML types.
-- All instances are defined through the 'OpmlGen' wrapper to avoid conflicts.
module Text.OPML.Arbitrary where

-- {{{ Imports
import           Data.Char
import           Data.Maybe
import           Data.NotEmpty
import           Data.Text                 (Text, find)
import           Data.Time.Clock
import           Data.Version

import           GHC.Generics

import           Network.URI

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
-- }}}

newtype OpmlGen a = OpmlGen { unwrap :: a }
deriving instance (Generic a) => Generic (OpmlGen a)
instance (Arbitrary (OpmlGen a)) => Arbitrary (OpmlGen (Maybe a)) where
  arbitrary = do
    a                <- arbitrary :: Gen (Maybe ())
    (OpmlGen result) <- arbitrary
    return . OpmlGen $ maybe Nothing (const $ Just result) a

-- | Alpha-numeric generator.
genAlphaNum :: Gen Char
genAlphaNum = oneof [choose('a', 'z'), suchThat arbitrary isDigit]

-- | OPML version may only be @1.0@, @1.1@ or @2.0@
instance Arbitrary (OpmlGen Version) where
  arbitrary = OpmlGen <$> (Version <$> elements [ [1, 0], [1, 1], [2, 0] ] <*> pure [])
  shrink (OpmlGen (Version a b)) = OpmlGen <$> (Version <$> shrink a <*> shrink b)

-- | Reasonable enough 'URI' generator.
instance Arbitrary (OpmlGen URI) where
  arbitrary = OpmlGen <$> (URI <$> genUriScheme <*> (unwrap <$> arbitrary) <*> genUriPath <*> genUriQuery <*> genUriFragment)
              where genUriPath = ("/" ++) <$> listOf1 genAlphaNum
                    genUriQuery = oneof [return "", ("?" ++) <$> listOf1 genAlphaNum]
                    genUriFragment = oneof [return "", ("#" ++) <$> listOf1 genAlphaNum]
                    genUriScheme = (++ ":") <$> listOf1 (choose('a', 'z'))
  -- shrink = genericShrink

-- | Reasonable enough 'URIAuth' generator.
instance Arbitrary (OpmlGen URIAuth) where
  arbitrary = do
    userInfo <- oneof [return "", (++ "@") <$> listOf1 genAlphaNum]
    regName <- listOf1 genAlphaNum
    port <- oneof [return "", (\x -> ":" ++ x) . show <$> choose(1 :: Int, 65535)]
    return . OpmlGen $ URIAuth userInfo regName port
  -- shrink = genericShrink


-- | Generates 'UTCTime' with rounded seconds.
instance Arbitrary (OpmlGen UTCTime) where
  arbitrary = do
    (UTCTime d s) <- arbitrary
    return . OpmlGen $ UTCTime d (fromIntegral (round s :: Int))
  -- shrink = genericShrink

-- | Generates 'OutlineBase''s categories.
-- This generator makes sure that the result has no @,@ nor @/@ characters, since those are used as separators.
instance Arbitrary (OpmlGen [[NE Text]]) where
  arbitrary = OpmlGen <$> listOf (listOf1 $ arbitrary `suchThat` (isNothing . find (\c -> c == ',' || c == '/') . original))
  shrink = genericShrink

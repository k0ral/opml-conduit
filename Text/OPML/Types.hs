{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
-- | OPML is an XML format for outlines.
--
-- Example:
--
-- > <opml version="2.0">
-- >   <head>
-- >     <title>OPML title</title>
-- >     <dateCreated>Mon, 31 Oct 2005 19:23:00 GMT</dateCreated>
-- >   </head>
-- >   <body>
-- >     <outline text="Outline 1" created="Mon, 31 Oct 2005 18:21:33 GMT"/>
-- >     <outline text="Outline 2" created="Mon, 31 Oct 2005 18:21:33 GMT"/>
-- >   </body>
-- > </opml>
module Text.OPML.Types
  ( -- * Top-level OPML structure
    Opml(..)
  , opmlVersion_
  , head_
  , outlines_
    -- * OPML header
  , OpmlHead(..)
  , Direction(..)
  , opmlTitle_
  , opmlCreated_
  , modified_
  , ownerName_
  , ownerEmail_
  , ownerId_
  , docs_
  , expansionState_
  , vertScrollState_
  , window_
    -- * OPML outlines
  , OpmlOutline(..)
  , _OpmlOutlineGeneric
  , _OpmlOutlineLink
  , _OpmlOutlineSubscription
    -- ** Generic outlines
  , OutlineBase(..)
  , mkOutlineBase
  , text_
  , isComment_
  , isBreakpoint_
  , outlineCreated_
  , categories_
    -- ** Subscription outlines
  , OutlineSubscription(..)
  , mkOutlineSubscription
  , xmlUri_
  , htmlUri_
  , description_
  , language_
  , subscriptionTitle_
  , subscriptionVersion_
  ) where

-- {{{ Imports
import           Control.Lens.TH
import           Control.Monad

import           Data.Default
import           Data.Hashable
import           Data.Hashable.Time  ()
import           Data.HashMap.Lazy
import           Data.List.NonEmpty
import           Data.NonNull
import           Data.Text
import           Data.Time.Clock
import           Data.Time.LocalTime ()
import           Data.Tree
import           Data.Version

import           GHC.Generics

import           Network.URI

import           Test.QuickCheck
import           Text.OPML.Arbitrary
-- }}}

-- Orphan instance
instance (Hashable a) => Hashable (NonNull a) where
  hashWithSalt s = hashWithSalt s . toNullable

data Direction = Top' | Left' | Bottom' | Right' deriving(Eq, Generic, Show)

instance Hashable Direction

instance Arbitrary Direction where
  arbitrary = elements [Top', Left', Right', Bottom']
  shrink = genericShrink


declareLenses [d|
  data OpmlHead = OpmlHead
    { opmlTitle_ :: Text
    , opmlCreated_ :: Maybe UTCTime
    , modified_ :: Maybe UTCTime
    , ownerName_ :: Text
    , ownerEmail_ :: Text
    , ownerId_ :: Maybe URI
    , docs_ :: Maybe URI
    , expansionState_ :: [Int]
    , vertScrollState_ :: Maybe Int
    , window_ :: HashMap Direction Int
    }
  |]

deriving instance Eq OpmlHead
deriving instance Generic OpmlHead
deriving instance Show OpmlHead
-- instance Hashable OpmlHead

-- | Use 'def' as a smart constructor. All fields are set to 'mempty'.
instance Default OpmlHead where
  def = OpmlHead mempty mzero mzero mempty mempty mzero mzero mzero mzero mempty

instance Arbitrary OpmlHead where
  arbitrary = OpmlHead <$> arbitrary
                       <*> (unwrap <$> arbitrary)
                       <*> (unwrap <$> arbitrary)
                       <*> arbitrary
                       <*> arbitrary
                       <*> (unwrap <$> arbitrary)
                       <*> (unwrap <$> arbitrary)
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
  shrink (OpmlHead a b c d e f g h i j) = OpmlHead <$> shrink a
                                                   <*> (unwrap <$> shrink (OpmlGen b))
                                                   <*> (unwrap <$> shrink (OpmlGen c))
                                                   <*> shrink d
                                                   <*> shrink e
                                                   <*> (unwrap <$> shrink (OpmlGen f))
                                                   <*> (unwrap <$> shrink (OpmlGen g))
                                                   <*> shrink h
                                                   <*> shrink i
                                                   <*> shrink j

declareLenses [d|
  data OutlineBase = OutlineBase
    { text_ :: NonNull Text
    , isComment_ :: Maybe Bool
    , isBreakpoint_ :: Maybe Bool
    , outlineCreated_ :: Maybe UTCTime
    , categories_ :: [NonEmpty (NonNull Text)] -- ^
    }
  |]

deriving instance Eq OutlineBase
deriving instance Generic OutlineBase
deriving instance Show OutlineBase
instance Hashable OutlineBase

instance Arbitrary OutlineBase where
  arbitrary = OutlineBase <$> genNonNull
                          <*> arbitrary
                          <*> arbitrary
                          <*> (unwrap <$> arbitrary)
                          <*> (unwrap <$> arbitrary)
  shrink (OutlineBase _ b c d e) = OutlineBase <$> []
                                               <*> shrink b
                                               <*> shrink c
                                               <*> shrink d
                                               <*> (unwrap <$> shrink (OpmlGen e))

-- | Smart constructor for 'OutlineBase'.
mkOutlineBase :: NonNull Text -> OutlineBase
mkOutlineBase t = OutlineBase t mzero mzero mzero mzero


declareLenses [d|
  data OutlineSubscription = OutlineSubscription
    { xmlUri_ :: URI
    , htmlUri_ :: Maybe URI
    , description_ :: Text
    , language_ :: Text
    , subscriptionTitle_ :: Text
    , subscriptionVersion_ :: Text
    }
  |]

deriving instance Eq OutlineSubscription
deriving instance Generic OutlineSubscription
deriving instance Show OutlineSubscription
-- instance Hashable OutlineSubscription

instance Arbitrary OutlineSubscription where
  arbitrary = OutlineSubscription <$> (unwrap <$> arbitrary)
                                  <*> (unwrap <$> arbitrary)
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
  shrink (OutlineSubscription a b c d e f) = OutlineSubscription <$> (unwrap <$> shrink (OpmlGen a)) <*> (unwrap <$> shrink (OpmlGen b)) <*> shrink c <*> shrink d <*> shrink e <*> shrink f

-- | Smart constructor for 'OutlineSubscription'
mkOutlineSubscription :: URI -> OutlineSubscription
mkOutlineSubscription uri = OutlineSubscription uri mzero mempty mempty mempty mempty


-- | Outlines are the main payload of an OPML document.
declarePrisms [d|
  data OpmlOutline = OpmlOutlineGeneric OutlineBase Text
                   | OpmlOutlineLink OutlineBase URI
                   | OpmlOutlineSubscription OutlineBase OutlineSubscription
  |]

deriving instance Eq OpmlOutline
deriving instance Generic OpmlOutline
deriving instance Show OpmlOutline
-- instance Hashable OpmlOutline

instance Arbitrary OpmlOutline where
  arbitrary = oneof [ OpmlOutlineGeneric <$> arbitrary <*> arbitrary
                    , OpmlOutlineLink <$> arbitrary <*> (unwrap <$> arbitrary)
                    , OpmlOutlineSubscription <$> arbitrary <*> arbitrary
                    ]
  shrink (OpmlOutlineGeneric a b) = OpmlOutlineGeneric <$> shrink a <*> shrink b
  shrink (OpmlOutlineLink a b) = OpmlOutlineLink <$> shrink a <*> (unwrap <$> shrink (OpmlGen b))
  shrink (OpmlOutlineSubscription a b) = OpmlOutlineSubscription <$> shrink a <*> shrink b


declareLenses [d|
  data Opml = Opml
    { opmlVersion_ :: Version
    , head_ :: OpmlHead
    , outlines_ :: Forest OpmlOutline
    }
  |]

deriving instance Eq Opml
deriving instance Generic Opml
deriving instance Show Opml

instance Default Opml where
  def = Opml (makeVersion [2, 0]) def mempty

instance Arbitrary Opml where
  arbitrary = Opml <$> (unwrap <$> arbitrary)
                   <*> arbitrary
                   <*> listOf (genOutlineTree 1)
  shrink (Opml a b c) = Opml <$> (unwrap <$> shrink (OpmlGen a)) <*> shrink b <*> shrink c

-- | Generate a tree of outlines with the given maximum depth.
-- This generator makes sure that only 'OpmlOutlineGeneric' may have children.
genOutlineTree :: Int -> Gen (Tree OpmlOutline)
genOutlineTree n = do
  root <- arbitrary
  case (n > 1, root) of
    (True, OpmlOutlineGeneric _ _) -> Node <$> pure root <*> listOf (genOutlineTree (n-1))
    (False, OpmlOutlineGeneric _ _) -> return $ Node root []
    _ -> return $ Node root []

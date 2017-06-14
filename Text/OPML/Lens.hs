{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Text.OPML.Lens (module Text.OPML.Lens) where

-- {{{ Imports
import           Lens.Simple

import           Text.OPML.Types
-- }}}

-- * 'Opml' lenses
makeLensesFor
  [ ("opmlVersion", "opmlVersionL")
  , ("opmlHead", "opmlHeadL")
  , ("opmlOutlines", "opmlOutlinesL")
  ] ''Opml

-- * 'OpmlHead' lenses
makeLensesFor
  [ ("opmlTitle", "opmlTitleL")
  , ("opmlCreated", "opmlCreatedL")
  , ("modified", "modifiedL")
  , ("ownerName", "ownerNameL")
  , ("ownerEmail", "ownerEmailL")
  , ("ownerId", "ownerIdL")
  , ("docs", "docsL")
  -- , ("expansionState", "expansionStateL")
  , ("vertScrollState", "vertScrollStateL")
  , ("windowBottom", "windowBottomL")
  , ("windowLeft", "windowLeftL")
  , ("windowRight", "windowRightL")
  , ("windowTop", "windowTopL")
  ] ''OpmlHead

expansionStateL :: Traversal' OpmlHead Int
expansionStateL inj a@OpmlHead { expansionState = es } = (\x -> a { expansionState = x }) <$> traverse inj es
{-# INLINE expansionStateL #-}

-- * 'OutlineSubscription' lenses
makeLensesFor
  [ ("xmlUri", "xmlUriL")
  , ("htmlUri", "htmlUriL")
  , ("description", "descriptionL")
  , ("language", "languageL")
  , ("subscriptionTitle", "subscriptionTitleL")
  , ("subscriptionVersion", "subscriptionVersionL")
  ] ''OutlineSubscription

-- * 'OutlineBase' lenses
makeLensesFor
  [ ("text", "textL")
  , ("isComment", "isCommentL")
  , ("isBreakpoint", "isBreakpointL")
  , ("outlineCreated", "outlineCreatedL")
  , ("categories", "categoriesL")
  ] ''OutlineBase


-- * 'OpmlOutline' traversals
makeTraversals ''OpmlOutline

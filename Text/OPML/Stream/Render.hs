{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Streaming renderer for the OPML 2.0 standard.
module Text.OPML.Stream.Render
  ( -- * Renderers
    renderOpmlHead
  , renderOpmlOutline
  , renderOpml
  ) where

-- {{{ Imports
import           Control.Lens.At
import           Control.Lens.Getter
import           Control.Monad

import           Data.Conduit
import           Data.List.NonEmpty     hiding (filter, map)
import           Data.Monoid
import           Data.NonNull
import           Data.Text              (Text, intercalate, pack, toLower)
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC822
import           Data.Tree
import           Data.Version
import           Data.XML.Types

import           Prelude                hiding (foldr, lookup)

import           Text.OPML.Types
import           Text.XML.Stream.Render
-- }}}

tshow :: (Show a) => a -> Text
tshow = pack . show

empty :: (Eq s, Monoid s) => s -> Bool
empty t = t == mempty

toMaybe :: (Eq s, Monoid s) => s -> Maybe s
toMaybe s | s == mempty = mempty
          | otherwise   = Just s

formatTime :: UTCTime -> Text
formatTime = formatTimeRFC822 . utcToZonedTime utc

formatCategories :: [NonEmpty (NonNull Text)] -> Maybe Text
formatCategories = toMaybe . intercalate "," . map (intercalate "/" . toList . fmap toNullable)

formatBool :: Bool -> Text
formatBool = toLower . tshow

-- | Render the @\<head\>@ section.
renderOpmlHead :: (Monad m) => OpmlHead -> Source m Event
renderOpmlHead input = tag "head" mempty $ do
  forM_ (input^.opmlCreated_)       $ tag "dateCreated" mempty . content . formatTime
  forM_ (input^.modified_)          $ tag "dateModified" mempty . content . formatTime
  forM_ (input^.docs_)              $ tag "docs" mempty . content . tshow
  unless (null es) . tag "expansionState" mempty . content . intercalate "," $ tshow <$> es
  unless (empty email)              $ tag "ownerEmail" mempty $ content email
  forM_ (input^.ownerId_)           $ tag "ownerId" mempty . content . tshow
  unless (empty name)               $ tag "ownerName" mempty $ content name
  unless (empty title)              $ tag "title" mempty $ content title
  forM_ (input^.vertScrollState_)   $ tag "vertScrollState" mempty . content . tshow
  forM_ (input^.window_.at Bottom') $ tag "windowBottom" mempty . content . tshow
  forM_ (input^.window_.at Left')   $ tag "windowLeft" mempty . content . tshow
  forM_ (input^.window_.at Right')  $ tag "windowRight" mempty . content . tshow
  forM_ (input^.window_.at Top')    $ tag "windowTop" mempty . content . tshow
  where es = input ^. expansionState_
        email = input ^. ownerEmail_
        name = input ^. ownerName_
        title = input ^. opmlTitle_

-- | Render an @\<outline\>@ section.
renderOpmlOutline :: (Monad m) => Tree OpmlOutline -> Source m Event
renderOpmlOutline (Node outline subOutlines) = tag "outline" attributes $ mapM_ renderOpmlOutline subOutlines
  where attributes = case outline of
          OpmlOutlineGeneric b t -> baseAttr b <> optionalAttr "type" (toMaybe t)
          OpmlOutlineLink b uri -> baseAttr b <> attr "type" "link" <> attr "url" (tshow uri)
          OpmlOutlineSubscription b s -> baseAttr b <> subscriptionAttr s
        baseAttr b = attr "text" (toNullable $ b^.text_)
                     <> optionalAttr "isComment" (formatBool <$> b^.isComment_)
                     <> optionalAttr "isBreakpoint" (formatBool <$> b^.isBreakpoint_)
                     <> optionalAttr "created" (formatTime <$> b^.outlineCreated_)
                     <> optionalAttr "category" (formatCategories $ b^.categories_)
        subscriptionAttr s = attr "type" "rss"
                             <> attr "xmlUrl" (tshow $ s^.xmlUri_)
                             <> optionalAttr "htmlUrl" (tshow <$> s^.htmlUri_)
                             <> optionalAttr "description" (toMaybe $ s^.description_)
                             <> optionalAttr "language" (toMaybe $ s^.language_)
                             <> optionalAttr "title" (toMaybe $ s^.subscriptionTitle_)
                             <> optionalAttr "version" (toMaybe $ s^.subscriptionVersion_)

-- | Render the top-level @\<opml\>@ section.
renderOpml :: (Monad m) => Opml -> Source m Event
renderOpml opml = tag "opml" (attr "version" . pack . showVersion $ opml^.opmlVersion_) $ do
  renderOpmlHead $ opml^.head_
  tag "body" mempty . mapM_ renderOpmlOutline $ opml^.outlines_

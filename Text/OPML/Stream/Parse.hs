{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
-- | Streaming parser for the OPML 2.0 standard.
--
-- The parser tries to be as lenient as possible. All functions may throw an 'OpmlException'.
module Text.OPML.Stream.Parse
  ( -- * Parsers
    parseOpml
  , parseOpmlHead
  , parseOpmlOutline
  ) where

-- {{{ Imports
import           Control.Applicative
import           Control.Lens.At
import           Control.Lens.Setter
import           Control.Monad
import           Control.Monad.Catch

import           Data.CaseInsensitive         hiding (map)
import           Data.Conduit.Parser
import           Data.Conduit.Parser.XML
import           Data.Default
import           Data.List.NonEmpty           hiding (filter, map)
import           Data.Maybe
import           Data.Monoid
import           Data.Monoid.Textual          hiding (map)
import           Data.MonoTraversable
import           Data.NonNull
import           Data.Text                    (Text, strip, unpack)
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC822
import           Data.Tree
import           Data.Version
import           Data.XML.Types

import           Network.URI                  (URI, parseURI)

import           Numeric

import           Text.OPML.Types
import           Text.Parser.Combinators
import           Text.ParserCombinators.ReadP (readP_to_S)
-- }}}

data OpmlException = MissingText
                   | InvalidBool Text
                   | InvalidDecimal Text
                   | InvalidTime Text
                   | InvalidURI Text
                   | InvalidVersion Text

deriving instance Eq OpmlException
instance Show OpmlException where
  show MissingText = "An outline is missing the 'text' attribute."
  show (InvalidBool t) = "Invalid boolean: " ++ unpack t
  show (InvalidDecimal t) = "Invalid decimal: " ++ unpack t
  show (InvalidURI t) = "Invalid URI: " ++ unpack t
  show (InvalidTime t) = "Invalid time: " ++ unpack t
  show (InvalidVersion t) = "Invalid version: " ++ unpack t
instance Exception OpmlException

asURI :: (MonadThrow m) => Text -> m URI
asURI t = maybe (throwM $ InvalidURI t) return . parseURI $ unpack t

asVersion :: (MonadThrow m) => Text -> m Version
asVersion v = case filter (onull . snd) . readP_to_S parseVersion $ unpack v of
  [(a, "")] -> return a
  _ -> throwM $ InvalidVersion v

asDecimal :: (MonadThrow m, Integral a) => Text -> m a
asDecimal t = case filter (onull . snd) . readSigned readDec $ unpack t of
  (result, _):_ -> return result
  _             -> throwM $ InvalidDecimal t

asExpansionState :: (MonadThrow m, Integral a) => Text -> m [a]
asExpansionState t = mapM asDecimal . filter (not . onull) . map strip $ split (== ',') t

asTime :: (MonadThrow m) => Text -> m UTCTime
asTime t = maybe (throwM $ InvalidTime t) (return . zonedTimeToUTC) $ parseTimeRFC822 t

-- The standard only accepts "true", and "false",
-- but it doesn't hurt to be more lenient
asBool :: (MonadThrow m) => Text -> m Bool
asBool t
  | mk t == "true" = return True
  | mk t == "false" = return False
  | otherwise = throwM $ InvalidBool t

asNonNull :: (MonoFoldable mono, MonadThrow m) => mono -> m (NonNull mono)
asNonNull = maybe (throwM MissingText) return . fromNullable

asCategories :: Text -> [NonEmpty (NonNull Text)]
asCategories = mapMaybe (nonEmpty . mapMaybe fromNullable . split (== '/')) . split (== ',')


dateCreated, dateModified :: MonadCatch m => ConduitParser Event m UTCTime
dateCreated = tagName "dateCreated" ignoreAttrs $ \_ -> content asTime
dateModified = tagName "dateModified" ignoreAttrs $ \_ -> content asTime

docs, ownerId :: MonadCatch m => ConduitParser Event m URI
docs = tagName "docs" ignoreAttrs $ \_ -> content asURI
ownerId = tagName "ownerId" ignoreAttrs $ \_ -> content asURI

expansionState :: (MonadCatch m, Integral a) => ConduitParser Event m [a]
expansionState = tagName "expansionState" ignoreAttrs $ \_ -> content asExpansionState

ownerEmail, ownerName, opmlTitle :: MonadCatch m => ConduitParser Event m Text
ownerEmail = tagName "ownerEmail" ignoreAttrs $ const textContent
ownerName = tagName "ownerName" ignoreAttrs $ const textContent
opmlTitle = tagName "title" ignoreAttrs $ const textContent

vertScrollState, windowBottom, windowLeft, windowRight, windowTop :: (MonadCatch m, Integral a) => ConduitParser Event m a
vertScrollState = tagName "vertScrollState" ignoreAttrs $ \_ -> content asDecimal
windowBottom = tagName "windowBottom" ignoreAttrs $ \_ -> content asDecimal
windowLeft = tagName "windowLeft" ignoreAttrs $ \_ -> content asDecimal
windowRight = tagName "windowRight" ignoreAttrs $ \_ -> content asDecimal
windowTop = tagName "windowTop" ignoreAttrs $ \_ -> content asDecimal


opmlHeadBuilders :: MonadCatch m => [ConduitParser Event m (Endo OpmlHead)]
opmlHeadBuilders = [ liftM (Endo . set opmlCreated_ . Just) dateCreated
                   , liftM (Endo . set modified_ . Just) dateModified
                   , liftM (Endo . set docs_ . Just) docs
                   , liftM (Endo . set expansionState_) expansionState
                   , liftM (Endo . set ownerEmail_) ownerEmail
                   , liftM (Endo . set ownerId_ . Just) ownerId
                   , liftM (Endo . set ownerName_) ownerName
                   , liftM (Endo . set opmlTitle_) opmlTitle
                   , liftM (Endo . set vertScrollState_ . Just) vertScrollState
                   , liftM (Endo . set (window_.at Bottom') . Just) windowBottom
                   , liftM (Endo . set (window_.at Left') . Just) windowLeft
                   , liftM (Endo . set (window_.at Right') . Just) windowRight
                   , liftM (Endo . set (window_.at Top') . Just) windowTop
                   , unknown >> return mempty
                   ]
  where unknown = tagPredicate (const True) ignoreAttrs $ \_ -> return ()

-- | Parse the @\<head\>@ section.
-- This function is more lenient than what the standard demands on the following points:
--
-- - each sub-element may be repeated, in which case only the last occurrence is taken into account;
-- - each unknown sub-element is ignored.
parseOpmlHead :: (MonadCatch m) => ConduitParser Event m OpmlHead
parseOpmlHead = named "OPML <head> section" $ tagName "head" ignoreAttrs $ \_ -> do
  builders <- many $ choice opmlHeadBuilders
  return $ (appEndo $ mconcat builders) def

-- | Parse an @\<outline\>@ section.
-- The value of type attributes are not case-sensitive, that is @type=\"LINK\"@ has the same meaning as @type="link"@.
parseOpmlOutline :: (MonadCatch m) => ConduitParser Event m (Tree OpmlOutline)
parseOpmlOutline = tagName "outline" attributes handler <?> "OPML <outline> section" where
  attributes = do
    otype <- optional $ textAttr "type"
    case mk <$> otype of
      Just "include" -> (,,,) otype <$> baseAttr <*> pure Nothing <*> (Just <$> linkAttr) <* ignoreAttrs
      Just "link" -> (,,,) otype <$> baseAttr <*> pure Nothing <*> (Just <$> linkAttr) <* ignoreAttrs
      Just "rss" -> (,,,) otype <$> baseAttr <*> (Just <$> subscriptionAttr) <*> pure Nothing <* ignoreAttrs
      _          -> (,,,) otype <$> baseAttr <*> pure Nothing <*> pure Nothing <* ignoreAttrs
  baseAttr = (,,,,) <$> attr "text" asNonNull
                    <*> optional (attr "isComment" asBool)
                    <*> optional (attr "isBreakpoint" asBool)
                    <*> optional (attr "created" asTime)
                    <*> optional (attr "category" (Just . asCategories))
  linkAttr = textAttr "url"
  subscriptionAttr = (,,,,,) <$> attr "xmlUrl" asURI
                             <*> optional (attr "htmlUrl" asURI)
                             <*> optional (textAttr "description")
                             <*> optional (textAttr "language")
                             <*> optional (textAttr "title")
                             <*> optional (textAttr "version")
  handler (_, b, Just s, _) = Node <$> (OpmlOutlineSubscription <$> baseHandler b <*> pure (subscriptionHandler s)) <*> pure []
  handler (_, b, _, Just l) = Node <$> (OpmlOutlineLink <$> baseHandler b <*> asURI l) <*> pure []
  handler (otype, b, _, _) = Node <$> (OpmlOutlineGeneric <$> baseHandler b <*> pure (fromMaybe mempty otype))
                                  <*> many parseOpmlOutline
  baseHandler (text, comment, breakpoint, created, category) = return $ OutlineBase text comment breakpoint created (fromMaybe mempty category)
  subscriptionHandler (uri, html, description, language, title, version) = OutlineSubscription uri html (fromMaybe mempty description) (fromMaybe mempty language) (fromMaybe mempty title) (fromMaybe mempty version)

-- | Parse the top-level @\<opml\>@ element.
parseOpml :: (MonadCatch m) => ConduitParser Event m Opml
parseOpml = tagName "opml" attributes handler <?> "<opml> section" where
  attributes = attr "version" asVersion <* ignoreAttrs
  handler version = Opml version
                      <$> parseOpmlHead
                      <*> tagName "body" ignoreAttrs (const $ many parseOpmlOutline) <?> "<body> section."

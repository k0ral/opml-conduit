{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
-- | Streaming parser for the OPML 2.0 standard.
--
-- The parser tries to be as lenient as possible. All functions may throw an 'OpmlException'.
module Text.OPML.Conduit.Parse
  ( -- * Parsers
    parseOpml
  , parseOpmlHead
  , parseOpmlOutline
    -- * Exceptions
  , OpmlException(..)
  ) where

-- {{{ Imports
import           Control.Applicative
import           Control.Foldl                as Fold
import           Control.Monad
import           Control.Monad.Catch

import           Data.CaseInsensitive         hiding (map)
import           Data.Conduit.Parser
import           Data.Conduit.Parser.XML
import           Data.List.NonEmpty           (NonEmpty, nonEmpty)
import           Data.Maybe
import           Data.Monoid
import           Data.Monoid.Textual          hiding (map)
import           Data.MonoTraversable
import           Data.NonNull                 (NonNull, fromNullable)
import           Data.Text                    (Text, strip, unpack)
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC822
import           Data.Tree
import           Data.Version
import           Data.XML.Types

import           Lens.Simple

import           Numeric

import           Prelude                      hiding (last)

import           Text.OPML.Types
import           Text.Parser.Combinators
import           Text.ParserCombinators.ReadP (readP_to_S)

import           URI.ByteString
-- }}}

data OpmlException = MissingText
                   | InvalidBool Text
                   | InvalidDecimal Text
                   | InvalidTime Text
                   | InvalidURI URIParseError
                   | InvalidVersion Text

deriving instance Eq OpmlException
deriving instance Show OpmlException

instance Exception OpmlException where
  displayException MissingText = "An outline is missing the 'text' attribute."
  displayException (InvalidBool t) = "Invalid boolean: " ++ unpack t
  displayException (InvalidDecimal t) = "Invalid decimal: " ++ unpack t
  displayException (InvalidURI e) = "Invalid URI: " ++ show e
  displayException (InvalidTime t) = "Invalid time: " ++ unpack t
  displayException (InvalidVersion t) = "Invalid version: " ++ unpack t

asURI :: (MonadThrow m) => Text -> m URI
asURI t = either (throwM . InvalidURI) return . parseURI laxURIParserOptions $ encodeUtf8 t

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

dateTag :: (MonadCatch m) => Name -> ConduitParser Event m UTCTime
dateTag name = tagName name ignoreAttrs $ \_ -> content asTime

uriTag :: (MonadCatch m) => Name -> ConduitParser Event m URI
uriTag name = tagName name ignoreAttrs $ \_ -> content asURI

expansionStateTag :: (MonadCatch m, Integral a) => ConduitParser Event m [a]
expansionStateTag = tagName "expansionState" ignoreAttrs $ \_ -> content asExpansionState

textTag :: (MonadCatch m) => Name -> ConduitParser Event m Text
textTag name = tagName name ignoreAttrs $ const textContent

decimalTag :: (Integral a, MonadCatch m) => Name -> ConduitParser Event m a
decimalTag name = tagName name ignoreAttrs $ const $ content asDecimal

unknownTag :: (MonadCatch m) => ConduitParser Event m ()
unknownTag = tagPredicate (const True) ignoreAttrs $ \_ -> return ()


data HeadPiece = HeadCreated UTCTime
               | HeadModified UTCTime
               | HeadDocs URI
               | HeadExpansionState [Int]
               | HeadOwnerEmail Text
               | HeadOwnerId URI
               | HeadOwnerName Text
               | HeadTitle Text
               | HeadVertScrollState Int
               | HeadWindowBottom Int
               | HeadWindowLeft Int
               | HeadWindowRight Int
               | HeadWindowTop Int
               | HeadUnknown

makeTraversals ''HeadPiece


-- | Parse the @\<head\>@ section.
-- This function is more lenient than what the standard demands on the following points:
--
-- - each sub-element may be repeated, in which case only the last occurrence is taken into account;
-- - each unknown sub-element is ignored.
parseOpmlHead :: (MonadCatch m) => ConduitParser Event m OpmlHead
parseOpmlHead = named "OPML <head> section" $ tagName "head" ignoreAttrs $ \_ -> do
  p <- many $ choice piece
  return $ flip fold p $ OpmlHead
    <$> handles _HeadTitle (lastDef mempty)
    <*> handles _HeadCreated last
    <*> handles _HeadModified last
    <*> handles _HeadOwnerName (lastDef mempty)
    <*> handles _HeadOwnerEmail (lastDef mempty)
    <*> handles _HeadOwnerId last
    <*> handles _HeadDocs last
    <*> handles _HeadExpansionState Fold.mconcat
    <*> handles _HeadVertScrollState last
    <*> handles _HeadWindowBottom last
    <*> handles _HeadWindowLeft last
    <*> handles _HeadWindowRight last
    <*> handles _HeadWindowTop last
  where piece = [ HeadCreated <$> dateTag "dateCreated"
                , HeadModified <$> dateTag "dateModified"
                , HeadDocs <$> uriTag "docs"
                , HeadExpansionState <$> expansionStateTag
                , HeadOwnerEmail <$> textTag "ownerEmail"
                , HeadOwnerId <$> uriTag "ownerId"
                , HeadOwnerName <$> textTag "ownerName"
                , HeadTitle <$> textTag "title"
                , HeadVertScrollState <$> decimalTag "vertScrollState"
                , HeadWindowBottom <$> decimalTag "windowBottom"
                , HeadWindowLeft <$> decimalTag "windowLeft"
                , HeadWindowRight <$> decimalTag "windowRight"
                , HeadWindowTop <$> decimalTag "windowTop"
                , HeadUnknown <$ unknownTag
                ]


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
  baseHandler (txt, comment, breakpoint, created, category) = return $ OutlineBase txt comment breakpoint created (fromMaybe mempty category)
  subscriptionHandler (uri, html, desc, lang, title, version) = OutlineSubscription uri html (fromMaybe mempty desc) (fromMaybe mempty lang) (fromMaybe mempty title) (fromMaybe mempty version)

-- | Parse the top-level @\<opml\>@ element.
parseOpml :: (MonadCatch m) => ConduitParser Event m Opml
parseOpml = tagName "opml" attributes handler <?> "<opml> section" where
  attributes = attr "version" asVersion <* ignoreAttrs
  handler version = Opml version
                      <$> parseOpmlHead
                      <*> tagName "body" ignoreAttrs (const $ many parseOpmlOutline) <?> "<body> section."

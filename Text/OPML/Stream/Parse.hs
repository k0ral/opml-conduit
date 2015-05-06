{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
-- | Streaming parser for the OPML 2.0 standard.
--
-- The parser tries to be as lenient as possible. All functions may throw an 'OpmlException'.
module Text.OPML.Stream.Parse
  ( -- * Parsers
    parseOpml
  , parseOpmlHead
  , parseOpmlOutline
    -- * Exceptions
  , OpmlException(..)
  ) where

-- {{{ Imports
import           Control.Lens.At
import           Control.Lens.Setter
import           Control.Monad
import           Control.Monad.Catch

import           Data.CaseInsensitive         hiding (map)
import           Data.Conduit
import           Data.Containers
import           Data.Map                     (Map)
import           Data.Maybe
import           Data.Monoid.Textual          hiding (map)
import           Data.MonoTraversable
import           Data.NotEmpty
import           Data.Text                    (Text, strip, unpack)
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC822
import           Data.Tree
import           Data.Version
import           Data.XML.Types

import           Network.URI                  (URI)
import qualified Network.URI                  as N

import           Numeric

import           Prelude                      hiding (foldr, lookup)

import           Text.OPML.Types
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.XML.Stream.Parse
-- }}}

data OpmlException = InvalidBool Text
                   | InvalidDecimal Text
                   | InvalidTime Text
                   | InvalidURI Text
                   | InvalidVersion Text

deriving instance Eq OpmlException
instance Show OpmlException where
  show (InvalidBool t) = "Invalid boolean: " ++ unpack t
  show (InvalidDecimal t) = "Invalid decimal: " ++ unpack t
  show (InvalidURI t) = "Invalid URI: " ++ unpack t
  show (InvalidTime t) = "Invalid time: " ++ unpack t
  show (InvalidVersion t) = "Invalid version: " ++ unpack t
instance Exception OpmlException

-- | Generic version of 'Network.URI.parseURI'.
parseURI :: (MonadThrow m) => Text -> m URI
parseURI t = maybe (throwM $ InvalidURI t) return . N.parseURI $ unpack t

parseVersion' :: (MonadThrow m) => Text -> m Version
parseVersion' v = case filter (onull . snd) . readP_to_S parseVersion $ unpack v of
  [(a, "")] -> return a
  _ -> throwM $ InvalidVersion v

parseDecimal :: (MonadThrow m, Integral a) => Text -> m a
parseDecimal t = case filter (onull . snd) . readSigned readDec $ unpack t of
  (result, _):_ -> return result
  _             -> throwM $ InvalidDecimal t

parseExpansionState :: (MonadThrow m, Integral a) => Text -> m [a]
parseExpansionState t = mapM parseDecimal . filter (not . onull) . map strip $ split (== ',') t

parseTime :: (MonadThrow m) => Text -> m UTCTime
parseTime t = maybe (throwM $ InvalidTime t) (return . zonedTimeToUTC) $ parseTimeRFC822 t

-- The standard only accepts "true", and "false",
-- but it doesn't hurt to be more lenient
parseBool :: (MonadThrow m) => Text -> m Bool
parseBool t
  | mk t == "true" = return True
  | mk t == "false" = return False
  | otherwise = throwM $ InvalidBool t


opmlHeadBuilder :: (MonadThrow m) => Map Text (Text -> OpmlHead -> m OpmlHead)
opmlHeadBuilder = [ ("dateCreated",     \v h -> set opmlCreated_ <$> (Just <$> parseTime v) <*> pure h)
                  , ("dateModified",    \v h -> set modified_ <$> (Just <$> parseTime v) <*> pure h)
                  , ("docs",            \v h -> set docs_ <$> (Just <$> parseURI v) <*> pure h)
                  , ("expansionState",  \v h -> set expansionState_ <$> parseExpansionState v <*> pure h)
                  , ("ownerEmail",      \v -> return . set ownerEmail_ v)
                  , ("ownerId",         \v h -> set ownerId_ <$> (Just <$> parseURI v) <*> pure h)
                  , ("ownerName",       \v -> return . set ownerName_ v)
                  , ("title",           \v -> return . set opmlTitle_ v)
                  , ("vertScrollState", \v h -> set vertScrollState_ <$> (Just <$> parseDecimal v) <*> pure h)
                  , ("windowBottom",    \v h -> set (window_.at Bottom') <$> (Just <$> parseDecimal v) <*> pure h)
                  , ("windowLeft",      \v h -> set (window_.at Left') <$> (Just <$> parseDecimal v) <*> pure h)
                  , ("windowRight",     \v h -> set (window_.at Right') <$> (Just <$> parseDecimal v) <*> pure h)
                  , ("windowTop",       \v h -> set (window_.at Top') <$> (Just <$> parseDecimal v) <*> pure h)
                  ]

-- | Parse the @\<head\>@ section.
-- This function is more lenient than what the standard demands on the following points:
--
-- - each sub-element may be repeated, in which case only the last occurrence is taken into account;
-- - each unknown sub-element is ignored.
parseOpmlHead :: (MonadThrow m) => Consumer Event m (Maybe OpmlHead)
parseOpmlHead = tagName "head" ignoreAttrs $ \_ -> foldM (\a f' -> f' a) def =<< many (tagHead `orE` unknownTag)
  where tagHead, unknownTag :: (MonadThrow m, MonadThrow m') => ConduitM Event o m (Maybe (OpmlHead -> m' OpmlHead))
        tagHead = tag ((`lookup` opmlHeadBuilder) . nameLocalName) (\f -> ignoreAttrs *> pure f) $ \f -> do
                    c <- content
                    return $ f c
        unknownTag = tagPredicate (const True) ignoreAttrs $ \_ -> return return


parseCategories :: Text -> [[NE Text]]
parseCategories = filter (not . onull) . map (mapMaybe notEmpty . split (== '/')) . split (== ',')

-- | Parse an @\<outline\>@ section.
-- The value of type attributes are not case-sensitive, that is @type=\"LINK\"@ has the same meaning as @type="link"@.
parseOpmlOutline :: (MonadThrow m) => Consumer Event m (Maybe (Tree OpmlOutline))
parseOpmlOutline = tagName "outline" attributes handler
  where attributes = do
          otype <- optionalAttr "type"
          case mk <$> otype of
            Just "include" -> (,,,) otype <$> baseAttr <*> pure Nothing <*> (Just <$> linkAttr) <* ignoreAttrs
            Just "link" -> (,,,) otype <$> baseAttr <*> pure Nothing <*> (Just <$> linkAttr) <* ignoreAttrs
            Just "rss" -> (,,,) otype <$> baseAttr <*> (Just <$> subscriptionAttr) <*> pure Nothing <* ignoreAttrs
            _          -> (,,,) otype <$> baseAttr <*> pure Nothing <*> pure Nothing <* ignoreAttrs
        baseAttr = (,,,,) <$> requireAttr "text"
                          <*> optionalAttr "isComment"
                          <*> optionalAttr "isBreakpoint"
                          <*> optionalAttr "created"
                          <*> optionalAttr "category"
        linkAttr = requireAttr "url"
        subscriptionAttr = (,,,,,) <$> requireAttr "xmlUrl"
                                   <*> optionalAttr "htmlUrl"
                                   <*> optionalAttr "description"
                                   <*> optionalAttr "language"
                                   <*> optionalAttr "title"
                                   <*> optionalAttr "version"
        handler (_, b, Just s, _) = Node <$> (OpmlOutlineSubscription <$> baseHandler b <*> subscriptionHandler s) <*> pure []
        handler (_, b, _, Just l) = Node <$> (OpmlOutlineLink <$> baseHandler b <*> parseURI l) <*> pure []
        handler (otype, b, _, _) = Node <$> (OpmlOutlineGeneric <$> baseHandler b <*> pure (fromMaybe mempty otype))
                                           <*> many parseOpmlOutline
        baseHandler (t, comment, breakpoint, created, category) =
          OutlineBase <$> notEmpty t
                      <*> pure (parseBool =<< comment)
                      <*> pure (parseBool =<< breakpoint)
                      <*> pure (parseTime =<< created)
                      <*> pure (parseCategories =<< otoList category)
        subscriptionHandler (uri, html, description, language, title, version) =
          OutlineSubscription <$> parseURI uri
                              <*> pure (parseURI =<< html)
                              <*> pure (fromMaybe mempty description)
                              <*> pure (fromMaybe mempty language)
                              <*> pure (fromMaybe mempty title)
                              <*> pure (fromMaybe mempty version)

-- | Parse the top-level @\<opml\>@ element.
parseOpml :: (MonadThrow m) => Consumer Event m (Maybe Opml)
parseOpml = tagName "opml" attributes handler
  where attributes = requireAttr "version" <* ignoreAttrs
        handler version = Opml <$> parseVersion' version
                               <*> force "Missing <head>." parseOpmlHead
                               <*> force "Missing <body>." (tagName "body" ignoreAttrs $ \_ -> many parseOpmlOutline)

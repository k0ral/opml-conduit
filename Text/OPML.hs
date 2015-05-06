-- | This module re-exports commonly used modules:
--
-- - 'Text.OPML.Stream.Parse'
-- - 'Text.OPML.Stream.Render'
-- - 'Text.OPML.Types'
module Text.OPML (module X) where

import           Text.OPML.Stream.Parse  as X
import           Text.OPML.Stream.Render as X
import           Text.OPML.Types         as X

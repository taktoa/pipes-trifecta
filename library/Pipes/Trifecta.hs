module Pipes.Trifecta where

import qualified Data.Text               as Text

import qualified Data.ByteString         as BS

import qualified Pipes.Parse             as Pipes
import qualified Pipes.Prelude           as Pipes

import qualified Text.Parser.Char        as Parsers
import qualified Text.Parser.Combinators as Parsers
import qualified Text.Parser.Token       as Parsers

-- try   :: m a -> m a
-- (<?>) :: m a -> String -> m a
-- skipMany :: m a -> m ()
-- skipSome :: m a -> m ()
-- unexpected :: String -> m a
-- eof :: m ()
-- notFollowedBy :: Show a => m a -> m ()

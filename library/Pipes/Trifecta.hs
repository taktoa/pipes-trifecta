{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE Trustworthy      #-}

-- | FIXME: write module documentation
module Pipes.Trifecta (parse) where

import           Data.ByteString      (ByteString, pack)
import           Data.Word            (Word8)

import           Control.Lens.Cons    (Cons, uncons)

import qualified Pipes.Parse          as P (Parser, draw, unDraw)

import qualified Text.Trifecta.Delta  as Tri (Delta)
import qualified Text.Trifecta.Result as Tri (Result)

import qualified Text.Trifecta.Parser as Tri (Parser, Step (..), feed, starve,
                                              stepParser)

-- | Given an initial 'Tri.Delta' and a 'Tri.Parser' (from @trifecta@),
-- a 'P.Parser' (from @pipes-parse@) is created, which will receive
-- 'ByteString'-like objects (specifically, anything that implements
-- the 'Control.Lens.Cons' typeclass) and spit out parser results
-- whenever the parser succeeds or fails.
--
-- FIXME: add examples
parse :: (Monad m, Cons s s Word8 Word8)
         => Tri.Delta -> Tri.Parser x -> P.Parser s m (Tri.Result x)
parse d p = P.draw >>= \case Just i  -> go (Tri.stepParser p d (toBS i)) Nothing
                             Nothing -> parse d p
  where
    go s@(Tri.StepFail _ _) Nothing  = return $ Tri.starve s
    go s@(Tri.StepDone _ _) Nothing  = return $ Tri.starve s
    go s                    Nothing  = P.draw >>= go s
    go s@(Tri.StepFail _ _) (Just i) = P.unDraw i >> go s Nothing
    go s@(Tri.StepDone _ _) (Just i) = P.unDraw i >> go s Nothing
    go s                    (Just i) = P.draw >>= go (Tri.feed (toBS i) s)

-- Helper function for converting 'Word8' streams into 'ByteString's
toBS :: (Cons s s Word8 Word8) => s -> ByteString
toBS = pack . go
  where
    go s = case uncons s of Just (h, t) -> h : go t
                            Nothing     -> []

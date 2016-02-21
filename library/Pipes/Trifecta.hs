{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipes.Trifecta where

import           Control.Exception
import           Control.Monad.Catch

import           Data.Foldable

import           Data.Word                   (Word8)

import qualified Control.Lens                as Lens
import qualified Control.Lens.Cons           as Lens

import qualified Data.ByteString             as BS
import qualified Data.Text                   as Text

import qualified Pipes                       as P
import qualified Pipes.Parse                 as P
import qualified Pipes.Prelude               as P

import qualified Text.Trifecta.Combinators   as Tri
import qualified Text.Trifecta.Delta         as Tri
import qualified Text.Trifecta.Parser        as Tri
import qualified Text.Trifecta.Result        as Tri

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import           Text.Parser.Token
import           Text.Parser.Token.Highlight

type TriParser x = (Tri.Delta, Tri.Parser x)

type Cons' s t = Lens.Cons s s t t

parseMaybe :: (Monad m, Cons' s Word8)
              => Tri.Delta -> Tri.Parser x -> P.Parser s m (Maybe x)
parseMaybe d p = resultToThrow <$> parse d p

data ParseException = ParseException String
                    deriving (Show)

instance Exception ParseException

resultToThrow :: (MonadThrow m) => Tri.Result x -> m x
resultToThrow (Tri.Success x) = return x
resultToThrow (Tri.Failure d) = throwM $ ParseException $ show d

resultToEither :: Tri.Result x -> Either Doc x
resultToEither (Tri.Success x) = Right x
resultToEither (Tri.Failure d) = Left d

parse :: forall m s x. (Monad m, Cons' s Word8)
         => Tri.Delta -> Tri.Parser x -> P.Parser s m (Tri.Result x)
parse d p = do input <- P.draw
               case input of
                 Just i  -> go (Tri.stepParser p d (consToBS i)) Nothing
                 Nothing -> parse d p
  where
    go :: Tri.Step x -> Maybe s -> P.Parser s m (Tri.Result x)
    go s@(Tri.StepDone _ _) (Just i) = P.unDraw i >> go s Nothing
    go s@(Tri.StepDone _ _) Nothing  = return $ Tri.starve s
    go s                    (Just i) = P.draw >>= go (Tri.feed (consToBS i) s)
    go s                    Nothing  = P.draw >>= go s

consToBS :: (Cons' s Word8) => s -> BS.ByteString
consToBS = BS.pack . consToList

consToList :: (Cons' s t) => s -> [t]
consToList = go
  where
    go s = case Lens.uncons s of Just (h, t) -> h : go t
                                 Nothing     -> []

{-
parse' (delta, parser) = do
  input <- P.await
  case input
    of Just i  -> go i
       Nothing ->
  let chunkSize = 16
  let go s state = case chunkBS chunkSize s
                     of Just (bs, rest)
                          -> go rest $ Tri.stepParser parser delta bs
                        Nothing -> return ()
  go input
-}

chunkBS :: (Integral i, Cons' s Word8) => i -> s -> Maybe (BS.ByteString, s)
chunkBS size stream = case chunk size stream
                      of ([], _) -> Nothing
                         (xs, s) -> Just (BS.pack xs, s)

chunk :: forall i s t. (Integral i, Lens.Cons s s t t) => i -> s -> ([t], s)
chunk size stream = if size > 0
                    then go size ([], stream)
                    else error "chunk: chunk size must be positive"
  where
    go :: i -> ([t], s) -> ([t], s)
    go 0 (ts, s) = (reverse ts, s)
    go n (ts, s) = case Lens.uncons s
                   of Just (sh, st) -> go (n - 1) (sh : ts, st)
                      Nothing       -> go 0       (ts,       s)





























data Expr v = EVar      !v
            | EPi       !v !(Expr v) !(Expr v)
            | ELambda   !v !(Expr v) !(Expr v)
            | EApp      !(Expr v) !(Expr v)
            | EUniverse !Int
            deriving (Eq, Show)

type V = String

piSym :: V
piSym     = "pi"     -- "Π"
lambdaSym :: V
lambdaSym = "lambda" -- "λ"

variableStyle :: IdentifierStyle Tri.Parser
variableStyle = IdentifierStyle
                { _styleName              = "variable"
                , _styleStart             = letter
                , _styleLetter            = alphaNum
                , _styleReserved          = [piSym, lambdaSym]
                , _styleHighlight         = Identifier
                , _styleReservedHighlight = ReservedIdentifier }

variableP :: Tri.Parser V
variableP = ident variableStyle

referenceP :: Tri.Parser (Expr V)
referenceP = EVar <$> variableP

universeP :: Tri.Parser (Expr V)
universeP = EUniverse . fromInteger <$> (text "Set" >> natural)

abstractionP :: String -> (V -> Expr V -> Expr V -> Expr V) -> Tri.Parser (Expr V)
abstractionP name constructor = do symbol name
                                   v <- variableP
                                   colon
                                   t <- exprP
                                   dot
                                   e <- exprP
                                   return $ constructor v t e

piP :: Tri.Parser (Expr V)
piP = abstractionP piSym EPi

lambdaP :: Tri.Parser (Expr V)
lambdaP = abstractionP lambdaSym ELambda

appP :: Tri.Parser (Expr V)
appP = do lhs <- exprP
          someSpace
          rhs <- exprP
          return $ EApp lhs rhs

exprP :: Tri.Parser (Expr V)
exprP = choice [ universeP
               , piP
               , lambdaP
               , parens exprP
               , referenceP
               , appP ]

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Class ( gets, modify, MonadState )
import Control.Monad.State.Lazy
import Control.Monad.Writer.Class ( MonadWriter(tell) )
import Data.Semigroup ( Sum(Sum) )
import Control.Monad.RWS.Lazy
import Control.Selective ( anyS, Selective )
import Data.List
main :: IO ()
main = do
    print 1
    print $ evalRWS (tick Player1 example) () Map.empty
    print $ evalRWS (tick Player1 goal) () Map.empty

data Player = Player1 | Player2 deriving (Eq, Show)

toggle :: Player -> Player
toggle Player1 = Player2
toggle Player2 = Player1

data Color = R | G | B | U deriving (Eq, Ord, Show, Enum, Bounded)

data S = S Int Color deriving (Eq, Ord, Show)

type Board = Map S Int

example :: Board
example = Map.fromList [(S 1 R, 2), (S 1 G, 2)]

goal :: Board
goal = Map.fromList [(S 1 R, 4), (S 1 G, 4), (S 1 B, 4), (S 1 U, 4)]

pullDown :: S -> Map S Int -> Map S Int
pullDown = Map.update (\v -> if v ==1 then Nothing else Just (v-1))

moves :: (MonadWriter (Sum Int) m) => Board -> m [Board -> Board]
moves b = let
    bb = Map.toList b
    k = Map.keys b
    -- bb = Map.toList (traceMe b)
    self = [Map.update (\v -> if v == 2 then Nothing else Just (v-2)) s . ins(S (2*h) c) | (s@(S h c), count)<-bb, count >= 2]
    sameHeight = concat [ [pullDown s1 . pullDown s2 . ins (S (h1+h2) c1), pullDown s1 . pullDown s2 . ins (S (h1+h2) c2) ] | s1@(S h1 c1) <- k, s2@(S h2 c2) <- k, h1 == h2, c1 < c2]
    sameColor = [ pullDown s1 . pullDown s2 . ins (S (h1+h2) c1) | s1@(S h1 c1) <- k, s2@(S h2 c2) <- k, h1 < h2, c1 == c2]
    in
        self <> sameHeight <> sameColor <$ tell (Sum 1)

tick :: (MonadState (Map Signature Player) m, MonadWriter (Sum Int) m, Selective m) =>  Player -> Board -> m Player
-- tick :: (MonadState (Map Board Player) m, MonadWriter (Sum Int) m, Selective m) =>  Player -> Board -> m Player
tick p b = do
    let sig = sign b
    seen <- gets $ Map.lookup sig
    case seen of
        Just p -> pure p
        Nothing -> do
            validMoves <- moves b
            if null validMoves then toggle p <$ modify (Map.insert sig (toggle p)) else do
                z <- anyS (\mv -> ( p ==) <$> tick (toggle p) mv) (validMoves <*> [b])
                let answ = if z then p else toggle p
                answ <$ modify (Map.insert sig answ)

ins :: S -> Board -> Board
ins s = Map.insertWith (+) s 1

type Signature = [[(Int, Int)]]

sign :: Board -> Signature
sign b = let
    kv = sort $ Map.toList b
    
    gb = groupBy (\(S _ c1, _) (S _ c2, _) -> c1 ==c2) kv
    z = fmap (\(S h _, v) -> (h, v)) <$> gb
    in
        sort $ fmap sort z
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Default
import Data.Maybe

main :: IO ()
main = print (def :: Board)

data Player = X | O deriving (Show, Eq, Ord)

newtype Value = V (Maybe Player)

instance Show Value where
  show (V Nothing) = " "
  show (V (Just X)) = "X"
  show (V (Just O)) = "O"

data Board = B {
  nw :: Maybe Player
  , n :: Maybe Player
  , ne :: Maybe Player
  , w :: Maybe Player
  , r :: Maybe Player
  , e :: Maybe Player
  , sw :: Maybe Player
  , s :: Maybe Player
  , se :: Maybe Player
  } deriving (Eq, Ord)

instance Default Board where
  def = B
    def def def
    def def def
    def def def
addMove :: Board -> Move -> Player -> Board
addMove b NW p = if isNothing $ nw b then b {nw = Just p} else b
addMove b N p = if isNothing $ n b then b {n = Just p} else b
addMove b NE p = if isNothing $ ne b then b {ne = Just p} else b
addMove b W p = if isNothing $ w b then b {w = Just p} else b
addMove b R p = if isNothing $ r b then b {r = Just p} else b
addMove b E p = if isNothing $ e b then b {e = Just p} else b
addMove b SW p = if isNothing $ sw b then b {sw = Just p} else b
addMove b S p = if isNothing $ s b then b {s = Just p} else b
addMove b SE p = if isNothing $ se b then b {se = Just p} else b

data Move =
  NW | N | NE |
  W  | R | E |
  SW | S | SE

instance Show Board where
  show b = unlines $ concat $ map (map (show . V)) [[nw b,n b,ne b],[w b,r b,e b],[sw b,s b,se b]]

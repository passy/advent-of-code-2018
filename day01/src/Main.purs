module Main where

import Prelude

import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(Nothing))
import Data.String (CodePoint, uncons)
import Data.String.CodePoints (codePointFromChar)
import Data.String.Yarn (lines)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

data Op = Add Int
        | Subtract Int

minus :: CodePoint
minus = codePointFromChar '-'

plus :: CodePoint
plus = codePointFromChar '+'

parse :: String -> Maybe Op
parse str = do
  {head: h, tail: t} <- uncons str
  parseOp h t

parseOp :: CodePoint -> String -> Maybe Op
parseOp x xs | x == minus = Subtract <$> Int.fromString xs
             | x == plus  = Add      <$> Int.fromString xs
             | otherwise  = Nothing

reduceOp :: Int -> Op -> Int
reduceOp b (Add a) = b + a
reduceOp b (Subtract a) = b - a

main :: Effect Unit
main = do
  logShow =<< map (foldl reduceOp 0)
          <$> sequence
          <$> map parse
          <$> lines
          <$> readTextFile UTF8 "input.txt"
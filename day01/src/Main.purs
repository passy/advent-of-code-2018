module Main where

import Prelude

import Data.Either (Either(Left, Right))
import Data.Foldable (foldl)
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Int as Int
import Data.List.Lazy as List
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.String (CodePoint, uncons)
import Data.String.CodePoints (codePointFromChar)
import Data.String.Yarn (lines)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
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

part1 :: Effect Unit
part1 =
  logShow =<< map (foldl reduceOp 0)
          <$> sequence
          <$> map parse
          <$> lines
          <$> readTextFile UTF8 "input.txt"

type ReduceState =
  { set :: HashSet Int
  , sum :: Int
  }

reduceRepeat :: ReduceState -> Op -> Either ReduceState ReduceState
reduceRepeat {set, sum} op =
  let sum' = reduceOp sum op
      set' = HashSet.insert sum' set
      res  = {set: set', sum: sum'}
  in
    if HashSet.member sum' set
    then Left res
    else Right res

part2 :: Effect Unit
part2 = do
  ops <- fromMaybe []
      <$> sequence
      <$> map parse
      <$> lines
      <$> readTextFile UTF8 "input.txt"

  let res = List.foldM reduceRepeat { set: HashSet.empty, sum: 0 } $ List.cycle $ List.fromFoldable ops
  case res of
    Right _ -> log "I fucked up."
    Left {sum} -> logShow sum

main :: Effect Unit
main = do
  part1
  part2
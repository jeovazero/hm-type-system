module HM.Result where

import Data.List

data Result a = Ok a | Fail String deriving (Show)

everyOrFail :: [Result a] -> Result [a]

everyOrFail [] = Ok []
everyOrFail (x:xs) = mapResult reverse $ foldl' accOrFail x' xs
  where x' = mapResult (: []) x

accOrFail :: Result [a] -> Result a -> Result [a]
accOrFail (Fail f) _ = Fail f
accOrFail (Ok acc) (Ok c) = Ok (c:acc)
accOrFail (Ok _) (Fail f) = Fail f

continueOrFail :: Result a -> Result a -> Result a
continueOrFail ff@(Fail _) _ = ff
continueOrFail (Ok _) r = r

mapResult :: (t -> a) -> Result t -> Result a
mapResult _ (Fail e) = Fail e
mapResult g (Ok a) = Ok $ g a

flattenResult :: Result (Result a) -> Result a
flattenResult (Ok (Ok a)) = Ok a
flattenResult (Ok (Fail f)) = Fail f
flattenResult (Fail f) = Fail f

constResult a = mapResult (const a)


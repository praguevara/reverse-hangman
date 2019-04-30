{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict
import Control.Monad.Trans (lift)
import Data.Text
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Text.IO (readFile)

data GuessStructure = GuessStructure String [Maybe Char]
  deriving Show

validCharacters :: String
validCharacters = ['a'..'z']

initialGuessStructure :: Int -> GuessStructure
initialGuessStructure len = GuessStructure validCharacters $ Prelude.replicate len Nothing

setLetter :: GuessStructure -> Char -> Int -> GuessStructure
setLetter (GuessStructure s ls) c p = GuessStructure (delete c s) $ Prelude.init a ++ Just c : b
  where (a, b) = Prelude.splitAt p ls

removeLetter :: GuessStructure -> Char -> GuessStructure
removeLetter (GuessStructure s ls) c = GuessStructure (delete c s) ls

readWords :: IO [Text]
readWords = Data.Text.words <$> Data.Text.IO.readFile "words.txt"

matchWord :: GuessStructure -> Text -> Bool
matchWord (GuessStructure s ls) w = Prelude.length ls == Data.Text.length w &&
  and (Prelude.zipWith (\l m -> case m of
    Nothing -> True
    Just c -> c == l) (unpack w) ls)

filterWords :: GuessStructure -> [Text] -> [Text]
filterWords = Prelude.filter . matchWord

getPositions :: IO [Int]
getPositions = do
  i <- getLine
  return $ read <$> Prelude.words i

newtype WordsWithLetter = WordsWithLetter (Map.Map Char Int)
  deriving Show

instance Semigroup WordsWithLetter where
  (<>) (WordsWithLetter a) (WordsWithLetter b) = WordsWithLetter $ Map.unionWith (+) a b

instance Monoid WordsWithLetter where
  mempty = WordsWithLetter Map.empty

countLetters :: Text -> WordsWithLetter
countLetters t = WordsWithLetter . Map.fromList $ Prelude.zip (unpack t) (repeat 1)

mostFrequentLetters :: WordsWithLetter -> String
mostFrequentLetters (WordsWithLetter m) = fst <$> sortBy (\(_, a) (_, b) -> compare b a)  (Map.toList m)

mostFrequentLettersInWords :: [Text] -> String
mostFrequentLettersInWords ws = mostFrequentLetters . mconcat $ countLetters <$> ws

guess :: GameState -> Maybe Char
guess (GameState _ ts _ ws) = case gs of
  [] -> Nothing
  (a:_) -> Just a
  where gs = mostFrequentLettersInWords ws \\ ts

data GameState = GameState {
  wrongs :: Int
, tried :: String
, guessStructure :: GuessStructure
, possibleWords :: [Text]
}

initialGameState :: Int -> IO GameState
initialGameState l = GameState 0 [] g <$> (filterWords g <$> readWords)
  where g = initialGuessStructure l

updateGameState :: Char -> [Int] -> GameState -> GameState
updateGameState c ps g = do
  let r = removeLetter (guessStructure g) c
  let i = Prelude.foldl (\a p -> setLetter a c p) r
  case ps of
    [] -> GameState (wrongs g + 1) (ts g) r (filterWords r (possibleWords g))
    xs -> GameState (wrongs g) (ts g) (i xs) (filterWords (i xs) (possibleWords g))
    where ts g = tried g ++ [c]
    
printGameState :: GameState -> IO ()
printGameState g@(GameState w t (GuessStructure _ l) pw) = mapM_ putStrLn
  [Prelude.replicate 20 '-',
  "Errors: " ++ show w,
  "Tried: " ++ show t,
  "Guessed: " ++ show (formatGap <$> l)]
  where
    formatGap (Just c) = c
    formatGap Nothing = '_'

getLength :: IO Int
getLength = do
  putStrLn "Input word length:"
  read <$> getLine

isWin :: GameState -> Bool
isWin (GameState _ _ (GuessStructure _ ls) _) = Prelude.all isJust ls

play :: GameState -> IO ()
play = evalStateT loop
  where 
    loop = do
      s <- get
      let g = guess s
      case g of
        Nothing -> do
          lift $ putStrLn "I don't know that word!"
          lift . print $ guessStructure s
        Just c -> do
          lift . putStrLn $ "I guess \'" ++ [c] ++ "\'"
          ps <- lift getPositions
          modify (updateGameState c ps)
          s <- get
          lift $ printGameState s
          if isWin s then
            lift $ putStrLn "I win!"
          else
            loop

main :: IO ()
main = do
  l <- getLength
  g <- initialGameState l
  printGameState g
  play g

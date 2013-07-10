-- Creates a 'spoonerized' sentence from a normal one (with beginning consonants of two words flipped).
-- Justin Leitgeb justin AT stackbuilders.com
-- Jul 10 2013

import System.Random
import Data.Char (toLower)
import Data.Array.IO
import Control.Monad
import Data.List (sort, intercalate)

-- Example run in ghci:
-- λ: main
-- Enter a sentence to spoonerize:
-- Stack Builders' R&D is the best thing in the world
-- Your spoonerized sentence:
-- "back Builders' R&D is the Stest thing in the world"


type Sequence    = Int
type Word        = String
type IsSpoonable = Bool

data WordInfo = WordInfo Sequence Word IsSpoonable
                deriving (Show)

instance Ord WordInfo where
  (WordInfo seq1 _ _) `compare` (WordInfo seq2 _ _) = seq1 `compare` seq2

instance Eq WordInfo where
    (WordInfo seq1 word1 bool1) == (WordInfo seq2 word2 bool2) = seq1 == seq2 && word1 == word2 && bool1 == bool2

type AnnotatedSentence = [WordInfo]

alphabet   = ['A'..'Z'] ++ ['a'..'z']
vowels     = "AEIOUaeiou"

-- | Randomly shuffle a list
--   /O(N)/
-- From http://www.haskell.org/haskellwiki/Random_shuffle#Imperative_algorithm
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

annotatedSentence :: String -> AnnotatedSentence
annotatedSentence sent =
    map (\(x, y, z) -> (WordInfo x y z)) wordTuples
    where sentence   = words sent
          wordTuples = zip3 [1..] sentence $ cycle [True]

isTooShort :: Word -> Bool
isTooShort word = length word <= 1

hasLeadingVowel :: Word -> Bool
hasLeadingVowel word = not (null word) && word !! 0 `elem` vowels

isSpoonableWord :: Word -> Bool
isSpoonableWord word = not (isTooShort word) && not (hasLeadingVowel word)

markSpoonableWords :: AnnotatedSentence -> AnnotatedSentence
markSpoonableWords sent =
    map (\(WordInfo x y z) -> (WordInfo x y (z && isSpoonableWord(y)))) sent

spoonableWords :: AnnotatedSentence -> AnnotatedSentence
spoonableWords words = filter (\(WordInfo _ _ isSpoonable) -> (isSpoonable)) words

splitWord :: String -> (String, String)
splitWord word = undefined

wordBeginning :: String -> String
wordBeginning word = takeWhile (isConsonant) word

wordEnding :: String -> String
wordEnding word = dropWhile (isConsonant) word

isConsonant :: Char -> Bool
isConsonant l = not $ l `elem` vowels

swapWordBeginnings :: (Word, Word) -> (Word, Word)
swapWordBeginnings (wordA, wordB) = (wordBeginning wordB ++ wordEnding wordA,
                                     wordBeginning wordA ++ wordEnding wordB)

spoonerize :: (WordInfo, WordInfo) -> (WordInfo, WordInfo)
spoonerize (WordInfo seqA wordA boolA, WordInfo seqB wordB boolB) =
    (WordInfo seqA newWordA boolA, WordInfo seqB newWordB boolB)
    where (newWordA, newWordB) = swapWordBeginnings(wordA, wordB)

substituteWords :: ([WordInfo], WordInfo, WordInfo) -> String
substituteWords (oldsentence, toSpoonerizeA, toSpoonerizeB) =
    intercalate " " stringsOnly
    where
      idsToReplace = map (\(WordInfo seq _ _) -> seq) [spoonerizedA, spoonerizedB]
      minusSpoonerized = filter (\(WordInfo seq _ _) -> (not (seq `elem` idsToReplace))) oldsentence
      (spoonerizedA, spoonerizedB) = spoonerize(toSpoonerizeA, toSpoonerizeB)
      newSentence = minusSpoonerized ++ [spoonerizedA, spoonerizedB]
      orderedWords = sort newSentence
      stringsOnly = map (\(WordInfo _ word _) -> word) orderedWords

main :: IO ()
main = do
  putStrLn "Enter a sentence to spoonerize:"
  line <- getLine
  let markedWords = markSpoonableWords $ annotatedSentence line
  shuffled <- shuffle $ spoonableWords markedWords
  let [toSpoonerizeA, toSpoonerizeB] = take 2 shuffled

  putStrLn "Your spoonerized sentence: "
  print $ substituteWords(markedWords, toSpoonerizeA, toSpoonerizeB)

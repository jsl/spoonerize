import System.Random
import Data.Array.IO
import Control.Monad
import Data.List (sort)

type Sequence        = Int
type Word            = String
type IsSpoonerizable = Bool

data WordInfo = WordInfo Sequence Word IsSpoonerizable
                deriving (Show)

instance Ord WordInfo where
  (WordInfo seq1 _ _) `compare` (WordInfo seq2 _ _) = seq1 `compare` seq2

instance Eq WordInfo where
    (WordInfo seq1 word1 bool1) == (WordInfo seq2 word2 bool2) =
        seq1 == seq2 && word1 == word2 && bool1 == bool2

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
    newArray n =  newListArray (1, n)

annotatedSentence :: String -> AnnotatedSentence
annotatedSentence sent =
    map (\(x, y, z) -> (WordInfo x y z)) wordTuples
    where sentence   = words sent
          wordTuples = zip3 [1..] sentence $ cycle [True]

isTooShort :: Word -> Bool
isTooShort word = length word <= 1

hasLeadingVowel :: Word -> Bool
hasLeadingVowel word = not (null word) && head word `elem` vowels

isSpoonerizableWord :: Word -> Bool
isSpoonerizableWord word = not (isTooShort word) &&
                           not (hasLeadingVowel word) &&
                           not (isAllConsonants word)

markSpoonerizableWords :: AnnotatedSentence -> AnnotatedSentence
markSpoonerizableWords =
    map (\(WordInfo x y z) -> (WordInfo x y (z && isSpoonerizableWord y)))

spoonerizableWords :: AnnotatedSentence -> AnnotatedSentence
spoonerizableWords = filter (\(WordInfo _ _ isSpoonerizable) -> isSpoonerizable)

wordBeginning :: String -> String
wordBeginning = takeWhile isConsonant

wordEnding :: String -> String
wordEnding = dropWhile isConsonant

isConsonant :: Char -> Bool
isConsonant l = l `notElem` vowels

isAllConsonants :: Word -> Bool
isAllConsonants = all isConsonant

swapWordBeginnings :: (Word, Word) -> (Word, Word)
swapWordBeginnings (wordA, wordB) = (wordBeginning wordB ++ wordEnding wordA,
                                     wordBeginning wordA ++ wordEnding wordB)

spoonerizeWords :: (WordInfo, WordInfo) -> (WordInfo, WordInfo)
spoonerizeWords (WordInfo seqA wordA boolA, WordInfo seqB wordB boolB) =
    (WordInfo seqA newWordA boolA, WordInfo seqB newWordB boolB)
    where (newWordA, newWordB) = swapWordBeginnings(wordA, wordB)

wordSequenceNumbers :: [WordInfo] -> [Int]
wordSequenceNumbers = map (\(WordInfo sequenceNumber _ _) -> sequenceNumber)

substituteWords :: ([WordInfo], WordInfo, WordInfo) -> String
substituteWords (oldsentence, toSpoonerizeA, toSpoonerizeB) =
    unwords $ map (\(WordInfo _ word _) -> word) orderedWords
    where
      sequencesToReplace = wordSequenceNumbers [spoonerizedA, spoonerizedB]
      minusSpoonerized = filter (\(WordInfo seq _ _) ->
                                 (seq `notElem` sequencesToReplace)) oldsentence

      (spoonerizedA, spoonerizedB) =
          spoonerizeWords(toSpoonerizeA, toSpoonerizeB)

      newSentence = minusSpoonerized ++ [spoonerizedA, spoonerizedB]
      orderedWords = sort newSentence

main :: IO ()
main = do
  putStrLn "Enter a sentence to spoonerize:"
  line <- getLine
  let markedWords = markSpoonerizableWords $ annotatedSentence line
  shuffled <- shuffle $ spoonerizableWords markedWords
  let [toSpoonerizeA, toSpoonerizeB] = take 2 shuffled

  putStrLn "Your spoonerized sentence: "
  print $ substituteWords(markedWords, toSpoonerizeA, toSpoonerizeB)

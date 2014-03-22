{- Phil's lab.
An anagram is a type of word play, the result of rearranging the letters of a word or phrase to 
produce a new word or phrase, using all the original letters exactly once. Write a program that 
finds the longest English anagram where the letters are completely scrambled (no letter is followed 
by the same letter in both words). For example Mary and army are anagrams, but the a is followed by 
r in both words, so it isn’t a scrambled anagram. Pot and top are scrambled anagrams.
Try using last week’s smaller dictionary first. This has words up to 15 letters long. If you get 
that working, you could try using the bigdictionary.txt or even biggestdictionary.txt. This has all 
kinds of words. It has entries that are not proper words. You will need to modify your algorithm to 
avoid noise. For example, turn all letters into lower case. Also, avoid any word that has 
punctuation in it. Also, no valid
￼￼
English word has the same letter repeated more than twice in a row. You might need to enlist some 
help from the user to accept or reject potential anagrams.
Use any data structures you want (see the Java library for ArrayList, TreeMap, HashMap, 
Arrays.sort() or Collections.sort()). Try to optimize your algorithm for speed (without overfitting it).

Ha! Java... no
-}



{- Imports:
	importing allows us to use functions from the below modules.
	import qualified gives a name to the package to avoid naming conflicts
	like the length function is defined in Data.ByteString and in standard Haskell (default package is called Prelude)
-}
import Data.ByteString.Char8 (pack)
import Data.List
import Data.Ord
import Data.Text.Encoding
import qualified Data.ByteString	as B 	-- Replacement for Strings.  Handles Text by Bytes rather than a [Char]
import qualified Data.Map 			as Map	-- Binary tree based Hashmap data structure... yeah,  thats a mouthful
import System.IO 							-- for dealing with IO. ugh..

{- Type Synonyms:
	Because I thought the .'s look ugly
	Type synonyms are a way of renaming types.
	The most noteworthy is String.  In Haskell String is just a list of characters.
	In Haskell's source, String is defined as such:
	type String = [Char]
	It is literally just a renaming of [Char], no more.
-}

type ByteString	= B.ByteString
type Map 		= Map.Map

-- Random variables
letterFrequencyThreshold = 5
sillyCharacterCount = 2

{- Finds the frequency of each letter in a String
	Returns a list of Pairs in the form of (String, frequency)

	I've used ByteStrings here for speed so I had to give the letters as ASCII values and 
	use B.length and B.filter

	to implement this with regular Strings just remove the B and use Chars.

	letterFrequency :: String -> [(Char, Int)]
	letterFrequency xs = [(x,c) | x <- ['a'..'z'], let c = length $ filter (x ==) xs, c > 0]
-}

--This type declaration works in GHCi,  not sure why it doesnt like it here
--letterFrequency :: ByteString -> [(GHC.Word.Word8, Int)]
letterFrequency xs = [(x,c) | x <- [97..122], let c = B.length $ B.filter (x ==) xs, c > 0]

{- Checks a word against a few Boolean functions -}
goodWord :: ByteString -> Bool
goodWord s =
	B.length s >= 4 &&  -- I dont really care for words under four letters
	B.length s <= 29 &&
	filterLetters s &&
	filterVowels s &&
	filterSillyCharacters s &&
	filterFrequencies s

{- Returns True if a word contains a vowel or y -}
filterVowels :: ByteString -> Bool
filterVowels text
	| B.null text 		= False 
	| x `elem` vowels	= True
	| otherwise 		= filterVowels $ B.tail text
	where vowels = [97,101,105,111,117,121] -- "aeiouy"
	      x  = B.head text

{- Checks a String against a lst of allowed characters -}
filterLetters :: ByteString -> Bool
filterLetters text
	| B.null text 			= True 
	| x `notElem` alphabet	= False
	| otherwise 			= filterLetters $ B.tail text
	where alphabet = [97..122] -- ['a' .. 'z']
	      x  = B.head text

{-Checks the String for high frequencies of letters-}
{-This is a curried function,  which means it is just a composition of other functions
	The way to spot a curried function is how it doesnt have a parameter... but still takes a 
	parameter.  Its kinda odd but it saves us on a few precious bytes and possible nanoseconds.
-}
filterFrequencies :: ByteString -> Bool
filterFrequencies = not . any (>= letterFrequencyThreshold) . map snd . letterFrequency

{-Checks the String for high frequencies of uncommon characters-}
filterSillyCharacters :: ByteString -> Bool
filterSillyCharacters str = count str <= sillyCharacterCount
	where count xs = maximum [c | x <- sillyCharacters, let c = B.length $ B.filter (x ==) xs]
	      sillyCharacters = [106,113,120,122] -- "qjzx"

{- Applies a function to a pair. 
	I havnt explained Pairs and tuples yet because I dont use them too often.
	I probably should because they were so useful in this program.

	Up until now we've had normal types like Strings, Ints, Bools and ByteStrings,
	but what is this?  a and b
	the type a comes up in a lot of type declarations.  e.g.

	head :: [a] -> a

	the a is a Type Variable.  Its a little gem of Polymorphism.
	the a literally stands for anything.  a can be any type.
	In the example above, head can take a list of anything, e.g. Int, Char, Bool 
	and return the same type.
	so for example
		head [1,2,3] 			:: [Int] -> Int
		head [1.2, 2.4, 5.65] 	:: [Double] -> Double
		head "Hello"			:: [Char] -> Char

	b is another Type Variable that could be any type, it can still be the same type as a.
	I chose the name b solely because it comes after a in the alphabet. I could have called it John.

	That's a lot of comment for one line of code...
-}
apply :: (a -> b) -> (a,a) -> (b,b)
apply f (a,b) = (f a, f b)

mapByLength :: [ByteString] -> Map Int [ByteString]
mapByLength = Map.fromListWith (++) . map f
	where f x = (B.length x, [x])

isAnagram :: ByteString -> [ByteString] -> (ByteString,ByteString)
isAnagram _   [] = apply pack ("Not","Found")
isAnagram str (x:xs) = if freq str == freq x && str /= x then (str,x) else isAnagram str xs
	where freq = sortBy (comparing fst) . letterFrequency


--Not finished yet
main = do
	let fileName = "input.txt"
	f <- openFile fileName ReadMode -- IO stuff
	dictionary <- B.hGetContents f 	-- More IO stuff
	{- B.split 10 == lines -}
	print . Map.lookup 4 . mapByLength . filter goodWord $ B.split 10 dictionary
	--V.mapM_ print . V.filter goodWord . V.fromList . T.lines $ decodeUtf8 dictionary
	print "done"

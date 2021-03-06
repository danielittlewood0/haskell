import Test.Hspec
import Test.QuickCheck
import Chapter7
import Data.List 
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

main = hspec $ do 
  describe "Data.List" $ do
    describe "numUniques" $ do 
      it "returns the number of unique elements of a list" $ do
        numUniques [1,2,3,4,3,2,1] `shouldBe` (4 :: Int)

    describe "intersperse" $ do
      it "intersperses an element between the list" $ do
        intersperse 'a' "bbbbb" `shouldBe` "babababab"

    describe "intercalate" $ do 
      it "same thing with a list" $ do
        intercalate "oo" ["d","td","td","t"] `shouldBe` "dootdootdoot"
        intercalate " " ["April","May","June"] `shouldBe` "April May June"

    describe "transpose" $ do
      it "flips rows and columns" $ do 
        transpose [[1,1,1],[2,2,2],[3,3,3]] `shouldBe` [[1,2,3],[1,2,3],[1,2,3]]

    describe "foldl', foldl1'" $ do
      it "non-lazy version of foldl, foldl1" $ do
        foldl1 (+) [1,2,3,4,5] `shouldBe` 15

    describe "concat" $ do
      it "concatenates a list of lists" $ do
        concat ["psycho","the","rapist"] `shouldBe` "psychotherapist"

    describe "concatMap" $ do
      it "concat $ map" $ do
        concatMap (++ [4]) [[1,2,3],[7,6,5]] `shouldBe` [1,2,3,4,7,6,5,4]

    describe "#and,#or" $ do
      it "[Bool] -> Bool" $ do
        and [True,True,True] `shouldBe` True
        or [True,True,True] `shouldBe` True
        and [True,False,True] `shouldBe` False
        or [True,False,True] `shouldBe` True
        and [False,False,False] `shouldBe` False
        or [False,False,False] `shouldBe` False

    describe "#all,#any" $ do
      it "(a -> Bool) -> [a] -> Bool" $ do
        [1,2,3] `shouldSatisfy` (all (<4))
        [1,2,3] `shouldSatisfy` (any (<2))

    describe "#iterate" $ do 
      it "array of x, fx, ffx, etc" $ do
        take 5 (iterate (+1) 1) `shouldBe` [1,2,3,4,5]

    describe "#splitAt" $ do 
      it "splits array at a point" $ do
        splitAt 3 "hello" `shouldBe` ("hel","lo")
        splitAt 100 "hello" `shouldBe` ("hello","")

    describe "#takeWhile,#dropWhile" $ do
      it "takes,drops from a list while predicate holds" $ do
        takeWhile (\x -> x**2 < 100) [1..] `shouldBe` [1..9]
        dropWhile (\x -> x**2 < 100) [1..15] `shouldBe` [10..15]

    describe "#span,#break" $ do
      it "(takeWhile,dropWhile), span (not . p)" $ do
        span (<10) [1..15] `shouldBe` ([1..9],[10..15])
        break (>=10) [1..15] `shouldBe` ([1..9],[10..15])

    describe "#sort" $ do
      it "sorts a list" $ do
        sort [4,5,21,2,24,1,4] `shouldBe` [1,2,4,4,5,21,24]

    describe "#group" $ do
      it "groups adjacent equal elements" $ do
        group "aaabbabbbaa" `shouldBe` ["aaa","bb","a","bbb","aa"]

    describe "#inits,#tails" $ do
      it "recursively takes init and tail" $ do
        inits "echo" `shouldBe` ["","e","ec","ech","echo"]
        tails "echo" `shouldBe` reverse ["","o","ho","cho","echo"]

    describe "#subList (mine)" $ do
      it "detects a sublist inside a list" $ do
        ([2,3] `subList` [1,2,3,4,5]) `shouldBe` True

    describe "#isInfixOf" $ do
      it "same as #subList" $ do
        ([2,3] `isInfixOf` [1,2,3,4,5]) `shouldBe` True

    describe "#isPrefixOf, #isSuffixOf" $ do
      it "(Eq a) => [a] -> [a] -> Bool" $ do
        "hey" `shouldSatisfy` (`isPrefixOf` "hey there!")
        "hey" `shouldSatisfy` not . (`isPrefixOf` "oh hey there")
        "there" `shouldSatisfy` (`isSuffixOf` "oh hey there")
        "there!" `shouldSatisfy` not . (`isSuffixOf` "oh hey there")

    describe "#elem,#notelem" $ do
      it "(Eq a) => a -> [a] -> Bool" $ do
        [1,2,3,4] `shouldSatisfy` (1 `elem`)
        [1,2,3,4] `shouldSatisfy` (5 `notElem`)

    describe "#partition" $ do
      it "(select,reject)" $ do
        let even = (\x -> (x `mod` 2) == 0)
        partition even [1,2,3,4,5] `shouldBe` ([2,4],[1,3,5])

    describe "#find" $ do
      it "detects the first element satisfying p (or Nothing)" $ do
        find (>3) [1..5] `shouldBe` Just 4
        find (>6) [1..5] `shouldBe` Nothing

    describe "#elemIndex" $ do
      it "returns the index of the first element (or Nothing)" $ do
        elemIndex 3 (reverse [1..9]) `shouldBe` Just 6
        elemIndex 30 (reverse [1..9]) `shouldBe` Nothing

    describe "#elemIndices" $ do
      it "#elemIndex with multiplicity" $ do
        elemIndices 'a' "abracadabra" `shouldBe` [0,3,5,7,10]

    describe "#findIndex, #findIndices" $ do
      it "maybe returns the index(es) of first satisfier of p" $ do
        findIndex even [1..5] `shouldBe` Just 1
        findIndices even [1..5] `shouldBe` [1,3]

    describe "#zip3,#zip4,#zipWith3,#zipWith4" $ do
      it "zip but 3-ary" $ do
        zip3 [1,2] [4,5,6] [7,8,9] `shouldBe` [(1,4,7),(2,5,8)]

      it "zipWith but 3-ary" $ do
        let f x y z = x + y + z
        zipWith3 f [1,2] [4,5,6] [7,8,9] `shouldBe` [12,15]

    describe "#lines,#unlines" $ do
      it "splits on \n" $ do
        lines "Hello\nWorld" `shouldBe` ["Hello","World"]

      it "does the opposite" $ do
        unlines ["Hello","World"] `shouldBe` "Hello\nWorld\n"

    describe "#words,#unlines" $ do
      it "splits on \\s" $ do
        words "Hello\nWorld" `shouldBe` ["Hello","World"]
        words "Hello World" `shouldBe` ["Hello","World"]

      it "does the opposite" $ do
        unwords ["Hello","World"] `shouldBe` "Hello World"

    describe "#nub" $ do
      it "uniq" $ do
        nub "abracadabra" `shouldBe` "abrcd"

    describe "#delete" $ do
      it "deletes the first occurrence" $ do
        delete 3 [1..5] `shouldBe` [1,2,4,5]

    describe "#\\\\" $ do
      it "list difference" $ do
        [1..9] \\ [3..7] `shouldBe` [1,2,8,9]

    describe "#union, #intersect" $ do
      it "the usual" $ do
        [1,2,3] `union` [2,3,4] `shouldBe` [1,2,3,4]
        [1,2,3] `intersect` [2,3,4] `shouldBe` [2,3]
      
    describe "#insert" $ do
      it "inserts x immediately before the first y >= x" $ do
        Data.List.insert 4 [3,2,5,1] `shouldBe` [3,2,4,5,1]

    describe "#generic" $ do
      it "returns a (Num a) rather than Int" $ do
        5 / (genericLength [1,2,3]) `shouldBe` (5/3 :: Float)

    describe "#nubBy, #groupBy, #deleteBy, #intersectBy, #unionBy" $ do 
      it "Provide custom == method" $ do
        let eq = \x -> \y -> x `mod` y == 0 || y `mod` x == 0
        deleteBy eq 12 [9,5,6,7,8] `shouldBe` [9,5,7,8]
        nubBy eq [2..25] `shouldBe` [2,3,5,7,11,13,17,19,23]

    describe "#sortBy, #insertBy, #maximumBy, #minimumBy" $ do
      it "same deal" $ do
        let ord (a,b) (c,d) = (a*d - b*c) `compare` 0
        sortBy ord [(1,2),(1,3),(2,7),(2,5),(3,8)] `shouldBe` 
                      [(2,7),(1,3),(3,8),(2,5),(1,2)]
    
  describe "Data.Char" $ do
    describe "#isControl" $ do
      it "" $ do
        '\n' `shouldSatisfy` isControl
        'n' `shouldSatisfy` not . isControl

    describe "#isSpace" $ do
      it "" $ do
        ' ' `shouldSatisfy` isSpace
        'n' `shouldSatisfy` not . isSpace

    describe "#isLower" $ do
      it "" $ do
        'n' `shouldSatisfy` isLower
        'N' `shouldSatisfy` not . isLower

    describe "#isUpper" $ do
      it "" $ do
        'N' `shouldSatisfy` isUpper
        'n' `shouldSatisfy` not . isUpper

    describe "#isAlpha" $ do
      it "" $ do
        'n' `shouldSatisfy` isAlpha
        '9' `shouldSatisfy` not . isAlpha

    describe "#isAlphaNum" $ do
      it "" $ do
        '9' `shouldSatisfy` isAlphaNum
        '@' `shouldSatisfy` not . isAlphaNum

    describe "#isPrint" $ do
      it "True if printable string" $ do
        'n' `shouldSatisfy` isPrint
        '\t' `shouldSatisfy` not . isPrint

    describe "#isDigit" $ do
      it "" $ do
        '9' `shouldSatisfy` isDigit
        'n' `shouldSatisfy` not . isDigit

    describe "#isOctDigit" $ do
      it "" $ do
        '7' `shouldSatisfy` isOctDigit
        '8' `shouldSatisfy` not . isOctDigit

    describe "#isHexDigit" $ do
      it "" $ do
        'a' `shouldSatisfy` isHexDigit
        'g' `shouldSatisfy` not . isHexDigit

    describe "#isLetter" $ do
      it "" $ do
        'n' `shouldSatisfy` isLetter
        '9' `shouldSatisfy` not . isLetter

    describe "#isMark" $ do
      it "Checks for unicode mark characters (accents)" $ do
        'n' `shouldSatisfy` not . isMark

    describe "#isNumber" $ do
      it "Same as isDigit for Chars?" $ do
        '9' `shouldSatisfy` isNumber
        'a' `shouldSatisfy` not . isNumber

    describe "#isPunctuation" $ do
      it "" $ do
        '.' `shouldSatisfy` isPunctuation
        'n' `shouldSatisfy` not . isPunctuation

    describe "#isSymbol" $ do
      it "" $ do
        '$' `shouldSatisfy` isSymbol
        'n' `shouldSatisfy` not . isSymbol

    describe "#isSeparator" $ do
      it "Checks for Unicode spaces and separators" $ do
        ' ' `shouldSatisfy` isSeparator
        'n' `shouldSatisfy` not . isSeparator

    describe "#isAscii" $ do
      it "First 128 characters of Unicode" $ do
        'n' `shouldSatisfy` isAscii
        '£' `shouldSatisfy` not . isAscii

    describe "#isLatin1" $ do
      it "First 256 characters of Unicode" $ do
        'n' `shouldSatisfy` isLatin1

    describe "#isAsciiUpper" $ do
      it "" $ do
        'A' `shouldSatisfy` isAsciiUpper
        'a' `shouldSatisfy` not . isAsciiUpper

    describe "#isAsciiLower" $ do
      it "" $ do
        'a' `shouldSatisfy` isAsciiLower
        'A' `shouldSatisfy` not . isAsciiLower

    describe "#generalCategory" $ do 
      it "Enumerates some classes of character" $ do
        generalCategory ' ' `shouldBe` Space  
        generalCategory 'A' `shouldBe` UppercaseLetter  
        generalCategory 'a' `shouldBe` LowercaseLetter  
        generalCategory '.' `shouldBe` OtherPunctuation  
        generalCategory '9' `shouldBe` DecimalNumber  

    describe "#toUpper" $ do
      it "Cnverts a character to upper-case." $ do
        toUpper 'a' `shouldBe` 'A'

      it "Spaces, numbers, and the like remain unchanged." $ do
        toUpper '1' `shouldBe` '1'

    describe "#toLower" $ do
      it " converts a character to lower-case." $ do
        toLower 'A' `shouldBe` 'a'

    describe "#toTitle" $ do
      it "For most characters, title-case is the same as upper-case." $ do
        toTitle 'a' `shouldBe` toUpper 'a'

    describe "#digitToInt" $ do
      it "Converts '0'..'9', 'a'..'f' or 'A'..'F' to Int." $ do
        map digitToInt "34538" `shouldBe` [3,4,5,3,8]  
        map digitToInt "FF85AB" `shouldBe` [15,15,8,5,10,11]  

    describe "#intToDigit" $ do
      it "Inverse function of digitToInt." $ do
        intToDigit 15 `shouldBe` 'f'  
        intToDigit 5 `shouldBe` '5'  

    describe "#ord" $ do
      it "Converts character to its Unicode number" $ do
        ord 'a' `shouldBe` 97  
        map ord "abcdefgh" `shouldBe` [97,98,99,100,101,102,103,104]   

    describe "#chr" $ do
      it "Converts Unicode character to char" $ do
        chr 97 `shouldBe` 'a'  

  describe "Data.Map" $ do
    describe "#findKey (mine)" $ do
      it "finds a key in a hash" $ do
        let succ = [(1,2),(2,3),(3,4)] :: [(Int,Int)]
        findKey 1 succ `shouldBe` Just 2
        findKey 4 succ `shouldBe` Nothing

    describe "#fromList,#toList" $ do
      it "turns [(k,v)] into an association (map)" $ do
        Map.toList (Map.fromList [(1,1),(2,2),(3,3)]) `shouldBe` 
                                                             [(1,1),(2,2),(3,3)] 

    describe "#empty" $ do
      it "Represents an empty map." $ do
        Map.empty `shouldBe` (Map.fromList ([] :: [(Int,Int)]))  

    describe "#insert" $ do
      it "Inserts a (key, value) into a map." $ do
        Map.insert 3 100 Map.empty `shouldBe` Map.fromList [(3,100)]  
        Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty)) 
                    `shouldBe` Map.fromList [(3,100),(4,200),(5,600)]  
        (Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty) 
                    `shouldBe` Map.fromList [(3,100),(4,200),(5,600)]  

    describe "#fromList' (mine)" $ do
      it "copies the standard fromList" $ do
        let xs = [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
        (fromList' xs) `shouldBe` (Map.fromList xs)

    describe "#null" $ do
      it "Checks if a map is empty." $ do
        Map.null Map.empty `shouldBe` True  
        (Map.null $ Map.fromList [(2,3),(5,5)]) `shouldBe` False  

    describe "#size" $ do
      it "Reports the size of a map." $ do
        Map.size Map.empty `shouldBe` 0  
        (Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]) `shouldBe` 5  

    describe "#singleton" $ do
      it "Turns a key and a value into a map." $ do
        Map.singleton 3 9 `shouldBe` Map.fromList [(3,9)]  
        (Map.insert 5 9 $ Map.singleton 3 9) `shouldBe` Map.fromList [(3,9),(5,9)]

    describe "#lookup" $ do
      it "Works like the Data.List lookup, only it operates on maps." $ do
        Map.lookup 1 (Map.singleton 1 3) `shouldBe` Just 3

    describe "#member" $ do
      it "Reports whether the key is in the map or not." $ do
        (Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]) `shouldBe` True  
        (Map.member 3 $ Map.fromList [(2,5),(4,5)]) `shouldBe` False  

    describe "#map, #filter" $ do
      it "Much like their list equivalents." $ do
        (Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]) 
                               `shouldBe` Map.fromList [(1,100),(2,400),(3,900)]  
        (Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]) 
                               `shouldBe` Map.fromList [(2,'A'),(4,'B')]  

    describe "#keys, #elems" $ do
      it "fst . Map.toList and snd . Map.toList" $ do
        let hash = Map.fromList [(2,3),(5,5),(6,7),(1,4)]
        (Map.keys hash) `shouldBe` [1,2,5,6]
        (Map.elems hash) `shouldBe` [4,3,5,7]

    describe "#fromListWith" $ do
      it "Duplicate keys are merged using the given function" $ do
        let list = [('A',1),('B',2),('A',3),('B',4),('A',5)]
            hash_with = Map.fromListWith (+) list
            hash = Map.fromList [('A',9),('B',6)]
        hash_with `shouldBe` hash

    describe "#insertWith" $ do
      it "Same as #fromListWith" $ do
        let hash = Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]
            merged = Map.fromList [(3,104),(5,103),(6,339)]  
        hash `shouldBe` merged

  describe "Data.Set" $ do
    describe "fromList" $ do
      it "Makes a Set out of a List" $ do
        let set = Set.fromList "abracadabra"
        set `shouldBe` Set.fromList "abrcd"

    describe "#intersection" $ do
      it "Intersection of two Sets" $ do
        let set1 = Set.fromList "abracadabra"
        let set2 = Set.fromList "alakazam"
        (set1 `Set.intersection` set2) `shouldBe` Set.fromList "a"

    describe "#difference" $ do
      it "Set difference" $ do
        let set1 = Set.fromList "abracadabra"
        let set2 = Set.fromList "alakazam"
        (set1 `Set.difference` set2) `shouldBe` Set.fromList "brcd"

    describe "#union" $ do
      it "Set union" $ do
        let set1 = Set.fromList "abracadabra"
        let set2 = Set.fromList "alakazam"
        (set1 `Set.union` set2) `shouldBe` Set.fromList "abrcdlkzm"

    describe "#null,#empty" $ do
      it "empty produces the empty Set, null checks for emptiness" $ do
        (Set.fromList [] :: Set.Set Int) `shouldSatisfy` Set.null
        (Set.empty :: Set.Set Int) `shouldSatisfy` Set.null
        (Set.fromList [1,2,3]) `shouldSatisfy` not . Set.null

    describe "#size" $ do
      it "Number of elements" $ do
        length (Set.fromList [1,1,1,2,3,2,1,3,2]) `shouldBe` 3

    describe "#member" $ do
      it "Checks for membership" $ do
        1 `shouldSatisfy` (`Set.member` Set.fromList [1,2,3])

    describe "#singleton" $ do
      it "Produces a singleton set from an element" $ do
        (Set.singleton 1) `shouldBe` (Set.fromList [1])

    describe "#insert" $ do
      it "Inserts a new, distinct element" $ do
        let set = (Set.fromList [1,2,3])
        (Set.insert 1 set) `shouldBe` set
        (Set.insert 4 set) `shouldBe` (Set.fromList [1,2,3,4])

    describe "#delete" $ do
      it "Deletes an element" $ do
        let set = (Set.fromList [1,2,3])
        (Set.delete 4 set) `shouldBe` set
        (Set.delete 1 set) `shouldBe` (Set.fromList [2,3])

    describe "#isSubsetOf, #isProperSubsetOf" $ do
      it "Checks for (proper) subsetness" $ do
        (Set.fromList [1..3]) `shouldSatisfy` (`Set.isSubsetOf` Set.fromList [1..5])
        (Set.fromList [1..3]) `shouldSatisfy` (`Set.isSubsetOf` Set.fromList [1..3])
        (Set.fromList [1..3]) `shouldSatisfy` 
                                           not . (`Set.isSubsetOf` Set.fromList [1])
        (Set.fromList [1..3]) `shouldSatisfy` 
                                        (`Set.isProperSubsetOf` Set.fromList [1..5])
        (Set.fromList [1..3]) `shouldSatisfy` 
                                  not . (`Set.isProperSubsetOf` Set.fromList [1..3])

    describe "#filter" $ do
      it "Filters the Set" $ do
        (Set.filter even (Set.fromList [1..7])) `shouldBe` (Set.fromList [2,4,6])

    describe "#map" $ do
      it "Maps over the Set" $ do
        (Set.map (+3) (Set.fromList [1..7])) `shouldBe` (Set.fromList [4..10])

    describe "#toList" $ do
      it "Puts the distinct elements back in an array" $ do
        (Set.toList (Set.fromList "abracadabra")) `shouldBe` "abcdr"

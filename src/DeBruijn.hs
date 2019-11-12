module DeBruijn where


removeBackOfStr :: String -> Int -> String
removeBackOfStr [] length = []
removeBackOfStr str 0 = []
removeBackOfStr (x:xs) length = x:(removeBackOfStr xs (length-1))

removeFrontOfStr :: String -> Int -> String
removeFrontOfStr [] index = []
removeFrontOfStr str 0 = str
removeFrontOfStr (x:xs) index = removeFrontOfStr xs (index-1)

getPartOfStr :: String -> Int -> Int -> String
getPartOfStr [] index length = []
getPartOfStr str 0 length = removeBackOfStr str length
getPartOfStr str index 0 = []
getPartOfStr str index length =
    removeBackOfStr (removeFrontOfStr str index) length

isInString :: String -> String -> Bool
isInString needle [] = False
isInString needle haystack =
    if (needle == (removeBackOfStr haystack (length needle)))
        then True
    else
        isInString needle (removeFrontOfStr haystack 1)

isCharInStr :: Char -> String -> Bool
isCharInStr ch [] = False
isCharInStr ch (x:xs) =
    if (ch == x)
        then True
    else
        isCharInStr ch xs

nbOccur :: Char -> String -> Int
nbOccur ch [] = 0
nbOccur ch (x:xs) =
    if (ch == x)
        then 1 + nbOccur ch xs
    else
        nbOccur ch xs
 
isCharUnique :: Char -> String -> Bool
isCharUnique ch [] = True
isCharUnique ch str = 
    if ((nbOccur ch str) == 1)
        then True
    else
        False


isAlphabetValid :: String -> Bool
isAlphabetValid [] = True
isAlphabetValid (x:xs) = 
    if (isCharUnique x (x:xs))
        then isAlphabetValid xs
    else
        False


concatenateStringsArr :: [String] -> [String] -> [String]
concatenateStringsArr [] arr = arr
concatenateStringsArr arr [] = arr
concatenateStringsArr (x:xs) (y:ys) =
    [x ++ y] ++ concatenateStringsArr xs ys


duplicate :: [String] -> Int -> [String]
duplicate arr 1 = arr
duplicate arr nb = arr ++ duplicate arr (nb-1)

extractEveryChar :: String -> [String]
extractEveryChar [] = []
extractEveryChar (x:xs) = 
    [[x]] ++ extractEveryChar xs

buildSequence :: String -> Int -> [String]
buildSequence [] nb = []
buildSequence str 0 = []
buildSequence str 1 = extractEveryChar str
buildSequence str nb =
    buildIntermediarySeq str (duplicate (buildSequence str (nb-1)) (length str))

buildIntermediarySeq :: String -> [String] -> [String]
buildIntermediarySeq [] arr = []
buildIntermediarySeq str [] = extractEveryChar str
buildIntermediarySeq str arr =
    concatenateStringsArr (buildSeriesChar str (div (length arr) (length str))) arr

buildSeriesChar :: String -> Int -> [String]
buildSeriesChar [] nb = []
buildSeriesChar str 0 = []
buildSeriesChar str 1 = extractEveryChar str
buildSeriesChar (x:xs) nb =
    duplicate [[x]] nb ++ buildSeriesChar xs nb


addMirror :: [String] -> [String]
addMirror [] = []
addMirror (x:xs) = 
    [x] ++ addMirror(xs) ++ [x]

rotateStr :: String -> Int -> String
rotateStr [] nb = []
rotateStr str 0 = str
rotateStr (x:xs) nb = (rotateStr (xs ++ [x]) (nb - 1))


{-clear isn't necessary-}
clearArr :: [String] -> [String]
clearArr [] = []
clearArr (x:xs) = clearArr (deleteDuplicate x xs)


checkDuplicate :: String -> String -> Int -> String
checkDuplicate str [] nb = []
checkDuplicate [] str2 nb = []
checkDuplicate str str2 0 = str2
checkDuplicate str str2 nb = 
    if (str == str2)
        then []
    else
        checkDuplicate str (rotateStr str2 1) (nb - 1)

{--remove rotated versions--}
deleteDuplicate :: String -> [String] -> [String]
deleteDuplicate [] arr = arr
deleteDuplicate str [] = []
deleteDuplicate str (x:xs) =
    if (isRotatedOf str x (length str))
        then (deleteDuplicate str xs)
    else
        x:(deleteDuplicate str xs)

removeInnerSequences :: [String] -> String -> [String]
removeInnerSequences [] str = []
removeInnerSequences arr [] = arr
removeInnerSequences (x:xs) str =
    if (isInString x str == True)
        then removeInnerSequences xs str
    else
        x:removeInnerSequences xs str

isRotatedOf :: String -> String -> Int -> Bool
isRotatedOf str1 str2 0 = False
isRotatedOf str1 [] nb = False
isRotatedOf [] str2 nb = False
isRotatedOf str1 str2 nb = 
    if (str1 == str2)
        then True
    else
        isRotatedOf str1 (rotateStr str2 1) (nb-1)

cleanArr :: [String] -> [String]
cleanArr [] = []
cleanArr (x:xs) = x:(cleanArr (deleteDuplicate x xs))

isDeBruijn :: [String] -> String -> Int -> Bool
isDeBruijn [] str nb = True
isDeBruijn arr str 0 = False
isDeBruijn arr str nb = isDeBruijn (removeInnerSequences arr str) (rotateStr str 1) (nb - 1)

uniqueDeBruijn :: [String] -> String -> String -> Bool
uniqueDeBruijn [] str1 str2 = False
uniqueDeBruijn arr [] str2 = False
uniqueDeBruijn arr str1 [] = False
uniqueDeBruijn arr str1 str2 = 
    if (not (isDeBruijn arr str1 (length str1)) || not (isDeBruijn arr str2 (length str2)))
        then False
    else
        if (not (isRotatedOf str1 str2 (length str1)))
            then True
        else False

{-Fire every no deBruijn string-}
cleanDeBruijn :: [String] -> [String] -> [String]
cleanDeBruijn [] list = []
cleanDeBruijn arr [] = []
cleanDeBruijn arr (l:list) =
    if (isDeBruijn arr l (length l))
        then l:(cleanDeBruijn arr list)
    else
        cleanDeBruijn arr list

cleanFlag :: String -> Int -> [String] -> [String]
cleanFlag [] nb to_clean = []
cleanFlag seq 0 to_clean = []
cleanFlag seq nb [] = []
cleanFlag seq nb to_clean = 
    cleanArr (cleanDeBruijn (buildSequence seq nb) to_clean)


checkFlag :: String -> Int -> String -> Bool
checkFlag [] nb line = False
checkFlag str 0 line = False
checkFlag str nb [] = False
checkFlag str nb line =
    isDeBruijn (buildSequence str nb) line (length line)
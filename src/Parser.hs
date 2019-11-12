module Parser where


import Data.Char

data Flag = Check Int String {--| Unique Int String | Clean Int String--}

parse :: [String] -> Maybe Flag
parse [] = Nothing
parse (intStr:otherArgs) =
    case parseInt intStr of
        Nothing -> Nothing
        Just int -> case otherArgs of
            [str, "--clean"] -> Just (Clean int str)
            [str, "--unique"] -> Just (Unique int str)
            [str, "--check"] -> Just (Check int str)
            ["--check"] -> Just (Check int "01")
            ["--unique"] -> Just (Unique int "01")
            ["--clean"] -> Just (Clean int "01")
            [] -> Nothing
            _ -> Nothing

parseInt :: String -> Maybe Int
parseInt [] = Nothing
parseInt str = 
    case all isDigit str of
        True -> checkInt(read str)
        False -> Nothing

checkInt :: Int -> Maybe Int
checkInt int = 
    if (int < 1)
        then Nothing
        else Just int
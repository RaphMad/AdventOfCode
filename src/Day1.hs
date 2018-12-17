module Day1 where

    import Data.Char
    import Text.ParserCombinators.Parsec
    import Text.Parsec.Char
    import Data.HashSet (HashSet)
    import qualified Data.HashSet as Set

    -- Parsing
    parseLine :: Parser Integer
    parseLine = do
        sign <- plusMinus
        number <- read <$> many digit
        return $ if sign == '+' then number else -number

    plusMinus :: Parser Char
    plusMinus = satisfy (\a -> a == '+' || a == '-')

    parseInput :: Parser [Integer]
    parseInput = sepEndBy parseLine endOfLine

    parseDay1 :: IO [Integer]
    parseDay1 = do
        result <- parseFromFile parseInput ".\\src\\inputs\\day1.txt"
        case result of
            Left err     -> error $ show err
            Right input  -> return input

    -- Calculcation
    day1 :: [Integer] -> Integer
    day1 = sum

    day1_2 :: [Integer] -> Integer
    day1_2 list = fst $ checkFrequencies Set.empty allFrequencies
        where allFrequencies = frequencies $ cycle list

    -- The frequencies are the partial sums of the frequency changes (prepended with 0).
    frequencies :: [Integer] -> [Integer]
    frequencies = scanl (+) 0

    -- Traverse the frequencies and keep track of the already visited one in a HashSet.
    -- As soon as a frequency is already in the Set, we have found our duplicate.
    checkFrequencies :: HashSet Integer -> [Integer] -> (Integer, HashSet Integer)
    checkFrequencies alreadySeen (x:rest)
        | x `Set.member` alreadySeen = (x, alreadySeen)
        | otherwise                  = checkFrequencies (x `Set.insert` alreadySeen) rest

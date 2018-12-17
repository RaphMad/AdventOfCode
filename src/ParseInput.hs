module ParseInput where

    import Data.Char
    import Text.ParserCombinators.Parsec
    import Text.Parsec.Char

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
    parseDay1 = do {
        result <- parseFromFile parseInput ".\\src\\inputs\\day1.txt"
        ; case result of
            Left err     -> error $ show err
            Right input  -> return input
        }

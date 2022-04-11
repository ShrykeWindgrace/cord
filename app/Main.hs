module Main where
import Control.Applicative ((<|>))


main :: IO ()
main = do
    print $ charAt abc 0 == Just 'A'
    print $ charAt abc 26 == Nothing
    print $ charAt abc 3 == Just 'D'

    print $ substring abc 3 5 == ['D'..'H']


-- In theory, we can inline the opertator <|> (Alternative instance for Maybe a) to avoid an import
-- Also, we can replace String by Text to avoid concatenation complexity (it is bad for Strings)
-- I 


-- part 1

data Cord = Leaf Int String
            | Node Int Cord Cord


abc :: Cord
abc = Node 26 (Leaf 5 ['A'..'E']) (Node 21 (Leaf 10 ['F'..'O']) (Leaf 11 ['P'..'Z']))

-- part 2

getLen :: Cord -> Int
getLen (Leaf l _) = l
getLen (Node l _ _) = l


-- index is zero-based
-- all hail lazyness that allow us to skip all but necessary leaves of the tree
charAt :: Cord -> Int -> Maybe Char
charAt (Leaf l s) n
    | n <  0 = Nothing
    | n >= l = Nothing
    | otherwise = Just (s !! n)

charAt (Node l left right) n
    | n <  0 = Nothing
    | n >= l = Nothing
    | otherwise = charAt left n <|> charAt right (n - getLen right)

-- part 3

-- start index is zero-based
substring :: Cord -> Int -> Int -> String
substring (Leaf len s) start count
    | count <= 0 = ""
    | start >= len = ""
    | start >= 0 = take count $ drop start s
    | start + count >= len = s
    | otherwise = take (count + start) s

substring n@(Node len left right) start count
    | count <= 0 = ""
    | start >= len = ""
    | start < 0 = substring n 0 (count + start)
    | otherwise = substring left start count <> substring right (start - getLen left) count

    -- the operator (<>) for String/Text is just a concatenation
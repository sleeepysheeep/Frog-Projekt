module Main where

import Data.Char (isAlpha, toLower)
import Test.HUnit

-- Farbcodierung für den Red-Black-Baum
data Color = Red | Black deriving (Eq, Show)

-- Der Red-Black-Baum: entweder leer oder ein Knoten mit Farbe, linkem und rechten Teilbaum, und einem Wert
data RBTree a = Empty | Node Color (RBTree a) a (RBTree a) deriving (Show)

-- Einfüge-Funktion für den Red-Black-Baum
insert :: Ord a => a -> RBTree a -> RBTree a
insert newValue t = blacken (insert' newValue t)
  where
    insert' :: Ord a => a -> RBTree a -> RBTree a
    insert' cur Empty = Node Red Empty cur Empty
    insert' cur (Node color left value right)
      | cur < value = balance color (insert' cur left) value right
      | cur > value = balance color left value (insert' cur right)
      | otherwise = Node color left value right

    -- Balanciert den Baum nach der Einfügung
    balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
    balance Red (Node Red (Node Red a leftValue b) middleValue c) rightValue d =
        Node Red (Node Black a leftValue b) middleValue (Node Black c rightValue d)
    balance Red (Node Red a leftValue (Node Red b middleValue c)) rightValue d =
        Node Red (Node Black a leftValue b) middleValue (Node Black c rightValue d)
    balance Red a leftValue (Node Red (Node Red b middleValue c) rightValue d) =
        Node Red (Node Black a leftValue b) middleValue (Node Black c rightValue d)
    balance Red a leftValue (Node Red b middleValue (Node Red c rightValue d)) =
        Node Red (Node Black a leftValue b) middleValue (Node Black c rightValue d)
    balance color left value right = Node color left value right

    -- Stellt sicher, dass der Baum nach der Einfügung schwarz bleibt
    blacken :: RBTree a -> RBTree a
    blacken (Node _ left value right) = Node Black left value right
    blacken Empty = Empty

-- Funktion zum Tokenisieren des Textes
tokenize :: String -> [String]
tokenize = words . map (\c -> if isAlpha c then toLower c else ' ')

-- In-Order Traversierung des Red-Black-Baums (sortiert die Elemente)
inOrderTraversal :: RBTree a -> [a]
inOrderTraversal Empty = []
inOrderTraversal (Node _ left value right) = inOrderTraversal left ++ [value] ++ inOrderTraversal right

-- Hauptfunktion
main :: IO ()
main = do
    -- Lies den Inhalt der Datei "war_and_peace.txt"
    content <- readFile "war_and_peace.txt"
    
    -- Tokenisiere den Inhalt
    let tokens = tokenize content
    
    -- Baue den Red-Black-Baum mit den einzigartigen Wörtern
    let tree = foldr insert Empty tokens
    
    -- Traversiere den Baum in-order, um die Wörter in sortierter Reihenfolge zu erhalten
    let uniqueWords = inOrderTraversal tree
    
    -- Schreibe die sortierten einzigartigen Wörter in die Datei "output.txt"
    writeFile "output.txt" (unlines uniqueWords)
    
    putStrLn "Tokenization and sorting completed, unique words written to output.txt"

    -- Tests ausführen
    _ <- runTestTT (TestList [testInsert, testTokenize])
    return ()

-- Test für die Funktion 'insert'
testInsert :: Test
testInsert = TestCase (assertEqual "Test, ob Einfügen von 'apple' funktioniert" (inOrderTraversal (insert "apple" Empty)) ["apple"])

-- Test für die Funktion 'tokenize'
testTokenize :: Test
testTokenize = TestCase (assertEqual "Test, ob Tokenize funktioniert" (tokenize "Apple Banana") ["apple", "banana"])

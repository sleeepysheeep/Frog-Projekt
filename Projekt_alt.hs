module Main where

import System.IO (readFile, writeFile)
import Data.Char (isAlpha, toLower)
import Test.HUnit

-- Farbcodierung für den Red-Black-Baum
data Color = Red | Black deriving (Eq, Show)

-- Der Red-Black-Baum: entweder leer oder ein Knoten mit Farbe, linkem und rechten Teilbaum, und einem Wert
data RBTree a = Empty | Node Color (RBTree a) a (RBTree a) deriving (Show)

-- Einfüge-Funktion für den Red-Black-Baum
insert :: Ord a => a -> RBTree a -> RBTree a
insert x t = blacken (insert' x t)
  where
    insert' :: Ord a => a -> RBTree a -> RBTree a
    insert' x Empty = Node Red Empty x Empty
    insert' x (Node color left value right)
      | x < value = balance color (insert' x left) value right
      | x > value = balance color left value (insert' x right)
      | otherwise = Node color left value right

    -- Balanciert den Baum nach der Einfügung
    balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
    balance Red (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d)
    balance Red (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d)
    balance Red a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
    balance Red a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d)
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
    let tree = foldl insert Empty tokens  -- Verwendung von foldl statt foldr
    
    -- Traversiere den Baum in-order, um die Wörter in sortierter Reihenfolge zu erhalten
    let uniqueWords = inOrderTraversal tree
    
    -- Schreibe die sortierten einzigartigen Wörter in die Datei "output.txt"
    writeFile "output.txt" (unlines uniqueWords)
    
    putStrLn "Tokenization and sorting completed, unique words written to output.txt"

-- Test für die Funktion 'insert'
testInsert :: Test
testInsert = TestCase (assertEqual "Test, ob Einfügen von 10 funktioniert" (inOrderTraversal (insert 10 Empty)) [10])

-- Test für die Funktion 'tokenize'
testTokenize :: Test
testTokenize = TestCase (assertEqual "Test, ob Tokenize funktioniert" (tokenize "Apple Banana") ["apple", "banana"])

-- Testlauf starten
main :: IO Counts
main = runTestTT (TestList [testInsert, testTokenize])

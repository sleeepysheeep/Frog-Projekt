module Main where

import System.IO (readFile, writeFile)
import Data.Char (isAlpha, toLower)
import qualified Data.Set as Set

-- Funktion zum Tokenisieren des Textes: 
-- Entfernt nicht-alphabetische Zeichen und wandelt alles in Kleinbuchstaben um
tokenize :: String -> [String]
tokenize = words . map (\c -> if isAlpha c then toLower c else ' ')

main = do
    -- Lies den Inhalt der Datei "war_and_peace.txt"
    content <- readFile "war_and_peace.txt"
    
    -- Tokenisiere den Inhalt
    let tokens = tokenize content
    
    -- Entferne Duplikate, indem die Wörter in ein Set eingefügt werden
    let uniqueWords = Set.toList (Set.fromList tokens)
    
    -- Schreibe die eindeutigen Wörter in die Datei "output.txt", jeweils ein Wort pro Zeile
    writeFile "output.txt" (unlines uniqueWords)
    
    putStrLn "Tokenization completed, unique words written to output.txt"

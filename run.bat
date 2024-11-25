@echo off
REM Kompiliere das Haskell-Programm
ghc -o Projekt Projekt.hs

REM Verarbeite test.txt
echo Verarbeite test.txt...
.\Projekt "test.txt"
echo Verarbeite test1.txt...
.\Projekt "test1.txt"
echo Verarbeite war_and_peace.txt...
.\Projekt "war_and_peace.txt"

REM Optional: Führe Tests aus, falls du welche definiert hast
REM runTestTT TestCase
echo Verarbeitung abgeschlossen!
pause

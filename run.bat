@echo off
REM Stelle sicher, dass Cabal verfügbar ist
echo Starte den Build-Prozess...

REM Baue das Haskell-Projekt
cabal build

REM Überprüfen, ob der Build erfolgreich war
if %errorlevel% neq 0 (
    echo Fehler beim Build-Prozess!
    pause
    exit /b %errorlevel%
)

REM Verarbeite test.txt
echo Verarbeite test.txt...
cabal run Project -- "test.txt"
if %errorlevel% neq 0 (
    echo Fehler beim Verarbeiten von test.txt!
    pause
    exit /b %errorlevel%
)

REM Verarbeite test1.txt
echo Verarbeite test1.txt...
cabal run Project -- "test1.txt"
if %errorlevel% neq 0 (
    echo Fehler beim Verarbeiten von test1.txt!
    pause
    exit /b %errorlevel%
)

REM Verarbeite war_and_peace.txt
echo Verarbeite war_and_peace.txt...
cabal run Project -- "war_and_peace.txt"
if %errorlevel% neq 0 (
    echo Fehler beim Verarbeiten von war_and_peace.txt!
    pause
    exit /b %errorlevel%
)

echo Verarbeitung abgeschlossen!
pause

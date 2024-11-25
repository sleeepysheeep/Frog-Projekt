Projekt: Red-Black Tree Textverarbeitung
----------------------------------------
Beschreibung:
Dieses Projekt liest eine Textdatei, tokenisiert den Inhalt und speichert die einzigartigen Wörter in einem Red-Black-Baum. Anschließend werden die Wörter in sortierter Reihenfolge in eine Ausgabedatei geschrieben.

Kompilierung:
1. Installiere GHC (Glasgow Haskell Compiler).
2. Führe den folgenden Befehl aus, um das Programm zu kompilieren:
   ghc -o Projekt Projekt.hs

Ausführung:
1. Um das Programm auszuführen, verwende den folgenden Befehl:
   ./Projekt <Pfad zur Textdatei>

Beispiel:
   ./Projekt /path/to/war_and_peace.txt

Output:
Das Programm erstellt eine Datei "output.txt" mit den sortierten einzigartigen Wörtern.

Tests:
- Für die Testdurchführung kannst du `runTestTT` im Hauptprogramm ausführen.

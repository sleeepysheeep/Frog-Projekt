Funktionale Programmierung Projekt: Red-Black Tree Textverarbeitung
----------------------------------------
Beschreibung:
Dieses Projekt liest eine Textdatei (war_and_peace.txt), tokenisiert den Inhalt und speichert die einzigartigen Wörter in einem Red-Black-Baum. Anschließend werden die Wörter in sortierter Reihenfolge in eine Ausgabedatei (output.txt) geschrieben.

Kompilierung:
1. Installieren von GHC (Glasgow Haskell Compiler).
2. In der GHCi-Shell zum Projektfolder navigieren.
3. Um das Programm zu kompilieren:
   cabal build

Ausführung:
1. Um das Programm auszuführen:
   cabal run

Output:
Das Programm erstellt eine Datei "output.txt" mit den sortierten einzigartigen Wörtern.

Tests:
- Werden in der Main automatisch ausgeführt.

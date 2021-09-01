# Intro
Leider habe ich das Projekt in der vorgegebenen Zeit nicht fertig bekommen. Die Kombination aus einer schwierigen Programmiersprache, die ich vorher nicht konnte, und einer nicht ganz einfachen Aufgabe, habe ich unterschätzt. Deshalb gibt es in dieser Datei ein paar Informationen, wie man den Code zumindest im Ansatz zum Laufen bringen kann.

# Setup
Die Sprachwerkzeuge können hier heruntergeladen werden: https://www.haskell.org/platform/
Wenn GHC und cabal installiert sind, kann im Projektverzeichnis (coderunner_generator) 'cabal run' ausgeführt werden. Es sollten alle Abhängigkeiten installiert werden. Danach wird das Programm ausgeführt.

# Nutzung
Aktuell werden lediglich die drei Beispieldateien im Input-Ordner analysiert. Das Programm erstellt pro Datei einen abstrakten Syntaxbaum, eine Symboltabelle und schließlich das Ergebnis als XML. Für das zweite Beispiel (02_missing_parts_of_code) werden nur die ersten beiden Stufen generiert, im Ergebnis steht eine Fehlermeldung. 

Für jede Datei müssen die Parameter manuell gesetzt werden (Das Programm fragt zwar nach 'self' und 'amount', allerdings funktioniert nur 'self'). Die Parameter müssen exakt eingetippt werden. Die Reihenfolge ist bei der Parameterabfrage noch nicht optimiert. Die REQUIRES-Beziehungen werden zwar korrekt angewendet und überprüft, allerdings nur im Nachhinein, bzw. nebenläufig. Deshalb werden gesetzte Parameter teilweise wieder überschrieben.

Auch die Tests haben es leider nicht ins endgültige Ergebnis geschafft.

Alles weitere werde ich bei der Präsentation erklären.
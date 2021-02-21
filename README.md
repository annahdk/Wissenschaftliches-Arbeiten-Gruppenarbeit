Gruppenarbeit GitHub Repository Wissenschaftliches Arbeiten <br>
Einführung: 

Gruppenmitglieder:

Leia Betting, Romina Dubrow, Kathrin Henkenherm, Anna Herdick, Tim Ritter und Luca Sauer 

Kontakt: leia.betting\@tu.dortmund.de, romina.dubrow\@tu-dortmund.de

--------------------------------------------------------------------------------

### Aktueller Status: <br>
Aufgabe 1 erledigt <br>
Aufgabe 2 erledigt <br>
Aufgabe 3 erledigt <br>
Aufgabe 4 in Bearbeitung <br>
Aufgabe 5 in Bearbeitung 

--------------------------------------------------------------------------------

### Inhalt: <br>
Aufgabe 1 (bearbeitet von Luca Sauer und Tim Ritter) <br>

Erstellen eines Datensatzes 

Aufgabe 2 (bearbeitet von Luca Sauer und Tim Ritter) <br>

Hochladen des Datensatzes in csv Format

Aufgabe 3 (bearbeitet von allen Gruppenmitgliedern) <br>

Erstellung eines R-Skripts mit verschiedenen Funktionen zur Analyse des Datensatzes <br>
Erstellung eines R-Skripts mit Helferfunktionen für das erste Skript

Aufgabe 4 (bearbeitet von Leia Betting, Romina Dubrow, Kathrin Henkenherm und Anna Herdick) <br>

Analyse des Datensatzes

Aufgabe 5 (bearbeitet von allen Gruppenmitgliedern) <br>

Diskussion der Ergebnisse und Erklärung der vorliegenden Zusammenhänge

--------------------------------------------------------------------------------

Es wurde R und R Studio in diesen Versionen genutzt:  
 R Studio 1.2.5042 
 R 4.0.3
 
--------------------------------------------------------------------------------

### Dateienliste: <br>

Aufgabe1-Github.R:    Simulation des Datensatz <br>

Aufgabe3.R:          Funktionen zur deskreptiven Datenanalyse <br>  

Datensatz.csv:        Datensatz aus Aufgabe 1 <br>

Hilfsfunktionen.R:   Verschiedene Hilfsfunktionen für Aufgabe 3<br>

--------------------------------------------------------------------------------

### Funktionen: 

#### deskr(x,...)

#### deskr_kat(x,...): 
Eine Funktion, die verschiedene geeignete deskriptive Statistiken für kategoriale Variablen berechnet und ausgibt.

Beispiele:

str(housing\$Freq) (Kardinal)<br>
str(housing\$Sat)  (Ordinal) <br>
str(housing\$Type) (Nominal)

deskr_kat(housing\$Freq)<br>
deskr_kat(housing\$Sat)<br>
deskr_kat(housing\$Type)<br>

#### zus_kat(x,y)

#### deskr_d(x,y)

#### e(x,Ordnung=TRUE)

#### katVis(x,y,z,w=NULL)

--------------------------------------------------------------------------------

### Hilfsfunktionen:

#### phi_str(x):

Berechnet das Phi-Streuungsmaß für einen Faktor

#### swap(x,y)

#### Faktor_ordnen(x,ord)

#### phi(x,y)

#### chi(x,y)

#### cramer(x,y)

#### pears(x,y)

#### pears.korr(x,y)

#### yule(x,y)

#### null(x,y)
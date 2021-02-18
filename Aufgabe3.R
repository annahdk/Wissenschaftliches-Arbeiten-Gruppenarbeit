###### Aufgabe 3 ########
# Funktionen

#a)
## Eine Funktion, die verschiedene geeignete deskriptive Statistiken
## für metrische Variablen berechnet und ausgibt

deskr <- function(x, ...){
  cat(" Deskriptive Statistiken: \n",
  "------------------------\n",
  "Arithm. Mittel:", mean(x, ...), "\n",
  "Median:", median(x, ...), "\n",
  "Varianz:", var(x, ...), "\n",
  "Std.abw.:", sd(x, ...), "\n",
  "Range:", range(x, ...), "\n",
  "Quantile: \n")
  for(i in 1:5){
    p <- c("0%:  ", "25%: ", "50%: ", "75%: ", "100%:")
    cat("  ", p[i] , quantile(x, type = 2, ...)[i], "\n")
  }
  cat(" ------------------------\n")
}

## Beispiele:
deskr(iris$Sepal.Length)
deskr(c(NA, 1:10), na.rm = TRUE)  ## mit NA und weitergereichtem na.rm

#b)
# Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# für kategoriale Variablen berechnet und ausgibt

deskr_kat <- function(x, ...){
  if(is.factor(x)){               ## Ist Eingabe ein Faktor?
    
    if(is.ordered(x)){            ## Ordinal
      
      cat("Ordinales Merkmal\n",
          "Modalwert:", levels(x)[which.max(table(x))], "\n",
          "Median:", levels(x)[quantile(x, type = 1, ...)[3]], "\n",
          "Quantile:", "\n")
      for (i in 1:5) {
        p <- c("0%:  ", "25%: ", "50%: ", "75%: ", "100%:")
        cat("  ", p[i] , levels(x)[quantile(x, type=1, ...)[i]], "\n")
      }
      cat("Phi-Streuungsmaß:", phi_str(x))
    }
    else{                         ## Nominal
      
      cat("Nominales Merkmal\n",
          "Modalwert:", levels(x)[which.max(table(x))], "\n",
          "Phi-Streuungsmaß:", phi_str(x))
    }
  }
  else stop("Funktion erwartet Faktor")
}


## Beispiele:

str(housing$Freq) ## Kardinal
str(housing$Sat)  ## Ordinal
str(housing$Type) ## Nominal

deskr_kat(housing$Freq)
deskr_kat(housing$Sat)
deskr_kat(housing$Type)

#c)

#d)

#e)


e <- function(x, Ordnung=TRUE,...){
  
     if(is.character(x)){ # wenn es einfach nur ein Vektor ohne geordnete Inhalte ist
    
        x <- factor(x,ordered=TRUE, levels=unique(x))}      
  
  
      if(is.numeric(x)|is.integer(x)){ # Wenn es ein Vektor metrischer Art ist
    
         x <- factor(x,ordered=TRUE)} # ich habe levels = x rausgenommen damit Zahlen sich
                                      # egal wie der Vektor angeordnet ist nach ihrer groesse
                                      # ordnen
      
    
    if(is.factor(x)){ #wenn es ein Faktor ist
      
       if(Ordnung){ # geordneter Faktor
        x <-  ordered(x[order(x)]) } # Jetzt ist der Faktor auch, wenn im Vektor die Werte vorher anders angeordnet waren, in der richtigen Reihenfolge
    
       else { # ungeordneter Faktor, man musste oben die gewuenschte Ordnung des Faktors angeben
       x <- Faktor_ordnen(x,...)   # ord = Vektor mit gewuenschter Ordnung des Faktors
        }
    
    }
       
  
  
     geordnet <- as.numeric(x) 
     g_4 <- quantile(geordnet,type = 1)[2] #untere quantilgrenze
     g4  <- quantile(geordnet,type = 1)[4] #obere quantilgrenze
     
     q_4 <- x[geordnet <= g_4] # unteres quantil
     q4 <- x[geordnet >= g4] # oberes quantil
     box <- x[geordnet > g_4 & geordnet < g4] # mittlere werte
    
  
 # ergebnis <- list("niedrig"=q_4,"mittel"=box,"hoch"=q4)
 # return(ergebnis)
  
    cat(" Quantilbasierte Kategorisierung: \n",
      "------------------------\n",
      "Niedrig:", as.character(q_4), "\n",
      "Mittel:", as.character(box), "\n",
      "Hoch:", as.character(q4), "\n",
      " ------------------------\n")
  
}



### Beispiel fuer ordinalen Vektor

## zum Testen, koennen wir dann auch wieder rausnehmen
# Faktor (geordnet)
#Akademisch <- c("Hauptschul","Realschul","Gymnasial","Hauptschul","Bachelor","Master","Dr","Prof")
#Akademisch1 <- factor(Akademisch,ordered=TRUE, levels=unique(Akademisch))

#Faktor(ungeordnet)
#Akademisch2 <- factor(Akademisch)


# Character -> muss noch geordnet werden
#Akademisch3 <- c("Hauptschul","Realschul","Gymnasial","Hauptschul","Bachelor","Master","Dr","Prof")

# Metrisch
#Zahlen <- 1:15


#e(Akademisch1)
#e(Akademisch2, Ordnung=FALSE, ord= unique(Akademisch))
#e(Akademisch3)
#e(Zahlen)
########################


#f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier kategorialen Variablen erstellt
library(ggplot2)
#Aufruf mit optionalem vierten Element
katVis <- function(x, y, z, w=NULL){
  # Variablen umsortieren, damit der Plot übersichtlich bleibt (Variable mit den wenigsten Leveln für color)
  if(which.min(c(nlevels(x), nlevels(y), nlevels(z)))!=3 & nlevels(x)<nlevels(y)){swap(x,z)}
  if(which.min(c(nlevels(x), nlevels(y), nlevels(z)))!=3 & nlevels(y)<nlevels(x)){swap(y,z)}
  # für drei Variablen
  if(is.null(w)){
    ggplot(data.frame(x,y,z)) + geom_jitter(aes(x,y,color=z), height=0.2, width=0.2) + scale_color_brewer(palette="Set1")
  }
  # für vier Variablen
  else{
    # umsortieren, Variable mit wenigsten Leveln für shape
    if(which.min(c(nlevels(x), nlevels(y), nlevels(z), nlevels(w)))!=4){swap(z,w)}
    ggplot(data.frame(x,y,z,w)) + geom_jitter(aes(x,y,color=z, shape=w), height=0.2, width=0.2) + scale_color_brewer(palette="Set1")
  }
}

##Testdaten
#x <- factor(sample(rep(letters[1:4], 10), 30))
#y <- ordered(sample(rep(1:7, 10), 30))
#z <- ordered(sample(rep(1:7, 10), 30))
#w <- factor(sample(rep(0:1, 100), 30))
#
#katVis(x,y,z,w)
#katVis(x,y,z)
#katVis(w,y,z,x)
#katVis(w,z)





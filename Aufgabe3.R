###### Aufgabe 3 ########
# Funktionen

#a)

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

## Hilfsfunktion Phi-Streuungsmaß

phi_str <- function(x){
  if(!is.factor(x)) stop("Funktion erwartet Faktor")
  
  ## Häufigkeitsverteilung der Levels:
  K <- length(levels(x)) 
  hk <- numeric(K)
  l <- length(x)
  for (i in 1:K) {
    s <- sum(x == levels(x)[i])
    hk[i] <- s/l
  }
  
  ## Berechnung des Phi-Streuungsmaß
  phi_min <- 2*(1-hk[which.max(table(x))])
  phi_max <- sum(abs(hk-1/K))
  
  phi_min/(phi_min+phi_max)
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

# Hilfsfunktion swap
swap <- function(x,y) {
  eval( parse( text = paste(
    "swap_unique_var_a<-", substitute(x), ";",
    substitute(x), "<-", substitute(y), ";",
    substitute(y), "<-swap_unique_var_a") ), env=parent.frame() )
}

##Testdaten
x <- factor(sample(rep(letters[1:4], 10), 30))
y <- ordered(sample(rep(1:7, 10), 30))
z <- ordered(sample(rep(1:7, 10), 30))
w <- factor(sample(rep(0:1, 100), 30))

katVis(x,y,z,w)
katVis(x,y,z)
katVis(w,y,z,x)
katVis(w,z)





## Hilfsfunktionen

## Phi-Streuungsmaß:

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


# Hilfsfunktion swap: Vertauschen zweier Variablen
swap <- function(x,y) {
  eval( parse( text = paste(
    "swap_unique_var_a<-", substitute(x), ";",
    substitute(x), "<-", substitute(y), ";",
    substitute(y), "<-swap_unique_var_a") ), env=parent.frame() )
}


### Hilfsfunktion fuer ungeordnete Faktoren Aufgabe e)

Faktor_ordnen <- function(x,ord){
 x <- factor(x,ordered=TRUE, levels=ord)
 return(x)
}

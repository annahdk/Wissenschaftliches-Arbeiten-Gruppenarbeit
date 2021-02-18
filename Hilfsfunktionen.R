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
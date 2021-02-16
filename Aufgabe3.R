###### Aufgabe 3 ########
# Funktionen

#a)

#b)

#c) Eine Funktion, die geeignete deskriptive bivariate Statistiken 
#   für den Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt


# alle kategoriellen Daten muessen hierfuer Faktoren und ordinale Daten zusaetzlich
# bereits richtig in R geordnet sein



zus_kat <- function(x, y){
  if(is.factor(x) && is.factor(y)){               ## Ist Eingabe ein Faktor?
    if(is.ordered(x) && is.ordered(y)){           ## Ordinal
      library(GoodmanKruskal)             
      x <- as.numeric(x)
      y <- as.numeric(y)
      cat("Ordinales Merkmal:\n",
          "Rangkorrelationskoeffizienten nach...\n",
          "...Spearman:", cor(x,y, method = "spearman"), 
          "\n ...Kendall:", cor(x,y, method = "kendall"), 
          "\n ...Goodman und Kruskal:", GKtau(x,y)$tauxy, "\n")
    }
    else{                                         ## Nominal
      cat("Nominales Merkmal:\n",
          "Kontingenzkoeffizienten...\n")
      if(length(levels(x)) == 2 && length(levels(y)) == 2){     ## beide dichotom?
        cat(if(null(x,y) == FALSE){"...nach Yule:"}, if(null(x,y) == FALSE){yule(x,y)}, "\n...Phi:", phi(x,y))
      } 
      else{
        cat("...nach Cramér:", cramer(x,y))
      } 
      cat("\n...nach Pearson:", pears(x,y), ", korrigiert:", pears.korr(x,y), "\n")
    }
  }
  else stop("Funktion erwartet Faktor")
}

# Testdaten:
# ordinal
o <- c("langweilig", "ok", "spannend") 
a <- ordered(sample(o, 10, replace = TRUE), levels = o) 
p <- c("Professor", "Doktor", "M. Sc.", "B. Sc.")
b <- ordered(sample(p, 10, replace = TRUE), levels = rev(p)) 
zus_kat(a,b)
# nominal dichotom
c <- factor(sample(c(rep("vergeben", 3), "single"), 10, replace = TRUE))
d <- factor(sample(c(rep("happy", 3), "sad"), 10, replace = TRUE))
zus_kat(c,d)
# nominal
e <- factor(sample(c(rep("vergeben", 3), "single"), 10, replace = TRUE))
f <- factor(sample(c("Professor", "Doktor", "M. Sc.", "B. Sc."), 10, replace = TRUE))
zus_kat(e,f)


## Hilfsfunktionen 

# Phi-Koeffizient

phi <- function(x,y){
  t <- table(x,y)
  (t[1,1] * t[2,2] - t[1,2] * t[2,1])/sqrt((t[1,1] + t[1,2]) * (t[2,1] + t[2,2])
                                           * (t[1,1] + t[2,1]) * (t[1,2] + t[2,2]))
}

# Chi-Koeffizient 

chi <- function(x,y){
  t <- addmargins(table(x,y))
  E <- rep(t[-length(levels(x)) + 1,length(levels(y)) + 1], each = length(levels(y))) * 
    rep((t[length(levels(x)) + 1,-length(levels(y)) + 1]/length(x)), length(levels(x)))
  sum(((as.vector(table(x,y)) - E)^2) %*% E^(-1))
}

# Cramér

cramer <- function(x,y){
sqrt(chi(x,y)/(length(x) * min(length(levels(x)), length(levels(y)))))
}

# Pearson

pears <- function(x,y){
  sqrt(chi(x,y)/(chi(x,y) + length(x)))
}

pears.korr <- function(x,y){
  pears(x,y) * sqrt(min(length(levels(x)), length(levels(y)))/(min(length(levels(x)), length(levels(y))) - 1))
}

# Yule 

yule <- function(x,y){
  t <- table(x,y)
  (t[1,1] * t[2,2] - t[1,2] * t[2,1])/(t[1,1] * t[2,2] + t[1,2] * t[2,1])
}


# Nullzeilenueberpruefung 

null <- function(x,y){
  z <- ifelse(addmargins(table(x,y))[,length(levels(y)) + 1] != 0, FALSE, TRUE)
  s <- ifelse(addmargins(table(x,y))[length(levels(x)) + 1, ] != 0, FALSE, TRUE)
  if(any(c(z,s))) TRUE else FALSE
}


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





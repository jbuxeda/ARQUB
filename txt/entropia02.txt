"entropia02"<-
  function(x)
  {
    # Calcula l'entropia d'un fitxer donat fila per fila.
    # Feta per Jaume Buxeda i Garrigos.
    n <- dim(x)[1]
    d <- dim(x)[2]
    varmat <- matrix(0, n, d + 2)
    varmat2 <- matrix(0, n, d)
    {
      # normalitza a 100% els valors de la matriu x (es la probabilitat inicial)
      varmat2 <- as.matrix(sweep(x, 1, apply(x, 1, sum), FUN = "/"))
    }
    for (j in 1:n)
    {
      for (k in 1:d)
      {
        if (varmat2[j, k] == 0) {
          varmat[j, k] <- 0} else {
            # per cada probabilitat es calcula el seu logaritme en base 2 (es el reciproc de la probabilitat perque l'inici de la formula es un signe negatiu)
            varmat[j, k] <- log((1/varmat2[j, k])) / log(2)
            # l'entropia es la suma dels logaritmes en base 2 multiplicats per la probabilitat
            varmat[j, d + 1] <- varmat[j, d + 1] + (varmat[j, k] * varmat2[j, k])
          }
      }
    } 
    # ara es calcula l'entropia relativa respecte del logaritme en base 2 del logaritme del numero de dimensions
    varmat[ , d + 2] <- varmat[ , d + 1] / (log(d) / log(2))
    # es controlen les etiquetes
    dimnames(varmat) <- list(c(dimnames(x)[[1]]), c(dimnames(x)[[2]], "H2", "H2%"))
    # sortida
    list(Entropia = as.data.frame(varmat), Probabilitat = as.data.frame(varmat2))
  }

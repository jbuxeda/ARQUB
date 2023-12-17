"evariationmatrix2"<-
  function(noux, idioma)
  {
    # Agafa el fitxer x calcula la matriu de variacio composicional d'acord amb l'article de Buxeda 1999.
    # Feta per Jaume Buxeda i Garrigos.
# -----Creem les matrius varmat i varmat2 on guardarem la MVC-----
    p <- dim(noux)[2]
    varmat <- matrix(0, p, p)
    varmat2 <- matrix(0, p+4, p)
# -----Fem el calcul de la MVC-----
    # Fem cada columna com la diagonal de la matriu de variancies-covariancies de la transformacio alr per cada component i
    for(i in 1:p) {
      varmat[ , i] <- diag(var(log(noux/noux[ , i])))
    }
    # Calculem el sumatori de cada columna
    varsum <- apply(varmat, 2, sum)
    # Calculem la variacio total
    totvar <- sum(varmat)/(2 * p)
    # calculem la proporcio que la vt representa de cada valor t.j
    varprop <- totvar/varsum
    # creem un vector on guardarem el resultat de les correlacions dels valors tij amb els totals 
    varcor <- vector(mode = "numeric", length = p)
    # fem el calcul de les correlacions, excloent els valors corresponents al component i
     for(i in 1:p) {
      varcor[i] <-cor(varmat[-c(i), i], varsum[-i])
     }
# -----Fem els grafics de la MVC-----
    # Creem una llista amb els noms dels components
    hola <- as.list(dimnames(noux)[[2]])
    # Iniciem els parametres dels grafics
    par(mar = c(5, 5, 4, 2) + 0.1, mgp = c(3, 1, 0))
    # fem els bivariants de cada divisor en transformacio alr amb les sumes de les columnes, excloent el component i
    for(i in 1:p) {
      # fem el diagrama de dispersio bivariant, sense posar punts
      plot(varsum[-i], varmat[-c(i), i], xlab = list(expression(tau[.j]), font = 2, cex = 1.5), ylab=list(paste("var(ln(x/", dimnames(noux[i])[[2]], "))", sep = ""), cex = 1, font = 2), type = "n", cex.axis = 0.8)
      # ara posem els punts amb les etiquetes dels components
      text(varsum[-i], varmat[-c(i), i], labels = dimnames(noux[-i])[[2]], cex = 0.5)
      # arrodonim la correlacio variancies i sumes, sense el valor del component i, i ho posem com a text en el grafic
      rvarcor <- round(varcor[i], dig = 6)
      text(min(varsum[-i]), max(varmat[-c(i), i]), label = bquote("r"[v * tau] * " = " * .(rvarcor)), cex = 0.7, adj = 0)
    }
# -----Escrivim la MVC en el seu format final-----
    # entrem els valors fila per fila
    for(i in 1:p) {
      varmat2[i, ] <- varmat[i, ]
    }
    varmat2[p+1, ] <- varsum
    varmat2[p+2, ] <- varprop
    varmat2[p+3, ] <- varcor
    varmat2[p+4, 1] <- totvar
    # entrem les etiquetes
    if (idioma == 4) {
      dimnames(varmat2) <- list(c(dimnames(noux)[[2]], "t.j", "tv/t.j", "r v,t", "tv"), c(dimnames(noux)[[2]]))
    } else {
      dimnames(varmat2) <- list(c(dimnames(noux)[[2]], "t.j", "vt/t.j", "r v,t", "vt"), c(dimnames(noux)[[2]])) 
    }
# -----Sortida-----
    varmat2
  }

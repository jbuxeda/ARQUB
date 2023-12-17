"inicialCurt"<-
  function(x, talls = c(0.3, 0.5, 0.9), nom = "Dades", nonum = 0, idioma = 1)
  {
    # Talls son els talls verticals que se situen segons el que la vt representa en cada transformacio alr. Si es posa nomes 0, no hi ha talls.
    # Nom te per defecte Dades (adaptat a l'idioma). Si es canvia, es pot posar el de les dades que es treballen com a titol. Llavors tambe es tindra el valor de n. Si es posa NA entre cometes, no hi ha titol ni valor n.
    # Idioma: 1- catala, 2- castella, 3- angles, 4- frances. nonum permet indicar quines columnes tenen dades no num�riques que no s'han d'utilitzar
    # Rutina per tal de calcular la matriu de variacio composicional i fer els grafics parcials i el grafic d'uniformitat composicional.
    # Feta per Jaume Buxeda i Garrigos.
    # -----Identifiquem si hi ha dades negatives o zeros en les dades a transformar, primer quan totes són numèriques i després quan hi ha no numèriques-----
    if (nonum[1] == 0) {
      if (min(x) <= 0) {
        cat("Negative or zero data - terminating.")
        return()
      }
    }
    if (nonum[1] > 0) {
      if(min(x[ , -c(nonum)]) <= 0) {
        cat("Negative or zero data - terminating.")
        return()
      }
    }
    # -----Opcions d'iniciacio per evitar la notacio cientifica i controlar els decimals-----
    options(scipen = 100, digits = 6)
    # -----Iniciem les finestres per les imatges (segons sistema operatiu, pero no inclou Linux)-----
    if(Sys.info()[["sysname"]] == "Windows") {
      windows(record = T)
    } else {
      quartz()
    }
    # -----Creacio de la matriu per guardar els valors dels talls (bbb)-----
    aaa <- length(talls)
    bbb <- matrix(0, aaa, 1)
    # -----Creacio de la matriu per guardar els valors de les traces alr i la vt (mat)-----
    if (nonum[1] == 0) {
      pop <- dim(x)[2] + 1 
    }
    else {
      pop <- dim(x)[2] - length(nonum) + 1
    }
    mat <- matrix(0, pop, 1)
    # -----Passem les dades de x a noux per assegurar-nos que treiem les variables no numeriques. Quan no n'hi ha, tambe, per comoditat-----
    if (nonum[1] == 0) {
      # creem una matriu per posar-hi els valors
      noux <- matrix(0, dim(x)[1], dim(x)[2])
      noux <- x
    } 
    else 
    {
      # creem una matriu per posar-hi els valors, treient les columnes no numeriques
      noux <- matrix(0, dim(x)[1], dim(x)[2] - length(nonum))
      noux <- x[ , -c(nonum)]
    }
    # -----Calcul de la matriu de variacio composicional (MVC) cridant la subrutina evariationmatrix2 i convertint-la en data.frame-----
    MVC <- evariationmatrix2(noux, idioma)
    MVC <- as.data.frame(MVC)
    # -----Inici del grafic d'uniformitat. Agafo la transposta per poder agafar les traces alr (ara columna pop), ordenar-ho i posar-ho a mat-----
    tMVC <- as.data.frame(t(MVC))
    ordre <- order(tMVC[ , c(pop)], decreasing = T)
    tMVC <- as.data.frame(tMVC[ordre, ])
    for(i in 1:(pop - 1)) {
      mat[i, 1] <- tMVC[i, pop]
    }
    # Afegeixo el valor de la vt
    mat[pop, 1] <- MVC[pop + 3, 1]
    # Afegeixo els noms de les files. Com ho faig amb la transposta reordenada, es facil tenir les etiquetes corresponents. Aixo servira pel grafic
    if (idioma == 3) {
      dimnames(mat)[[1]] <- c(dimnames(tMVC)[[1]], "tv")
    } else {
      dimnames(mat)[[1]] <- c(dimnames(tMVC)[[1]], "vt")
    }
    # -----Ara inicio els calculs de l'entropia-----
    # N'agafo la transposta, excepte el valor de la vt
    tmat <- t(mat[c(1:pop - 1), ])
    # Calculo l'entropia amb la subrutina entropia02 sobre els valors de les traces alr als que se'ls ha restat la vt
    tmatentro <- entropia02(tmat - mat[pop, 1])
    # -----Ara faig el grafic d'uniformitat composicional-----
    # Poso els marges
    par(mar = c(5, 5, 4, 2) + 0.1, mgp = c(3, 1, 0))
    # faig el grafic
    if (nom == "NA") {
      plot(mat[ , 1], type = "p", pch = 16, ylab = list(expression(italic(tau[.j])), font = 2, cex = 2), xlab = "", xaxt = "n", ylim = c(0, trunc(mat[1, 1]) + 1), cex.axis = 0.8)      
    }
    else {
      # Si es deixa el nom per defecte, cal tenir en compte l'idioma
      if (nom == "Dades") {
        if (idioma == 2) {nom = "Datos"}
        if (idioma == 3) {nom = "Data"}
        if (idioma == 4) {nom = "Donn�es"}
      } 
      plot(mat[ , 1], type = "p", pch = 16, ylab = list(expression(italic(tau[.j])), font = 2, cex = 2), xlab = "", xaxt = "n", ylim = c(0, trunc(mat[1, 1]) + 1), cex.axis = 0.8, main = bquote(paste(.(nom), " (" * italic("n") *" = ", .(dim(x)[1]), ")")))
    }
    # Ara fem les etiquetes dels elements amb format segons la subrutina etiqueteselements
    eti <- etiqueteselements(mat)
    # Fem les marques a l'eix d'abscisses
    axis(1, at = 1:pop, labels = eti, font = 2, las = 2)
    # Posem les linies entre els punts dels components
    lines(mat[c(1:pop - 1), 1])
    # Posem la linia de punts de la vt
    lines(c(0, pop), c(mat[pop, 1], mat[pop, 1]), lty = 3)
    # Posem les linies dels talls
    for (i in 1:aaa) {
      # Calculem quants valor t.j son menors que cadascun dels talls
      bbb[i, 1] <- length(which(MVC[pop + 1, ] < talls[i]))
      if (bbb[i, 1] > 0) {
        # Ara posem el tall. Les coordenades inferiors son el numero de components i mig, per la x, i el valor y manim del grafic (par("usr")[3])
        # el superior: el mateix per la x i per la y es una formula empirica on se suma a 2.5 vegades la vt el valor de la vt dividit pel valor del tall
        segments(bbb[i, 1] + 0.5, par("usr")[3], bbb[i, 1] + 0.5, (mat[pop, 1] * 2.5) + (mat[pop, 1] / talls[i]), lty=3)
        # ara posem les etiquetes sobre fons blanc (boxed.labels). Les coordenades son els components i mig, per la x, i la meitat de la vt, per la y
        boxed.labels(bbb[i, 1] + 0.5, mat[pop, 1] / 2, talls[i], cex = 0.6, col = "black", bg = "white", border = NA)
      }
    }
    # Per finalitzar, introduim l'entropia de la informacio i el seu valor percentual respecte del maxim possible. Tambe la vt
    h2 <- round(tmatentro$Entropia[1, pop], dig = 2)
    h2p <- round(tmatentro$Entropia[1, pop + 1] * 100, dig = 2)
    vt <- round(mat[pop, 1], dig = 2)
    # Els calculs de les coordenades, amb formules empiriques. Per la vt es te en compte l'idioma
    text(pop - 5, (trunc(mat[1,1]) + 1) - ((trunc(mat[1, 1]) + 1) / 14), label = bquote("     " * italic("H")[2] * " = " * .(h2) * " Sh"), cex = 0.8, adj = 0)
    text(pop - 5, (trunc(mat[1,1]) + 1) - ((trunc(mat[1, 1]) + 1) / 8.5), label = bquote("  " * italic("H")[2] * "% = " * .(h2p)), cex = 0.8, adj = 0)
    if (idioma == 3) {
      text(pop - 5, (trunc(mat[1,1]) + 1) - ((trunc(mat[1, 1]) + 1) / 6.2), label = bquote("      " * italic("tv") * " = " * .(vt)), cex = 0.8, adj = 0)
    } else {
      text(pop - 5, (trunc(mat[1,1]) + 1) - ((trunc(mat[1, 1]) + 1) / 6.2), label = bquote("      " * italic("vt") * " = " * .(vt)), cex = 0.8, adj = 0)
    }
    # guardem el grafic en format emf i pdf
    xxx <- recordPlot()
    # -----Ara grabem els grafics guardats-----
    emf("uniformitat.emf")
    replayPlot(xxx)
    dev.off()
    pdf("uniformitat.pdf")
    replayPlot(xxx)
    dev.off()
    # -----Restablim els parametres-----
    par(mar=c(5,4,4,2)+0.1,mgp=c(3,1,0))
    # -----Llistem les sortides escrites-----
    list(MVC = MVC, Entropia = tmatentro$Entropia, Probabilitat = tmatentro$Probabilitat)
  }

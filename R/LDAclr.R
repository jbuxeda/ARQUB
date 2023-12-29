"LDAclr"<-
  function(xclr, y = NA, pprevia = 0, grupx = 1, llegendax = grupx, ggrupx = grupx, gllegendax = ggrupx, grupy = 1, llegenday = grupy, ggrupy = grupy, gllegenday = ggrupy, titol1 = 0, titol2 = 0, idioma = 1, paleta = 1, paleta0 = NA, color = "chocolate", simbol = 0, simbol0 = NA, midall = 0.8, nom = NA, lds = 3, colpf = "grey85", tex = 1, cexpunts = 1)
  {
    # Fa un MASS::lda() sobre el fitxer xclr, que ja es un fitxer clr. S'hi poden comparar les mostres del fitxer y, que tambe ha de ser un fitxer clr. Idioma: 1- catala, 2- castella, 3- angles, 4- frances
    # grupx i llegendax son columnes de xclr. pprevia (per defecte 0) es la probabilitat a priori. 0 implica la proporcio de cada grup. 1, la mateixa probabilitat per tots els grups.
    # Els grups son obligatoris i cal especificar a quina columna hi ha el factor de grup (a la 1 per defecte) i a quina la llegenda (si no es diu res, s'interpreta que es el mateix que els grups).
    # Els grups s'enumeren d'1 en endavant i, si hi ha fitxer y, les numeracions segueixen i no tornen a comensar.
    # A la paleta: 0- lliure, 1- arqub (per defecte), 2- arqub en blanc i negre, 3- rainbow, 4- heat.colors, 5- terrain.colors, 6- topo.colors, 7- cm.colors.
    # Si la paleta es 0 vol dir que s'escullen els colors. La manera es, per exemple, paleta0 = c("red", "blue").
    # El grup ha de ser un as.factor i la llegenda, si no es el grup, as.character, pero la rutina ja ho vigila. El nom es pel grafic de les funcions discriminants. Per defecte, no n'hi ha (NA). Si n'hi ha, s'afegeix n de xclr
    # color es refereix al que tenen les barres del diagrama sobre les variancies entre grups (maxim 10).
    # simbol es pels simbols que s'utilitzaran per representar els individus, per defecte (simbol = 0) es 21. Si simbol <> 0, llavors s'agafa
    # el llistat de simbols que sigui, per exemple, simbol0 = c(21, 24). Els simbols han d'anar entre el 21 i el 25. En cas que hi hagi dues categoriques (que ggrupx sigui diferent de grupx) simbol0 es obligatori per posar els simbols de la segona categorica.
    # midall (per defecte 0.8) es la mida del text de la llegenda. Si es canvia aquest argument, la llegenda pot ser mes gran (midall > 0.8; per exemple, midall = 1) o mes petita (midall < 0.8; per exemple, midall = 0.6)
    # lds (per defecte 3) es el numero de funcions discriminants amb les que es fan els grafics.
    # Les uniques variables numeriques son les que es faran servir per calcular les funcions disciminants.
    # Si la llegenda es doble, ggrupx sera diferent de grupx; Llavors el grup y tambe pot tenir un ggrupy diferent de grupy. Els dos poden tenir la seva propia llegenda (gllegendax i gllegenday).
    # titol1 i titol2 son els titols en cas de llegendes dobles. Si es volen deixar els titols buits es deixa el 0 (per defecte).
    # Les fletxes i etiquetes dels coeficients no estandarditzats s'introdueixen en els grafics a partir de les instruccions creades a per la funcio lda.arrows
    # creada per l'usuari Tyler i publicada a https://stackoverflow.com/questions/17232251/how-can-i-plot-a-biplot-for-lda-in-r
    # per aixo colpf indica el color que han de tenir les fletxes i tex la mida de les etiquetes. Finalment, cexpunts indica la mida dels simbols del grafic
    # Feta per Jaume Buxeda i Garrigos.
    # -----Primer ens assegurem que el grupx i ggrupx, si n'hi ha, i la llegenda, si n'hi ha, son factor i carocter respectivament-----    
    if (grupx > 0) {
      xclr[ , grupx] <- as.factor(xclr[ , grupx])
      if (llegendax != grupx) {
        xclr[ , llegendax] <- as.character(xclr[ , llegendax])
      }
      if (ggrupx != grupx) {
        xclr[ , ggrupx] <- as.factor(xclr[ , ggrupx])
        if (gllegendax != ggrupx) {
          xclr[ , gllegendax] <- as.character(xclr[ , gllegendax])
        }
        
      }
    }
    # -----Controlem que idioma tingui un valor valid-----
    if (idioma < 1 | idioma > 4) {return(cat("Non valid idioma value - terminating."))}
    # -----Controlem que pprevia tingui un valor valid-----
    if (pprevia < 0 | pprevia > 1) {return(cat("Non valid pprevia value - terminating."))}
    # -----Controlem que paleta tingui un valor valid-----
    if (paleta < 0 | paleta > 7) {return(cat("Non valid paleta value - terminating."))}
    # -----Controlem que simbol tingui un valor valid-----
    if (simbol != 0) {
      if (min(simbol0) < 21 | max(simbol0) > 25) {
        return(cat("Non valid simbol value - terminating."))
      }
    }
    # controlem que, si hi ha dues variables categoriques, s'hagin assignat els simbols a simbol0
    if (ggrupx != grupx) {
      if (is.na(simbol0)) {
        return(cat("With two categorical variables simbol0 is needed - terminating."))
      }
    }
    # -----Iniciem les finestres per les imatges (segons sistema operatiu, pero no inclou Linux)-----
    if(Sys.info()[["sysname"]] == "Windows") {
      windows(record = T)
    } else {
      quartz()
    }
    #-----retenim ara les variables numeriques sobre les que es fara el lda()-----
    verifica <- unlist(lapply(xclr, is.numeric))
    xclrnum <- xclr[ , verifica]
    #-----Fem el lda()-----
    if (pprevia == 0) {
      xclrlda <- lda(x = xclrnum, grouping = xclr[ , grupx])
    } else {
      xclrlda <- lda(x = xclrnum, grouping = xclr[ , grupx], prior = rep(1, nlevels(xclr[ , grupx])) / nlevels(xclr[ , grupx]))
    }
    bgv <- round((xclrlda$svd^2 / sum(xclrlda$svd^2)) * 100, digit = 2)
    # bgv es la variancia entre grups explicada per cada discriminant lineal
    # fem ara el Predict
    xclrldaPredict <- predict(xclrlda)
    # salvem els coeficients dels discriminants lineals
    coefsLDA <- as.data.frame(xclrlda$scaling)
    # salvem la taula de confusio
    taulaxvar <- table(xclr[ , grupx], xclrldaPredict$class)
    # creem un fitxer x amb la classificacio
    xclrClass <- cbind(xclrldaPredict$class, xclr)
    colnames(xclrClass)[1] <- "Class"
    # creem un fitxer de projeccions LD que incorporen grup i, si escau, llegenda
    projeccionsLD <- as.data.frame(xclrldaPredict$x)
    if (llegendax == grupx) {
      projeccionsLD <- cbind(xclr[ , grupx], projeccionsLD)
      colnames(projeccionsLD)[1] <- "G"
      nonumLD <- 1
    } else {
      projeccionsLD <- cbind(xclr[ , grupx], xclr[ , llegendax], projeccionsLD)
      colnames(projeccionsLD)[1] <- "G"
      colnames(projeccionsLD)[2] <- "Llegenda"
      nonumLD <- 2
    }
    #-----Fem el diagrama de barres de les variancies-----
    # Agafem un maxim de 10 funcions disciminants
    npcs = min(10, length(xclrlda$svd))
    # agafem el valor de les bgv i calculem l'eix de les y, per tal que sigui mes alt
    afer <- xclrlda$svd[1:npcs]^2
    if (afer[1] <= 1) {
      sostre <- ceiling((afer[1]) * 100 / 5)
      sostre <- (sostre * 5) / 100
    }
    if (afer[1] > 1) {
      sostre <- ceiling((afer[1]) * 10 / 5)
      sostre <- (sostre * 6) / 10
    }
    # calculem els % de les variancies
    sdevpercent <- as.vector(as.character(format(bgv, nsamll = 2)))
    # escollim el titol de l'eix y segons idioma
    if (idioma == 1) {titoly <- as.character("Variàncies entre grups")}
    if (idioma == 2) {titoly <- as.character("Varianzas entre grupos")}
    if (idioma == 3) {titoly <- as.character("Between-group variances")}
    if (idioma == 4) {titoly <- as.character("Variances inter-groupe")}
    # faig el grafic i el salvo
    # primer miro les mides de lletres segons funcions discriminants
    if (npcs > 8) {
      cexnames = 0.65
      cexpercent = 0.7
    } else {
      cexnames = 0.75
      cexpercent = 0.8
    }
    bar <- barplot(afer, col = color, cex.axis = 0.75, cex.names = 0.65, ylim = c(0, sostre), ylab = titoly)
    # ara calculo quantes etiquetes de percent ha de posar
    sdevpercentnpcs <- sdevpercent[1:npcs]
    text(bar, afer, pos = 3, labels = paste(sdevpercentnpcs, "%", sep = ""), cex = cexpercent)
    # Salvo el diagrama de barres de les covariancies com a pdf i emf   
    barvar <- recordPlot()
    pdf("BarplotVar.pdf")
    replayPlot(barvar)
    dev.off()
    emf("BarplotVar.emf")
    replayPlot(barvar)
    dev.off()
    #-----Prepara simbols, paletes i llegenda-----
    # Preparem la llegenda, si es el cas. Primer els grups que hi ha, tant si hi ha grup de comparacio com si no
    if (is.na(y)) {
      # aixo nomes es cert si no hi ha un grup per projectar
      # calculo el numero de grups i guardo el numero en nivellsx
      idllegenda <- vector(length = nlevels(xclr[ , grupx]))
      nivellsx <- nlevels(xclr[ , grupx])
      nivellstot <- nivellsx
      # grup per grup, assigno l'etiqueta, tenint en compte si llegendax es com grupx o com llegendax
      for (ll in 1:nlevels(xclr[ , grupx])) {
        z <- which(xclr[ , grupx] == levels(xclr[ , grupx])[ll])
        if (llegendax != grupx) {
          idllegenda[ll] <- xclr[z[1], llegendax]
        } else {
          idllegenda[ll] <- as.character(xclr[z[1], grupx])
        }
      }
      if (ggrupx != grupx) {
        # aixo nomes es cert si no hi ha un grup per projectar i s'usa una segona variable categorica
        # calculo el numero de grups i guardo el numero en nivellsxg
        gidllegenda <- vector(length = nlevels(xclr[ , ggrupx]))
        nivellsxg <- nlevels(xclr[ , ggrupx])
        # grup per grup, assigno l'etiqueta, tenint en compte si llegendax es com ggrupx o com gllegendax
        for (ll in 1:nivellsxg) {
          z <- which(xclr[ , ggrupx] == levels(xclr[ , ggrupx])[ll])
          if (gllegendax != ggrupx) {
            gidllegenda[ll] <- xclr[z[1], gllegendax]
          } else {
            gidllegenda[ll] <- as.character(xclr[z[1], ggrupx])
          }
        }
      }
    } else {
      # ara si que hi ha grup de comparacio per projectar
      # -----Primer ens assegurem que el grupy i ggrupy, si n'hi ha, i la llegenday, si n'hi ha, d'y son factor i caracter respectivament-----    
      y[ , grupy] <- as.factor(y[ , grupy])
      if (llegenday != grupy) {
        y[ , llegenday] <- as.character(y[ , llegenday])
      } 
      if (ggrupy != grupy) {
        y[ , ggrupy] <- as.factor(y[ , ggrupy])
        if (gllegenday != grupy) {
          y[ , gllegenday] <- as.character(y[ , gllegenday])
        } 
      }
      # Ara cal fer un data.frame amb grupx i llegendax combinats amb grupy i llegenday (per si hi ha grups repetits a y que ja estan a x)
      # d'aqui en treure un data.matrix per assignar directament les etiquetes, aixi com el numero de grups (nivellstot), quant n'hi ha a x (nivellsx), a y (nivellsy) i quants d'y no estan a x (nivellsynous)
      grupconjunt <- matrix(0, dim(xclr)[1] + dim(y)[1], 2)
      grupconjunt <- as.data.frame(grupconjunt)
      for (g in 1:dim(xclr)[1]) {
        grupconjunt[g , 1] <- as.character(xclr[g , grupx])
        grupconjunt[g , 2] <- as.character(xclr[g , llegendax])
      }
      for (g in (dim(xclr)[1] + 1):(dim(xclr)[1] + dim(y)[1])) {
        grupconjunt[g , 1] <- as.character(y[(g - dim(xclr)[1]), grupy])
        grupconjunt[g , 2] <- as.character(y[(g - dim(xclr)[1]), llegenday])
      }
      grupconjunt[ , 1] <- as.factor(grupconjunt[ , 1])
      grupconjunt[ , 2] <- as.character(grupconjunt[ , 2])
      nivellstot <- nlevels(grupconjunt[ , 1])
      idllegenda <- vector(length = nivellstot)
      nivellsx <- nlevels(xclr[ , grupx])
      nivellsy <- nlevels(y[ , grupy])
      nivellsynous <- nivellstot - nivellsx
      for (ll in 1:nivellstot) {
        z <- which(grupconjunt[ , 1] == levels(grupconjunt[ , 1])[ll])
        idllegenda[ll] <- grupconjunt[z[1], 2]
      }
      # si la llegenda es doble, preparem la segona part de la llegenda
      if (ggrupx != grupx) {
        # ara si que hi ha grup de comparacio per projectar
        # Ara cal fer un data.frame amb grupx i llegendax combinats amb grupy i llegenday (per si hi ha grups repetits a y que ja estan a x)
        # d'aqui en treure una matrix per assignar directament les etiquetes, aixi com el numero de grups (nivellstot), quant n'hi ha a x (nivellsx), a y (nivellsy) i quants d'y no estan a x (nivellsynous)
        ggrupconjunt <- matrix(0, dim(xclr)[1] + dim(y)[1], 2)
        ggrupconjunt <- as.data.frame(ggrupconjunt)
        for (g in 1:dim(xclr)[1]) {
          ggrupconjunt[g , 1] <- as.character(xclr[g , ggrupx])
          ggrupconjunt[g , 2] <- as.character(xclr[g , gllegendax])
        }
        for (g in (dim(xclr)[1] + 1):(dim(xclr)[1] + dim(y)[1])) {
          ggrupconjunt[g , 1] <- as.character(y[(g - dim(xclr)[1]), ggrupy])
          ggrupconjunt[g , 2] <- as.character(y[(g - dim(xclr)[1]), gllegenday])
        }
        ggrupconjunt[ , 1] <- as.factor(ggrupconjunt[ , 1])
        ggrupconjunt[ , 2] <- as.character(ggrupconjunt[ , 2])
        nivellstotg <- nlevels(ggrupconjunt[ , 1])
        gidllegenda <- vector(length = nivellstotg)
        nivellsxg <- nlevels(xclr[ , ggrupx])
        nivellsyg <- nlevels(xclr[ , ggrupy])
        nivellsynousg <- nivellstotg - nivellsxg
        for (ll in 1:nivellstotg) {
          z <- which(ggrupconjunt[ , 1] == levels(ggrupconjunt[ , 1])[ll])
          gidllegenda[ll] <- ggrupconjunt[z[1], 2]
        }
      }
    }
    # ara faig la llegenda final (totidllegenda) incloent titols, si escau
    if (ggrupx == grupx) {
      totidllegenda <- idllegenda
    } else {
      if (titol1 == 0 & titol2 == 0) {      
        totidllegenda <- c(idllegenda, NA, gidllegenda)
      }
      if (titol1 != 0 & titol2 == 0) {
        totidllegenda <- c(bquote(.(titol1)), idllegenda, NA, gidllegenda)
      }
      if (titol1 == 0 & titol2 != 0) {
        totidllegenda <- c(idllegenda, bquote(.(titol2)), gidllegenda)
      }
      if (titol1 != 0 & titol2 != 0) {
        totidllegenda <- c(bquote(.(titol1)), idllegenda, bquote(.(titol2)), gidllegenda)
      }
    }
    # faig els simbols pels grups
    if (ggrupx == grupx) {
      if (simbol == 0) {
        simbols <- as.vector(rep(21, nivellstot))
      } else {
        simbols <- vector(length = length(simbol0))
        simbols <- simbol0
      }
    } else {
      simbols <- vector(length = length(simbol0))
      simbols <- simbol0
    }
    # Fem les paletes arqub en color (arqub) i en blanc i negre (arqubBN)
    if (paleta == 1) {
      arqub <- vector(length = 15)
      arqub <- c("gray75", "grey9", "cyan", "red", "goldenrod1", "dodgerblue", "darkgoldenrod4", "chartreuse1",  "darkgreen", "indianred1", "blue", "darkmagenta", "maroon1", "aquamarine", "lightpink")
      colx <- arqub
    }
    if (paleta == 2) {
      arqubBN <- vector(length = 7)
      arqubBN <- c("white", "grey90", "grey70", "grey50", "grey30", "grey10", "black")
      colx <- arqub
    }
    # Fer la paleta lliure, si es el que toca
    if (paleta == 0) {
      colx <- vector(length = length(paleta0))
      colx <- paleta0
    }
    # Ara s'assignen les paletes fetes per R
    if (paleta == 3) {
      colx <- c(rainbow(nlevels(xclr[ , grupx]) + 1))
    }
    if (paleta == 4) {
      colx <- c(heat.colors(nlevels(xclr[ , grupx]) + 1))
    }
    if (paleta == 5) {
      colx <- c(terrain.colors(nlevels(xclr[ , grupx]) + 1))
    }
    if (paleta == 6) {
      colx <- c(topo.colors(nlevels(xclr[ , grupx]) + 1))
    }
    if (paleta == 7) {
      colx <- c(cm.colors(nlevels(xclr[ , grupx]) + 1))
    }
    #-----Abans de fer els grafics, mirem si hi ha grup y i fem la seva projeccio, deixant tot a punt-----
    if (!is.na(y)) {
      #-----retenim ara les variables numeriques de y sobre les que es fara la lda()-----
      verifica <- unlist(lapply(y, is.numeric))
      ynum <- y[ , verifica]
      xclrldaPredictNewdata <- predict(xclrlda, newdata = ynum)
      # ara faig la taula de confusio d'y
      taulay <- table(y[ , grupy], xclrldaPredictNewdata$class)
      # creem un fitxer y amb la classificacio
      yClass <- cbind(xclrldaPredictNewdata$class, y)
      colnames(yClass)[1] <- "Class"
      # ara afageixo aquestes projeccions al fitxer de projeccions LD
      projeccionsyLD <- as.data.frame(xclrldaPredictNewdata$x)
      if (nonumLD == 1) {
        projeccionsyLD <- cbind(y[ , grupy], projeccionsyLD)
        colnames(projeccionsyLD)[1] <- "G"
      } else {
        projeccionsyLD <- cbind(y[ , grupy], y[ , llegenday], projeccionsyLD)
        colnames(projeccionsyLD)[1] <- "G"
        colnames(projeccionsyLD)[2] <- "Llegenda"
      }
      projeccionsyLD$G <- as.factor(projeccionsyLD$G)
      projeccionsLD <- rbind(projeccionsLD, projeccionsyLD)
    }
    #-----Faig el grafic de sortida-----
    # primer agafo els valors dels coeficients per a fer les fletxes com a Unstandardized coefficient biplot, seguint el codi de la funcio lda.arrows https://stackoverflow.com/questions/17232251/how-can-i-plot-a-biplot-for-lda-in-r
    heads <- coef(xclrlda)
    for (k in 1:(lds - 1)) {
      for (l in (k + 1):lds) {  
        # per poder escalar automàticament les fletxes poso a 1 les quatre direccions del grafic
        myscale1 <- 1
        myscale2 <- 1
        myscale3 <- 1
        myscale4 <- 1
        # faig el nom amb que es gravara el grafic
        titolld <- as.character(paste("GraficDiscriminant", k, l, sep = ""))
        # faig ara el grafic segons si hi ha grafic de comparacio o no i si hi ha nom o no
        if (is.na(y)) {
          if (is.na(nom))  {
            # tots els grafics es faran igual, primer es fa el grafic buit, despres es calcula el factor per tal que les etiquetes i les fletxes no surtin de lloc i es fan, despres els punts
            # faig el gràfic buit
            plot(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], xlab = paste("LD", k, " (VE = ", sdevpercent[k], " %)", sep = ""), ylab = paste("LD", l, " (VE = ", sdevpercent[l], " %)", sep = ""), cex.lab = 1.2, type = "n")
            # calculo l'escala de les fletxes 
            if (max(projeccionsLD[ , k + nonumLD]) < max(heads[ , k]) * 0.9) {
              myscale1 <- 0.9 * (max(projeccionsLD[ , k + nonumLD]) / max(heads[ , k]))
            }
            if (min(projeccionsLD[ , k + nonumLD]) > min(heads[ , k]) * 0.9) {
              myscale2 <- 0.9 * (min(projeccionsLD[ , k + nonumLD]) / min(heads[ , k]))
            }
            if (max(projeccionsLD[ , l + nonumLD]) < max(heads[ , l]) * 0.9) {
              myscale3 <- 0.9 * (max(projeccionsLD[ , l + nonumLD]) / max(heads[ , l]))
            }
            if (min(projeccionsLD[ , l + nonumLD]) > min(heads[ , l]) * 0.9) {
              myscale4 <- 0.9 * (min(projeccionsLD[ , l + nonumLD]) / min(heads[ , l]))
            }
            myscale <- min(myscale1, myscale2, myscale3, myscale4)
            # faig les fletxes i les seves etiquetes
            arrows(x0 = 0, y0 = 0, x1 = myscale * heads[, k], y1 = myscale * heads[ , l], col = colpf, length = 0.1, angle = 20)
            text(myscale * 1.05 * heads[ , c(k, l)], labels = row.names(heads), cex = tex)
            # poso els punts al grafic
            points(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], cex.lab = 1.2, pch = simbols[unclass(xclr[ , ggrupx])], col = "grey50", bg = colx[unclass(projeccionsLD[ , 1])], cex = cexpunts)
          } else {
            # tots els grafics es faran igual, primer es fa el grafic buit, despres es calcula el factor per tal que les etiquetes i les fletxes no surtin de lloc i es fan, despres els punts
            # faig el gràfic buit
            plot(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], xlab = paste("LD", k, " (VE = ", sdevpercent[k], " %)", sep = ""), ylab = paste("LD", l, " (VE = ", sdevpercent[l], " %)", sep = ""), main = bquote(paste(.(nom), " (n = ", .(dim(xclr)[1]), ")")), cex.lab = 1.2, cex.main = 1.2, type = "n")
            # calculo l'escala de les fletxes 
            if (max(projeccionsLD[ , k + nonumLD]) < max(heads[ , k]) * 0.9) {
              myscale1 <- 0.9 * (max(projeccionsLD[ , k + nonumLD]) / max(heads[ , k]))
            }
            if (min(projeccionsLD[ , k + nonumLD]) > min(heads[ , k]) * 0.9) {
              myscale2 <- 0.9 * (min(projeccionsLD[ , k + nonumLD]) / min(heads[ , k]))
            }
            if (max(projeccionsLD[ , l + nonumLD]) < max(heads[ , l]) * 0.9) {
              myscale3 <- 0.9 * (max(projeccionsLD[ , l + nonumLD]) / max(heads[ , l]))
            }
            if (min(projeccionsLD[ , l + nonumLD]) > min(heads[ , l]) * 0.9) {
              myscale4 <- 0.9 * (min(projeccionsLD[ , l + nonumLD]) / min(heads[ , l]))
            }
            myscale <- min(myscale1, myscale2, myscale3, myscale4)
            # faig les fletxes i les seves etiquetes
            arrows(x0 = 0, y0 = 0, x1 = myscale * heads[, k], y1 = myscale * heads[ , l], col = colpf, length = 0.1, angle = 20)
            text(myscale * 1.05 * heads[ , c(k, l)], labels = row.names(heads), cex = tex)
            # poso els punts al grafic
            points(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], cex.lab = 1.2, pch = simbols[unclass(xclr[ , ggrupx])], col = "grey50", bg = colx[unclass(projeccionsLD[ , 1])], cex = cexpunts)
            #            plot(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], xlab = paste("LD", k, " (VE = ", sdevpercent[k], " %)", sep = ""), ylab = paste("LD", l, " (VE = ", sdevpercent[l], " %)", sep = ""), main = bquote(paste(.(nom), " (n = ", .(dim(xclr)[1]), ")")), cex.lab = 1.2, cex.main = 1.2, pch = simbols[unclass(xclr[ , ggrupx])], col = "grey50", bg = colx[unclass(projeccionsLD[ , 1])])
          }
        } else {
          if (ggrupx == grupx) {
            if (is.na(nom))  {
              # tots els grafics es faran igual, primer es fa el grafic buit, despres es calcula el factor per tal que les etiquetes i les fletxes no surtin de lloc i es fan, despres els punts
              # faig el gràfic buit
              plot(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], xlab = paste("LD", k, " (VE = ", sdevpercent[k], " %)", sep = ""), ylab = paste("LD", l, " (VE = ", sdevpercent[l], " %)", sep = ""), cex.lab = 1.2, type = "n")
              # calculo l'escala de les fletxes 
              if (max(projeccionsLD[ , k + nonumLD]) < max(heads[ , k]) * 0.9) {
                myscale1 <- 0.9 * (max(projeccionsLD[ , k + nonumLD]) / max(heads[ , k]))
              }
              if (min(projeccionsLD[ , k + nonumLD]) > min(heads[ , k]) * 0.9) {
                myscale2 <- 0.9 * (min(projeccionsLD[ , k + nonumLD]) / min(heads[ , k]))
              }
              if (max(projeccionsLD[ , l + nonumLD]) < max(heads[ , l]) * 0.9) {
                myscale3 <- 0.9 * (max(projeccionsLD[ , l + nonumLD]) / max(heads[ , l]))
              }
              if (min(projeccionsLD[ , l + nonumLD]) > min(heads[ , l]) * 0.9) {
                myscale4 <- 0.9 * (min(projeccionsLD[ , l + nonumLD]) / min(heads[ , l]))
              }
              myscale <- min(myscale1, myscale2, myscale3, myscale4)
              # faig les fletxes i les seves etiquetes
              arrows(x0 = 0, y0 = 0, x1 = myscale * heads[, k], y1 = myscale * heads[ , l], col = colpf, length = 0.1, angle = 20)
              text(myscale * 1.05 * heads[ , c(k, l)], labels = row.names(heads), cex = tex)
              # poso els punts al grafic
              points(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], cex.lab = 1.2, pch = simbols[unclass(grupconjunt[ , 1])], col = "grey50", bg = colx[unclass(projeccionsLD[ , 1])], cex = cexpunts)
              #              plot(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], xlab = paste("LD", k, " (VE = ", sdevpercent[k], " %)", sep = ""), ylab = paste("LD", l, " (VE = ", sdevpercent[l], " %)", sep = ""), cex.lab = 1.2, pch = simbols[unclass(grupconjunt[ , 1])], col = "grey50", bg = colx[unclass(projeccionsLD[ , 1])])
            } else {
              # tots els grafics es faran igual, primer es fa el grafic buit, despres es calcula el factor per tal que les etiquetes i les fletxes no surtin de lloc i es fan, despres els punts
              # faig el gràfic buit
              plot(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], xlab = paste("LD", k, " (VE = ", sdevpercent[k], " %)", sep = ""), ylab = paste("LD", l, " (VE = ", sdevpercent[l], " %)", sep = ""), main = bquote(paste(.(nom), " (n = ", .(dim(xclr)[1]), ")")), cex.lab = 1.2, cex.main = 1.2, type = "n")
              # calculo l'escala de les fletxes 
              if (max(projeccionsLD[ , k + nonumLD]) < max(heads[ , k]) * 0.9) {
                myscale1 <- 0.9 * (max(projeccionsLD[ , k + nonumLD]) / max(heads[ , k]))
              }
              if (min(projeccionsLD[ , k + nonumLD]) > min(heads[ , k]) * 0.9) {
                myscale2 <- 0.9 * (min(projeccionsLD[ , k + nonumLD]) / min(heads[ , k]))
              }
              if (max(projeccionsLD[ , l + nonumLD]) < max(heads[ , l]) * 0.9) {
                myscale3 <- 0.9 * (max(projeccionsLD[ , l + nonumLD]) / max(heads[ , l]))
              }
              if (min(projeccionsLD[ , l + nonumLD]) > min(heads[ , l]) * 0.9) {
                myscale4 <- 0.9 * (min(projeccionsLD[ , l + nonumLD]) / min(heads[ , l]))
              }
              myscale <- min(myscale1, myscale2, myscale3, myscale4)
              # faig les fletxes i les seves etiquetes
              arrows(x0 = 0, y0 = 0, x1 = myscale * heads[, k], y1 = myscale * heads[ , l], col = colpf, length = 0.1, angle = 20)
              text(myscale * 1.05 * heads[ , c(k, l)], labels = row.names(heads), cex = tex)
              # poso els punts al grafic
              points(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], cex.lab = 1.2, pch = simbols[unclass(grupconjunt[ , 1])], col = "grey50", bg = colx[unclass(projeccionsLD[ , 1])], cex = cexpunts)
              #              plot(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], xlab = paste("LD", k, " (VE = ", sdevpercent[k], " %)", sep = ""), ylab = paste("LD", l, " (VE = ", sdevpercent[l], " %)", sep = ""), main = bquote(paste(.(nom), " (n = ", .(dim(xclr)[1]), ")")), cex.lab = 1.2, cex.main = 1.2, pch = simbols[unclass(grupconjunt[ , 1])], col = "grey50", bg = colx[unclass(projeccionsLD[ , 1])])
            }
          } else {
            if (is.na(nom))  {
              # tots els grafics es faran igual, primer es fa el grafic buit, despres es calcula el factor per tal que les etiquetes i les fletxes no surtin de lloc i es fan, despres els punts
              # faig el gràfic buit
              plot(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], xlab = paste("LD", k, " (VE = ", sdevpercent[k], " %)", sep = ""), ylab = paste("LD", l, " (VE = ", sdevpercent[l], " %)", sep = ""), cex.lab = 1.2, type = "n")
              # calculo l'escala de les fletxes 
              if (max(projeccionsLD[ , k + nonumLD]) < max(heads[ , k]) * 0.9) {
                myscale1 <- 0.9 * (max(projeccionsLD[ , k + nonumLD]) / max(heads[ , k]))
              }
              if (min(projeccionsLD[ , k + nonumLD]) > min(heads[ , k]) * 0.9) {
                myscale2 <- 0.9 * (min(projeccionsLD[ , k + nonumLD]) / min(heads[ , k]))
              }
              if (max(projeccionsLD[ , l + nonumLD]) < max(heads[ , l]) * 0.9) {
                myscale3 <- 0.9 * (max(projeccionsLD[ , l + nonumLD]) / max(heads[ , l]))
              }
              if (min(projeccionsLD[ , l + nonumLD]) > min(heads[ , l]) * 0.9) {
                myscale4 <- 0.9 * (min(projeccionsLD[ , l + nonumLD]) / min(heads[ , l]))
              }
              myscale <- min(myscale1, myscale2, myscale3, myscale4)
              # faig les fletxes i les seves etiquetes
              arrows(x0 = 0, y0 = 0, x1 = myscale * heads[, k], y1 = myscale * heads[ , l], col = colpf, length = 0.1, angle = 20)
              text(myscale * 1.05 * heads[ , c(k, l)], labels = row.names(heads), cex = tex)
              # poso els punts al grafic
              points(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], cex.lab = 1.2, pch = simbols[unclass(ggrupconjunt[ , 1])], col = "grey50", bg = colx[unclass(projeccionsLD[ , 1])], cex = cexpunts)
              #              plot(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], xlab = paste("LD", k, " (VE = ", sdevpercent[k], " %)", sep = ""), ylab = paste("LD", l, " (VE = ", sdevpercent[l], " %)", sep = ""), cex.lab = 1.2, pch = simbols[unclass(ggrupconjunt[ , 1])], col = "grey50", bg = colx[unclass(projeccionsLD[ , 1])])
            } else {
              # tots els grafics es faran igual, primer es fa el grafic buit, despres es calcula el factor per tal que les etiquetes i les fletxes no surtin de lloc i es fan, despres els punts
              # faig el gràfic buit
              plot(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], xlab = paste("LD", k, " (VE = ", sdevpercent[k], " %)", sep = ""), ylab = paste("LD", l, " (VE = ", sdevpercent[l], " %)", sep = ""), main = bquote(paste(.(nom), " (n = ", .(dim(xclr)[1]), ")")), cex.lab = 1.2, cex.main = 1.2, type = "n")
              # calculo l'escala de les fletxes 
              if (max(projeccionsLD[ , k + nonumLD]) < max(heads[ , k]) * 0.9) {
                myscale1 <- 0.9 * (max(projeccionsLD[ , k + nonumLD]) / max(heads[ , k]))
              }
              if (min(projeccionsLD[ , k + nonumLD]) > min(heads[ , k]) * 0.9) {
                myscale2 <- 0.9 * (min(projeccionsLD[ , k + nonumLD]) / min(heads[ , k]))
              }
              if (max(projeccionsLD[ , l + nonumLD]) < max(heads[ , l]) * 0.9) {
                myscale3 <- 0.9 * (max(projeccionsLD[ , l + nonumLD]) / max(heads[ , l]))
              }
              if (min(projeccionsLD[ , l + nonumLD]) > min(heads[ , l]) * 0.9) {
                myscale4 <- 0.9 * (min(projeccionsLD[ , l + nonumLD]) / min(heads[ , l]))
              }
              myscale <- min(myscale1, myscale2, myscale3, myscale4)
              # faig les fletxes i les seves etiquetes
              arrows(x0 = 0, y0 = 0, x1 = myscale * heads[, k], y1 = myscale * heads[ , l], col = colpf, length = 0.1, angle = 20)
              text(myscale * 1.05 * heads[ , c(k, l)], labels = row.names(heads), cex = tex)
              # poso els punts al grafic
              points(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], cex.lab = 1.2, pch = simbols[unclass(ggrupconjunt[ , 1])], col = "grey50", bg = colx[unclass(projeccionsLD[ , 1])], cex = cexpunts)
              #              plot(projeccionsLD[ , c(k + nonumLD, l + nonumLD)], xlab = paste("LD", k, " (VE = ", sdevpercent[k], " %)", sep = ""), ylab = paste("LD", l, " (VE = ", sdevpercent[l], " %)", sep = ""), main = bquote(paste(.(nom), " (n = ", .(dim(xclr)[1]), ")")), cex.lab = 1.2, cex.main = 1.2, pch = simbols[unclass(ggrupconjunt[ , 1])], col = "grey50", bg = colx[unclass(projeccionsLD[ , 1])])
            }
          }
        }
        # ara busco el punt per posar la llegenda
        cc <- locator(n = 1)
        # faig la llegenda
        # primer miro que els simbols estiguin be si hi ha dues categoriques
        if (ggrupx != grupx) {
          if (titol1 == 0 & titol2 == 0) {      
            simbols2 <- c(rep(simbols[1], nivellstot), NA, simbols)
            textfont <- 1
            colx2 <- c(colx[1:nivellstot], NA, rep("white", nivellstot))
          }
          if (titol1 != 0 & titol2 == 0) {
            simbols2 <- c(NA, rep(simbols[1], nivellstot), NA, simbols)
            textfont <- c(2, rep(1, nivellstot), 2, rep(1, length(simbols)))
            colx2 <- c(NA, colx[1:nivellstot], NA, rep("white", nivellstot))
          }
          if (titol1 == 0 & titol2 != 0) {
            simbols2 <- c(rep(simbols[1], nivellstot), NA, simbols)
            textfont <- c(rep(1, nivellstot), 2, rep(1, length(simbols)))
            colx2 <- c(colx[1:nivellstot], NA, rep("white", nivellstot))
          }
          if (titol1 != 0 & titol2 != 0) {
            simbols2 <- c(NA, rep(simbols[1], nivellstot), NA, simbols)
            textfont <- c(2, rep(1, nivellstot), 2, rep(1, length(simbols)))
            colx2 <- c(NA, colx[1:nivellstot], NA, rep("white", nivellstot))
          }
          legend(cc$x, cc$y, legend = totidllegenda, bty="n", pch = simbols2, pt.bg = colx2, cex = midall, text.font = textfont)
        } else {
          legend(cc$x, cc$y, legend = totidllegenda, bty="n", pch = simbols, pt.bg = colx, cex = midall)
        }
        # els gravo com a pdf i emf
        xx <- recordPlot()
        nombiplot <- paste(titolld, ".pdf", sep = "")
        pdf(nombiplot)
        replayPlot(xx)
        dev.off()
        nombiplot <- paste(titolld, ".emf", sep = "")
        emf(nombiplot)
        replayPlot(xx)
        dev.off()
      }
    }
    #-----Ara faig la sortida escrita-----
    if (is.na(y)) {
      list(Coeficients_dels_discriminants_lineals = coefsLDA, Taula_Confusio_x = taulaxvar, Fitxer_x_amb_Class = xclrClass, Projeccions_LD = projeccionsLD)
    } else {
      list(Coeficients_dels_discriminants_lineals = coefsLDA, Taula_Confusio_x = taulaxvar, Fitxer_x_amb_Class = xclrClass, Taula_Confusio_y = taulay, Fitxer_y_amb_Class = yClass, Projeccions_LD = projeccionsLD)
    }
  }

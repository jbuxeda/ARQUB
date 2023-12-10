"ACPacomp"<-
  function(x, nom = "NA", grup = 0, llegenda = grup, ggrup = grup, gllegenda = ggrup, titol1 = 0, titol2 = 0, idioma = 1, paleta = 1, paleta0 = NA, color = "chocolate", cps = 3, simbol = 0, simbol0 = NA, midall = 0.8, elli = T, pelli = 0.95, nelli = 2, noelli = 0)
  {
    # Fa els biplots de l'ACP del fitxer x, al que automaticament transforma en acomp. Idioma: 1- catala, 2- castella, 3- angles, 4- frances
    # Si grup = 0 (per defecte) s'interpreta que no es fan grups per colors i utilitza les etiquetes dels individus. Si hi ha grups, cal especificar a quina columna hi ha el factor de grup i a quina la llegenda.
    # A la paleta: 0- lliure, 1- arqub (per defecte), 2- arqub en blanc i negre, 3- rainbow, 4- heat.colors, 5- terrain.colors, 6- topo.colors, 7- cm.colors.
    # Si la paleta es 0 vol dir que s'escullen els colors. La manera es, per exemple, paleta0 = c("red", "blue").
    # El nom, si es posa, anira als biplot. cps son els components principals que s'usen per fer els biplots dos a dos.
    # El grup ha de ser un as.factor i la llegenda, si no es el grup, as.character.
    # color es refereix al que tenen les barres del diagrama sobre les covariancies dels components principals (maxim 10).
    # simbol es pels simbols que s'utilitzaran per representar els individus, per defecte (simbol = 0) es 21. Si simbol <> 0, llavors s'agafa
    # el llistat de simbols que sigui, per exemple, simbol0 = c(21, 24). Els simbols han d'anar entre el 21 i el 25.
    # midall (per defecte 0.8) es la mida del text de la llegenda. Si es canvia aquest argument, la llegenda pot ser mes gran (midall > 0.8; per exemple, midall = 1) o mes petita (midall < 0.8; per exemple, midall = 0.6)
    # Si la llegenda es doble, ggrup sera diferent de grup. Els dos poden tenir la seva propia llegenda (llegenda i gllegenda).
    # titol1 i titol2 son els titols en cas de llegendes dobles. Si es volen deixar els titols buits es deixa el 0 (per defecte).
    # elli (= T) indica que es vol, en el grafic de simbols, les el·lipses dels grups (si n'hi ha) (i amb n > 2 -per defecte, nelli). noelli (per defecte NA) permet dir per quins grups no es volen el·lipses. 
    # Si es aixi, pelli (= 0.95) indica la probabilitat de les el·lipses. Els grups sobre els que es fan son els de "grup"
    # Feta per Jaume Buxeda i Garrigos.
    # -----Primer ens assegurem que el grup, si n'hi ha, i la llegenda, si n'hi ha, son factor i caracter respectivament-----    
    if (grup > 0) {
      x[ , grup] <- as.factor(x[ , grup])
      if (llegenda != grup) {
        x[ , llegenda] <- as.character(x[ , llegenda])
      }
      if (ggrup != grup) {
        x[ , ggrup] <- as.factor(x[ , ggrup])
        if (gllegenda != ggrup) {
          x[ , gllegenda] <- as.character(x[ , gllegenda])
        }
      }
    }
    # -----Controlem que idioma tingui un valor valid-----
    if (idioma < 1 | idioma > 4) {return(cat("Non valid idioma value - terminating."))}
    # -----Controlem que paleta tingui un valor valid-----
    if (paleta < 0 | paleta > 7) {return(cat("Non valid paleta value - terminating."))}
    # -----Controlem que simbol tingui un valor valid-----
    if (simbol != 0) {
      if (min(simbol0) < 21 | max(simbol0) > 25) {
        return(cat("Non valid simbol value - terminating."))
      }
    }
    # controlem que si hi ha dues variables categoriques, s'hagin assignat els simbols a simbol0
    if (ggrup != grup) {
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
    #-----Identifiquem ara les variables numeriques sobre les que es fara l'ACP-----
    verifica <- unlist(lapply(x, is.numeric))
    xacomp <- x[ , verifica]
    #-----Fem la transformacio acomp i l'analisi de components principals-----
    xacomp <- acomp(xacomp)
    xacompACP <- princomp(xacomp)
    #-----Fem el diagrama de barres de les variancies-----
    # Agafem un maxim de 10 components principals
    npcs = min(10, length(xacompACP$sdev))
    # agafem el valor de les variancies i calculem l'eix de les y, per tal que sigui mes alt
    afer <- xacompACP$sdev[1:npcs]^2
    if (afer[1] <= 1) {
      sostre <- ceiling((afer[1]) * 100/5)
      sostre <- (sostre * 5) / 100
    }
    if (afer[1] > 1) {
      sostre <- ceiling((afer[1]) * 10/5)
      sostre <- (sostre * 6) / 10
    }
    # calculem els % de les variancies
    sdevpercent <- as.vector(as.character(format(round(((xacompACP$sdev^2) / sum(xacompACP$sdev^2)) * 100, digit = 2), nsamll = 2)))
    # escollim el titol de l'eix y segons idioma
    if (idioma == 1) {titoly <- as.character("Variàncies")}
    if (idioma == 2) {titoly <- as.character("Varianzas")}
    if (idioma == 3 | idioma == 4) {titoly <- as.character("Variances")}
    # faig el grafic i el salvo
    # primer miro les mides de lletres segons components principals
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
    barvar <- recordPlot()
    # faig el biplot pels components principals indicats (cps) amb etiquetes
    for (i in 1:(cps - 1)) {
      for (j in (i + 1):cps) {
        for (cada in 0:1) {
          # Miro el titol del grafic
          if (nom == "NA") {
            if (cada == 1) {
              titol2b <- as.character(paste("EtBiplotCov", i, j, sep = ""))
              if (idioma == 1) {titol <- as.character("Biplot de covariància")}
              if (idioma == 2) {titol <- as.character("Biplot de covarianza")}
              if (idioma == 3) {titol <- as.character("Covariance biplot")}
              if (idioma == 4) {titol <- as.character("Biplot de covarince")}
            } else {
              titol2b <- as.character(paste("EtBiplotForm", i, j, sep = ""))
              if (idioma == 1) {titol <- as.character("Biplot de forma")}
              if (idioma == 2) {titol <- as.character("Biplot de forma")}
              if (idioma == 3) {titol <- as.character("Form biplot")}
              if (idioma == 4) {titol <- as.character("Biplot de forme")}
            }
          } else {
            if (cada == 1) {
              titol2b <- as.character(paste("EtBiplotCov", i, j, sep = ""))
              if (idioma == 1) {titol <- as.character(paste(nom, ". Biplot de covariància", sep = ""))}
              if (idioma == 2) {titol <- as.character(paste(nom, ". Biplot de covarianza", sep = ""))}
              if (idioma == 3) {titol <- as.character(paste(nom, ". Covariance biplot", sep = ""))}
              if (idioma == 4) {titol <- as.character(paste(nom, ". Biplot de covarince", sep = ""))}
            } else {
              titol2b <- as.character(paste("EtBiplotForm", i, j, sep = ""))
              if (idioma == 1) {titol <- as.character(paste(nom, ". Biplot de forma", sep = ""))}
              if (idioma == 2) {titol <- as.character(paste(nom, ". Biplot de forma", sep = ""))}
              if (idioma == 3) {titol <- as.character(paste(nom, ". Form biplot", sep = ""))}
              if (idioma == 4) {titol <- as.character(paste(nom, ". Biplot de forme", sep = ""))}
            }
          }
          biplot(xacompACP, choice = c(i, j), cex = c(0.6, 0.9), col = c("grey50", "black"), xlab = paste("Comp.", i, " (VE = ", sdevpercent[i], " %)", sep = ""), ylab = paste("Comp.", j, " (VE = ", sdevpercent[j], " %)", sep = ""), main = titol, scale = cada, cex.lab = 1.2, cex.main = 1.2) 
          xx <- recordPlot()
          nombiplot <- paste(titol2b, ".pdf", sep = "")
          pdf(nombiplot)
          replayPlot(xx)
          dev.off()
          nombiplot <- paste(titol2b, ".emf", sep = "")
          emf(nombiplot)
          replayPlot(xx)
          dev.off()
        }
      }
    }
    # -----Faig el biplot pels components principals indicats (cps) amb colors, si escau-----
    # calculo el numero de grups i guardo el numero en nivellsx
    idllegenda <- vector(length = nlevels(x[ , grup]))
    nivellsx <- nlevels(x[ , grup])
    nivellstot <- nivellsx
    # grup per grup, assigno l'etiqueta, tenint en compte si llegendax es com grup o com llegenda
    for (ll in 1:nlevels(x[ , grup])) {
      z <- which(x[ , grup] == levels(x[ , grup])[ll])
      if (llegenda != grup) {
        idllegenda[ll] <- x[z[1], llegenda]
      } else {
        idllegenda[ll] <- as.character(x[z[1], grup])
      }
    }
    if (ggrup != grup) {
      # aixo nomes es cert si s'usa una segona variable categorica
      # calculo el numero de grups de la segona categorica i guardo el numero en nivellsxg
      gidllegenda <- vector(length = nlevels(x[ , ggrup]))
      nivellsxg <- nlevels(x[ , ggrup])
      # grup per grup, assigno l'etiqueta, tenint en compte si llegenda es com ggrup o com gllegenda
      for (ll in 1:nivellsxg) {
        z <- which(x[ , ggrup] == levels(x[ , ggrup])[ll])
        if (gllegenda != ggrup) {
          gidllegenda[ll] <- x[z[1], gllegenda]
        } else {
          gidllegenda[ll] <- as.character(x[z[1], ggrup])
        }
      }
    }
    # ara faig la llegenda final (totidllegenda) incloent titols, si escau
    if (ggrup == grup) {
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
    # Ara simbols i paletes, i tambe els biplots
    if (grup > 0) {
      # faig els simbols pels grups
      if (ggrup == grup) {
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
        colx <- c(rainbow(nlevels(x[ , grup])))[as.factor(x[ , grup])]
      }
      if (paleta == 4) {
        colx <- c(heat.colors(nlevels(x[ , grup])))[as.factor(x[ , grup])]
      }
      if (paleta == 5) {
        colx <- c(terrain.colors(nlevels(x[ , grup])))[as.factor(x[ , grup])]
      }
      if (paleta == 6) {
        colx <- c(topo.colors(nlevels(x[ , grup])))[as.factor(x[ , grup])]
      }
      if (paleta == 7) {
        colx <- c(cm.colors(nlevels(x[ , grup])))[as.factor(x[ , grup])]
      }
      # controlo els biplots
      for (i in 1:(cps - 1)) {
        for (j in (i + 1):cps) {
          for (cada in 0:1) {
            # Miro el titol del grafic
            if (nom == "NA") {
              if (cada == 1) {
                titol2 <- as.character(paste("CoBiplotCov", i, j, sep = ""))
                if (idioma == 1) {titol <- as.character("Biplot de covariància")}
                if (idioma == 2) {titol <- as.character("Biplot de covarianza")}
                if (idioma == 3) {titol <- as.character("Covariance biplot")}
                if (idioma == 4) {titol <- as.character("Biplot de covarince")}
              } else {
                titol2 <- as.character(paste("CoBiplotForm", i, j, sep = ""))
                if (idioma == 1) {titol <- as.character("Biplot de forma")}
                if (idioma == 2) {titol <- as.character("Biplot de forma")}
                if (idioma == 3) {titol <- as.character("Form biplot")}
                if (idioma == 4) {titol <- as.character("Biplot de forme")}
              }
            } else {
              if (cada == 1) {
                titol2 <- as.character(paste("CoBiplotCov", i, j, sep = ""))
                if (idioma == 1) {titol <- as.character(paste(nom, ". Biplot de covariància", sep = ""))}
                if (idioma == 2) {titol <- as.character(paste(nom, ". Biplot de covarianza", sep = ""))}
                if (idioma == 3) {titol <- as.character(paste(nom, ". Covariance biplot", sep = ""))}
                if (idioma == 4) {titol <- as.character(paste(nom, ". Biplot de covarince", sep = ""))}
              } else {
                titol2 <- as.character(paste("CoBiplotForm", i, j, sep = ""))
                if (idioma == 1) {titol <- as.character(paste(nom, ". Biplot de forma", sep = ""))}
                if (idioma == 2) {titol <- as.character(paste(nom, ". Biplot de forma", sep = ""))}
                if (idioma == 3) {titol <- as.character(paste(nom, ". Form biplot", sep = ""))}
                if (idioma == 4) {titol <- as.character(paste(nom, ". Biplot de forme", sep = ""))}
              }
            }
            # faig el grafic
            coloredBiplot(xacompACP, pc.biplot = F, xlabs.pc = simbols[unclass(x[ , ggrup])], xlabs.bg = colx[unclass(x[ , grup])], choice = c(i, j), cex = c(1.3, 0.9), col = c("grey50", "black"), xlab = paste("Comp.", i, " (VE = ", sdevpercent[i], " %)", sep = ""), ylab = paste("Comp.", j, " (VE = ", sdevpercent[j], " %)", sep = ""), main = titol, scale = cada, cex.lab = 1.2, cex.main = 1.2) 
            # faig les el·lipses, si escau
            if (elli == T) {
              par(new = TRUE, las = 1, mar = c(5, 4, 4, 2) + 0.1 + c(0, 2, 1, 2))
              lam2 <- xacompACP$sdev[c(i, j)]
              lam2 <- lam2 * sqrt(xacompACP$n.obs)
              # lam2 <- lam2^cada
              if (cada == 0) {lam2 <- 1}
              tab <- t(t(xacompACP$scores[, c(i, j)]) / lam2)
              rangx1 <- c(-abs(min(tab[ , 1], na.rm=TRUE)), abs(max(tab[ , 1], na.rm=TRUE)))
              rangx2 <- c(-abs(min(tab[ , 2], na.rm=TRUE)), abs(max(tab[ , 2], na.rm=TRUE)))
              xlim <- ylim <- rangx1 <- rangx2 <- range(rangx1, rangx2)
              plot.window(xlim = xlim, ylim = ylim, las = 1)
              # points(tab[ , c(i, j)])
              for (niv in 1:nivellsx){
                tabgrup <- tab[x[ , grup] == levels(x[ , grup])[niv], ]
                ndegrup <- table(x[ , grup])
                # cat(niv, sep="\n")
                # cat(tabgrup)
                # cat("", sep="\n")
                # cat(ndegrup[niv], sep="\n")
                if (ndegrup[niv] > nelli) {
                  if (niv != noelli) {
                    dataEllipse(tabgrup, levels = pelli, add = T, col = "grey50", center.pch = F, lwd = 1, plot.points = F)
                  }
                }
              }
              par(las = 1, mar = c(5, 4, 4, 2) + 0.1)
            }
            # busco el punt per posar les llegendes
            cc <- locator(n = 1)
            # faig la llegenda
            # primer miro que els simbols estiguin be si hi ha dues categoriques
            if (ggrup != grup) {
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
            # els salvo
            xx <- recordPlot()
            nombiplot <- paste(titol2, ".pdf", sep = "")
            pdf(nombiplot)
            replayPlot(xx)
            dev.off()
            nombiplot <- paste(titol2, ".emf", sep = "")
            emf(nombiplot)
            replayPlot(xx)
            dev.off()
          }
        }
      }
    }
    #-----Gravo els grafics-----
    # Primer el digrama de barres de les covariancies
    pdf("BarplotVar.pdf")
    replayPlot(barvar)
    dev.off()
    emf("BarplotVar.emf")
    replayPlot(barvar)
    dev.off()
    #-----Sortida-----
    xacompACP
  }

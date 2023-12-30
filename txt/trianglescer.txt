"trianglescer"<-
  function(x, grup = 0, idioma = 1, paleta = 1, ordenar = 1, llegenda = grup, encerclat = 1, mida = 0.8, color = "gray75", paleta0 = NA)
  {
    # Fa els tres triangles del fitxer x. Idioma: 1- catala, 2- castella, 3- angles, 4- frances
    # Si grup = 0 (per defecte) s'interpreta que no es fan grups per colors i el color ve donat per color. Si hi ha grups, cal especificar a quina columna hi ha el factor de grup i cal utilizar paleta.
    # A la paleta: 0- lliure, 1- arqub (per defecte), 2- arqub en blanc i negre, 3- rainbow, 4- heat.colors, 5- terrain.colors, 6- topo.colors, 7- cm.colors.
    # Si la paleta es 0 vol dir que s'escullen els colors. La manera es, per exemple, paleta0 = c("red", "blue").
    # A ordernar, per defecte es 1- no. Si es posa 2 ordena el fitxer segons el factor de grup.
    # El grup ha de ser un as.factor i la llegenda, si no es el grup, as.character.
    # Encerclat vol dir que el punt te un cercle gris envoltant-lo per defecte, 1. Si no es vol, es 2 (no).
    # mida es refereix a la mida dels punts. Si no s'indica res, el valor per defecte es 0.8
    # Feta per Jaume Buxeda i Garrigos.
    # -----Primer ens assegurem que el grup, si n'hi ha, i la llegenda, si n'hi ha, son factor i caracter respectivament-----    
    if (grup > 0) {
      x[ , grup] <- as.factor(x[ , grup])
      if (llegenda != grup) {
        x[ , llegenda] <- as.character(x[ , llegenda])
      }
    }
    # -----Controlem que idioma tingui un valor valid-----
    if (idioma < 1 | idioma > 4) {return(cat("Non valid idioma value - terminating."))}
    # -----Controlem que paleta tingui un valor valid-----
    if (paleta < 0 | paleta > 7) {return(cat("Non valid paleta value - terminating."))}
    # -----Controlem que ordenar tingui un valor valid-----
    if (ordenar < 1 | ordenar > 2) {return(cat("Non valid ordenar value - terminating."))}
    # -----Controlem que encerclat tingui un valor valid-----
    if (encerclat < 1 | encerclat > 2) {return(cat("Non valid encerclat value - terminating."))}
    # -----Reordenem el fitxer segons grup si s'ha posat aquesta opcio (2)-----    
    if (ordenar == 2) {x <- x[order(x[ , grup]), ]}
    # -----Fem les paletes arqub en color (arqub) i en blanc i negre (arqubBN)-----
    arqub <- vector(length = 15)
    arqub <- c("gray75", "grey9", "cyan", "red", "goldenrod1", "dodgerblue", "darkgoldenrod4", "chartreuse1",  "darkgreen", "indianred1", "blue", "darkmagenta", "maroon1", "aquamarine", "lightpink")
    arqubBN <- vector(length = 7)
    arqubBN <- c("white", "grey90", "grey70", "grey50", "grey30", "grey10", "black")
    # -----Fer la paleta lliure, si es el que toca-----
    if (paleta == 0) {
      lliure <- vector(length = length(paleta0))
      lliure <- paleta0
    }
    # -----Preparem la llegenda, si es el cas-----
    if (grup > 0) {
      idllegenda <- vector(length = nlevels(x[ , grup]))
      for (ll in 1:nlevels(x[ , grup])) {idllegenda[ll] <- ll}
      if (llegenda != grup) {
        for (ll in 1:nlevels(x[ , grup])) {
          z <- which(x[ , grup] == levels(x[ , grup])[ll])
          idllegenda[ll] <- x[z[1], llegenda]
        }
      }
    }
    # -----Ara s'assignen les paletes fetes per R-----
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
    # -----Iniciem les finestres per les imatges (segons sistema operatiu, pero no inclou Linux) i li donem els marges-----
    if(Sys.info()[["sysname"]] == "Windows") {
      windows(record = T)
    } else {
      quartz()
    }
    par(mar = c(0, 0, 0, 0) + 0.1, mgp = c(3, 1, 0))
    # -----Iniciem la matriu on es calcularan els valors pel grafic-----
    n <- dim(x)[1]
    punts <- matrix(0, n, 4)
    # -----Fem el triangles en bucle. Primer inicio el bucle-----
    for (triang in 1:3) {
      # Dibuixo el triangle base
      plot(c(0, 100, 50, 0), c(0, 0, 100, 0), xlab = "", ylab = "", axes = F, type = "n", xlim = c(-20, 120), ylim = c(-20, 120))
      lines(c(0, 100, 50, 0), c(0, 0, 86.60254, 0))
      lines(c(25, 62.5), c(0, 64.951905), lty = 4, col= "grey")
      lines(c(50, 75), c(0, 43.30127), lty = 4, col = "grey")
      lines(c(75, 87.5), c(0, 21.650635), lty = 4, col = "grey")
      lines(c(12.5, 87.5), c(21.650635, 21.650635), lty = 3, col = "grey")
      lines(c(25, 75), c(43.30127, 43.30127),lty = 3, col = "grey")
      lines(c(37.5, 62.5), c(64.951905, 64.951905), lty = 3, col = "grey")
      lines(c(25, 12.5), c(0, 21.650635), lty = 2, col = "grey")
      lines(c(50, 25), c(0, 43.30127), lty = 2, col = "grey")
      lines(c(75, 37.5), c(0, 64.951905), lty = 2, col = "grey")
      # El que ve ara es pel primer triangle
      if (triang == 1) {
        # Dibuixo els triangles d'equilibri termodinamic
        lines(c(0, 100, 50, 0), c(0, 0, 86.60254, 0))
        lines(c(50, 58.245), c(86.60254, 37.403637))
        lines(c(58.245, 25.86), c(37.403637, 44.7908337))
        lines(c(58.245, 48.135), c(37.403637, 18.9746165))
        lines(c(58.245, 85.9), c(37.403637, 24.4219163))
        lines(c(25.86, 48.135), c(44.7908337, 18.9746165))
        # Dibuixo el titol
        if (idioma == 1) {
          text(50, 115, labels = expression(bold("Sistema CaO-Al"["2"]*"O"["3"]*"-SiO"["2"])), cex = 1.4)
          text(50, 108, labels = expression(paste("(", italic("w"), "%)")), font = 2)
        }
        if (idioma == 2) {
          text(50, 115, labels = expression(bold("Sistema CaO-Al"["2"]*"O"["3"]*"-SiO"["2"])), cex = 1.4)
          text(50, 108, labels = expression(paste("(", italic("w"), "%)")), font = 2)
        }
        if (idioma == 3) {
          text(50, 115, labels = expression(bold("CaO-Al"["2"]*"O"["3"]*"-SiO"["2"]~"System")), cex = 1.4)
          text(50, 108, labels = expression(paste("(", italic("w"), "%)")), font = 2)
        }
        if (idioma == 4) {
          text(50, 115, labels = expression(bold("Système CaO-Al"["2"]*"O"["3"]*"-SiO"["2"])), cex = 1.4)
          text(50, 108, labels = expression(paste("(", italic("w"), "%)")), font = 2)
        }
        # Etiquetes dels vertex
        text(100, -10, labels = expression(bold("Al"["2"]*"O"["3"])), pos = 2, cex = 1.4)
        text(60, 82.272413, labels = expression(bold("SiO"["2"])), pos = 4, cex = 1.4)
        text(-5, 5, labels = expression(bold("CaO")), pos = 2, cex = 1.4)
        # Etiquetes de les fases
        text(c(50, 30.3, 59.5, 82.5, 48.135), c(89.2006162, 45.8993462, 35.0740287, 23.25278199, 17.320508), labels = c("Qz", "Wo", "An", "Mul", "Gh"), cex = 1, font = 2)
        # Calcul dels punts dels individus
        punts[ , 1] <- x$"Al2O3"
        punts[ , 2] <- x$"SiO2"
        punts[ , 3] <- x$"CaO"
        punts[ , c(1:3)] <- sweep(punts[ , c(1:3)]/0.01, 1, apply(punts[ , c(1:3)], 1, sum), FUN = "/")
        # La coordenada x es el valor de l'eix inferior mes el catet del triangle rectangle. La seva hipotenusa
        # es el valor de l'eix dret i es multiplica pel sinus de 30 graus, que es 0.5
        punts[ , 4] <- punts[ , 1] + (punts[ , 2]/2)
      }
      # El que ve ara es pel segon triangle
      if (triang == 2) {
        # Dibuixo els triangles d'equilibri termodinamic
        lines(c(0, 100, 50, 0), c(0, 0, 86.60254, 0))
        lines(c(50, 58.245), c(86.60254, 37.403637))
        lines(c(58.245, 25.86), c(37.403637, 44.7908337))
        lines(c(58.245, 48.135), c(37.403637, 18.9746165))
        lines(c(58.245, 85.9), c(37.403637, 24.4219163))
        lines(c(25.86, 48.135), c(44.7908337, 18.9746165))
        # Dibuixo el titol
        if (idioma == 1) {
          text(50, 115, labels = expression(bold("Triangle ceràmic")), cex = 1.4)
          text(50, 108, labels = expression(paste("(", italic("w"), "%)")), font = 2)
        }
        if (idioma == 2) {
          text(50, 115, labels = expression(bold("Triángulo cerámico")), cex = 1.4)
          text(50, 108, labels = expression(paste("(", italic("w"), "%)")), font = 2)
        }
        if (idioma == 3) {
          text(50, 115, labels = expression(bold("Ceramic triangle")), cex = 1.4)
          text(50, 108, labels = expression(paste("(", italic("w"), "%)")), font = 2)
        }
        if (idioma == 4) {
          text(50, 115, labels = expression(bold("Système (CaO + MgO + Fe"["2"]*"O"["3"]*")-Al"["2"]*"O"["3"]*"-SiO"["2"])), cex = 1.4)
          text(50, 108, labels = expression(paste("(", italic("w"), "%)")), font = 2)
        }
        # Etiquetes dels vertex
        text(100, -10, labels = expression(bold("Al"["2"]*"O"["3"])), pos = 2, cex = 1.4)
        text(60, 82.272413, labels = expression(bold("SiO"["2"])), pos = 4, cex = 1.4)
        text(2.8, 14.9, labels = expression(bold("Fe"["2"]*"O"["3"]~"+")), pos = 2, cex = 1.4)
        text(0.2, 9.2, labels = expression(bold("MgO +")), pos = 2, cex = 1.4)
        text(-5, 5, labels = expression(bold("CaO")), pos = 2, cex = 1.4)      
        # Etiquetes de les fases
        text(c(50, 30.3, 59.5, 82.5, 48.135), c(89.2006162, 45.8993462, 35.0740287, 23.25278199, 17.320508), labels = c("Qz", "Wo", "An", "Mul", "Gh"), cex = 1, font = 2)      
        # Calcul dels punts dels individus
        punts[ , 1] <- x$"Al2O3"
        punts[ , 2] <- x$"SiO2"
        punts[ , 3] <- x$"CaO"+x$"MgO"+x$"Fe2O3"
        punts[ , c(1:3)] <- sweep(punts[ , c(1:3)]/0.01, 1, apply(punts[ , c(1:3)], 1, sum), FUN = "/")
        # La coordenada x es el valor de l'eix inferior mes el catet del triangle rectangle. La seva hipotenusa
        # es el valor de l'eix dret i es multiplica pel sinus de 30 graus, que es 0.5
        punts[ , 4] <- punts[ , 1] + (punts[ , 2]/2)
      }
      # El que ve ara es pel tercer triangle
      if (triang == 3) {
        # Dibuixo els triangles d'equilibri termodinamic
        lines(c(0, 100, 50, 0), c(0, 0, 86.60254, 0))
        lines(c(50, 46.355), c(86.60254, 48.0557494))
        lines(c(25.86, 46.355), c(44.7908337, 48.0557494))
        lines(c(70.075, 46.355), c(51.8316202, 48.0557494))
        lines(c(36.825, 46.355), c(38.1657394, 48.0557494))
        lines(c(78.65, 46.355), c(36.9792846, 48.0557494))
        lines(c(36.825, 25.86), c(38.1657394, 44.7908337))
        lines(c(36.825, 78.65), c(38.1657394, 36.9792846))
        lines(c(36.825, 44.96), c(38.1657394, 33.2553754))
        lines(c(78.65, 44.96), c(36.9792846, 33.2553754))      
        # Dibuixo el titol
        if (idioma == 1) {
          text(50, 115, labels = expression(bold("Sistema CaO-MgO-SiO"["2"])), cex = 1.4)
          text(50, 108, labels = expression(paste("(", italic("w"), "%)")), font = 2)
        }
        if (idioma == 2) {
          text(50, 115, labels = expression(bold("Sistema CaO-MgO-SiO"["2"])), cex = 1.4)
          text(50, 108,labels = expression(paste("(", italic("w"), "%)")), font = 2)
        }
        if (idioma == 3) {
          text(50, 115, labels = expression(bold("CaO-MgO-SiO"["2"]~"System")), cex = 1.4)
          text(50, 108, labels = expression(paste("(", italic("w"), "%)")), font = 2)
        }
        if (idioma == 4) {
          text(50, 115, labels = expression(bold("Système CaO-MgO-SiO"["2"])), cex = 1.4)
          text(50, 108, labels = expression(paste("(", italic("w"), "%)")), font = 2)
        }
        # Etiquetes dels vertex
        text(100, -10, labels = expression(bold("MgO")), pos = 2, cex = 1.4)
        text(60, 82.272413, labels = expression(bold("SiO"["2"])), pos = 4, cex = 1.4)
        text(-5, 5, labels = expression(bold("CaO")), pos = 2, cex = 1.4)      
        # Etiquetes de les fases
        text(c(50, 27.2, 46.9, 69.4, 35.4, 77.6, 44.96), c(89.2006162, 41.6, 45.2, 49.8, 37, 35, 31), labels=c("Qz", "Wo", "Di", "En", "Ak", "Fo", "Mtc"), cex = 1, font = 2)      
        # Calcul dels punts dels individus
        punts[ , 1] <- x$"MgO"
        punts[ , 2] <- x$"SiO2"
        punts[ , 3] <- x$"CaO"
        punts[ , c(1:3)] <- sweep(punts[ , c(1:3)]/0.01, 1, apply(punts[ , c(1:3)], 1, sum), FUN = "/")
        # La coordenada x es el valor de l'eix inferior mes el catet del triangle rectangle. La seva hipotenusa
        # es el valor de l'eix dret i es multiplica pel sinus de 30 graus, que es 0.5
        punts[ , 4] <- punts[ , 1] + (punts[ , 2]/2)
      }
      # Etiquetes dels eixos
      text(c(1, 25, 50, 75, 98), c(-2, -2, -2, -2, -2), labels = c("0", "25", "50", "75", "100"), cex = 0.8)
      text(c(102, 90.5, 78, 65.5, 54), c(1.7320508, 21.650635, 43.30127, 64.951905, 85.7365146), labels = c("0", "25", "50", "75", "100"), cex = 0.8)
      text(c(48, 34.5, 22, 9.5, -3), c(85.7365146, 64.951905, 43.30127, 21.650635, 1.7320508), labels = c("0", "25", "50", "75", "100"), cex = 0.8)
      #  Dibuix dels punts
      # La seguent linia crida una subrutina que dibuixa els punts, pero en la nova estructura, en bucle, no cal.
      # dibuixpunts(punts, grup, idioma, paleta, llegenda, encerclat, color, colx, lliure, arqub, arqubBN, idllegenda, x, n)
      #  Dibuix dels punts per grups
      if (grup > 0) {
        if (paleta == 0) {
          if (encerclat == 2) {
            for (w in 1:n) {
              points(punts[w, 4], (punts[w, 2] * 0.8660254), pch = 19, cex = mida, col = lliure[x[w, grup]])
            }
            legend(-15, 91, bty = "n", pch = 19, legend = idllegenda[1:nlevels(x[ , grup])], col = lliure[1:nlevels(x[ , grup])])
          }
          if (encerclat == 1) {
            for (w in 1:n) {
              points(punts[w, 4], (punts[w, 2] * 0.8660254), pch = 19, cex = mida, col = lliure[x[w, grup]])
              points(punts[w, 4], (punts[w, 2] * 0.8660254), pch = 21, cex = mida)
            }
            legend(-15, 91, bty = "n", pch = 19, legend = idllegenda[1:nlevels(x[ , grup])], col = lliure[1:nlevels(x[ , grup])])
            legend(-15, 91, bty = "n", pch = 21, legend = rep("", nlevels(x[ , grup])))
          }
        }
        if (paleta == 1) {
          if (encerclat == 2) {
            for (w in 1:n) {
              points(punts[w, 4], (punts[w, 2] * 0.8660254), pch = 19, cex = mida, col = arqub[x[w, grup]])
            }
            legend(-15, 91, bty = "n", pch = 19, legend = idllegenda[1:nlevels(x[ , grup])], col = arqub[1:nlevels(x[ , grup])])
          }
          if (encerclat == 1) {
            for (w in 1:n) {
              points(punts[w, 4], (punts[w, 2] * 0.8660254), pch = 19, cex = mida, col = arqub[x[w, grup]])
              points(punts[w, 4], (punts[w, 2] * 0.8660254), pch = 21, cex = mida)
            }
            legend(-15, 91, bty = "n", pch = 19, legend = idllegenda[1:nlevels(x[ , grup])], col = arqub[1:nlevels(x[ , grup])])
            legend(-15, 91, bty = "n", pch = 21, legend = rep("", nlevels(x[ , grup])))
          }
        }
        if (paleta == 2) {
          if (encerclat == 2) {
            for (w in 1:n) {
              points(punts[w, 4], (punts[w, 2] * 0.8660254), pch = 19, cex = mida, col=arqubBN[x[w, grup]])
            }
            legend(-15, 91, bty = "n", pch = 19, legend = idllegenda[1:nlevels(x[ , grup])], col = arqubBN[1:nlevels(x[ , grup])])
          }
          if (encerclat == 1) {
            for (w in 1:n) {
              points(punts[w, 4], (punts[w, 2] * 0.8660254), pch = 19, cex = mida, col=arqubBN[x[w, grup]])
              points(punts[w, 4], (punts[w, 2] * 0.8660254), pch = 21, cex = mida)
            }
            legend(-15, 91, bty = "n", pch = 19, legend = idllegenda[1:nlevels(x[ , grup])], col = arqubBN[1:nlevels(x[ , grup])])
            legend(-15, 91, bty = "n", pch = 21, legend = rep("", nlevels(x[ , grup])))
          }
        }
        if (paleta==3) {
          if (encerclat==2) {
            points(punts[,4],(punts[,2]*0.8660254),pch=19,cex = mida, col=colx)
            legend(-15,91, bty="n",pch=19, legend = idllegenda[1:nlevels(x[,grup])], col = rainbow(nlevels(x[,grup])))
          }
          if (encerclat==1) {
            points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=mida,bg=colx)
            legend(-15,91, bty="n",pch=19, legend = idllegenda[1:nlevels(x[,grup])], col = rainbow(nlevels(x[,grup])))
            legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
          }
        }
        if (paleta==4) {
          if (encerclat==2) {
            points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=mida,col=colx)
            legend(-15,91, bty="n",pch=19, legend = idllegenda[1:nlevels(x[,grup])], col = heat.colors(nlevels(x[,grup])))
          }
          if (encerclat==1) {
            points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=mida,bg=colx)
            legend(-15,91, bty="n",pch=19, legend = idllegenda[1:nlevels(x[,grup])], col = heat.colors(nlevels(x[,grup])))
            legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
          }
        }
        if (paleta==5) {
          if (encerclat==2) {
            points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=mida,col=colx)
            legend(-15,91, bty="n",pch=19, legend = idllegenda[1:nlevels(x[,grup])], col = terrain.colors(nlevels(x[,grup])))
          }
          if (encerclat==1) {
            points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=mida,bg=colx)
            legend(-15,91, bty="n",pch=19, legend = idllegenda[1:nlevels(x[,grup])], col = terrain.colors(nlevels(x[,grup])))
            legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
          }
        }
        if (paleta==6) {
          if (encerclat==2) {
            points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=mida,col=colx)
            legend(-15,91, bty="n",pch=19, legend = idllegenda[1:nlevels(x[,grup])], col = topo.colors(nlevels(x[,grup])))
          }
          if (encerclat==1) {
            points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=mida,bg=colx)
            legend(-15,91, bty="n",pch=19, legend = idllegenda[1:nlevels(x[,grup])], col = topo.colors(nlevels(x[,grup])))
            legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
          }
        }
        if (paleta==7) {
          if (encerclat==2) {
            points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=mida,col=colx)
            legend(-15,91, bty="n",pch=19, legend = idllegenda[1:nlevels(x[,grup])], col = cm.colors(nlevels(x[,grup])))
          }
          if (encerclat==1) {
            points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=mida,bg=colx)
            legend(-15,91, bty="n",pch=19, legend = idllegenda[1:nlevels(x[,grup])], col = cm.colors(nlevels(x[,grup])))
            legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(x[,grup])))
          }
        }
      }
      #  Dibuix dels punts sense grups
      if (grup == 0) {
        if (encerclat == 2) {
          points(punts[ , 4], (punts[ , 2] * 0.8660254), pch = 19, cex = mida, col = color)
        }
        if (encerclat == 1) {
          points(punts[ , 4], (punts[ , 2] * 0.8660254), pch = 21, cex = mida, bg = color)
        }
      }
      # -----Salvo els triangles en format emf i pdf generant un nom per cadascun d'ells-----
      xxx <- recordPlot()
      nomemf <- paste("triangle",triang,".emf",sep = "")
      emf(nomemf)
      replayPlot(xxx)
      dev.off()
      nompdf <- paste("triangle",triang,".pdf",sep = "")
      pdf(nompdf)
      replayPlot(xxx)
      dev.off()
      # -----Fi del bucle-----
    }
  }

"etiqueteselements"<-
  function(x)
  {
    # Fa les etiquetes per un plot fent els subscripts. Han d'estar una en cada fila.
    # Feta per Jaume Buxeda i Garrigos.
    n <- dim(x)[1]
    etiquetes <- vector(length = n)
    etiquetes[] <- dimnames(x)[[1]]
    for(i in 1:n) {
      if (regexpr ("^Fe2O3$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Fe"["2"]*"O"["3"]))
      if (regexpr ("^FeO$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("FeO"))
      if (regexpr ("^Fe$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Fe"))
      if (regexpr ("^Al2O3$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Al"["2"]*"O"["3"]))
      if (regexpr ("^Al$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Al"))
      if (regexpr ("^P2O5$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("P"["2"]*"O"["5"]))
      if (regexpr ("^P$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("P"))
      if (regexpr ("^TiO2$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("TiO"["2"]))
      if (regexpr ("^Ti$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Ti"))
      if (regexpr ("^Na2O$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Na"["2"]*"O"))
      if (regexpr ("^Na$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Na"))
      if (regexpr ("^K2O$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("K"["2"]*"O"))
      if (regexpr ("^K$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("K"))
      if (regexpr ("^SiO2$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("SiO"["2"]))
      if (regexpr ("^Si$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Si"))
      if (regexpr ("^MnO$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("MnO"))
      if (regexpr ("^Mn$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Mn"))
      if (regexpr ("^MgO$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("MgO"))
      if (regexpr ("^Mg$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Mg"))
      if (regexpr ("^CaO$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("CaO"))
      if (regexpr ("^Ca$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Ca"))
      if (regexpr ("^Ba$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Ba"))
      if (regexpr ("^BaO$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("BaO"))
      if (regexpr ("^Rb$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Rb"))
      if (regexpr ("^Nb$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Nb"))
      if (regexpr ("^Pb$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Pb"))
      if (regexpr ("^Zr$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Zr"))
      if (regexpr ("^Y$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Y"))
      if (regexpr ("^Sr$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Sr"))
      if (regexpr ("^Ce$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Ce"))
      if (regexpr ("^Ga$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Ga"))
      if (regexpr ("^V$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("V"))
      if (regexpr ("^Zn$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Zn"))
      if (regexpr ("^Cu$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Cu"))
      if (regexpr ("^Ni$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Ni"))
      if (regexpr ("^Cr$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Cr"))
      if (regexpr ("^Th$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Th"))
      if (regexpr ("^Mo$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Mo"))
      if (regexpr ("^Sn$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Sn"))
      if (regexpr ("^W$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("W"))
      if (regexpr ("^Co$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Co"))
      if (regexpr ("^LOI$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("LOI"))
      if (regexpr ("vt", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("vt"))
      if (regexpr ("tv", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("tv"))
      if (regexpr ("^As$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("As"))
      if (regexpr ("^Ca$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Ca"))
      if (regexpr ("^La$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("La"))
      if (regexpr ("^Lu$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Lu"))
      if (regexpr ("^Na$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Na"))
      if (regexpr ("^Nd$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Nd"))
      if (regexpr ("^Sm$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Sm"))
      if (regexpr ("^U$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("U"))
      if (regexpr ("^Yb$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Yb"))
      if (regexpr ("^Cs$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Cs"))
      if (regexpr ("^Eu$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Eu"))
      if (regexpr ("^Fe$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Fe"))
      if (regexpr ("^Hf$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Hf"))
      if (regexpr ("^Sb$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Sb"))
      if (regexpr ("^Sc$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Sc"))
      if (regexpr ("^Ta$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Ta"))
      if (regexpr ("^Tb$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Tb"))
      if (regexpr ("^Al$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Al"))
      if (regexpr ("^Gd$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Gd"))
      if (regexpr ("^K$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("K"))
      if (regexpr ("^Dy$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Dy"))
      if (regexpr ("^Ho$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Ho"))
      if (regexpr ("^Mg$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Mg"))
      if (regexpr ("^Er$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Er"))
      if (regexpr ("^Mn$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Mn"))
      if (regexpr ("^Pr$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Pr"))
      if (regexpr ("^Ti$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Ti"))
      if (regexpr ("^Tm$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Tm"))
      if (regexpr ("^Ppc$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("Ppc"))
      if (regexpr ("^SO3$", as.character(etiquetes[i])) == T) etiquetes[i] <- expression(bold("SO"["3"]))
    }
    as.vector(etiquetes)
  }

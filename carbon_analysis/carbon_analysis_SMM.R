library(knitr)
library(BIOMASS)
library(dplyr)

#Load datasets
primer <- read.csv("./carbon_analysis/primer_tree_measurement.csv")
sekunder <- read.csv("./carbon_analysis/secunder_tree_measurement.csv")

#Retrieve wood density 
  ##Check and retrieve taxonomy First, check for any typo in the taxonomy
  Taxo <- correctTaxo(genus = primer$genus, species = primer$species, useCache = T, verbose = F)
  primer$genusCorr <- Taxo$genusCorrected
  primer$speciesCorr <- Taxo$speciesCorrected
  #If needed, retrieve APG III families and orders from genus names
  APG <- getTaxonomy(primer$genusCorr, findOrder = T)
  primer$familyAPG <- APG$family
  primer$orderAPG <- APG$order
  #Retrieve wood density using the plot level average if no genus level information is available
  dataWD <- getWoodDensity(
    genus = primer$genusCorr,
    species = primer$speciesCorr,
    stand = primer$plotId
  )
  #Below the number of wood density value estimated at the species, genus and plot level:
  ##At species level
  sum(dataWD$levelWD == "species")
  ##At genus level
  sum(dataWD$levelWD == "genus")
  ##At plot level
  sum(!dataWD$levelWD %in% c("genus", "species"))

#Build height-diameter models
  ##You may compare different models at once
  result <- modelHD(
    D = sekunder$D,
    H = sekunder$H,
    useWeight = TRUE
  )
  kable(result)
  ##Compute the local H-D model with the lowest RSE
  HDmodel <- modelHD(
    D = sekunder$D,
    H = sekunder$H,
    method = "log2",
    useWeight = TRUE
  )
  ##Compute models specific to given stands
  HDmodelPerPlot <- modelHD(
    D = sekunder$D, H = sekunder$H, method = "weibull",
    useWeight = T, plot = sekunder$plotId
  )
  ResHD <- t(sapply(HDmodelPerPlot, function(x) c(coef(x$model), RSE = x$RSE)))
  kable(ResHD, row.names = T, digits = 3)

#Retrieve height data
  ##Retrieve height data from a local Height-diameter model
  dataHlocal <- retrieveH(
    D = primer$D,
    model = HDmodel
  )
  ##Retrieve height data from a Feldpaush et al. (2012) averaged model
  dataHfeld <- retrieveH(
    D = primer$D,
    region = "SEAsia"
  )
  ##Retrieve height data from Chave et al. (2012) equation 6
  dataHchave <- retrieveH(
    D = primer$D,
    coord = primer[, c("long", "lat")]
  )

#Estimate AGB
  ##organize data
  primer$WD <- dataWD$meanWD
  primer$H <- dataHlocal$H
  primer$Hfeld <- dataHfeld$H
  ##Compute AGB(Mg) per tree
  AGBtree <- computeAGB(
    D = primer$D,
    WD = primer$WD,
    H = primer$H
  )
  ##Compute AGB(Mg) per plot (need to be divided by plot area to get Mg/ha)
  AGBplot <- summaryByPlot(AGBtree, primer$plotId)
  AGBplot$TonHa <- AGBplot$AGB*100
  ##Compute AGB(Mg) per tree without height information (Eq. 7 from Chave et al. (2014))
  AGBplotChave <- summaryByPlot(
    computeAGB(
      D = primer$D, WD = primer$WD,
      coord = primer[, c("long", "lat")]
    ),
    primer$plotId
  )
  ##Compute AGB(Mg) per tree with Feldpausch et al. (2012) regional H-D model
  AGBplotFeld <- summaryByPlot(
    computeAGB(
      D = primer$D, WD = primer$WD,
      H = primer$Hfeld
    ),
    plot = primer$plotId
  )

#Propagate AGB errors
  ##Organize data
  primer$sdWD <- dataWD$sdWD
  primer$HfeldRSE <- dataHfeld$RSE
  ##Propagate error for all tree at once using the local HD model constructed above (modelHD), 
  ##i.e. non-independent allometric errors will be assigned to all trees at each iteration, independently of plots.
  resultMC <- AGBmonteCarlo(D = primer$D, WD = primer$WD, errWD = primer$sdWD, HDmodel = HDmodel, Dpropag = "chave2004")
  Res <- summaryByPlot(resultMC$AGB_simu, primer$plotId)
  Res <- Res[order(Res$AGB), ]
  plot(Res$AGB, pch = 20, xlab = "Plots", ylab = "AGB", ylim = c(0, max(Res$Cred_97.5)), las = 1, cex.lab = 1.3)
  segments(seq(nrow(Res)), Res$Cred_2.5, seq(nrow(Res)), Res$Cred_97.5, col = "red")
  ##Using the Feldpaush regional HD averaged model (code only given)
  resultMC <- AGBmonteCarlo(
    D = primer$D,
    WD = primer$WD,
    errWD = primer$sdWD,
    H = primer$Hfeld,
    errH = primer$HfeldRSE,
    Dpropag = "chave2004"
  )
  
  Res <- summaryByPlot(resultMC$AGB_simu, primer$plotId)
  Res <- Res[order(Res$AGB), ]
  plot(Res$AGB, pch = 20, xlab = "Plots", ylab = "AGB", ylim = c(0, max(Res$Cred_97.5)), las = 1, cex.lab = 1.3)
  segments(seq(nrow(Res)), Res$Cred_2.5, seq(nrow(Res)), Res$Cred_97.5, col = "red")
  ##Per plot using the Chave et al. (2014) Equation 7 (code only given)
  resultMC <- AGBmonteCarlo(
    D = primer$D,
    WD = primer$WD,
    errWD = primer$sdWD,
    coord = primer[, c("long", "lat")],
    Dpropag = "chave2004"
  )
  Res <- summaryByPlot(resultMC$AGB_simu, primer$plotId)
  Res <- Res[order(Res$AGB), ]
  plot(Res$AGB, pch = 20, xlab = "Plots", ylab = "AGB", ylim = c(0, max(Res$Cred_97.5)), las = 1, cex.lab = 1.3)
  segments(seq(nrow(Res)), Res$Cred_2.5, seq(nrow(Res)), Res$Cred_97.5, col = "red")

#Some tricks
##Mixing measured and estimated height values 
##If you want to use a mix of directly-measured height and of estimated ones, you may do the following steps.
#Build a vector of H and RSE where we assume an error of 0.5 m on directly measured trees
sekunder$Hmix <- sekunder$H
sekunder$RSEmix <- 0.5
filt <- is.na(sekunder$Hmix)
sekunder$Hmix[filt] <- retrieveH(sekunder$D, model = HDmodel)$H[filt]
sekunder$RSEmix[filt] <- HDmodel$RSE
#Apply the AGBmonteCarlo by setting the height values and their errors (which depend on whether the tree was directly measured or estimated)
wd <- getWoodDensity(sekunder$genus, sekunder$species)
resultMC <- AGBmonteCarlo(
  D = sekunder$D, WD = wd$meanWD, errWD = wd$sdWD,
  H = sekunder$Hmix, errH = sekunder$RSEmix,
  Dpropag = "chave2004"
)
Res <- summaryByPlot(resultMC$AGB_simu, sekunder$plotId)
Res <- Res[order(Res$AGB), ]
plot(Res$AGB, pch = 20, xlab = "Plots", ylab = "AGB (Mg/ha)", ylim = c(0, max(Res$Cred_97.5)), las = 1, cex.lab = 1.3)
segments(1:nrow(Res), Res$Cred_2.5, 1:nrow(Res), Res$Cred_97.5, col = "red")

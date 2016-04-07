# Lautaro Rossi
# 07/04/2016
# Codi sota llicència GNU GPL v3

##dependencies --------------------------------------------------------
setwd("ruta del directori")
load("./codi_paisos.RData")

### llibreries
#install.packages("WDI")
#install.packages("dplyr")
library(WDI)
library(dplyr)
library(tidyr)

### WDIsearch() poblacio, area, area forestal, accés aigua, mortalitat menors 5
### anys, etc. ## es poden buscar els codis de més variables (ex.: WDIsearch("gdp"))
## afegir els codis desitjats a indvars (entre "" i després de una coma)

## variables incial sescollides ---------------------------------------
indvars <-
  c(
    "SE.PRM.CMPT.FE.ZS",
    "SI.POV.GINI",
    "SH.STA.ACSN",
    "CC.EST",
    "SH.H2O.SAFE.ZS",
    "AG.LND.TOTL.K2",
    "SP.POP.TOTL",
    "IQ.CPA.TRAN.XQ"
  )
depvars <- c("SH.DYN.MORT")
anyinici <- 2010
anyfinal <- 2013

anymortalitat <- 2014


## escriure en ordre el nom desitjat per a cada variable --------------

nomindvars <-
  c(
    "estudispimarisdona",
    "Gini",
    "accessanitat",
    "controlcorrupcio",
    "accesaigua",
    "areakm2",
    "poblacio",
    "CPIA"
  )
nomdepvars <- paste0("mortalitat", anymortalitat)

## acces API world bank indicators ------------------------------------
## (s'ha d'haver creat la variable paisos amb dadesbancmundial.R o codi_paisos.RData)
tdades = WDI(
  indicator = depvars,
  country = paisos,
  start = anymortalitat,
  end = anymortalitat
)
tdades <- tbl_df(tdades)
tdades <- select(tdades,-year)
names(tdades) <- c("codi", "pais", nomdepvars)


# indicador per anys ------------------------------------------------------

var = 1
for (var in 1:length(indvars)) {
  wbdat = WDI(
    indicator = indvars[var],
    country = paisos,
    start = anyinici,
    end = anyfinal
  )
  wbdat <- tbl_df(wbdat)
  wbdat <- spread_(wbdat, key_col = "year", value_col = indvars[var])
  names(wbdat) <- c("codi", "pais", paste0(nomindvars[var], anyinici:anyfinal))
  wbdat <- select(wbdat,-pais)
  tdades <- full_join(tdades, wbdat, by = "codi")
}


# neteja NAs --------------------------------------------------------------
colSums(is.na(tdades))
rowSums(is.na(tdades))

tdades <- tdades[, !(colSums(is.na(tdades)) >= 4)]
## filter(tdades, (rowSums(is.na(tdades)) >= 5))
tdades <- filter(tdades,!(rowSums(is.na(tdades)) >= 5))
View(tdades)

## save(tdades, file="tdades_mortalitat2014.RData")

# carregar RCommander -----------------------------------------------------

library(Rcmdr)

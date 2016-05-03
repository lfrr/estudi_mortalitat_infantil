# Lautaro Rossi
# 07/04/2016
# Codi sota llicència GNU GPL v3

##dependencies
#se tiene que definir la carpeta de trabajo para cargar los datos codipaisos
# se define con la funcion setwd o borrar y hacerlo desde Rstudio con session>set working directory>choose directory
setwd("ruta del directori")
load("./codi_paisos.RData")

### llibreries
# si no se tienen las siguienetes librerias instaladas, quitar las almohadillas a las dos siguientes lineas>
#install.packages("WDI")
#install.packages("dplyr")
library(WDI)
library(dplyr)

# atención: se tiene que usart la funcion WDisearch para encontrar los codigos de las vbariables que se quieren utilizar:
### WDIsearch() poblacio, area, area forestal, accés aigua, mortalitat menors 5 anys, etc.
# ejemplo::: WDIsearch("population") y dará como resultado todos los codigos de las variables que estan relacionadas con la poblacion
## es poden buscar els codis de més variables (ex.: WDIsearch("gdp"))
## afegir els codis desitjats a indvars (entre "" i després de una coma)

## variables incial sescollides 
indvars <- c("SI.POV.GINI", "SH.STA.ACSN", "CC.EST",
             "SH.H2O.SAFE.ZS", "AG.LND.TOTL.K2", "SP.POP.TOTL", "IQ.CPA.TRAN.XQ", 
             "HH.DHS.PCR.F",
             "HH.DHS.SCR.F",
             "HH.MICS.SCR.F",
             "SE.PRM.CMPT.FE.ZS",
             "NV.IND.TOTL.ZS",
             "EN.ATM.CO2E.PP.GD")
depvars <- c("SH.DYN.MORT")

## escriure en ordre el nom desitjat per a cada variable

nomindvars <- c("Gini", "accessanitat", "controlcorrupcio", "accesaigua",
                "areakm2", "poblacio", "CPIA",
                "DHSprimcompletionf",
                "DHSseccompletionf",
                "MICSseccompletionf",
                "primcompletionfpercent",
                "inudstrialitzacio",
                "co2perpib")
nomdepvars <- c("mortalitatinf")

## acces API world bank indicators (s'ha d'haver creat la variable paisos amb dadesbancmundial.R o codi_paisos.RData)
## les variables explicatives es prenen de 2013 i la variable explicada de 2014
wbdat = WDI(indicator = indvars, country = paisos, start=1980, end=2013)
wbdat2 = WDI(indicator = depvars, country = paisos, start=2014, end=2014)

# ¡¡NO TOCAR NADA DESDE AQUI!!
wbdat <- tbl_df(wbdat)
wbdat2 <- tbl_df(wbdat2)

wbdat <- arrange(wbdat, country, desc(year))
# View(wbdat)

##???????????? Escollir la útlima dada disponible de cada variable per a cada país ????????????##

colums <- dim(wbdat)[2] - 1
dades<- tbl_df(data.frame(matrix(nrow=length(paisos), ncol=colums)))

## donar noms a les variables
names(dades) <- c("codi", "pais", nomindvars)
p <- 1
j <- 3
i <- 1
for(p in 1:length(paisos)){

  datpais <- filter(wbdat, iso2c == paisos[p])
  m <- dim(datpais)[1]
  n <- dim(datpais)[2]
  
  for(j in 3:colums){
    for(i in 1:m){
      if(!is.na(datpais[i, j+1])){
        dades[p,1] <- datpais[i,1]
        dades[p,2] <- datpais[i,2]
        dades[p,j] <- datpais[i,j+1]
        break
      }
    }
  }
  j <- 0
  i <- 0
}

names(wbdat2) <- c("codi", "pais", nomdepvars, "any")
mortalitatinf <- select(wbdat2, -any,-pais)
dades <- full_join(dades, mortalitatinf, by="codi")
# View(dades)

colSums(is.na(dades))
rowSums(is.na(dades))
sum(complete.cases(dades))

dades <- dades[,!(colSums(is.na(dades)) >= 16)]
dades <- dades[which(complete.cases(dades)),]


## filter(dades, (rowSums(is.na(dades)) >= 2))
## dades <- filter(dades, !(rowSums(is.na(dades)) >= 2))
dades

## esta función guardará los datos en el directorio
save(dades, file="./dadespat.RData")
load("./dadespat_ultimany.RData")
library(Rcmdr)

# no ejecutar esta sección si se cambia algo del script
if(FALSE){
# estadística descriptiva -------------------------------------------------

dades$controlcorrupcio <- (dades$controlcorrupcio+2)^(1)
dades$controlcorrupcio <- (dades$controlcorrupcio)^(-0.13)
hist(dades$controlcorrupcio)
shapiro.test(dades$controlcorrupcio)

hist(dades$accessanitat)
shapiro.test(dades$primcompletionfpercent^(1))
dades$primcompletionfpercent <- dades$primcompletionfpercent^(4)

scatterplotMatrix(~ controlcorrupcio + 
                    accessanitat + 
                    primcompletionfpercent
                  , 
                  transform=FALSE, 
                  data=dades)

shapiro.test(log(dades$accessanitat))

# selecció model ----------------------------------------------------------

RegModel.1 <- 
  lm(mortalitatinf~accesaigua+accessanitat+co2perpib+controlcorrupcio+Gini+inudstrialitzacio+primcompletionfpercent,
     data=dades)
summary(RegModel.1)
library(MASS, pos=21)
stepwise(RegModel.1, direction='backward/forward', 
         criterion='BIC')
stepwise(RegModel.1, direction='forward/backward', 
         criterion='BIC')
regr3var <- 
  lm(mortalitatinf~accessanitat+controlcorrupcio+primcompletionfpercent,
     data=dades)
summary(regr3var)
}

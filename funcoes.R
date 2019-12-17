# install.packages('purrr')
# install.packages('assertthat')
# install.packages('dplyr')
# #library(WriteXLS)
# install.packages("maps")
# install.packages('rJava')
# install.packages('ggplot2')
# install.packages('igraph')
# install.packages('varhandle')
# install.packages('ggforce')
# install.packages('ggraph')
# install.packages('RgoogleMaps')
# install.packages('ggmap')
# install.packages('xlsjars') #not avaliable for this version
# install.packages('xlsx')
# install.packages('readxl')
# install.packages('curl')
# install.packages('httr')
# install.packages('tidyverse')
# install.packages('svglite')
# install.packages('xlsx')
# install.packages('readxl')
# install.packages('htmltab')
# install.packages('maps_data') #not avaliable for this version
# install.packages("devtools")
# #install.packages('ghit') not avaliable for this version
# install.packages("WriteXLS")
# install.packages('comtradr')
# install.packages('WDI')
# install.packages('imfr')

library(xlsx)
library(rJava)
library(readxl)
library(readr)
library(imfr)
library(WDI)
library(comtradr)
'%!in%' <- function(x,y)!('%in%'(x,y))


#for having lists of levels of the countries
read.xlsx("selectedCountries.xlsx")
ctrNivDes <- function(nvlDevelop,tipo) {
  if (tipo == "iso2") {
    return(selectedCountries$ISO_2_IMF[which(selectedCountries$Dev_Status == nvlDevelop)])
  } else if (tipo == "iso3") {
    return(selectedCountries$ISO_3_UN[which(selectedCountries$Dev_Status == nvlDevelop)])
  } else {print("Escolha o codigo ISO2 ou ISO3")}
}

#função para converter iso3 em iso2
converteIso <- function(iso3){
  return(as.character(selectedCountries$ISO_2_IMF[which(selectedCountries$ISO_3_UN==as.character(iso3))]))
}
#converte de iso3 p/ iso2
converteIso3 <- function(iso3){
  return(as.character(selectedCountries$ISO_2_IMF[which(selectedCountries$ISO_3_UN==as.character(iso3))]))
}

#converte iso2 p; iso3
converteIso2 <- function(iso2){
  return(as.character(selectedCountries$ISO_3_UN[which(selectedCountries$ISO_2_IMF==as.character(iso2))]))
}



#listas por diversos tipos de agregados
listAgreg <- function(categoria,tipo,iso) {
  if (categoria == "DevLevel")
    if (iso == 2) {
      return(selectedCountries$ISO_2_IMF[which(selectedCountries$Dev_Status == tipo)])
    } else if (iso == 3) {
      return(selectedCountries$ISO_3_UN[which(selectedCountries$Dev_Status == tipo)])
    } else {print("Escolha o codigo ISO2 ou ISO3")}
  else if (categoria == "Renda") {
    if (iso == 2) {
      return(selectedCountries$ISO_2_IMF[which(selectedCountries$Income_Level == tipo)])
    } else if (iso == 3) {
      return(selectedCountries$ISO_3_UN[which(selectedCountries$Income_Level == tipo)])
    } else {print("Escolha o codigo ISO2 ou ISO3")}
  }
  else if (categoria == "Regiao") {
    if (iso == 2) {
      return(selectedCountries$ISO_2_IMF[which(selectedCountries$Region == tipo)])
    } else if (iso == 3) {
      return(selectedCountries$ISO_3_UN[which(selectedCountries$Region == tipo)])
    } else {print("Escolha o codigo ISO2 ou ISO3")}
  }
  else if (categoria == "SubRegiao") {
    if (iso == 2) {
      return(selectedCountries$ISO_2_IMF[which(selectedCountries$Sub_Region == tipo)])
    } else if (iso == 3) {
      return(selectedCountries$ISO_3_UN[which(selectedCountries$Sub_Region == tipo)])
    } else {print("Escolha o codigo ISO2 ou ISO3")}
  }
  else {print("Escolha uma categoria correta - DevLevel, Renda, Regiao, SubRegiao")} 
}


#funcao nao esta
'%!in%' <- function(x,y)!('%in%'(x,y))

#funcao deflatora
deflaciona <- function(valor,ano,land,tp,iso) {
  if (iso == 2) {
    if (tp == "Imp") {
      return(valor*indImpAtual$TM.VAL.MRCH.XD.WD[which(indImpAtual$year==ano&indImpAtual$iso2c==land)])}
    else if (tp == "Exp") {
      return(valor*indExpAtual$TX.VAL.MRCH.XD.WD[which(indExpAtual$year==ano&indImpAtual$iso2c==land)])}
    else {print("escolha um tp correto")}
  }
  else if (iso == 3) {
    if (tp == "Imp") {
      return(valor*indImpAtual$TM.VAL.MRCH.XD.WD[which(
        indImpAtual$year==ano&(indImpAtual$iso2c==selectedCountriesDef$ISO_2_IMF[which(
          selectedCountriesDef$ISO_3_UN==land)]))])}
    else if (tp == "Exp") {
      return(valor*indExpAtual$TX.VAL.MRCH.XD.WD[which(
        indExpAtual$year==ano&(indExpAtual$iso2c==selectedCountriesDef$ISO_2_IMF[which(
          selectedCountriesDef$ISO_3_UN==land)]))])}
    else {print("escolha um tp correto")}
  }
}
#deflaciona(1000,2000,"US","Imp",2)


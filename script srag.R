
# --------------------------------------------------------- 

# CIDACS FIOCRUZ

# script SRAG

# last update 14 02 2022


# --------------------------------------------------------- packages

library( "data.table" )
library( "openxlsx" )
library( "flextable" )
library( "chron")
library( "ggplot2" )
library( "ggalluvial" )
library( "lubridate")
library( "tidyverse" )
library( "reshape2" )
library( "stringr" )
library( "DiagrammeR" )
require( "magrittr" )
require( "DiagrammeRsvg" )
require( "xml2" )
library( "rsvg" )



# --------------------------------------------------------- links


link.princ <- "C:/IVAN 2067/Consultorias 2021/@ CIDACS/ivan lima/SRAG"
link.entrada <- "C:/IVAN 2067/Consultorias 2021/@ CIDACS/ivan lima/SRAG/ENTRADAS"
link.saida <- "C:/IVAN 2067/Consultorias 2021/@ CIDACS/ivan lima/SRAG/SAIDAS"

setwd( link.princ )

# --------------------------------------------------------- constants

constantes <- function()
  
{
  
  UF      <- c( "RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL",
                "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")
  UF.cod  <- as.character( c(  11 ,  12,   13,   14,   15,   16,   17,   21,   22,   23,   24,   25,   26,   27,
                               28,   29,   31,   32,   33,   35,   41,   42,   43,   50,   51,   52,   53) )
  UF.nome <- c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins","Maranhão","Piauí","Ceará","Rio Grande do Norte",
               "Paraíba","Pernambuco","Alagoas","Sergipe","Bahia","Minas Gerais","Espírito Santo","Rio de Janeiro",
               "São Paulo","Paraná","Santa Catarina","Rio Grande do Sul","Mato Grosso do Sul","Mato Grosso",
               "Goiás","Distrito Federal")
  Regiao <- c( rep("Norte", 7), rep("Nordeste", 9), rep("Sudeste", 4),  rep("Sul", 3), rep("Centro-oeste", 4) )
  
  UFcods <- data.frame( UF = UF, UF.cod = UF.cod, UF.nome = UF.nome , Regiao = Regiao, stringsAsFactors = FALSE )
  UFcods <<- UFcods[ order( UFcods$UF ), ]
  Anos <<- 2000:2015
  
  Data.Hora <<- format(Sys.time(), "%a %b %d %X %Y")
  
  # Mensagem
  
  cat(" ------------------------------------------","\n")
  cat("\n")
  cat("  Gerado os objetos:                       ","\n")
  cat("  UFcods                                   ","\n")
  cat("  Anos                                     ","\n")
  cat("  Data.Hora                                ","\n")
  cat("\n")
  cat(" ------------------------------------------","\n")
  
}

constantes()

# --- output

UFcods$Regiao.eng[ UFcods$Regiao == "Norte"] <- "North"
UFcods$Regiao.eng[ UFcods$Regiao == "Nordeste"] <- "Northeast"
UFcods$Regiao.eng[ UFcods$Regiao == "Centro-oeste"] <- "Central-West"
UFcods$Regiao.eng[ UFcods$Regiao == "Sudeste"] <- "Southeast"
UFcods$Regiao.eng[ UFcods$Regiao == "Sul"] <- "South"

setwd(link.saida)

saveRDS( UFcods, "UFcods.RDS")

setwd(link.princ)

# --------------------------------------------------------- 

# options(tibble.width = Inf, tibble.print_max = Inf)

# --------------------------------------------------------- file input


setwd( link.entrada )

srag <- read.csv("SRAG.csv")

srag$Ano <- substr( srag$DT_SIN_PRI, 7, 10 )

srag$id <- 1:nrow( srag )

# summary DT_SIN_PRI

srag$DT_SIN_PRI <- dmy( srag$DT_SIN_PRI )

table(srag$DT_SIN_PRI,useNA = "always")

summary( srag$DT_SIN_PRI )

# min = 2019-12-29
# max = 2021-11-28
# NA = 20

# --- capitals

setwd( link.entrada )

capitais <- read.xlsx( "municipios capitais Brasil.xlsx" )

capitais$codigo6 <- substr( capitais$codigo, 1, 6 )


# --- epidemiological week

setwd( link.entrada )

semana.epi.2020 <- read.csv2("semanas epi 2020.txt")
semana.epi.2021 <- read.csv2("semanas epi 2021.txt")

semana.epi.cuts <- c(semana.epi.2020$d1, semana.epi.2021$d1)

setwd( link.princ )

# --------------------------------------------------------- set study period

#periodo.estudo <- c("2019-12-28","2021-11-14")

#periodo.estudo.date <- ymd( periodo.estudo )

#periodo.estudo.date.interval <- interval( periodo.estudo.date[1], periodo.estudo.date[2] )

# --------------------------------------------------------- filter database 

# --- filter database using date of first symtomps [DT_SIN_PRI]

#dentro <-  srag$DT_SIN_PRI %within% periodo.estudo.date.interval 

srag.estudo <- srag

rm(srag)
gc()

# ---- empty rows

vazio <- srag.estudo[is.na(srag.estudo$DT_SIN_PRI),]

srag.estudo <- srag.estudo[ !srag.estudo$id %in% vazio$id, ]

# --------------------------------------------------------- add epidemiological week and count

# use DT_SIN_PRI

srag.estudo$semana <- cut( srag.estudo$DT_SIN_PRI , 
                           breaks = as.Date(semana.epi.cuts,
                           format = c("%d/%m/%Y") ) )

srag.estudo$conta <- 1



# --------------------------------------------------------- semana do SEM_PRI e semana epidemiologica

srag.estudo.SEM_PRI.semana.conta <- aggregate( srag.estudo$conta, 
                                               by = list( srag.estudo$SEM_PRI, srag.estudo$semana ),
                                               sum )

tabela.semana.numero <- srag.estudo.SEM_PRI.semana.conta[,1:2]

names(tabela.semana.numero) <- c("Epidemiological week number", "First day")

tabela.semana.numero.2020 <- tabela.semana.numero[1:53,]
tabela.semana.numero.2021 <- tabela.semana.numero[54:101,]

# output

setwd( link.saida )

saveRDS(tabela.semana.numero.2020,"tabela.semana.numero.2020.RDS")
saveRDS(tabela.semana.numero.2021,"tabela.semana.numero.2021.RDS")

write.csv2( tabela.semana.numero.2020,"tabela.semana.numero.2020.csv" , row.names = FALSE )
write.csv2( tabela.semana.numero.2021,"tabela.semana.numero.2021.csv" , row.names = FALSE )

setwd( link.princ )



# --------------------------------------------------------- recoding

# --- REGIAO

UFS <- UFcods$UF

srag.estudo$REGIAO <- character( nrow(srag.estudo) )


lapply(  UFS , function( x ){
  
  srag.estudo$REGIAO[srag.estudo$SG_UF_NOT == x ] <<- UFcods$Regiao[ UFcods$UF == x ]
  
})


srag.estudo$REGIAO_ENG <- character( nrow( srag.estudo ) )

srag.estudo$REGIAO_ENG[ srag.estudo$REGIAO == "Centro-oeste"] <- "Central-West"
srag.estudo$REGIAO_ENG[ srag.estudo$REGIAO == "Norte"] <- "North"
srag.estudo$REGIAO_ENG[ srag.estudo$REGIAO == "Sul"] <- "South"
srag.estudo$REGIAO_ENG[ srag.estudo$REGIAO == "Sudeste"] <- "Southeast"
srag.estudo$REGIAO_ENG[ srag.estudo$REGIAO == "Nordeste"] <- "Northeast"

srag.estudo$REGIAO_ENG <- as.factor( srag.estudo$REGIAO_ENG )

table(srag.estudo$REGIAO_ENG, useNA = 'always')

# --- SEXO

table(srag.estudo$CS_SEXO, useNA = "always" )

srag.estudo$CS_SEXO_ENG <- character( nrow( srag.estudo ) )

srag.estudo$CS_SEXO_ENG[srag.estudo$CS_SEXO == "F"] <- "Female"
srag.estudo$CS_SEXO_ENG[srag.estudo$CS_SEXO == "M"] <- "Male"
srag.estudo$CS_SEXO_ENG[srag.estudo$CS_SEXO == "I"] <- "Indeterminate"

table( srag.estudo$CS_SEXO_ENG, useNA = "always" )

srag.estudo$CS_SEXO_ENG <- as.factor( srag.estudo$CS_SEXO_ENG )

srag.estudo$CS_SEXO_ENG <- factor( srag.estudo$CS_SEXO_ENG , levels = levels( srag.estudo$CS_SEXO_ENG )[c(1,3,2)])

table(srag.estudo$CS_SEXO_ENG, useNA = 'always')

# --- EVOLUCAO

srag.estudo$EVOLUCAO_ENG <- character( nrow( srag.estudo ) )

srag.estudo$EVOLUCAO_ENG[srag.estudo$EVOLUCAO == "4"] <- "Indeterminate"
srag.estudo$EVOLUCAO_ENG[srag.estudo$EVOLUCAO == "5"] <- "Indeterminate"
srag.estudo$EVOLUCAO_ENG[srag.estudo$EVOLUCAO == "1"] <- "Recovery"
srag.estudo$EVOLUCAO_ENG[srag.estudo$EVOLUCAO == "2"] <- "Death"
srag.estudo$EVOLUCAO_ENG[srag.estudo$EVOLUCAO == "3"] <- "Death from other causes"
srag.estudo$EVOLUCAO_ENG[srag.estudo$EVOLUCAO == "9"] <- "Ignored"
srag.estudo$EVOLUCAO_ENG[is.na(srag.estudo$EVOLUCAO)] <- "Indeterminate"

srag.estudo$EVOLUCAO_ENG <- as.factor( srag.estudo$EVOLUCAO_ENG )

srag.estudo$EVOLUCAO_ENG <- factor( srag.estudo$EVOLUCAO_ENG, 
                                    levels = levels( srag.estudo$EVOLUCAO_ENG )[c(5,1:4)]  )

table(srag.estudo$EVOLUCAO_ENG,useNA = 'always')

# --- HOSPITAL

srag.estudo$HOSPITAL_ENG <- character( nrow( srag.estudo ) )

table( srag.estudo$HOSPITAL,useNA = "always")

srag.estudo$HOSPITAL_ENG[srag.estudo$HOSPITAL == '1'] <- "Yes"
srag.estudo$HOSPITAL_ENG[srag.estudo$HOSPITAL == '2'] <- "No"
srag.estudo$HOSPITAL_ENG[srag.estudo$HOSPITAL == '9'] <- "Indeterminate"
srag.estudo$HOSPITAL_ENG[srag.estudo$HOSPITAL_ENG == ''] <- "Indeterminate"

table( srag.estudo$HOSPITAL_ENG,useNA = "always")

srag.estudo$HOSPITAL_ENG <- as.factor( srag.estudo$HOSPITAL_ENG )

srag.estudo$HOSPITAL_ENG <- factor( srag.estudo$HOSPITAL_ENG, 
                                    levels = levels( srag.estudo$HOSPITAL_ENG )[c(3,2,1)])

table(srag.estudo$HOSPITAL_ENG,useNA = 'always')

# ---- UTI

srag.estudo$UTI_ENG <- character( nrow( srag.estudo ) )

table( srag.estudo$UTI,useNA = "always")

srag.estudo$UTI_ENG[ srag.estudo$UTI == '1' ] <- "Yes"
srag.estudo$UTI_ENG[ srag.estudo$UTI == '2' ] <- "No"
srag.estudo$UTI_ENG[  srag.estudo$UTI_ENG == '' ] <- "Indeterminate"

srag.estudo$UTI_ENG <- as.factor( srag.estudo$UTI_ENG )

srag.estudo$UTI_ENG <- factor( srag.estudo$UTI_ENG, 
                               levels = levels( srag.estudo$UTI_ENG )[c(3,2,1)])

table( srag.estudo$UTI_ENG,useNA = "always")

# ---- age calculation

idade.cortes <- c(-1,17,29,39,49,64,74,84,200)

periodo.idades <- c("1905-01-01","2021-11-14")

periodo.idades <- ymd( periodo.idades )

periodo.idades.interval <- interval( periodo.idades[1], periodo.idades[2] )

srag.estudo$DT_NASC <- dmy( srag.estudo$DT_NASC )

DT_NASC.dentro <- srag.estudo$DT_NASC %within% periodo.idades.interval

DT_NASC.dentro.df <- data.frame( DT_NASC.dentro = DT_NASC.dentro, DT_NASC = srag.estudo$DT_NASC )

summary(srag.estudo$DT_NASC )

srag.estudo$DT_NASC[ !DT_NASC.dentro] <- NA

srag.estudo$idade.cal <- as.numeric( round((srag.estudo$DT_SIN_PRI - srag.estudo$DT_NASC )/365, 0 ) )

srag.estudo.cuts <- cut( srag.estudo$idade.cal, breaks = idade.cortes , 
                         labels = c("0-17",
                                    "18-29",
                                    "30-39",
                                    "40-49",
                                    "50-64",
                                    "65-74",
                                    "75-84",
                                    "85 or more") )

srag.estudo.cuts <- as.character( srag.estudo.cuts )
srag.estudo.cuts[ is.na(srag.estudo.cuts)] <- "Indeterminate"

srag.estudo$srag.estudo.cuts <- srag.estudo.cuts

srag.estudo$srag.estudo.cuts <- as.factor( srag.estudo$srag.estudo.cuts )


# ---- zone

# zone of residence.

table(srag.estudo$CS_ZONA,useNA = "always")

srag.estudo$CS_ZONA_ENG <- character( nrow( srag.estudo ) )

srag.estudo$CS_ZONA_ENG[ srag.estudo$CS_ZONA == 1 ] <- "Urban"
srag.estudo$CS_ZONA_ENG[ srag.estudo$CS_ZONA == 2 ] <- "Rural"
srag.estudo$CS_ZONA_ENG[ srag.estudo$CS_ZONA == 3 ] <- "Peri-urban"
srag.estudo$CS_ZONA_ENG[ srag.estudo$CS_ZONA == 9 ] <- "Indeterminate"
srag.estudo$CS_ZONA_ENG[ is.na( srag.estudo$CS_ZONA ) ] <- "Indeterminate"

srag.estudo$CS_ZONA_ENG <- as.factor( srag.estudo$CS_ZONA_ENG )

srag.estudo$CS_ZONA_ENG <- factor( srag.estudo$CS_ZONA_ENG, levels = levels( srag.estudo$CS_ZONA_ENG)[c(4,3,2,1)]) 

table(srag.estudo$CS_ZONA_ENG,useNA = "always")


# ---- final classification

srag.estudo$CLASSI_FIN_ENG <- character( nrow( srag.estudo ) )

srag.estudo$CLASSI_FIN_ENG[srag.estudo$CLASSI_FIN == '1'] <- "SARS by influenza"
srag.estudo$CLASSI_FIN_ENG[srag.estudo$CLASSI_FIN == '2'] <- "SARS by another respiratory virus"
srag.estudo$CLASSI_FIN_ENG[srag.estudo$CLASSI_FIN == '3'] <- "SARS by another etiological agent"
srag.estudo$CLASSI_FIN_ENG[srag.estudo$CLASSI_FIN == '4'] <- "SARS unspecified"
srag.estudo$CLASSI_FIN_ENG[srag.estudo$CLASSI_FIN == '5'] <- "SARS by Covid-19"
srag.estudo$CLASSI_FIN_ENG[ srag.estudo$CLASSI_FIN_ENG == '' ] <- "Indeterminate"

srag.estudo$CLASSI_FIN_ENG <- as.factor( srag.estudo$CLASSI_FIN_ENG )

srag.estudo$CLASSI_FIN_ENG <- factor( srag.estudo$CLASSI_FIN_ENG, levels = levels( srag.estudo$CLASSI_FIN_ENG )[c(4,5,6,2,3,1)] )

table(srag.estudo$CLASSI_FIN_ENG,useNA = "always")



# ---- scholarity

srag.estudo$CS_ESCOL_N_ENG <- character( nrow( srag.estudo ) )

srag.estudo$CS_ESCOL_N_ENG[ srag.estudo$CS_ESCOL_N == 0 ] <- "No education"
srag.estudo$CS_ESCOL_N_ENG[ srag.estudo$CS_ESCOL_N == 1 ] <- "Fundamental - First years"
srag.estudo$CS_ESCOL_N_ENG[ srag.estudo$CS_ESCOL_N == 2 ] <- "Fundamental - Last years"
srag.estudo$CS_ESCOL_N_ENG[ srag.estudo$CS_ESCOL_N == 3 ] <- "High school"
srag.estudo$CS_ESCOL_N_ENG[ srag.estudo$CS_ESCOL_N == 4 ] <- "Graduated"
srag.estudo$CS_ESCOL_N_ENG[ srag.estudo$CS_ESCOL_N == 5 ] <- "Not applicable"
srag.estudo$CS_ESCOL_N_ENG[ srag.estudo$CS_ESCOL_N == 9 ] <- "Indeterminate"
srag.estudo$CS_ESCOL_N_ENG[ is.na( srag.estudo$CS_ESCOL_N ) ] <- "Indeterminate"

srag.estudo$CS_ESCOL_N_ENG <- as.factor( srag.estudo$CS_ESCOL_N_ENG )

srag.estudo$CS_ESCOL_N_ENG <- factor( srag.estudo$CS_ESCOL_N_ENG , levels( srag.estudo$CS_ESCOL_N_ENG )[c(6,1,2,3,4,7,5)])

table(srag.estudo$CS_ESCOL_N_ENG,useNA = "always")

# ---- fever

table( srag.estudo$FEBRE, useNA = "always" )

srag.estudo$FEBRE_ENG <- character( nrow( srag.estudo ) )

srag.estudo$FEBRE_ENG[srag.estudo$FEBRE == 1] <- "Yes"
srag.estudo$FEBRE_ENG[srag.estudo$FEBRE == 2] <- "No"
srag.estudo$FEBRE_ENG[srag.estudo$FEBRE == 9] <- "Indeterminate"
srag.estudo$FEBRE_ENG[is.na(srag.estudo$FEBRE)] <- "Indeterminate"

table( srag.estudo$FEBRE_ENG, useNA = "always" )


# ---- cough

table( srag.estudo$TOSSE, useNA = "always" )

srag.estudo$TOSSE_ENG <- character( nrow( srag.estudo ) )

srag.estudo$TOSSE_ENG[srag.estudo$TOSSE == 1] <- "Yes"
srag.estudo$TOSSE_ENG[srag.estudo$TOSSE == 2] <- "No"
srag.estudo$TOSSE_ENG[srag.estudo$TOSSE == 9] <- "Indeterminate"
srag.estudo$TOSSE_ENG[is.na(srag.estudo$TOSSE)] <- "Indeterminate"

table( srag.estudo$TOSSE_ENG, useNA = "always" )

# ---- throat

table( srag.estudo$GARGANTA, useNA = "always" )

srag.estudo$GARGANTA_ENG <- character( nrow( srag.estudo ) )

srag.estudo$GARGANTA_ENG[srag.estudo$GARGANTA == 1] <- "Yes"
srag.estudo$GARGANTA_ENG[srag.estudo$GARGANTA == 2] <- "No"
srag.estudo$GARGANTA_ENG[srag.estudo$GARGANTA == 9] <- "Indeterminate"
srag.estudo$GARGANTA_ENG[is.na(srag.estudo$GARGANTA)] <- "Indeterminate"

table( srag.estudo$GARGANTA_ENG, useNA = "always" )

# ---- dyspnea

table( srag.estudo$DISPNEIA, useNA = "always" )

srag.estudo$DISPNEIA_ENG <- character( nrow( srag.estudo ) )

srag.estudo$DISPNEIA_ENG[srag.estudo$DISPNEIA == 1] <- "Yes"
srag.estudo$DISPNEIA_ENG[srag.estudo$DISPNEIA == 2] <- "No"
srag.estudo$DISPNEIA_ENG[srag.estudo$DISPNEIA == 9] <- "Indeterminate"
srag.estudo$DISPNEIA_ENG[is.na(srag.estudo$DISPNEIA)] <- "Indeterminate"

table( srag.estudo$DISPNEIA_ENG, useNA = "always" )

# ---- Respiratory Discomfort

table( srag.estudo$DESC_RESP, useNA = "always" )

srag.estudo$DESC_RESP_ENG <- character( nrow( srag.estudo ) )

srag.estudo$DESC_RESP_ENG[srag.estudo$DESC_RESP == 1] <- "Yes"
srag.estudo$DESC_RESP_ENG[srag.estudo$DESC_RESP == 2] <- "No"
srag.estudo$DESC_RESP_ENG[srag.estudo$DESC_RESP == 9] <- "Indeterminate"
srag.estudo$DESC_RESP_ENG[is.na(srag.estudo$DESC_RESP)] <- "Indeterminate"

table( srag.estudo$DESC_RESP_ENG, useNA = "always" )

# ---- saturation

table( srag.estudo$SATURACAO, useNA = "always" )

srag.estudo$SATURACAO_ENG <- character( nrow( srag.estudo ) )

srag.estudo$SATURACAO_ENG[srag.estudo$SATURACAO == 1] <- "Yes"
srag.estudo$SATURACAO_ENG[srag.estudo$SATURACAO == 2] <- "No"
srag.estudo$SATURACAO_ENG[srag.estudo$SATURACAO == 9] <- "Indeterminate"
srag.estudo$SATURACAO_ENG[is.na(srag.estudo$SATURACAO)] <- "Indeterminate"

table( srag.estudo$SATURACAO_ENG, useNA = "always" )

# diarrheia


table( srag.estudo$DIARREIA, useNA = "always" )

srag.estudo$DIARREIA_ENG <- character( nrow( srag.estudo ) )

srag.estudo$DIARREIA_ENG[srag.estudo$DIARREIA == 1] <- "Yes"
srag.estudo$DIARREIA_ENG[srag.estudo$DIARREIA == 2] <- "No"
srag.estudo$DIARREIA_ENG[srag.estudo$DIARREIA == 9] <- "Indeterminate"
srag.estudo$DIARREIA_ENG[is.na(srag.estudo$DIARREIA)] <- "Indeterminate"

table( srag.estudo$DIARREIA_ENG, useNA = "always" )

# vomit


table( srag.estudo$VOMITO, useNA = "always" )

srag.estudo$VOMITO_ENG <- character( nrow( srag.estudo ) )

srag.estudo$VOMITO_ENG[srag.estudo$VOMITO == 1] <- "Yes"
srag.estudo$VOMITO_ENG[srag.estudo$VOMITO == 2] <- "No"
srag.estudo$VOMITO_ENG[srag.estudo$VOMITO == 9] <- "Indeterminate"
srag.estudo$VOMITO_ENG[is.na(srag.estudo$VOMITO)] <- "Indeterminate"

table( srag.estudo$VOMITO_ENG, useNA = "always" )


# abdominal pain

table( srag.estudo$DOR_ABD, useNA = "always" )

srag.estudo$DOR_ABD_ENG <- character( nrow( srag.estudo ) )

srag.estudo$DOR_ABD_ENG[srag.estudo$DOR_ABD == 1] <- "Yes"
srag.estudo$DOR_ABD_ENG[srag.estudo$DOR_ABD == 2] <- "No"
srag.estudo$DOR_ABD_ENG[srag.estudo$DOR_ABD == 9] <- "Indeterminate"
srag.estudo$DOR_ABD_ENG[is.na(srag.estudo$DOR_ABD)] <- "Indeterminate"

table( srag.estudo$DOR_ABD_ENG , useNA = "always" )

# fatigue

table( srag.estudo$FADIGA, useNA = "always" )

srag.estudo$FADIGA_ENG <- character( nrow( srag.estudo ) )

srag.estudo$FADIGA_ENG[srag.estudo$FADIGA == 1] <- "Yes"
srag.estudo$FADIGA_ENG[srag.estudo$FADIGA == 2] <- "No"
srag.estudo$FADIGA_ENG[srag.estudo$FADIGA == 9] <- "Indeterminate"
srag.estudo$FADIGA_ENG[is.na(srag.estudo$FADIGA)] <- "Indeterminate"
srag.estudo$FADIGA_ENG[srag.estudo$FADIGA_ENG == ''] <- "Indeterminate"

table( srag.estudo$FADIGA_ENG  , useNA = "always" )

# --- numero condicoes

cond <- c("FEBRE_ENG", "TOSSE_ENG",  "GARGANTA_ENG", "DISPNEIA_ENG", "DESC_RESP_ENG", "SATURACAO_ENG",  "DIARREIA_ENG",    "VOMITO_ENG",   "DOR_ABD_ENG", "FADIGA_ENG")

srag.estudo.cond <- srag.estudo[,cond]

Num.cond <- apply(srag.estudo.cond, 1, function( x ){
  
  sum( x == "Yes" )
  
})

barplot(table(Num.cond))


srag.estudo$Num.cond <- Num.cond

Num.cond.tb <- table( srag.estudo$SG_UF_NOT, srag.estudo$Num.cond  )

Num.cond.tb.BR <- colSums( Num.cond.tb )

Num.cond.tb2 <- rbind( BR = Num.cond.tb.BR, Num.cond.tb )

Num.cond.tb2.linha <- rowSums( Num.cond.tb2 )


Num.cond.tb.p <- (round( (Num.cond.tb2) / Num.cond.tb2.linha * 100, 2 ))

rownames(Num.cond.tb.p) <- c( "Brazil", UFcods$UF.nome )
rownames(Num.cond.tb2) <- c( "Brazil", UFcods$UF.nome )

setwd(link.saida)

saveRDS(Num.cond.tb.p, "numero.condicoes.prop.RDS")

write.csv2(Num.cond.tb.p, "numero.condicoes.prop.csv", row.names = FALSE )

saveRDS(Num.cond.tb2, "numero.condicoes.RDS")

write.csv2(Num.cond.tb2, "numero.condicoes.csv", row.names = FALSE )

setwd(link.princ)





# OUTRO_SIN

table( srag.estudo$OUTRO_SIN, useNA = "always" )



# OUTRO_SIN DESCRICAO


#table( srag.estudo$OUTRO_DES, useNA = "always" )


#OUTRO_DES <- srag.estudo$OUTRO_DES


#OUTRO_DES.l <- lapply( OUTRO_DES, function( x ){
  
#x <- gsub( pattern = "/",replacement = ",", x )
#x <- gsub( pattern = "-",replacement = ",", x )
#x <- gsub( pattern = " E ",replacement = ",", x )
#x <- gsub( pattern = "[+]",replacement = ",", x )
#x <- gsub( pattern = ";",replacement = ",", x )
#sai <- str_trim( strsplit( x ,split = ",")[[1]] )

#})



# --------------------------------------------------------- tables

# Fever

tb.FEBRE_ENG <- table(srag.estudo$FEBRE_ENG,useNA = "always")

tabela.resumo.FEBRE_ENG <- data.frame( Unidade = character( 28 ),
                                     nr = numeric(28),
                                     yes = numeric( 28 ),
                                     no = numeric( 28 ),
                                     indet = numeric( 28 ))

tabela.resumo.FEBRE_ENG$Unidade <- c("Brazil", UFcods$UF.nome )

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$FEBRE_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  yes <- tb[ "Yes" ]
  no <- tb[ "No" ]
  indet <- tb[ "Indeterminate"]
  
  sai.df <- data.frame( UF = x, nr = nr, yes = yes, no = no, indet = indet )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.FEBRE_ENG$nr[  tabela.resumo.FEBRE_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.FEBRE_ENG$yes[  tabela.resumo.FEBRE_ENG$Unidade == "Brazil"] <- tb.FEBRE_ENG["Yes"]
tabela.resumo.FEBRE_ENG$no[  tabela.resumo.FEBRE_ENG$Unidade == "Brazil"] <- tb.FEBRE_ENG["No"]
tabela.resumo.FEBRE_ENG$indet[  tabela.resumo.FEBRE_ENG$Unidade == "Brazil"] <- tb.FEBRE_ENG["Indeterminate"]

tabela.resumo.FEBRE_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.FEBRE_ENG$yes[2:28] <-  data.ini.fin.df$yes
tabela.resumo.FEBRE_ENG$no[2:28] <- data.ini.fin.df$no
tabela.resumo.FEBRE_ENG$indet[2:28] <-  data.ini.fin.df$indet

names(tabela.resumo.FEBRE_ENG) <- c("Unit","Total","Yes",'No','Indeterminate')

tabela.resumo.FEBRE_ENG$Yes.p <-  round( tabela.resumo.FEBRE_ENG$Yes / tabela.resumo.FEBRE_ENG$Total * 100 , 2 )
tabela.resumo.FEBRE_ENG$No.p <-  round( tabela.resumo.FEBRE_ENG$No / tabela.resumo.FEBRE_ENG$Total * 100 , 2 )
tabela.resumo.FEBRE_ENG$Indeterminate.p <-  round( tabela.resumo.FEBRE_ENG$Indeterminate / tabela.resumo.FEBRE_ENG$Total * 100 , 2 )

names(tabela.resumo.FEBRE_ENG) <- c("Unit","Total","Yes","No","Indeterminate","Yes (%)","No (%)","Indeterminate (%)")

#tabela.resumo.FEBRE_ENG$Total <- format( tabela.resumo.FEBRE_ENG$Total, big.mark = "," )
#tabela.resumo.FEBRE_ENG$Yes <- format( tabela.resumo.FEBRE_ENG$Yes, big.mark = "," )
#tabela.resumo.FEBRE_ENG$No <- format( tabela.resumo.FEBRE_ENG$No, big.mark = "," )
#tabela.resumo.FEBRE_ENG$Indeterminate <- format( tabela.resumo.FEBRE_ENG$Indeterminate, big.mark = "," )

setwd(link.saida)

saveRDS(tabela.resumo.FEBRE_ENG, "tabela.resumo.FEBRE_ENG.RDS")

write.csv2(tabela.resumo.FEBRE_ENG, "tabela.resumo.FEBRE_ENG.csv", row.names = FALSE )

setwd(link.princ)



# Cough

tb.TOSSE_ENG <- table(srag.estudo$TOSSE_ENG,useNA = "always")

tabela.resumo.TOSSE_ENG <- data.frame( Unidade = character( 28 ),
                                       nr = numeric(28),
                                       yes = numeric( 28 ),
                                       no = numeric( 28 ),
                                       indet = numeric( 28 ))

tabela.resumo.TOSSE_ENG$Unidade <- c("Brazil", UFcods$UF.nome )

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$TOSSE_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  yes <- tb[ "Yes" ]
  no <- tb[ "No" ]
  indet <- tb[ "Indeterminate"]
  
  sai.df <- data.frame( UF = x, nr = nr, yes = yes, no = no, indet = indet )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.TOSSE_ENG$nr[  tabela.resumo.TOSSE_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.TOSSE_ENG$yes[  tabela.resumo.TOSSE_ENG$Unidade == "Brazil"] <- tb.TOSSE_ENG["Yes"]
tabela.resumo.TOSSE_ENG$no[  tabela.resumo.TOSSE_ENG$Unidade == "Brazil"] <- tb.TOSSE_ENG["No"]
tabela.resumo.TOSSE_ENG$indet[  tabela.resumo.TOSSE_ENG$Unidade == "Brazil"] <- tb.TOSSE_ENG["Indeterminate"]

tabela.resumo.TOSSE_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.TOSSE_ENG$yes[2:28] <-  data.ini.fin.df$yes
tabela.resumo.TOSSE_ENG$no[2:28] <- data.ini.fin.df$no
tabela.resumo.TOSSE_ENG$indet[2:28] <-  data.ini.fin.df$indet

names(tabela.resumo.TOSSE_ENG) <- c("Unit","Total","Yes",'No','Indeterminate')

tabela.resumo.TOSSE_ENG$Yes.p <-  round( tabela.resumo.TOSSE_ENG$Yes / tabela.resumo.TOSSE_ENG$Total * 100 , 2 )
tabela.resumo.TOSSE_ENG$No.p <-  round( tabela.resumo.TOSSE_ENG$No / tabela.resumo.TOSSE_ENG$Total * 100 , 2 )
tabela.resumo.TOSSE_ENG$Indeterminate.p <- round( tabela.resumo.TOSSE_ENG$Indeterminate / tabela.resumo.TOSSE_ENG$Total * 100 , 2 )

names(tabela.resumo.TOSSE_ENG) <- c("Unit","Total","Yes","No","Indeterminate","Yes (%)","No (%)","Indeterminate (%)")

#tabela.resumo.TOSSE_ENG$Total <- format( tabela.resumo.TOSSE_ENG$Total, big.mark = "," )
#tabela.resumo.TOSSE_ENG$Yes <- format( tabela.resumo.TOSSE_ENG$Yes, big.mark = "," )
#tabela.resumo.TOSSE_ENG$No <- format( tabela.resumo.TOSSE_ENG$No, big.mark = "," )
#tabela.resumo.TOSSE_ENG$Indeterminate <- format( tabela.resumo.TOSSE_ENG$Indeterminate, big.mark = "," )

setwd(link.saida)

saveRDS(tabela.resumo.TOSSE_ENG, "tabela.resumo.TOSSE_ENG.RDS")

write.csv2(tabela.resumo.TOSSE_ENG, "tabela.resumo.TOSSE_ENG.csv", row.names = FALSE )

setwd(link.princ)



# throat

tb.GARGANTA_ENG <- table(srag.estudo$GARGANTA_ENG,useNA = "always")

tabela.resumo.GARGANTA_ENG <- data.frame( Unidade = character( 28 ),
                                       nr = numeric(28),
                                       yes = numeric( 28 ),
                                       no = numeric( 28 ),
                                       indet = numeric( 28 ))

tabela.resumo.GARGANTA_ENG$Unidade <- c("Brazil", UFcods$UF.nome )

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$GARGANTA_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  yes <- tb[ "Yes" ]
  no <- tb[ "No" ]
  indet <- tb[ "Indeterminate"]
  
  sai.df <- data.frame( UF = x, nr = nr, yes = yes, no = no, indet = indet )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.GARGANTA_ENG$nr[  tabela.resumo.GARGANTA_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.GARGANTA_ENG$yes[  tabela.resumo.GARGANTA_ENG$Unidade == "Brazil"] <- tb.GARGANTA_ENG["Yes"]
tabela.resumo.GARGANTA_ENG$no[  tabela.resumo.GARGANTA_ENG$Unidade == "Brazil"] <- tb.GARGANTA_ENG["No"]
tabela.resumo.GARGANTA_ENG$indet[  tabela.resumo.GARGANTA_ENG$Unidade == "Brazil"] <- tb.GARGANTA_ENG["Indeterminate"]

tabela.resumo.GARGANTA_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.GARGANTA_ENG$yes[2:28] <-  data.ini.fin.df$yes
tabela.resumo.GARGANTA_ENG$no[2:28] <- data.ini.fin.df$no
tabela.resumo.GARGANTA_ENG$indet[2:28] <-  data.ini.fin.df$indet

names(tabela.resumo.GARGANTA_ENG) <- c("Unit","Total","Yes",'No','Indeterminate')

tabela.resumo.GARGANTA_ENG$Yes.p <-  round( tabela.resumo.GARGANTA_ENG$Yes / tabela.resumo.GARGANTA_ENG$Total * 100 , 2 )
tabela.resumo.GARGANTA_ENG$No.p <-  round( tabela.resumo.GARGANTA_ENG$No / tabela.resumo.GARGANTA_ENG$Total * 100 , 2 )
tabela.resumo.GARGANTA_ENG$Indeterminate.p <-  round( tabela.resumo.GARGANTA_ENG$Indeterminate / tabela.resumo.GARGANTA_ENG$Total * 100 , 2 )

names(tabela.resumo.GARGANTA_ENG) <- c("Unit","Total","Yes","No","Indeterminate","Yes (%)","No (%)","Indeterminate (%)")

#tabela.resumo.GARGANTA_ENG$Total <- format( tabela.resumo.GARGANTA_ENG$Total, big.mark = "," )
#tabela.resumo.GARGANTA_ENG$Yes <- format( tabela.resumo.GARGANTA_ENG$Yes, big.mark = "," )
#tabela.resumo.GARGANTA_ENG$No <- format( tabela.resumo.GARGANTA_ENG$No, big.mark = "," )
#tabela.resumo.GARGANTA_ENG$Indeterminate <- format( tabela.resumo.GARGANTA_ENG$Indeterminate, big.mark = "," )

setwd(link.saida)

saveRDS(tabela.resumo.GARGANTA_ENG, "tabela.resumo.GARGANTA_ENG.RDS")

write.csv2(tabela.resumo.GARGANTA_ENG, "tabela.resumo.GARGANTA_ENG.csv", row.names = FALSE )

setwd(link.princ)



# dyspnea

tb.DISPNEIA_ENG <- table(srag.estudo$DISPNEIA_ENG,useNA = "always")

tabela.resumo.DISPNEIA_ENG <- data.frame( Unidade = character( 28 ),
                                          nr = numeric(28),
                                          yes = numeric( 28 ),
                                          no = numeric( 28 ),
                                          indet = numeric( 28 ))

tabela.resumo.DISPNEIA_ENG$Unidade <- c("Brazil", UFcods$UF.nome )

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$DISPNEIA_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  yes <- tb[ "Yes" ]
  no <- tb[ "No" ]
  indet <- tb[ "Indeterminate"]
  
  sai.df <- data.frame( UF = x, nr = nr, yes = yes, no = no, indet = indet )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.DISPNEIA_ENG$nr[  tabela.resumo.DISPNEIA_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.DISPNEIA_ENG$yes[  tabela.resumo.DISPNEIA_ENG$Unidade == "Brazil"] <- tb.DISPNEIA_ENG["Yes"]
tabela.resumo.DISPNEIA_ENG$no[  tabela.resumo.DISPNEIA_ENG$Unidade == "Brazil"] <- tb.DISPNEIA_ENG["No"]
tabela.resumo.DISPNEIA_ENG$indet[  tabela.resumo.DISPNEIA_ENG$Unidade == "Brazil"] <- tb.DISPNEIA_ENG["Indeterminate"]

tabela.resumo.DISPNEIA_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.DISPNEIA_ENG$yes[2:28] <-  data.ini.fin.df$yes
tabela.resumo.DISPNEIA_ENG$no[2:28] <- data.ini.fin.df$no
tabela.resumo.DISPNEIA_ENG$indet[2:28] <-  data.ini.fin.df$indet

names(tabela.resumo.DISPNEIA_ENG) <- c("Unit","Total","Yes",'No','Indeterminate')


tabela.resumo.DISPNEIA_ENG$Yes.p <-  round( tabela.resumo.DISPNEIA_ENG$Yes / tabela.resumo.DISPNEIA_ENG$Total * 100 , 2 )
tabela.resumo.DISPNEIA_ENG$No.p <-  round( tabela.resumo.DISPNEIA_ENG$No / tabela.resumo.DISPNEIA_ENG$Total * 100 , 2 )
tabela.resumo.DISPNEIA_ENG$Indeterminate.p <-  round( tabela.resumo.DISPNEIA_ENG$Indeterminate / tabela.resumo.DISPNEIA_ENG$Total * 100 , 2 )

names(tabela.resumo.DISPNEIA_ENG) <- c("Unit","Total","Yes","No","Indeterminate","Yes (%)","No (%)","Indeterminate (%)")

#tabela.resumo.DISPNEIA_ENG$Total <- format( tabela.resumo.DISPNEIA_ENG$Total, big.mark = "," )
#tabela.resumo.DISPNEIA_ENG$Yes <- format( tabela.resumo.DISPNEIA_ENG$Yes, big.mark = "," )
#tabela.resumo.DISPNEIA_ENG$No <- format( tabela.resumo.DISPNEIA_ENG$No, big.mark = "," )
#tabela.resumo.DISPNEIA_ENG$Indeterminate <- format( tabela.resumo.DISPNEIA_ENG$Indeterminate, big.mark = "," )


setwd(link.saida)

saveRDS(tabela.resumo.DISPNEIA_ENG, "tabela.resumo.DISPNEIA_ENG.RDS")

write.csv2(tabela.resumo.DISPNEIA_ENG, "tabela.resumo.DISPNEIA_ENG.csv", row.names = FALSE )

setwd(link.princ)





# Respiratory Discomfort

tb.DESC_RESP_ENG <- table(srag.estudo$DESC_RESP_ENG,useNA = "always")

tabela.resumo.DESC_RESP_ENG <- data.frame( Unidade = character( 28 ),
                                          nr = numeric(28),
                                          yes = numeric( 28 ),
                                          no = numeric( 28 ),
                                          indet = numeric( 28 ))

tabela.resumo.DESC_RESP_ENG$Unidade <- c("Brazil", UFcods$UF.nome )

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$DESC_RESP_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  yes <- tb[ "Yes" ]
  no <- tb[ "No" ]
  indet <- tb[ "Indeterminate"]
  
  sai.df <- data.frame( UF = x, nr = nr, yes = yes, no = no, indet = indet )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.DESC_RESP_ENG$nr[  tabela.resumo.DESC_RESP_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.DESC_RESP_ENG$yes[  tabela.resumo.DESC_RESP_ENG$Unidade == "Brazil"] <- tb.DESC_RESP_ENG["Yes"]
tabela.resumo.DESC_RESP_ENG$no[  tabela.resumo.DESC_RESP_ENG$Unidade == "Brazil"] <- tb.DESC_RESP_ENG["No"]
tabela.resumo.DESC_RESP_ENG$indet[  tabela.resumo.DESC_RESP_ENG$Unidade == "Brazil"] <- tb.DESC_RESP_ENG["Indeterminate"]

tabela.resumo.DESC_RESP_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.DESC_RESP_ENG$yes[2:28] <-  data.ini.fin.df$yes
tabela.resumo.DESC_RESP_ENG$no[2:28] <- data.ini.fin.df$no
tabela.resumo.DESC_RESP_ENG$indet[2:28] <-  data.ini.fin.df$indet

names(tabela.resumo.DESC_RESP_ENG) <- c("Unit","Total","Yes",'No','Indeterminate')


tabela.resumo.DESC_RESP_ENG$Yes.p <- round( tabela.resumo.DESC_RESP_ENG$Yes / tabela.resumo.DESC_RESP_ENG$Total * 100 , 2 )
tabela.resumo.DESC_RESP_ENG$No.p <- round( tabela.resumo.DESC_RESP_ENG$No / tabela.resumo.DESC_RESP_ENG$Total * 100 , 2 )
tabela.resumo.DESC_RESP_ENG$Indeterminate.p <- round( tabela.resumo.DESC_RESP_ENG$Indeterminate / tabela.resumo.DESC_RESP_ENG$Total * 100 , 2 )

names(tabela.resumo.DESC_RESP_ENG) <- c("Unit","Total","Yes","No","Indeterminate","Yes (%)","No (%)","Indeterminate (%)")

#tabela.resumo.DESC_RESP_ENG$Total <- format( tabela.resumo.DESC_RESP_ENG$Total, big.mark = "," )
#tabela.resumo.DESC_RESP_ENG$Yes <- format( tabela.resumo.DESC_RESP_ENG$Yes, big.mark = "," )
#tabela.resumo.DESC_RESP_ENG$No <- format( tabela.resumo.DESC_RESP_ENG$No, big.mark = "," )
#tabela.resumo.DESC_RESP_ENG$Indeterminate <- format( tabela.resumo.DESC_RESP_ENG$Indeterminate, big.mark = "," )

setwd(link.saida)

saveRDS(tabela.resumo.DESC_RESP_ENG, "tabela.resumo.DESC_RESP_ENG.RDS")

write.csv2(tabela.resumo.DESC_RESP_ENG, "tabela.resumo.DESC_RESP_ENG.csv", row.names = FALSE )

setwd(link.princ)



# saturation


tb.SATURACAO_ENG <- table(srag.estudo$SATURACAO_ENG,useNA = "always")

tabela.resumo.SATURACAO_ENG <- data.frame( Unidade = character( 28 ),
                                           nr = numeric(28),
                                           yes = numeric( 28 ),
                                           no = numeric( 28 ),
                                           indet = numeric( 28 ))

tabela.resumo.SATURACAO_ENG$Unidade <- c("Brazil", UFcods$UF.nome )

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$SATURACAO_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  yes <- tb[ "Yes" ]
  no <- tb[ "No" ]
  indet <- tb[ "Indeterminate"]
  
  sai.df <- data.frame( UF = x, nr = nr, yes = yes, no = no, indet = indet )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.SATURACAO_ENG$nr[  tabela.resumo.SATURACAO_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.SATURACAO_ENG$yes[  tabela.resumo.SATURACAO_ENG$Unidade == "Brazil"] <- tb.SATURACAO_ENG["Yes"]
tabela.resumo.SATURACAO_ENG$no[  tabela.resumo.SATURACAO_ENG$Unidade == "Brazil"] <- tb.SATURACAO_ENG["No"]
tabela.resumo.SATURACAO_ENG$indet[  tabela.resumo.SATURACAO_ENG$Unidade == "Brazil"] <- tb.SATURACAO_ENG["Indeterminate"]

tabela.resumo.SATURACAO_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.SATURACAO_ENG$yes[2:28] <-  data.ini.fin.df$yes
tabela.resumo.SATURACAO_ENG$no[2:28] <- data.ini.fin.df$no
tabela.resumo.SATURACAO_ENG$indet[2:28] <-  data.ini.fin.df$indet

names(tabela.resumo.SATURACAO_ENG) <- c("Unit","Total","Yes",'No','Indeterminate')


tabela.resumo.SATURACAO_ENG$Yes.p <-  round( tabela.resumo.SATURACAO_ENG$Yes / tabela.resumo.SATURACAO_ENG$Total * 100 , 2 )
tabela.resumo.SATURACAO_ENG$No.p <-  round( tabela.resumo.SATURACAO_ENG$No / tabela.resumo.SATURACAO_ENG$Total * 100 , 2 )
tabela.resumo.SATURACAO_ENG$Indeterminate.p <-  round( tabela.resumo.SATURACAO_ENG$Indeterminate / tabela.resumo.SATURACAO_ENG$Total * 100 , 2 )

names(tabela.resumo.SATURACAO_ENG) <- c("Unit","Total","Yes","No","Indeterminate","Yes (%)","No (%)","Indeterminate (%)")

#tabela.resumo.SATURACAO_ENG$Total <- format( tabela.resumo.SATURACAO_ENG$Total, big.mark = "," )
#tabela.resumo.SATURACAO_ENG$Yes <- format( tabela.resumo.SATURACAO_ENG$Yes, big.mark = "," )
#tabela.resumo.SATURACAO_ENG$No <- format( tabela.resumo.SATURACAO_ENG$No, big.mark = "," )
#tabela.resumo.SATURACAO_ENG$Indeterminate <- format( tabela.resumo.SATURACAO_ENG$Indeterminate, big.mark = "," )



setwd(link.saida)

saveRDS(tabela.resumo.SATURACAO_ENG, "tabela.resumo.SATURACAO_ENG.RDS")

write.csv2(tabela.resumo.SATURACAO_ENG, "tabela.resumo.SATURACAO_ENG.csv", row.names = FALSE )

setwd(link.princ)


# diarrheia



tb.DIARREIA_ENG <- table( srag.estudo$DIARREIA_ENG,useNA = "always")

tabela.resumo.DIARREIA_ENG <- data.frame( Unidade = character( 28 ),
                                           nr = numeric(28),
                                           yes = numeric( 28 ),
                                           no = numeric( 28 ),
                                           indet = numeric( 28 ))

tabela.resumo.DIARREIA_ENG$Unidade <- c("Brazil", UFcods$UF.nome )

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$DIARREIA_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  yes <- tb[ "Yes" ]
  no <- tb[ "No" ]
  indet <- tb[ "Indeterminate"]
  
  sai.df <- data.frame( UF = x, nr = nr, yes = yes, no = no, indet = indet )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.DIARREIA_ENG$nr[  tabela.resumo.DIARREIA_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.DIARREIA_ENG$yes[  tabela.resumo.DIARREIA_ENG$Unidade == "Brazil"] <- tb.DIARREIA_ENG["Yes"]
tabela.resumo.DIARREIA_ENG$no[  tabela.resumo.DIARREIA_ENG$Unidade == "Brazil"] <- tb.DIARREIA_ENG["No"]
tabela.resumo.DIARREIA_ENG$indet[  tabela.resumo.DIARREIA_ENG$Unidade == "Brazil"] <- tb.DIARREIA_ENG["Indeterminate"]

tabela.resumo.DIARREIA_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.DIARREIA_ENG$yes[2:28] <-  data.ini.fin.df$yes
tabela.resumo.DIARREIA_ENG$no[2:28] <- data.ini.fin.df$no
tabela.resumo.DIARREIA_ENG$indet[2:28] <-  data.ini.fin.df$indet

names(tabela.resumo.DIARREIA_ENG) <- c("Unit","Total","Yes",'No','Indeterminate')

tabela.resumo.DIARREIA_ENG$Yes.p <-  round( tabela.resumo.DIARREIA_ENG$Yes / tabela.resumo.DIARREIA_ENG$Total * 100 , 2 )
tabela.resumo.DIARREIA_ENG$No.p <-  round( tabela.resumo.DIARREIA_ENG$No / tabela.resumo.DIARREIA_ENG$Total * 100 , 2 )
tabela.resumo.DIARREIA_ENG$Indeterminate.p <-  round( tabela.resumo.DIARREIA_ENG$Indeterminate / tabela.resumo.DIARREIA_ENG$Total * 100 , 2 )

names(tabela.resumo.DIARREIA_ENG) <- c("Unit","Total","Yes","No","Indeterminate","Yes (%)","No (%)","Indeterminate (%)")

#tabela.resumo.DIARREIA_ENG$Total <- format( tabela.resumo.DIARREIA_ENG$Total, big.mark = "," )
#tabela.resumo.DIARREIA_ENG$Yes <- format( tabela.resumo.DIARREIA_ENG$Yes, big.mark = "," )
#tabela.resumo.DIARREIA_ENG$No <- format( tabela.resumo.DIARREIA_ENG$No, big.mark = "," )
#tabela.resumo.DIARREIA_ENG$Indeterminate <- format( tabela.resumo.DIARREIA_ENG$Indeterminate, big.mark = "," )

setwd(link.saida)

saveRDS(tabela.resumo.DIARREIA_ENG, "tabela.resumo.DIARREIA_ENG.RDS")

write.csv2(tabela.resumo.DIARREIA_ENG, "tabela.resumo.DIARREIA_ENG.csv", row.names = FALSE )

setwd(link.princ)


# vomit


tb.VOMITO_ENG <- table( srag.estudo$VOMITO_ENG,useNA = "always")

tabela.resumo.VOMITO_ENG <- data.frame( Unidade = character( 28 ),
                                          nr = numeric(28),
                                          yes = numeric( 28 ),
                                          no = numeric( 28 ),
                                          indet = numeric( 28 ))

tabela.resumo.VOMITO_ENG$Unidade <- c("Brazil", UFcods$UF.nome )

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$VOMITO_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  yes <- tb[ "Yes" ]
  no <- tb[ "No" ]
  indet <- tb[ "Indeterminate"]
  
  sai.df <- data.frame( UF = x, nr = nr, yes = yes, no = no, indet = indet )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.VOMITO_ENG$nr[  tabela.resumo.VOMITO_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.VOMITO_ENG$yes[  tabela.resumo.VOMITO_ENG$Unidade == "Brazil"] <- tb.VOMITO_ENG["Yes"]
tabela.resumo.VOMITO_ENG$no[  tabela.resumo.VOMITO_ENG$Unidade == "Brazil"] <- tb.VOMITO_ENG["No"]
tabela.resumo.VOMITO_ENG$indet[  tabela.resumo.VOMITO_ENG$Unidade == "Brazil"] <- tb.VOMITO_ENG["Indeterminate"]

tabela.resumo.VOMITO_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.VOMITO_ENG$yes[2:28] <-  data.ini.fin.df$yes
tabela.resumo.VOMITO_ENG$no[2:28] <- data.ini.fin.df$no
tabela.resumo.VOMITO_ENG$indet[2:28] <-  data.ini.fin.df$indet

names(tabela.resumo.VOMITO_ENG) <- c("Unit","Total","Yes",'No','Indeterminate')

tabela.resumo.VOMITO_ENG$Yes.p <-  round( tabela.resumo.VOMITO_ENG$Yes / tabela.resumo.VOMITO_ENG$Total * 100 , 2 )
tabela.resumo.VOMITO_ENG$No.p <-  round( tabela.resumo.VOMITO_ENG$No / tabela.resumo.VOMITO_ENG$Total * 100 , 2 )
tabela.resumo.VOMITO_ENG$Indeterminate.p <-  round( tabela.resumo.VOMITO_ENG$Indeterminate / tabela.resumo.VOMITO_ENG$Total * 100 , 2 )

names(tabela.resumo.VOMITO_ENG) <- c("Unit","Total","Yes","No","Indeterminate","Yes (%)","No (%)","Indeterminate (%)")

#tabela.resumo.VOMITO_ENG$Total <- format( tabela.resumo.VOMITO_ENG$Total, big.mark = "," )
#tabela.resumo.VOMITO_ENG$Yes <- format( tabela.resumo.VOMITO_ENG$Yes, big.mark = "," )
#tabela.resumo.VOMITO_ENG$No <- format( tabela.resumo.VOMITO_ENG$No, big.mark = "," )
#tabela.resumo.VOMITO_ENG$Indeterminate <- format( tabela.resumo.VOMITO_ENG$Indeterminate, big.mark = "," )

setwd(link.saida)

saveRDS(tabela.resumo.VOMITO_ENG, "tabela.resumo.VOMITO_ENG.RDS")

write.csv2(tabela.resumo.VOMITO_ENG, "tabela.resumo.VOMITO_ENG.csv", row.names = FALSE )

setwd(link.princ)



# abdominal pain


tb.DOR_ABD_ENG <- table( srag.estudo$DOR_ABD_ENG,useNA = "always")

tabela.resumo.DOR_ABD_ENG <- data.frame( Unidade = character( 28 ),
                                        nr = numeric(28),
                                        yes = numeric( 28 ),
                                        no = numeric( 28 ),
                                        indet = numeric( 28 ))

tabela.resumo.DOR_ABD_ENG$Unidade <- c("Brazil", UFcods$UF.nome )

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$DOR_ABD_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  yes <- tb[ "Yes" ]
  no <- tb[ "No" ]
  indet <- tb[ "Indeterminate"]
  
  sai.df <- data.frame( UF = x, nr = nr, yes = yes, no = no, indet = indet )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.DOR_ABD_ENG$nr[  tabela.resumo.DOR_ABD_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.DOR_ABD_ENG$yes[  tabela.resumo.DOR_ABD_ENG$Unidade == "Brazil"] <- tb.DOR_ABD_ENG["Yes"]
tabela.resumo.DOR_ABD_ENG$no[  tabela.resumo.DOR_ABD_ENG$Unidade == "Brazil"] <- tb.DOR_ABD_ENG["No"]
tabela.resumo.DOR_ABD_ENG$indet[  tabela.resumo.DOR_ABD_ENG$Unidade == "Brazil"] <- tb.DOR_ABD_ENG["Indeterminate"]

tabela.resumo.DOR_ABD_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.DOR_ABD_ENG$yes[2:28] <-  data.ini.fin.df$yes
tabela.resumo.DOR_ABD_ENG$no[2:28] <- data.ini.fin.df$no
tabela.resumo.DOR_ABD_ENG$indet[2:28] <-  data.ini.fin.df$indet

names(tabela.resumo.DOR_ABD_ENG) <- c("Unit","Total","Yes",'No','Indeterminate')

tabela.resumo.DOR_ABD_ENG$Yes.p <-  round( tabela.resumo.DOR_ABD_ENG$Yes / tabela.resumo.DOR_ABD_ENG$Total * 100 , 2 )
tabela.resumo.DOR_ABD_ENG$No.p <-  round( tabela.resumo.DOR_ABD_ENG$No / tabela.resumo.DOR_ABD_ENG$Total * 100 , 2 )
tabela.resumo.DOR_ABD_ENG$Indeterminate.p <-  round( tabela.resumo.DOR_ABD_ENG$Indeterminate / tabela.resumo.DOR_ABD_ENG$Total * 100 , 2 )

names(tabela.resumo.DOR_ABD_ENG) <- c("Unit","Total","Yes","No","Indeterminate","Yes (%)","No (%)","Indeterminate (%)")

#tabela.resumo.DOR_ABD_ENG$Total <- format( tabela.resumo.DOR_ABD_ENG$Total, big.mark = "," )
#tabela.resumo.DOR_ABD_ENG$Yes <- format( tabela.resumo.DOR_ABD_ENG$Yes, big.mark = "," )
#tabela.resumo.DOR_ABD_ENG$No <- format( tabela.resumo.DOR_ABD_ENG$No, big.mark = "," )
#tabela.resumo.DOR_ABD_ENG$Indeterminate <- format( tabela.resumo.DOR_ABD_ENG$Indeterminate, big.mark = "," )

setwd(link.saida)

saveRDS(tabela.resumo.DOR_ABD_ENG, "tabela.resumo.DOR_ABD_ENG.RDS")

write.csv2(tabela.resumo.DOR_ABD_ENG, "tabela.resumo.DOR_ABD_ENG.csv", row.names = FALSE )

setwd(link.princ)


# fatigue


tb.FADIGA_ENG <- table( srag.estudo$FADIGA_ENG,useNA = "always")

tabela.resumo.FADIGA_ENG <- data.frame( Unidade = character( 28 ),
                                         nr = numeric(28),
                                         yes = numeric( 28 ),
                                         no = numeric( 28 ),
                                         indet = numeric( 28 ))

tabela.resumo.FADIGA_ENG$Unidade <- c("Brazil", UFcods$UF.nome )

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$FADIGA_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  yes <- tb[ "Yes" ]
  no <- tb[ "No" ]
  indet <- tb[ "Indeterminate"]
  
  sai.df <- data.frame( UF = x, nr = nr, yes = yes, no = no, indet = indet )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.FADIGA_ENG$nr[  tabela.resumo.FADIGA_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.FADIGA_ENG$yes[  tabela.resumo.FADIGA_ENG$Unidade == "Brazil"] <- tb.FADIGA_ENG["Yes"]
tabela.resumo.FADIGA_ENG$no[  tabela.resumo.FADIGA_ENG$Unidade == "Brazil"] <- tb.FADIGA_ENG["No"]
tabela.resumo.FADIGA_ENG$indet[  tabela.resumo.FADIGA_ENG$Unidade == "Brazil"] <- tb.FADIGA_ENG["Indeterminate"]

tabela.resumo.FADIGA_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.FADIGA_ENG$yes[2:28] <-  data.ini.fin.df$yes
tabela.resumo.FADIGA_ENG$no[2:28] <- data.ini.fin.df$no
tabela.resumo.FADIGA_ENG$indet[2:28] <-  data.ini.fin.df$indet

names(tabela.resumo.FADIGA_ENG) <- c("Unit","Total","Yes",'No','Indeterminate')

tabela.resumo.FADIGA_ENG$Yes.p <-  round( tabela.resumo.FADIGA_ENG$Yes / tabela.resumo.FADIGA_ENG$Total * 100 , 2 )
tabela.resumo.FADIGA_ENG$No.p <-  round( tabela.resumo.FADIGA_ENG$No / tabela.resumo.FADIGA_ENG$Total * 100 , 2 )
tabela.resumo.FADIGA_ENG$Indeterminate.p <- round( tabela.resumo.FADIGA_ENG$Indeterminate / tabela.resumo.FADIGA_ENG$Total * 100 , 2 )

names(tabela.resumo.FADIGA_ENG) <- c("Unit","Total","Yes","No","Indeterminate","Yes (%)","No (%)","Indeterminate (%)")

#tabela.resumo.FADIGA_ENG$Total <- format( tabela.resumo.FADIGA_ENG$Total, big.mark = "," )
#tabela.resumo.FADIGA_ENG$Yes <- format( tabela.resumo.FADIGA_ENG$Yes, big.mark = "," )
#tabela.resumo.FADIGA_ENG$No <- format( tabela.resumo.FADIGA_ENG$No, big.mark = "," )
#tabela.resumo.FADIGA_ENG$Indeterminate <- format( tabela.resumo.FADIGA_ENG$Indeterminate, big.mark = "," )

setwd(link.saida)

saveRDS(tabela.resumo.FADIGA_ENG, "tabela.resumo.FADIGA_ENG.RDS")

write.csv2(tabela.resumo.FADIGA_ENG, "tabela.resumo.FADIGA_ENG.csv", row.names = FALSE )

setwd(link.princ)





# ----  DT_SIN_PRI  Data de 1ºs sintomas


tabela.resumo.DT_SIN_PRI <- data.frame( Unidade = character( 28 ),
                                        nr = numeric(28),
                                        data.ini = character( 28 ),
                                        data.fin = character( 28 ),
                                        n.na = numeric( 28 ))

tabela.resumo.DT_SIN_PRI$Unidade <- c("Brazil", UFcods$UF.nome )


data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$DT_SIN_PRI[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  sai.min <- min( sele , na.rm = TRUE  )
  sai.max <- max( sele , na.rm = TRUE  )
  
  nr <- length(sele)
  
  n.na <- sum(is.na( sele ))
  
  sai.df <- data.frame( UF = x , nr = nr ,sai.min = sai.min,   sai.max = sai.max, n.na = n.na )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.DT_SIN_PRI$nr[  tabela.resumo.DT_SIN_PRI$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.DT_SIN_PRI$data.ini[  tabela.resumo.DT_SIN_PRI$Unidade == "Brazil"] <- as.character( min( srag.estudo$DT_SIN_PRI , na.rm = TRUE  ) )
tabela.resumo.DT_SIN_PRI$data.fin[  tabela.resumo.DT_SIN_PRI$Unidade == "Brazil"] <- as.character( max( srag.estudo$DT_SIN_PRI , na.rm = TRUE  ) )
tabela.resumo.DT_SIN_PRI$n.na[  tabela.resumo.DT_SIN_PRI$Unidade == "Brazil"] <- sum( is.na(srag.estudo$DT_SIN_PRI))

tabela.resumo.DT_SIN_PRI$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.DT_SIN_PRI$data.ini[2:28] <-  as.character( data.ini.fin.df$sai.min )
tabela.resumo.DT_SIN_PRI$data.fin[2:28] <-  as.character( data.ini.fin.df$sai.max )
tabela.resumo.DT_SIN_PRI$n.na[2:28] <- data.ini.fin.df$n.na[1:27]

names(tabela.resumo.DT_SIN_PRI) <- c("Unit","Total","Inital date",'Final date','Indeterminate')

tabela.resumo.DT_SIN_PRI$p <- round( tabela.resumo.DT_SIN_PRI$Total / sum( tabela.resumo.DT_SIN_PRI$Total[2:28] ) * 100, 2 )

names(tabela.resumo.DT_SIN_PRI)[6] <- "Percentage"

tabela.resumo.DT_SIN_PRI <- tabela.resumo.DT_SIN_PRI[c('Unit','Total', 'Percentage', 'Inital date', 'Final date', 'Indeterminate')]

setwd(link.saida)

saveRDS( tabela.resumo.DT_SIN_PRI, "tabela.resumo.DT_SIN_PRI.RDS" )

write.csv2(tabela.resumo.DT_SIN_PRI, "tabela.resumo.DT_SIN_PRI.csv", row.names = FALSE )

setwd(link.princ)


# ---- TABELAS RESUMO: CS_SEXO.ENG  Sexo

tb.CS_SEXO_ENG <- table(srag.estudo$CS_SEXO_ENG,useNA = "always")

srag.estudo$CS_SEXO_ENG <- as.factor( srag.estudo$CS_SEXO_ENG )

tabela.resumo.CS_SEXO_ENG <- data.frame( Unidade = character( 28 ),
                                        nr = numeric(28),
                                        fem = numeric( 28 ),
                                        mas = numeric( 28 ),
                                        indet = numeric( 28 ))

tabela.resumo.CS_SEXO_ENG$Unidade <- c("Brazil",UFcods$UF.nome)

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$CS_SEXO_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  fem <- tb[ "Female" ]
  mas <- tb[ "Male" ]
  indet <- tb[ "Indeterminate"]
  
  sai.df <- data.frame( UF = x, nr = nr, fem = fem, mas = mas, indet = indet )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.CS_SEXO_ENG$nr[  tabela.resumo.CS_SEXO_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.CS_SEXO_ENG$fem[  tabela.resumo.CS_SEXO_ENG$Unidade == "Brazil"] <- tb.CS_SEXO_ENG["Female"]
tabela.resumo.CS_SEXO_ENG$mas[  tabela.resumo.CS_SEXO_ENG$Unidade == "Brazil"] <- tb.CS_SEXO_ENG["Male"]
tabela.resumo.CS_SEXO_ENG$indet[  tabela.resumo.CS_SEXO_ENG$Unidade == "Brazil"] <- tb.CS_SEXO_ENG["Indeterminate"]

tabela.resumo.CS_SEXO_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.CS_SEXO_ENG$fem[2:28] <-  data.ini.fin.df$fem
tabela.resumo.CS_SEXO_ENG$mas[2:28] <- data.ini.fin.df$mas
tabela.resumo.CS_SEXO_ENG$indet[2:28] <-  data.ini.fin.df$indet

names(tabela.resumo.CS_SEXO_ENG) <- c("Unit","Total","Female",'Male','Indeterminate')

tabela.resumo.CS_SEXO_ENG$Female.p <- round( tabela.resumo.CS_SEXO_ENG$Female / tabela.resumo.CS_SEXO_ENG$Total * 100, 2 )

tabela.resumo.CS_SEXO_ENG$Male.p <- round( tabela.resumo.CS_SEXO_ENG$Male / tabela.resumo.CS_SEXO_ENG$Total * 100, 2 )

tabela.resumo.CS_SEXO_ENG$Indeterminate.p <- round( tabela.resumo.CS_SEXO_ENG$Indeterminate / tabela.resumo.CS_SEXO_ENG$Total * 100, 2 )

names(tabela.resumo.CS_SEXO_ENG)[6:8] <- c("Female (%)", "Male (%)", "Indeterminate (%)")

setwd(link.saida)

saveRDS(tabela.resumo.CS_SEXO_ENG, "tabela.resumo.CS_SEXO.ENG.RDS")

write.csv2(tabela.resumo.CS_SEXO_ENG, "tabela.resumo.CS_SEXO_ENG.csv", row.names = FALSE )

setwd(link.princ)



# TABELAS RESUMO: EVOLUCAO_ENG  EVOLUCAO

tb.EVOLUCAO_ENG <- table(srag.estudo$EVOLUCAO_ENG)

tabela.resumo.EVOLUCAO_ENG <- data.frame( Unidade = character( 28 ),
                                     nr = numeric(28),
                                     death = numeric( 28 ),
                                     Death_other_causes = numeric( 28 ),
                                     Recovery = numeric( 28 ),
                                     Ignored = numeric( 28 ),
                                     Indeterminate = numeric( 28 ) )

tabela.resumo.EVOLUCAO_ENG$Unidade <- c("Brazil", UFcods$UF.nome)


data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$EVOLUCAO_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  tb <-  table( sele )  
  
  sai.df <- data.frame( UF = x , 
                        nr = length( sele ),
                        Recovery = tb["Recovery"],
                        Death = tb["Death"],
                        Death_other_causes = tb["Death from other causes"], 
                        Ignored = tb["Ignored"],
                        Indeterminate = tb["Indeterminate"] )
  
})

data.ini.fin <- do.call( rbind, data.ini.fin )

tabela.resumo.EVOLUCAO_ENG$nr[  tabela.resumo.EVOLUCAO_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.EVOLUCAO_ENG$death[  tabela.resumo.EVOLUCAO_ENG$Unidade == "Brazil"] <- tb.EVOLUCAO_ENG["Death"]
tabela.resumo.EVOLUCAO_ENG$Death_other_causes[  tabela.resumo.EVOLUCAO_ENG$Unidade == "Brazil"] <- tb.EVOLUCAO_ENG["Death from other causes"]
tabela.resumo.EVOLUCAO_ENG$Ignored[  tabela.resumo.EVOLUCAO_ENG$Unidade == "Brazil"] <- tb.EVOLUCAO_ENG["Ignored"]
tabela.resumo.EVOLUCAO_ENG$Indeterminate[  tabela.resumo.EVOLUCAO_ENG$Unidade == "Brazil"] <- tb.EVOLUCAO_ENG["Indeterminate"]
tabela.resumo.EVOLUCAO_ENG$Recovery[  tabela.resumo.EVOLUCAO_ENG$Unidade == "Brazil"] <- tb.EVOLUCAO_ENG["Recovery"]

tabela.resumo.EVOLUCAO_ENG$nr[2:28] <-  data.ini.fin$nr
tabela.resumo.EVOLUCAO_ENG$death[2:28] <-  data.ini.fin$Death
tabela.resumo.EVOLUCAO_ENG$Death_other_causes[2:28] <-  data.ini.fin$Death_other_causes
tabela.resumo.EVOLUCAO_ENG$Ignored[2:28] <-  data.ini.fin$Ignored
tabela.resumo.EVOLUCAO_ENG$Indeterminate[2:28] <-  data.ini.fin$Indeterminate
tabela.resumo.EVOLUCAO_ENG$Recovery[2:28] <-  data.ini.fin$Recovery

names(tabela.resumo.EVOLUCAO_ENG) <- c("Unit","Total","Recovery","Deaths",'Deaths from other causes','Ignored',"Indeterminate")


setwd(link.saida)

saveRDS(tabela.resumo.EVOLUCAO_ENG, "tabela.resumo.EVOLUCAO_ENG.RDS")

write.csv2(tabela.resumo.EVOLUCAO_ENG, "tabela.resumo.EVOLUCAO_ENG.csv", row.names = FALSE )

setwd(link.princ)

tabela.resumo.EVOLUCAO_ENG.P <- tabela.resumo.EVOLUCAO_ENG

tabela.resumo.EVOLUCAO_ENG.P$Recovery.p <- round( tabela.resumo.EVOLUCAO_ENG.P$Recovery / tabela.resumo.EVOLUCAO_ENG.P$Total * 100, 2)
tabela.resumo.EVOLUCAO_ENG.P$Deaths.p <- round( tabela.resumo.EVOLUCAO_ENG.P$Deaths / tabela.resumo.EVOLUCAO_ENG.P$Total * 100, 2)
tabela.resumo.EVOLUCAO_ENG.P$`Deaths from other causes.p` <- round( tabela.resumo.EVOLUCAO_ENG.P$`Deaths from other causes` / tabela.resumo.EVOLUCAO_ENG.P$Total * 100, 2)
tabela.resumo.EVOLUCAO_ENG.P$Ignored.p <- round( tabela.resumo.EVOLUCAO_ENG.P$Ignored / tabela.resumo.EVOLUCAO_ENG.P$Total * 100, 2)
tabela.resumo.EVOLUCAO_ENG.P$Indeterminate.p <- round( tabela.resumo.EVOLUCAO_ENG.P$Indeterminate / tabela.resumo.EVOLUCAO_ENG.P$Total * 100, 2)

tabela.resumo.EVOLUCAO_ENG.P <- tabela.resumo.EVOLUCAO_ENG.P[,c("Unit","Total","Deaths.p","Deaths from other causes.p", "Ignored.p", "Indeterminate.p")]

names(tabela.resumo.EVOLUCAO_ENG.P)[3:6] <- c("Deaths (%)", "Deaths from other causes (%)", "Ignored (%)", "Indeterminate (%)")

setwd(link.saida)

saveRDS(tabela.resumo.EVOLUCAO_ENG.P, "tabela.resumo.EVOLUCAO_ENG.P.RDS")

write.csv2(tabela.resumo.EVOLUCAO_ENG.P, "tabela.resumo.EVOLUCAO_ENG.P.csv", row.names = FALSE )

setwd(link.princ)





# DT_INTERNA

 
table( srag.estudo$DT_INTERNA, useNA = "always" )

if( is.character( srag.estudo$DT_INTERNA ) ) srag.estudo$DT_INTERNA <- dmy( srag.estudo$DT_INTERNA )

summary(srag.estudo$DT_INTERNA)

limite.periodo <-  interval( dmy("27/12/2019"),dmy("21/12/2021") ) 

DT_INTERNA.in <- srag.estudo$DT_INTERNA  %within%  limite.periodo


srag.estudo$DT_INTERNA[ !DT_INTERNA.in  ] <- NA

tabela.resumo.DT_INTERNA <- data.frame( Unidade = character( 28 ),
                                        nr = numeric(28),
                                        data.ini = character( 28 ),
                                        data.fin = character( 28 ),
                                        n.na = numeric( 28 ))

tabela.resumo.DT_INTERNA$Unidade <- c("Brazil", UFcods$UF.nome )


data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$DT_INTERNA[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  sai.min <- min( sele , na.rm = TRUE)
  sai.max <- max( sele , na.rm = TRUE)
  
  nr <- length(sele)
  
  n.na <- sum(is.na( sele ))
  
  sai.df <- data.frame( UF = x , nr = nr ,sai.min = sai.min,   sai.max = sai.max, n.na = n.na )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.DT_INTERNA$nr[  tabela.resumo.DT_INTERNA$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.DT_INTERNA$data.ini[  tabela.resumo.DT_INTERNA$Unidade == "Brazil"] <- as.character( min( srag.estudo$DT_INTERNA , na.rm = TRUE  ) )
tabela.resumo.DT_INTERNA$data.fin[  tabela.resumo.DT_INTERNA$Unidade == "Brazil"] <- as.character( max( srag.estudo$DT_INTERNA , na.rm = TRUE  ) )
tabela.resumo.DT_INTERNA$n.na[  tabela.resumo.DT_INTERNA$Unidade == "Brazil"] <- sum( is.na(srag.estudo$DT_INTERNA ))

tabela.resumo.DT_INTERNA$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.DT_INTERNA$data.ini[2:28] <-  as.character( data.ini.fin.df$sai.min )
tabela.resumo.DT_INTERNA$data.fin[2:28] <-  as.character( data.ini.fin.df$sai.max )
tabela.resumo.DT_INTERNA$n.na[2:28] <-   data.ini.fin.df$n.na

names(tabela.resumo.DT_INTERNA) <- c("Unit","Total","Inital date",'Final date','Indeterminate')


tabela.resumo.DT_INTERNA$p <- round( tabela.resumo.DT_INTERNA$Indeterminate /  tabela.resumo.DT_INTERNA$Total  * 100, 2 )

names(tabela.resumo.DT_INTERNA)[6] <- "Indeterminate (%)"


setwd(link.saida)

saveRDS(tabela.resumo.DT_INTERNA, "tabela.resumo.DT_INTERNA.RDS")

write.csv2(tabela.resumo.DT_INTERNA, "tabela.resumo.DT_INTERNA.csv", row.names = FALSE )

setwd(link.princ)


# UTI_ENG

tb.UTI_ENG <- table(srag.estudo$UTI_ENG,useNA = "always")

tabela.resumo.UTI_ENG <- data.frame( Unidade = character( 28 ),
                                         nr = numeric(28),
                                         yes = numeric( 28 ),
                                         no = numeric( 28 ),
                                         indet = numeric( 28 ))

tabela.resumo.UTI_ENG$Unidade <- c("Brazil", UFcods$UF.nome )

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$UTI_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  yes <- tb[ "Yes" ]
  no <- tb[ "No" ]
  indet <- tb[ "Indeterminate"]
  
  sai.df <- data.frame( UF = x, nr = nr, yes = yes, no = no, indet = indet )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.UTI_ENG$nr[  tabela.resumo.UTI_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.UTI_ENG$yes[  tabela.resumo.UTI_ENG$Unidade == "Brazil"] <- tb.UTI_ENG["Yes"]
tabela.resumo.UTI_ENG$no[  tabela.resumo.UTI_ENG$Unidade == "Brazil"] <- tb.UTI_ENG["No"]
tabela.resumo.UTI_ENG$indet[  tabela.resumo.UTI_ENG$Unidade == "Brazil"] <- tb.UTI_ENG["Indeterminate"]

tabela.resumo.UTI_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.UTI_ENG$yes[2:28] <-  data.ini.fin.df$yes
tabela.resumo.UTI_ENG$no[2:28] <- data.ini.fin.df$no
tabela.resumo.UTI_ENG$indet[2:28] <-  data.ini.fin.df$indet

names(tabela.resumo.UTI_ENG) <- c("Unit","Total","Yes",'No','Indeterminate')

tabela.resumo.UTI_ENG$Yes.p <- round(  tabela.resumo.UTI_ENG$Yes / tabela.resumo.UTI_ENG$Total * 100, 2 )
tabela.resumo.UTI_ENG$No.p <- round(  tabela.resumo.UTI_ENG$No / tabela.resumo.UTI_ENG$Total * 100, 2 )
tabela.resumo.UTI_ENG$Indeterminate.p <- round(  tabela.resumo.UTI_ENG$Indeterminate / tabela.resumo.UTI_ENG$Total * 100, 2 )

names(tabela.resumo.UTI_ENG)[6:8] <- c("Yes (%)", "No (%)", "Indeterminate (%)")


setwd(link.saida)

saveRDS(tabela.resumo.UTI_ENG, "tabela.resumo.UTI_ENG.RDS")

write.csv2(tabela.resumo.UTI_ENG, "tabela.resumo.UTI_ENG.csv", row.names = FALSE )

setwd(link.princ)

# ZONE CS_ZONA_ENG


tb.CS_ZONA_ENG <- table(srag.estudo$CS_ZONA_ENG,useNA = "always")

tabela.resumo.CS_ZONA_ENG <- data.frame( Unidade = character( 28 ),
                                     nr = numeric(28),
                                     Urban = numeric( 28 ),
                                     Rural = numeric( 28 ),
                                     Peri.urban = numeric( 28 ),
                                     indet = numeric( 28 ))

tabela.resumo.CS_ZONA_ENG$Unidade <- c("Brazil", UFcods$UF.nome )

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$CS_ZONA_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  tb <- table( sele )
  
  Urban <- tb[ "Urban" ]
  Rural <- tb[ "Rural" ]
  Peri.urban <- tb[ "Peri-urban" ]
  indet <- tb[ "Indeterminate"]
  
  sai.df <- data.frame( UF = x, nr = nr, Urban = Urban, Rural = Rural, Peri.urban = Peri.urban, indet = indet )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.CS_ZONA_ENG$nr[  tabela.resumo.CS_ZONA_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.CS_ZONA_ENG$Urban[  tabela.resumo.CS_ZONA_ENG$Unidade == "Brazil"] <- tb.CS_ZONA_ENG["Urban"]
tabela.resumo.CS_ZONA_ENG$Rural[  tabela.resumo.CS_ZONA_ENG$Unidade == "Brazil"] <- tb.CS_ZONA_ENG["Rural"]
tabela.resumo.CS_ZONA_ENG$Peri.urban[  tabela.resumo.CS_ZONA_ENG$Unidade == "Brazil"] <- tb.CS_ZONA_ENG["Peri-urban"]
tabela.resumo.CS_ZONA_ENG$indet[  tabela.resumo.CS_ZONA_ENG$Unidade == "Brazil"] <- tb.CS_ZONA_ENG["Indeterminate"]

tabela.resumo.CS_ZONA_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.CS_ZONA_ENG$Urban[2:28] <-  data.ini.fin.df$Urban
tabela.resumo.CS_ZONA_ENG$Rural[2:28] <- data.ini.fin.df$Rural
tabela.resumo.CS_ZONA_ENG$Peri.urban[2:28] <- data.ini.fin.df$Peri.urban
tabela.resumo.CS_ZONA_ENG$indet[2:28] <-  data.ini.fin.df$indet

names(tabela.resumo.CS_ZONA_ENG) <- c("Unit","Total","Urban",'Rural',"Peri-urban",'Indeterminate')

tabela.resumo.CS_ZONA_ENG$Urban.p <- round( tabela.resumo.CS_ZONA_ENG$Urban / tabela.resumo.CS_ZONA_ENG$Total * 100, 2)
tabela.resumo.CS_ZONA_ENG$Rural.p <- round( tabela.resumo.CS_ZONA_ENG$Rural / tabela.resumo.CS_ZONA_ENG$Total * 100, 2)
tabela.resumo.CS_ZONA_ENG$`Peri-urban.p` <- round( tabela.resumo.CS_ZONA_ENG$`Peri-urban` / tabela.resumo.CS_ZONA_ENG$Total * 100, 2)
tabela.resumo.CS_ZONA_ENG$Indeterminate.p <- round( tabela.resumo.CS_ZONA_ENG$Indeterminate / tabela.resumo.CS_ZONA_ENG$Total * 100, 2)

names(tabela.resumo.CS_ZONA_ENG)[7:10] <- c("Urban (%)", "Rural (%)","Peri-urban (%)", "Indeterminate (%)")

setwd(link.saida)

saveRDS(tabela.resumo.CS_ZONA_ENG, "tabela.resumo.CS_ZONA_ENG.RDS")

write.csv2(tabela.resumo.CS_ZONA_ENG, "tabela.resumo.CS_ZONA_ENG.csv", row.names = FALSE )

setwd(link.princ)


# CLASSI_FIN_ENG

tb.CLASSI_FIN_ENG <- table(srag.estudo$CLASSI_FIN_ENG)

# srag.estudo$tb.CLASSI_FIN_ENG <- as.factor( srag.estudo$tb.CLASSI_FIN_ENG )

tabela.resumo.tb.CLASSI_FIN_ENG <- data.frame( Unidade = character( 28 ),
                                        nr = numeric(28),
                                        'SARS by Covid-19' = numeric( 28 ),
                                        'SARS by influenza' = numeric( 28 ),
                                        'SARS unspecified' = numeric( 28 ),
                                        'SARS by another etiological agent' = numeric( 28 ),
                                        'SARS by another respiratory virus' = numeric( 28 ),
                                        'Indeterminate' = numeric( 28 )
)

tabela.resumo.tb.CLASSI_FIN_ENG$Unidade <- c("Brazil",UFcods$UF.nome)

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$CLASSI_FIN_ENG[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  a <- tb[ 'SARS by Covid-19' ]
  b <- tb[ 'SARS by influenza' ]
  c <- tb[ 'SARS unspecified']
  d <- tb[ 'SARS by another etiological agent' ]
  e <- tb[ 'SARS by another respiratory virus' ]
  f <- tb[ 'Indeterminate']
  
  sai.df <- data.frame( UF = x, nr = nr, a = a, b = b, c = c, d = d, e = e, f = f )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.tb.CLASSI_FIN_ENG$nr[  tabela.resumo.tb.CLASSI_FIN_ENG$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.tb.CLASSI_FIN_ENG$SARS.by.Covid.19[  tabela.resumo.tb.CLASSI_FIN_ENG$Unidade == "Brazil"] <- tb.CLASSI_FIN_ENG["SARS by Covid-19"]
tabela.resumo.tb.CLASSI_FIN_ENG$SARS.by.influenza[  tabela.resumo.tb.CLASSI_FIN_ENG$Unidade == "Brazil"] <- tb.CLASSI_FIN_ENG["SARS by influenza"]
tabela.resumo.tb.CLASSI_FIN_ENG$SARS.unspecified[  tabela.resumo.tb.CLASSI_FIN_ENG$Unidade == "Brazil"] <- tb.CLASSI_FIN_ENG["SARS unspecified"]
tabela.resumo.tb.CLASSI_FIN_ENG$SARS.by.another.etiological.agent[  tabela.resumo.tb.CLASSI_FIN_ENG$Unidade == "Brazil"] <- tb.CLASSI_FIN_ENG["SARS by another etiological agent"]
tabela.resumo.tb.CLASSI_FIN_ENG$SARS.by.another.respiratory.virus[  tabela.resumo.tb.CLASSI_FIN_ENG$Unidade == "Brazil"] <- tb.CLASSI_FIN_ENG["SARS by another respiratory virus"]
tabela.resumo.tb.CLASSI_FIN_ENG$Indeterminate[  tabela.resumo.tb.CLASSI_FIN_ENG$Unidade == "Brazil"] <- tb.CLASSI_FIN_ENG["Indeterminate"]

tabela.resumo.tb.CLASSI_FIN_ENG$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.tb.CLASSI_FIN_ENG$SARS.by.Covid.19[2:28] <-  data.ini.fin.df$a
tabela.resumo.tb.CLASSI_FIN_ENG$SARS.by.influenza[2:28] <- data.ini.fin.df$b
tabela.resumo.tb.CLASSI_FIN_ENG$SARS.unspecified[2:28] <-  data.ini.fin.df$c
tabela.resumo.tb.CLASSI_FIN_ENG$SARS.by.another.etiological.agent[2:28] <-  data.ini.fin.df$d
tabela.resumo.tb.CLASSI_FIN_ENG$SARS.by.another.respiratory.virus[2:28] <- data.ini.fin.df$e
tabela.resumo.tb.CLASSI_FIN_ENG$Indeterminate[2:28] <-  data.ini.fin.df$f

names(tabela.resumo.tb.CLASSI_FIN_ENG) <- c("Unit",
                                     "Total",
                                     "SARS by Covid-19",
                                     'SARS by influenza',
                                     'SARS unspecified',
                                     "SARS by another etiological agent",
                                     "SARS by another respiratory virus",
                                     "Indeterminate")

setwd(link.saida)

saveRDS(tabela.resumo.tb.CLASSI_FIN_ENG, "tabela.resumo.CLASSI_FIN.RDS")

write.csv2(tabela.resumo.tb.CLASSI_FIN_ENG, "tabela.resumo.tb.CLASSI_FIN.csv", row.names = FALSE )

setwd(link.princ)

tabela.resumo.tb.CLASSI_FIN_ENG$`SARS by Covid-19.p` <- round( tabela.resumo.tb.CLASSI_FIN_ENG$`SARS by Covid-19` / tabela.resumo.tb.CLASSI_FIN_ENG$Total * 100, 2 )
tabela.resumo.tb.CLASSI_FIN_ENG$`SARS by influenza.p` <- round( tabela.resumo.tb.CLASSI_FIN_ENG$`SARS by influenza` / tabela.resumo.tb.CLASSI_FIN_ENG$Total * 100, 2 )
tabela.resumo.tb.CLASSI_FIN_ENG$`SARS unspecified.p` <- round( tabela.resumo.tb.CLASSI_FIN_ENG$`SARS unspecified` / tabela.resumo.tb.CLASSI_FIN_ENG$Total * 100, 2 )
tabela.resumo.tb.CLASSI_FIN_ENG$`SARS by another etiological agent.p` <- round( tabela.resumo.tb.CLASSI_FIN_ENG$`SARS by another etiological agent` / tabela.resumo.tb.CLASSI_FIN_ENG$Total * 100, 2 )
tabela.resumo.tb.CLASSI_FIN_ENG$`SARS by another respiratory virus.p` <- round( tabela.resumo.tb.CLASSI_FIN_ENG$`SARS by another respiratory virus` / tabela.resumo.tb.CLASSI_FIN_ENG$Total * 100, 2 )
tabela.resumo.tb.CLASSI_FIN_ENG$Indeterminate.p <- round( tabela.resumo.tb.CLASSI_FIN_ENG$Indeterminate / tabela.resumo.tb.CLASSI_FIN_ENG$Total * 100, 2 )

tabela.resumo.tb.CLASSI_FIN_ENG.p <- tabela.resumo.tb.CLASSI_FIN_ENG[,c(1,9:14)]

names(tabela.resumo.tb.CLASSI_FIN_ENG.p)[2:7] <- c("SARS by Covid-19 (%)",
                                                                        "SARS by influenza (%)",
                                                                        "SARS unspecified (%)",
                                                                        "SARS by another etiological agent (%)",
                                                                        "SARS by another respiratory virus (%)",
                                                                        "Indeterminate (%)")

setwd(link.saida)

saveRDS(tabela.resumo.tb.CLASSI_FIN_ENG.p, "tabela.resumo.tb.CLASSI_FIN_ENG.p.RDS")

write.csv2(tabela.resumo.tb.CLASSI_FIN_ENG.p, "tabela.resumo.tb.CLASSI_FIN_ENG.p.csv", row.names = FALSE )

setwd(link.princ)

# estudo.cuts

srag.estudo$srag.estudo.cuts <- as.factor( srag.estudo$srag.estudo.cuts )

tb.srag.estudo.cuts <- table(srag.estudo$srag.estudo.cuts)

tabela.resumo.srag.estudo.cuts <- data.frame( Unidade = character( 28 ),
                                              nr = numeric(28),
                                              '0-17 years' = numeric( 28 ),
                                              '18-29 years' = numeric( 28 ),
                                              '30-39 years' = numeric( 28 ),
                                              '40-49 years' = numeric( 28 ),
                                              '50-64 years' = numeric( 28 ),
                                              '65-74 years' = numeric( 28 ),
                                              '75-84 years' = numeric( 28 ),
                                              '85 or more years' = numeric( 28 ),
                                              'Indeterminate' = numeric( 28 ) )

tabela.resumo.srag.estudo.cuts$Unidade <- c("Brazil",UFcods$UF.nome)

data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo$srag.estudo.cuts[ srag.estudo$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  nr <- length(sele)
  
  tb <- table( sele )
  
  a <- tb[ '0-17' ]
  b <- tb[ '18-29' ]
  c <- tb[ '30-39']
  d <- tb[ '40-49' ]
  e <- tb[ '50-64' ]
  f <- tb[ '65-74' ]
  g <- tb[ '75-84' ]
  h <- tb[ '85 or more' ]
  i <- tb[ 'Indeterminate']
  
  sai.df <- data.frame( UF = x, nr = nr, a = a, b = b, c = c, d = d, e = e, f = f ,g = g, h = h, i = i )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.srag.estudo.cuts$nr[  tabela.resumo.srag.estudo.cuts$Unidade == "Brazil"] <- nrow( srag.estudo )
tabela.resumo.srag.estudo.cuts$X0.17.years[  tabela.resumo.srag.estudo.cuts$Unidade == "Brazil"] <- tb.srag.estudo.cuts["0-17"]
tabela.resumo.srag.estudo.cuts$X18.29.years[  tabela.resumo.srag.estudo.cuts$Unidade == "Brazil"] <- tb.srag.estudo.cuts["18-29"]
tabela.resumo.srag.estudo.cuts$X30.39.years[  tabela.resumo.srag.estudo.cuts$Unidade == "Brazil"] <- tb.srag.estudo.cuts["30-39"]
tabela.resumo.srag.estudo.cuts$X40.49.years[  tabela.resumo.srag.estudo.cuts$Unidade == "Brazil"] <- tb.srag.estudo.cuts["40-49"]
tabela.resumo.srag.estudo.cuts$X50.64.years[  tabela.resumo.srag.estudo.cuts$Unidade == "Brazil"] <- tb.srag.estudo.cuts["50-64"]
tabela.resumo.srag.estudo.cuts$X65.74.years[  tabela.resumo.srag.estudo.cuts$Unidade == "Brazil"] <- tb.srag.estudo.cuts["65-74"]
tabela.resumo.srag.estudo.cuts$X75.84.years[  tabela.resumo.srag.estudo.cuts$Unidade == "Brazil"] <- tb.srag.estudo.cuts["75-84"]
tabela.resumo.srag.estudo.cuts$X85.or.more.years[  tabela.resumo.srag.estudo.cuts$Unidade == "Brazil"] <- tb.srag.estudo.cuts["85 or more"]
tabela.resumo.srag.estudo.cuts$Indeterminate[  tabela.resumo.srag.estudo.cuts$Unidade == "Brazil"] <- tb.srag.estudo.cuts["Indeterminate"]

tabela.resumo.srag.estudo.cuts$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.srag.estudo.cuts$X0.17.years[2:28] <-  data.ini.fin.df$a
tabela.resumo.srag.estudo.cuts$X18.29.years[2:28] <- data.ini.fin.df$b
tabela.resumo.srag.estudo.cuts$X30.39.years[2:28] <-  data.ini.fin.df$c
tabela.resumo.srag.estudo.cuts$X40.49.years[2:28] <-  data.ini.fin.df$d
tabela.resumo.srag.estudo.cuts$X50.64.years[2:28] <- data.ini.fin.df$e
tabela.resumo.srag.estudo.cuts$X65.74.years[2:28] <-  data.ini.fin.df$f
tabela.resumo.srag.estudo.cuts$X75.84.years[2:28] <-  data.ini.fin.df$g
tabela.resumo.srag.estudo.cuts$X85.or.more.years[2:28] <- data.ini.fin.df$h
tabela.resumo.srag.estudo.cuts$Indeterminate[2:28] <-  data.ini.fin.df$i

names(tabela.resumo.srag.estudo.cuts) <- c("Unit",
                                           "Total",
                                           '0-17',
                                           '18-29',
                                           '30-39',
                                           '40-49',
                                           '50-64',
                                           '65-74',
                                           '75-84',
                                           '85 or more',
                                           "Indeterminate")


setwd(link.saida)

saveRDS(tabela.resumo.srag.estudo.cuts, "tabela.resumo.srag.estudo.cuts.RDS")

write.csv2(tabela.resumo.srag.estudo.cuts, "tabela.resumo.srag.estudo.cuts.csv", row.names = FALSE )

setwd(link.princ)

tabela.resumo.srag.estudo.cuts.p <- tabela.resumo.srag.estudo.cuts

tabela.resumo.srag.estudo.cuts.p$`0-17.p` <- round( tabela.resumo.srag.estudo.cuts.p$`0-17` / tabela.resumo.srag.estudo.cuts.p$Total * 100, 2 )
tabela.resumo.srag.estudo.cuts.p$`18-29.p` <- round( tabela.resumo.srag.estudo.cuts.p$`18-29` / tabela.resumo.srag.estudo.cuts.p$Total * 100, 2 )
tabela.resumo.srag.estudo.cuts.p$`30-39.p` <- round( tabela.resumo.srag.estudo.cuts.p$`30-39` / tabela.resumo.srag.estudo.cuts.p$Total * 100, 2 )
tabela.resumo.srag.estudo.cuts.p$`40-49.p` <- round( tabela.resumo.srag.estudo.cuts.p$`40-49` / tabela.resumo.srag.estudo.cuts.p$Total * 100, 2 )
tabela.resumo.srag.estudo.cuts.p$`50-64.p` <- round( tabela.resumo.srag.estudo.cuts.p$`50-64` / tabela.resumo.srag.estudo.cuts.p$Total * 100, 2 )
tabela.resumo.srag.estudo.cuts.p$`65-74.p` <- round( tabela.resumo.srag.estudo.cuts.p$`65-74` / tabela.resumo.srag.estudo.cuts.p$Total * 100, 2 )
tabela.resumo.srag.estudo.cuts.p$`75-84.p` <- round( tabela.resumo.srag.estudo.cuts.p$`75-84` / tabela.resumo.srag.estudo.cuts.p$Total * 100, 2 )
tabela.resumo.srag.estudo.cuts.p$`85 or more.p` <- round( tabela.resumo.srag.estudo.cuts.p$`85 or more` / tabela.resumo.srag.estudo.cuts.p$Total * 100, 2 )
tabela.resumo.srag.estudo.cuts.p$Indeterminate.p <- round( tabela.resumo.srag.estudo.cuts.p$Indeterminate / tabela.resumo.srag.estudo.cuts.p$Total * 100, 2 )

tabela.resumo.srag.estudo.cuts.p <- tabela.resumo.srag.estudo.cuts.p[,c('Unit',"0-17.p", "18-29.p","30-39.p","40-49.p", "50-64.p", "65-74.p", "75-84.p", "85 or more.p", "Indeterminate.p")]

names(tabela.resumo.srag.estudo.cuts.p)[2:10] <- c("0-17 (%)",  "18-29 (%)",  "30-39 (%)",  "40-49 (%)" ,  "50-64 (%)",  "65-74 (%)", 
                                             "75-84 (%)",  "85 or more (%)",  "Indeterminate (%)")


setwd(link.saida)

saveRDS(tabela.resumo.srag.estudo.cuts.p, "tabela.resumo.srag.estudo.cuts.p.RDS")

write.csv2(tabela.resumo.srag.estudo.cuts.p, "tabela.resumo.srag.estudo.cuts.p.csv", row.names = FALSE )

setwd(link.princ)


# Final Classification by Evolution.

cruz1 <- table( srag.estudo$CLASSI_FIN_ENG, srag.estudo$EVOLUCAO_ENG )

cruz1 <- addmargins( cruz1 )

colnames(cruz1)[6] <-"Total"
rownames(cruz1)[7] <-"Total"

cruz1.p <- round( cruz1[,1:5] / cruz1[,6] * 100 ,2 )


# --- SAIDA

setwd( link.saida )

saveRDS(cruz1, "tabela.CLASSI_FIN.EVOLUCAO_ENG.RDS")

write.csv2(cruz1, "tabela.CLASSI_FIN.EVOLUCAO_ENG.csv", row.names = FALSE )

saveRDS(cruz1.p, "tabela.CLASSI_FIN.EVOLUCAO_ENG.perc.RDS")

write.csv2(cruz1.p, "tabela.CLASSI_FIN.EVOLUCAO_ENG.perc.csv", row.names = FALSE )


setwd( link.entrada )

# ---


#  DT_SIN_PRI  Data de 1ºs sintomas com CLASSI_FIN = SARS by Covid-19


srag.estudo.covid <- srag.estudo[ srag.estudo$CLASSI_FIN_ENG == "SARS by Covid-19",]

tabela.resumo.DT_SIN_PRI.covid <- data.frame( Unidade = character( 28 ),
                                        nr = numeric(28),
                                        data.ini = character( 28 ),
                                        data.fin = character( 28 ),
                                        n.na = numeric( 28 ))

tabela.resumo.DT_SIN_PRI.covid$Unidade <- c("Brazil",UFcods$UF.nome)


data.ini.fin <- lapply( UFcods$UF.nome, function( x ){
  
  sele <- srag.estudo.covid$DT_SIN_PRI[ srag.estudo.covid$SG_UF_NOT ==  UFcods$UF[UFcods$UF.nome == x] ]
  
  sai.min <- min( sele )
  sai.max <- max( sele )
  
  nr <- length(sele)
  
  n.na <- sum(is.na( sele ))
  
  sai.df <- data.frame( UF = x , nr = nr ,sai.min = sai.min,   sai.max = sai.max, n.na = n.na )
  
})

data.ini.fin.df <- do.call( rbind, data.ini.fin )

tabela.resumo.DT_SIN_PRI.covid$nr[  tabela.resumo.DT_SIN_PRI.covid$Unidade == "Brazil"] <- nrow( srag.estudo.covid )
tabela.resumo.DT_SIN_PRI.covid$data.ini[  tabela.resumo.DT_SIN_PRI.covid$Unidade == "Brazil"] <- as.character( min( srag.estudo.covid$DT_SIN_PRI ) )
tabela.resumo.DT_SIN_PRI.covid$data.fin[  tabela.resumo.DT_SIN_PRI.covid$Unidade == "Brazil"] <- as.character( max( srag.estudo.covid$DT_SIN_PRI ) )


tabela.resumo.DT_SIN_PRI.covid$nr[2:28] <-  data.ini.fin.df$nr
tabela.resumo.DT_SIN_PRI.covid$data.ini[2:28] <-  as.character( data.ini.fin.df$sai.min )
tabela.resumo.DT_SIN_PRI.covid$data.fin[2:28] <-  as.character( data.ini.fin.df$sai.max )

names(tabela.resumo.DT_SIN_PRI.covid) <- c("Unit","Total","Inital date",'Final date','Indeterminate')

tabela.resumo.DT_SIN_PRI.covid <- tabela.resumo.DT_SIN_PRI.covid[ order( tabela.resumo.DT_SIN_PRI.covid$`Inital date`,decreasing = FALSE),]

setwd(link.saida)

saveRDS( tabela.resumo.DT_SIN_PRI.covid, "tabela.resumo.DT_SIN_PRI.covid.RDS" )

write.csv2(tabela.resumo.DT_SIN_PRI.covid, "tabela.resumo.DT_SIN_PRI.covid.csv", row.names = FALSE )

setwd(link.princ)

# --------------------------------------------------------- flow chart

# n geral

n.geral <- nrow(srag.estudo)


t1 <- table( srag.estudo$CLASSI_FIN_ENG )
addmargins(t1)

n2 <- sum(t1[c('SARS by influenza',
               'SARS unspecified',
               'SARS by another etiological agent',
               'SARS by another respiratory virus')])

n3 <- t1["SARS by Covid-19"]
  
n4 <- t1["Indeterminate"]

c(n2,n3,n4) / n.geral * 100

  
srag.estudo.covd <- srag.estudo[ srag.estudo$CLASSI_FIN_ENG == "SARS by Covid-19",]


t2 <- table(srag.estudo.covd$UTI_ENG,srag.estudo.covd$HOSPITAL_ENG) 
t2 <- addmargins(t2)

c( t2[2,4] , t2[1,4] , t2[3,4] )

c( t2[2,4] , t2[1,4] , t2[3,4] ) / t2[4,4] * 100

srag.estudo.covd.CLINICAL.BEDS <- srag.estudo.covd[srag.estudo.covd$UTI_ENG == "No",]

t3 <- table( srag.estudo.covd.CLINICAL.BEDS$EVOLUCAO_ENG )
t3 <- addmargins(t3)

srag.estudo.covd.CLINICAL.ICU <- srag.estudo.covd[srag.estudo.covd$UTI_ENG == "Yes",]

t4 <- table( srag.estudo.covd.CLINICAL.ICU$EVOLUCAO_ENG )
t4 <- addmargins(t4)

c( t3[1], t3[2], sum(t3[3:5]) )

c( t3[1], t3[2], sum(t3[3:5]) ) / sum(c( t3[1], t3[2], sum(t3[3:5]) )) * 100

c( t4[1], t4[2], sum(t4[3:5]) )

c( t4[1], t4[2], sum(t4[3:5]) ) / sum(c( t4[1], t4[2], sum(t4[3:5]) )) * 100


srag.fluxo <- grViz(diagram = "digraph flowchart {
  node [fontname = arial, shape = rectangle, color = black]
  tab1 [label = '@@1']
  tab2 [label = '@@2']
  tab3 [label = '@@3']
  tab4 [label = '@@4']
  tab5 [label = '@@5']
  tab6 [label = '@@6']
  tab7 [label = '@@7']
  tab8 [label = '@@8']
  tab9 [label = '@@9']
  tab10 [label = '@@10']
  tab11 [label = '@@11']
  tab12 [label = '@@12']
  tab13 [label = '@@13']
  
  tab1 -> tab2; 
  tab1 -> tab3;
  tab1 -> tab4;
  tab3 -> tab5;
  tab3 -> tab6;
  tab3 -> tab7;
  tab5 -> tab8;
  tab5 -> tab9;
  tab5 -> tab10;
  tab6 -> tab11;
  tab6 -> tab12;
  tab6 -> tab13;
 
}
  
  [1]: 'Severe Acute Respiratory Syndrome(SARS)\\nCovered Period by Date of First Symptoms: 01/17/2021 to 11/22/2021\\nAll cases require hospitalization\\n n = 2,825,170'
  [2]: 'SARS by Others Agents\\n n = 777,128 (27.51%)'
  [3]: 'SARS by Covid-19\\n n = 1,863,934 (65.98%)'
  [4]: 'Indeterminate\\n n =  184,108  (6.51%)'
  [5]: 'Hospitalised in clinical beds\\n n = 999,253 (53.61%)'
  [6]: 'Hospitalised in ICU\\n n = 603,922 (32.40%)'
  [7]: 'Indeterminate\\n n = 260,759 (13.99%)'
  [8]: 'Cure\\n n = 743,640 (74.42%)' 
  [9]: 'Death\\n n = 173,551 (17.37%)'
  [10]: 'Other\\n n = 82,062 (8.21%)'
  [11]: 'Cure\\n n = 238,513 (39.49%)' 
  [12]: 'Death\\n n = 327,860 (54.29%)'
  [13]: 'Other\\n n = 37,549 (6.22%)'
  ")


# ---- 



srag.fluxo %>%
  export_svg() %>%
  read_xml() %>%
  write_xml("srag fluxo.svg")


srag.fluxo %>%
  export_svg %>%
  charToRaw %>% 
  rsvg_png("srag fluxo.png")

# --------------------------------------------------------- figures

# ---- SARS cases by week number

srag.estudo.semana.conta <- aggregate( srag.estudo$conta, by = list( srag.estudo$semana), sum )

srag.estudo.semana.conta$Group.1 <- as.character( srag.estudo.semana.conta$Group.1 )
srag.estudo.semana.conta$Group.1 <- ymd( srag.estudo.semana.conta$Group.1 )

names(srag.estudo.semana.conta) <- c("semana","Casos")

srag.estudo.semana.conta$semana <- as.character( srag.estudo.semana.conta$semana)


setwd( link.saida )

png( "SRAG cases by week number.png", width = 15000, height = 8000, res = 550 )

pp1 <- ggplot(srag.estudo.semana.conta, aes(x = semana, y = Casos )) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  theme_bw() +
  scale_x_discrete(labels = as.character( c(1:53,1:48) )) +
  theme( panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  xlab("Epidemiological week") +
  ylab("Cases") +
  labs(title = "" , caption = "") +
  theme(plot.title = element_text(size=22)) +
  theme( axis.title.x = element_text(size = 24),
         axis.title.y = element_text(size = 24) )  +
  theme( axis.text=element_text(size=15),
         axis.title=element_text(size=15) ) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black") ) +
  
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) 

plot(pp1)

dev.off()

setwd( link.princ )


# SARS cases by week number by region

srag.estudo.semana.conta.REGIAO <- aggregate( srag.estudo$conta, by = list( srag.estudo$semana, srag.estudo$REGIAO_ENG ), sum )
srag.estudo.semana.conta.REGIAO$Group.1 <- as.character( srag.estudo.semana.conta.REGIAO$Group.1 )
srag.estudo.semana.conta.REGIAO$Group.1 <- ymd( srag.estudo.semana.conta.REGIAO$Group.1 )

names(srag.estudo.semana.conta.REGIAO) <- c("semana","REGIAO","Casos")

srag.estudo.semana.conta.REGIAO$semana <- as.character( srag.estudo.semana.conta.REGIAO$semana )

setwd( link.saida )

png( "SRAG cases by week number by region.png",  width = 15000, height = 10000, res = 550 )

pp1.1 <- ggplot( srag.estudo.semana.conta.REGIAO, aes( x = semana, y = Casos )) +
  geom_bar(stat = "identity", width = 0.7, color = "black" ) +
  theme_bw() +
  scale_x_discrete(labels = as.character( c(1:53,1:48) )) +
  theme( panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  xlab("Epidemiological week") +
  ylab("Cases") +
  theme(plot.title = element_text(size=22)) +
  theme(strip.text.x = element_text(size = 18 )) +
  theme( axis.title.x = element_text(size = 24),
         axis.title.y = element_text(size = 24) )  +
  theme( axis.text=element_text(size=15),
         axis.title=element_text(size=15) ) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black") ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  facet_wrap( ~ REGIAO , ncol = 1, scales = "free")

plot(pp1.1)

dev.off()

setwd( link.princ)


# ---- SARS cases by week number by sex


srag.estudo.semana.conta.sexo <- aggregate( srag.estudo$conta, 
                                            by = list( srag.estudo$semana, 
                                                       srag.estudo$CS_SEXO_ENG), sum )

srag.estudo.semana.conta.sexo$Group.1 <- as.character( srag.estudo.semana.conta.sexo$Group.1 )
srag.estudo.semana.conta.sexo$Group.1 <- ymd( srag.estudo.semana.conta.sexo$Group.1 )

names(srag.estudo.semana.conta.sexo) <- c("Semana","Sex","Casos")

srag.estudo.semana.conta.sexo$Semana <- as.character( srag.estudo.semana.conta.sexo$Semana )

srag.estudo.semana.conta.sexo$Sex <- as.factor( srag.estudo.semana.conta.sexo$Sex )

# srag.estudo.semana.conta.sexo$Sex <- factor( srag.estudo.semana.conta.sexo$Sex , levels = levels(srag.estudo.semana.conta.sexo$Sex )[c(1,3,2)])


setwd( link.saida )

png( "SRAG cases by week number by sex.png", width = 15000, height = 8000, res = 550 )

pp2 <- ggplot(srag.estudo.semana.conta.sexo, aes(x = Semana, y = Casos , fill = Sex )) +
  geom_bar(stat = "identity", width = 0.7) +
  theme_bw() +
  scale_x_discrete(labels = as.character( c(1:53,1:48) )) +
  theme( panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  xlab("Epidemiological week") +
  ylab("Cases") +
  labs(title = "" , caption = "") +
  theme(plot.title = element_text(size=22)) +
  theme( axis.title.x = element_text(size = 24),
         axis.title.y = element_text(size = 24) )  +
  theme( axis.text=element_text(size=15),
         axis.title=element_text(size=15) ) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black") ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values=c("gray80","gray50", "gray1" )) +
  
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=28), #change legend title font size
        legend.text = element_text(size=24)) + #change legend text font size
  theme(legend.position="bottom") 

plot(pp2)

dev.off()

setwd( link.princ)

# ---- SARS cases by evolution

srag.estudo.semana.conta.evolucao <- aggregate( srag.estudo$conta, 
                                                by = list( srag.estudo$semana,
                                                           srag.estudo$EVOLUCAO_ENG), sum )

srag.estudo.semana.conta.evolucao$Group.1 <- as.character( srag.estudo.semana.conta.evolucao$Group.1 )
srag.estudo.semana.conta.evolucao$Group.1 <- ymd( srag.estudo.semana.conta.evolucao$Group.1 )

names(srag.estudo.semana.conta.evolucao) <- c("Semana","Evolucao","Casos")
srag.estudo.semana.conta.evolucao$Evolucao <- as.factor( srag.estudo.semana.conta.evolucao$Evolucao)

srag.estudo.semana.conta.evolucao$Semana <- as.character( srag.estudo.semana.conta.evolucao$Semana)


setwd( link.saida )

png( "SRAG cases by evolution.png",  width = 15000, height = 8000, res = 550 )

pp3 <- ggplot(srag.estudo.semana.conta.evolucao, aes(x = Semana, y = Casos , fill = Evolucao )) +
  geom_bar( stat="identity", color="black",
            width = 0.7) +
  theme_bw() +
  theme( panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  scale_x_discrete(labels = as.character( c(1:53,1:48) )) +
  xlab("Epidemiological week") +
  ylab("Cases") +
  labs(title = "" , caption = "") +
  theme(plot.title = element_text(size=22)) +
  theme( axis.title.x = element_text(size = 24),
         axis.title.y = element_text(size = 24) )  +
  theme( axis.text=element_text(size=15),
         axis.title=element_text(size=15) ) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black") ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values=  rev(c("gray99","gray75","grey55","grey35", "grey0")))  +
  guides(fill=guide_legend(title="Evolution")) +
  
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=28), #change legend title font size
        legend.text = element_text(size=24)) + #change legend text font size
  theme(legend.position="bottom")

plot(pp3)

dev.off()

setwd( link.princ)


# --- EVOLUCAO_ENG by UF REGIAO TOTAL

srag.estudo.semana.conta.evolucao <- aggregate( srag.estudo$conta, 
                                                by = list( srag.estudo$SG_UF_NOT, 
                                                           srag.estudo$REGIAO_ENG,
                                                           srag.estudo$EVOLUCAO_ENG),
                                                sum )

names(srag.estudo.semana.conta.evolucao) <- c("UF","Região","Evolução","conta")

srag.estudo.semana.conta.evolucao$Região <- as.factor( srag.estudo.semana.conta.evolucao$Região )
srag.estudo.semana.conta.evolucao$UF <- as.factor( srag.estudo.semana.conta.evolucao$UF )
srag.estudo.semana.conta.evolucao$Evolução <- as.factor( srag.estudo.semana.conta.evolucao$Evolução )

regioes <- unique( UFcods$Regiao )

srag.estudo.semana.conta.evolucao$Evolução <- factor( srag.estudo.semana.conta.evolucao$Evolução,
                                                      levels(srag.estudo.semana.conta.evolucao$Evolução)[rev(1:5)] )


setwd( link.saida )

png( "evolution by UF TOTAL.png", width = 11000, height = 8000, res = 600 )

p.evolucao <- ggplot( srag.estudo.semana.conta.evolucao, aes( x = UF, y = conta,  fill = Evolução,) ) + 
  geom_bar( stat="identity", color="white",
            width = 0.7, position = position_dodge(preserve = 'single') ) +
  theme_bw() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16 )) + 
  theme( axis.title.x = element_text(size = 18),
         axis.title.y = element_text(size = 18) )  +
  theme(strip.text.x = element_text(size = 18 )) +
  guides(color=guide_legend("")) + 
  labs(title = "", caption = "") +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size=28)) +
  theme(legend.position="right",
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1.5, 'cm') ,
        legend.text = element_text(size= 16 ),
        legend.title=element_text(size=16) )  +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme(
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black") ) +
  theme(  panel.grid.major.x = element_blank() ) +
  facet_wrap(~ Região, nrow = 3,scales = "free") +
  scale_fill_manual(values = c("grey80", "grey65", "grey50","grey30", "grey1") ) +
  guides(fill=guide_legend(title="Evolution")) +
  theme( panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=28), #change legend title font size
        legend.text = element_text(size=24)) + #change legend text font size
  theme(legend.position="bottom")

plot(p.evolucao)

dev.off()

setwd( link.princ)


# EVOLUCAO_ENG by UF REGIAO PROPORÇÂO

srag.estudo.semana.conta.evolucao.prop <- lapply( UFcods$UF, function( x ){
  
  sele <- srag.estudo.semana.conta.evolucao[ srag.estudo.semana.conta.evolucao$UF == x ,]
  
  sele$Prop <- round( sele$conta / sum( sele$conta ) , 2 )
  
  sele
  
})

srag.estudo.semana.conta.evolucao.prop <- do.call( rbind, srag.estudo.semana.conta.evolucao.prop )

setwd( link.saida )

png( "evolution by UF PROPORTION.png", width = 11000, height = 8000, res = 600 )

p.evolucao <- ggplot( srag.estudo.semana.conta.evolucao.prop, aes( x = UF, y = Prop,  fill = Evolução,) ) + 
  geom_bar( stat="identity", color="white",
            width = 0.7, position = position_dodge(preserve = 'single') ) +
  theme_bw() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16 )) + 
  theme(strip.text.x = element_text(size = 18 )) +
  guides(color=guide_legend("")) + 
  labs(title = "", caption = "") +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size=28)) +
  theme(legend.position="right",
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1.5, 'cm') ,
        legend.text = element_text(size= 16 ),
        legend.title=element_text(size=16) )  +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme(
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black") ) +
  theme( panel.grid.major.x = element_blank() ) +
  facet_wrap(~Região,nrow = 3,scales = "free") +
  scale_fill_manual(values = c("grey80", "grey65", "grey50","grey30", "grey1") ) +
  guides(fill=guide_legend(title="Evolution")) +
  theme(legend.position="bottom") + 
  theme( panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=28), #change legend title font size
        legend.text = element_text(size=24)) + #change legend text font size
  scale_y_continuous(labels = scales::percent,limits = c(0,1))

plot(p.evolucao)

dev.off()

setwd( link.princ)



# ---- SARS cases by HOSPITAL_ENG

table( srag.estudo$HOSPITAL_ENG,useNA = "always")

srag.estudo.semana.conta.hospital <- aggregate( srag.estudo$conta, by = list( srag.estudo$semana,
                                                                              srag.estudo$HOSPITAL_ENG), sum )


srag.estudo.semana.conta.hospital$Group.1 <- as.character( srag.estudo.semana.conta.hospital$Group.1 )
srag.estudo.semana.conta.hospital$Group.1 <- ymd( srag.estudo.semana.conta.hospital$Group.1 )

names(srag.estudo.semana.conta.hospital) <- c("Semana","Hospital","Casos")
#srag.estudo.semana.conta.hospital$Hospital <- as.factor( srag.estudo.semana.conta.hospital$Hospital)


srag.estudo.semana.conta.hospital$Semana <- as.character( srag.estudo.semana.conta.hospital$Semana)

setwd( link.saida )

png( "SRAG cases by hospital.png",  width = 13000, height = 6000, res = 500 )

pp3 <- ggplot(srag.estudo.semana.conta.hospital, aes(x = Semana, y = Casos , fill = Hospital )) +
  geom_bar( stat="identity", color="black",
            width = 0.7) +
  theme_bw() +
  scale_x_discrete(labels = as.character( c(1:53,1:48) )) +
  xlab("Epidemiological week") +
  ylab("Cases") +
  theme(  panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  theme(strip.text.x = element_text(size = 18 )) +
  theme( axis.title.x = element_text(size = 20),
         axis.title.y = element_text(size = 20) )  +
  theme( axis.text=element_text(size=14),
         axis.title=element_text(size=14) ) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black") ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values=  c("gray90","grey65", "grey0"))  +
  guides(fill=guide_legend(title="Hospitalization")) +
  
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=28), #change legend title font size
        legend.text = element_text(size=24)) + #change legend text font size
  theme(legend.position="bottom")

plot(pp3)

dev.off()

setwd( link.princ)





# ---- HOSPITAL_ENG by UF REGIAO TOTAL


srag.estudo.semana.conta.HOSPITAL_ENG <- aggregate( srag.estudo$conta, 
                                                by = list( srag.estudo$SG_UF_NOT, 
                                                           srag.estudo$REGIAO_ENG,
                                                           srag.estudo$HOSPITAL_ENG),
                                                sum )

names(srag.estudo.semana.conta.HOSPITAL_ENG) <- c("UF","Região","Hospitaliza","conta")

srag.estudo.semana.conta.HOSPITAL_ENG$Região <- as.factor( srag.estudo.semana.conta.HOSPITAL_ENG$Região )
srag.estudo.semana.conta.HOSPITAL_ENG$UF <- as.factor( srag.estudo.semana.conta.HOSPITAL_ENG$UF )
srag.estudo.semana.conta.HOSPITAL_ENG$Hospitaliza <- as.factor( srag.estudo.semana.conta.HOSPITAL_ENG$Hospitaliza )

regioes <- unique( UFcods$Regiao )

setwd( link.saida )

png( "hospitalization by UF TOTAL.png", width = 11000, height = 8000, res = 600 )

p.Hospitaliza <- ggplot( srag.estudo.semana.conta.HOSPITAL_ENG, aes( x = UF, y = conta,  fill = Hospitaliza,) ) + 
  geom_bar( stat="identity", color="white",
            width = 0.7, position = position_dodge(preserve = 'single') ) +
  theme_bw() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16 )) + 
  theme( axis.title.x = element_text(size = 18),
         axis.title.y = element_text(size = 18) )  +
  theme(strip.text.x = element_text(size = 18 )) +
  guides(color=guide_legend("")) + 
  labs(title = "", caption = "") +
  xlab("") +
  ylab("") +
  theme(  panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  theme(plot.title = element_text(size=28)) +
  theme(legend.position="right",
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1.5, 'cm') ,
        legend.text = element_text(size= 16 ),
        legend.title=element_text(size=16) )  +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme(
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black") ) +
  theme( panel.grid.major.x = element_blank() ) +
  facet_wrap(~ Região, nrow = 3,scales = "free") +
  scale_fill_manual(values = c("gray90","grey65", "grey0") ) +
  guides(fill=guide_legend(title="Hospitalization")) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=28), #change legend title font size
        legend.text = element_text(size=24)) + #change legend text font size
  theme(legend.position="bottom")

plot(p.Hospitaliza)

dev.off()

setwd( link.princ )

# ---- HOSPITAL_ENG by UF REGIAO PROPORTION

srag.estudo.semana.conta.HOSPITAL_ENG.prop <- lapply( UFcods$UF, function( x ){
  
  sele <- srag.estudo.semana.conta.HOSPITAL_ENG[ srag.estudo.semana.conta.HOSPITAL_ENG$UF == x ,]
  
  sele$Prop <- round( sele$conta / sum( sele$conta ) , 2 )
  
  sele
  
})

srag.estudo.semana.conta.HOSPITAL_ENG.prop <- do.call( rbind, srag.estudo.semana.conta.HOSPITAL_ENG.prop )

setwd( link.saida )

png( "HOSPITAL by UF PROPORTION.png", width = 11000, height = 8000, res = 600 )

p.Hospitaliza <- ggplot( srag.estudo.semana.conta.HOSPITAL_ENG.prop, aes( x = UF, y = Prop,  fill = Hospitaliza,) ) + 
  geom_bar( stat="identity", color="white",
            width = 0.7, position = position_dodge(preserve = 'single') ) +
  theme_bw() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16 )) + 
  theme(strip.text.x = element_text(size = 18 )) +
  guides(color=guide_legend("")) + 
  labs(title = "", caption = "") +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size=28)) +
  theme(legend.position="right",
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1.5, 'cm') ,
        legend.text = element_text(size= 16 ),
        legend.title=element_text(size=16) )  +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme(
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black") ) +
  theme(  panel.grid.major.x = element_blank() ) +
  facet_wrap(~Região,nrow = 3,scales = "free") +
  scale_fill_manual(values =  c("gray90","grey65", "grey0") ) +
  guides(fill=guide_legend(title="Evolution")) +
  theme(  panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  theme(legend.position="bottom") + 
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=28), #change legend title font size
        legend.text = element_text(size=24)) + #change legend text font size
  scale_y_continuous(labels = scales::percent,limits = c(0,1))

plot(p.Hospitaliza)

dev.off()

setwd( link.princ )


# ---- Cases by Date of Symptoms and UTI.

srag.estudo.semana.UTI_ENG <- aggregate( srag.estudo$conta,
                                           by = list( srag.estudo$semana, 
                                                      srag.estudo$UTI_ENG), sum )

srag.estudo.semana.UTI_ENG$Group.1 <- as.character( srag.estudo.semana.UTI_ENG$Group.1 )
srag.estudo.semana.UTI_ENG$Group.1 <- ymd( srag.estudo.semana.UTI_ENG$Group.1 )

names(srag.estudo.semana.UTI_ENG) <- c("Semana","uti","Casos")
srag.estudo.semana.UTI_ENG$uti <- as.factor( srag.estudo.semana.UTI_ENG$uti)

srag.estudo.semana.UTI_ENG$Semana <- as.character( srag.estudo.semana.UTI_ENG$Semana)


setwd( link.saida )

png( "SRAG cases by UTI.png",  width = 13000, height = 6000, res = 600 )

pp3 <- ggplot(srag.estudo.semana.UTI_ENG, aes(x = Semana, y = Casos , fill = uti )) +
  geom_bar( stat="identity", color="black",
            width = 0.7) +
  theme_bw() +
  scale_x_discrete(labels = as.character( c(1:53,1:48) )) +
  xlab("Epidemiological week") +
  ylab("Cases") +
  theme( panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  theme(strip.text.x = element_text(size = 18 )) +
  theme( axis.title.x = element_text(size = 20),
         axis.title.y = element_text(size = 20) )  +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=12) ) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black") ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values= c("gray90", "grey55","grey10"))  +
  guides(fill=guide_legend(title="UTI")) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=28), #change legend title font size
        legend.text = element_text(size=24)) + #change legend text font size
  theme(legend.position="bottom")

plot(pp3)

dev.off()

setwd( link.princ )




# --- SARS cases by idade restricted to EVOLUCAO_ENG death and CLASSI_FIN_ENG SARS by Covid-19


srag.estudo.obitocovid <- srag.estudo[srag.estudo$EVOLUCAO_ENG == "Death" & srag.estudo$CLASSI_FIN_ENG == "SARS by Covid-19",]


srag.estudo.semana.conta.idade <- aggregate( srag.estudo.obitocovid$conta, 
                                             by = list( srag.estudo.obitocovid$semana, 
                                                        srag.estudo.obitocovid$srag.estudo.cuts), sum )


srag.estudo.semana.conta.idade$Group.1 <- as.character( srag.estudo.semana.conta.idade$Group.1 )
srag.estudo.semana.conta.idade$Group.1 <- ymd( srag.estudo.semana.conta.idade$Group.1 )

names(srag.estudo.semana.conta.idade) <- c("Semana","idade","Casos")
srag.estudo.semana.conta.idade$idade <- as.factor( srag.estudo.semana.conta.idade$idade)


srag.estudo.semana.conta.idade$Semana <- as.character( srag.estudo.semana.conta.idade$Semana)



setwd( link.saida )

png( "SRAG cases by idade restricted to EVOLUCAO_ENG death and CLASSI_FIN_ENG SARS by Covid-19.png",  width = 13000, height = 6000, res = 600 )

pp3 <- ggplot(srag.estudo.semana.conta.idade, aes(x = Semana, y = Casos , fill = idade )) +
  geom_bar( stat="identity", color="black",
            width = 0.7) +
  theme_bw() +
  scale_x_discrete(labels = as.character( c(1:53,1:46) )) +
  xlab("Epidemiological week") +
  ylab("Cases") +
  theme(  panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  theme(strip.text.x = element_text(size = 18 )) +
  theme( axis.title.x = element_text(size = 20),
         axis.title.y = element_text(size = 20) )  +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=12) ) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black") ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values= c("gray99","gray90","grey85","grey70", "grey55","gray40","gray25",
                              "grey10","grey1"))  +
  guides(fill=guide_legend(title="Age Group")) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=28), #change legend title font size
        legend.text = element_text(size=24)) + #change legend text font size
  theme(legend.position="bottom")

plot(pp3)

dev.off()

setwd( link.princ )


# --- SARS cases by escolaridade

srag.estudo.semana.conta.CS_ESCOL_N <- aggregate( srag.estudo$conta, by = list( srag.estudo$semana,
                                                                                srag.estudo$CS_ESCOL_N_ENG), sum )


srag.estudo.semana.conta.CS_ESCOL_N$Group.1 <- as.character( srag.estudo.semana.conta.CS_ESCOL_N$Group.1 )
srag.estudo.semana.conta.CS_ESCOL_N$Group.1 <- ymd( srag.estudo.semana.conta.CS_ESCOL_N$Group.1 )

names(srag.estudo.semana.conta.CS_ESCOL_N) <- c("Semana","Escola","Casos")
srag.estudo.semana.conta.CS_ESCOL_N$Escola <- as.factor( srag.estudo.semana.conta.CS_ESCOL_N$Escola)

srag.estudo.semana.conta.CS_ESCOL_N$Semana <- as.character( srag.estudo.semana.conta.CS_ESCOL_N$Semana)

setwd( link.saida )

png( "SRAG cases by escolaridade.png",  width = 13000, height = 6000, res = 500 )

pp3 <- ggplot(srag.estudo.semana.conta.CS_ESCOL_N, aes(x = Semana, y = Casos , fill = Escola )) +
  geom_bar( stat="identity", color="black",
            width = 0.7) +
  theme_bw() +
  scale_x_discrete(labels = as.character( c(1:53,1:46) )) +
  xlab("Epidemiological week") +
  ylab("Cases") +
  theme( panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  theme(strip.text.x = element_text(size = 18 )) +
  theme( axis.title.x = element_text(size = 20),
         axis.title.y = element_text(size = 20) )  +
  theme( axis.text=element_text(size=14),
         axis.title=element_text(size=14) ) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black") ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values= rev( c("gray99","gray85","grey75","grey60", "grey45","grey25","grey1")))  +
  guides(fill=guide_legend(title="Scholarity")) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=28), #change legend title font size
        legend.text = element_text(size=24)) + #change legend text font size
  theme(legend.position="bottom")

plot(pp3)

dev.off()

setwd( link.princ )

# ---- SARS cases by zone

srag.estudo.semana.conta.CS_ZONA <- aggregate( srag.estudo$conta, by = list( srag.estudo$semana,
                                                                             srag.estudo$CS_ZONA_ENG), sum )

srag.estudo.semana.conta.CS_ZONA$Group.1 <- as.character( srag.estudo.semana.conta.CS_ZONA$Group.1 )
srag.estudo.semana.conta.CS_ZONA$Group.1 <- ymd( srag.estudo.semana.conta.CS_ZONA$Group.1 )

names(srag.estudo.semana.conta.CS_ZONA) <- c("Semana","Zona","Casos")
srag.estudo.semana.conta.CS_ZONA$Zona <- as.factor( srag.estudo.semana.conta.CS_ZONA$Zona)

srag.estudo.semana.conta.CS_ZONA$Semana  <- as.character( srag.estudo.semana.conta.CS_ZONA$Semana )

setwd( link.saida )

png( "SRAG cases by zone.png",  width = 13000, height = 6000, res = 500 )

pp3 <- ggplot(srag.estudo.semana.conta.CS_ZONA, aes(x = Semana, y = Casos , fill = Zona )) +
  geom_bar( stat="identity", color="black",
            width = 0.7) +
  theme_bw() +
  scale_x_discrete(labels = as.character( c(1:53,1:46) )) +
  xlab("Epidemiological week") +
  ylab("Cases") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  theme(strip.text.x = element_text(size = 18 )) +
  theme( axis.title.x = element_text(size = 20),
         axis.title.y = element_text(size = 20) )  +
  theme( axis.text=element_text(size=14),
         axis.title=element_text(size=14) ) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black") ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values=  c("gray90","grey65", "grey45","grey25"))  +
  guides(fill=guide_legend(title="Zone")) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=28), #change legend title font size
        legend.text = element_text(size=24)) + #change legend text font size
  theme(legend.position="bottom")

plot(pp3)

dev.off()

setwd( link.princ )

# --- SARS cases by class final

srag.estudo.semana.conta.CLASSI_FIN <- aggregate( srag.estudo$conta, by = list( srag.estudo$semana,
                                                                                srag.estudo$CLASSI_FIN_ENG), sum )


srag.estudo.semana.conta.CLASSI_FIN$Group.1 <- as.character( srag.estudo.semana.conta.CLASSI_FIN$Group.1 )
srag.estudo.semana.conta.CLASSI_FIN$Group.1 <- ymd( srag.estudo.semana.conta.CLASSI_FIN$Group.1 )

names(srag.estudo.semana.conta.CLASSI_FIN) <- c("Semana","Class","Casos")
srag.estudo.semana.conta.CLASSI_FIN$Class <- as.factor( srag.estudo.semana.conta.CLASSI_FIN$Class)

srag.estudo.semana.conta.CLASSI_FIN$Semana  <- as.character( srag.estudo.semana.conta.CLASSI_FIN$Semana )

setwd( link.saida )

png( "SRAG cases by class final.png",  width = 13000, height = 7000, res = 500 )

pp3 <- ggplot(srag.estudo.semana.conta.CLASSI_FIN, aes(x = Semana, y = Casos , fill = Class )) +
  geom_bar( stat="identity", color="black",
            width = 0.7) +
  theme_bw() +
  scale_x_discrete(labels = as.character( c(1:53,1:48) )) +
  xlab("Epidemiological week") +
  ylab("Cases") +
  theme(   panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  theme(strip.text.x = element_text(size = 18 )) +
  theme( axis.title.x = element_text(size = 20),
         axis.title.y = element_text(size = 20) )  +
  theme( axis.text=element_text(size=14),
         axis.title=element_text(size=14) ) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black") ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values= ( c("gray95","gray80","grey65","grey50", "grey35","grey20","grey1")))  +
  guides(fill=guide_legend(title="Final Classification")) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=28), #change legend title font size
        legend.text = element_text(size=24)) + #change legend text font size
  theme(legend.position="bottom")

plot(pp3)

dev.off()

setwd( link.princ )






# --- SRAG cases by idade

srag.estudo.semana.conta.idade <- aggregate( srag.estudo$conta, by = list( srag.estudo$semana, srag.estudo$srag.estudo.cuts), sum )

srag.estudo.semana.conta.idade$Group.1 <- as.character( srag.estudo.semana.conta.idade$Group.1 )
srag.estudo.semana.conta.idade$Group.1 <- ymd( srag.estudo.semana.conta.idade$Group.1 )

names(srag.estudo.semana.conta.idade) <- c("Semana","idade","Casos")
srag.estudo.semana.conta.idade$idade <- as.factor( srag.estudo.semana.conta.idade$idade)


srag.estudo.semana.conta.idade$Semana <- as.character( srag.estudo.semana.conta.idade$Semana)

setwd( link.saida )

png( "SRAG cases by idade.png",  width = 13000, height = 6000, res = 600 )

pp3 <- ggplot(srag.estudo.semana.conta.idade, aes(x = Semana, y = Casos , fill = idade )) +
  geom_bar( stat="identity", color="black",
            width = 0.7) +
  theme_bw() +
  scale_x_discrete(labels = as.character( c(1:53,1:48) )) +
  xlab("Epidemiological week") +
  ylab("Cases") +
  theme( panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  theme(strip.text.x = element_text(size = 18 )) +
  theme( axis.title.x = element_text(size = 20),
         axis.title.y = element_text(size = 20) )  +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=12) ) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black") ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values= c("gray99","gray90","grey85","grey70", "grey55","gray40","gray25",
                              "grey10","grey1"))  +
  guides(fill=guide_legend(title="Age Group")) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=28), #change legend title font size
        legend.text = element_text(size=24)) + #change legend text font size
  theme(legend.position="bottom")

plot(pp3)

dev.off()

setwd( link.princ )



# ==============================================================================

# Mortality and Incidence of Covid-19 by Age Group.

srag.estudo.covid <- srag.estudo[ srag.estudo$CLASSI_FIN_ENG == "SARS by Covid-19",]

srag.estudo.obitocovid <- srag.estudo[srag.estudo$EVOLUCAO_ENG == "Death" & srag.estudo$CLASSI_FIN_ENG == "SARS by Covid-19",]

# --

srag.estudo.IDADE <- aggregate( srag.estudo$conta, 
                                by = list( srag.estudo$srag.estudo.cuts), sum )

srag.estudo.IDADE$Group.1 <- as.character( srag.estudo.IDADE$Group.1 )


srag.estudo.IDADE.COVID19 <- aggregate( srag.estudo.covid$conta, 
                                            by = list( srag.estudo.covid$srag.estudo.cuts), sum )

srag.estudo.IDADE.COVID19$Group.1 <- as.character( srag.estudo.IDADE.COVID19$Group.1  )

srag.estudo.OBITO.COVID19.IDADE <- aggregate( srag.estudo.obitocovid$conta, 
                                                  by = list( srag.estudo.obitocovid$srag.estudo.cuts), sum )

srag.estudo.OBITO.COVID19.IDADE$Group.1 <- as.character( srag.estudo.OBITO.COVID19.IDADE$Group.1 )


srag.estudo.OBITO.COVID19.ANO.IDADE <- aggregate( srag.estudo.obitocovid$conta, 
                                                  by = list( srag.estudo.obitocovid$srag.estudo.cuts), sum )

srag.estudo.OBITO.COVID19.ANO.IDADE$Group.1 <- as.character( srag.estudo.OBITO.COVID19.ANO.IDADE$Group.1 )

tab.2000.2001 <- data.frame( idade = srag.estudo.IDADE.COVID19$Group.1,
                             casos.obitos.covid19 = srag.estudo.OBITO.COVID19.ANO.IDADE$x,
                             casos.covid.total = srag.estudo.IDADE.COVID19$x,
                             casos.total = srag.estudo.IDADE$x,
                             perc1 = round( srag.estudo.OBITO.COVID19.ANO.IDADE$x / srag.estudo.IDADE.COVID19$x * 100, 3),
                             perc2 = round( srag.estudo.IDADE.COVID19$x / srag.estudo.IDADE$x * 100, 3) )

tab.2000.2001.total <- data.frame( idade = "Total",
                                   casos.obitos.covid19 = sum( tab.2000.2001$casos.obitos.covid19),
                                   casos.covid.total = sum( tab.2000.2001$casos.covid.total),
                                   casos.total = sum( tab.2000.2001$casos.total ))

tab.2000.2001.total$perc1 <- round(  tab.2000.2001.total$casos.obitos.covid19 / tab.2000.2001.total$casos.covid.total * 100 , 3 )
tab.2000.2001.total$perc2 <- round(  tab.2000.2001.total$casos.covid.total / tab.2000.2001.total$casos.total * 100 , 3 )

tab.2000.2001 <- rbind( tab.2000.2001, tab.2000.2001.total )

names(tab.2000.2001) <- c("Age Group",
                          "Death and Final Classification by Covid19",
                          "Final Classification by Covid19",
                          "Total",
                          "Mortality by Covid19 (%)",
                          "Incidence of Covid19 (%)")

setwd(link.saida)

saveRDS(tab.2000.2001, "Mortality.Incidence.2020.2021.ENG.RDS")

write.csv2(tab.2000.2001, "Mortality.Incidence.2020.2021.ENG.csv", row.names = FALSE )

setwd(link.princ)


# --- POR UF

setwd(link.saida)

pos <- 1

lapply( UFcods$UF, function( x ){
  
  srag.estudo.sele <- srag.estudo[ srag.estudo$SG_UF_NOT == x ,]
  
  
  srag.estudo.covid <- srag.estudo.sele[ srag.estudo.sele$CLASSI_FIN_ENG == "SARS by Covid-19",]
  
  srag.estudo.obitocovid <- srag.estudo.sele[srag.estudo.sele$EVOLUCAO_ENG == "Death" & 
                                               srag.estudo.sele$CLASSI_FIN_ENG == "SARS by Covid-19",]
  
  # --
  
  srag.estudo.IDADE <- aggregate( srag.estudo.sele$conta, 
                                  by = list( srag.estudo.sele$srag.estudo.cuts), sum , drop = FALSE)
  
  srag.estudo.IDADE$x[ is.na( srag.estudo.IDADE$x )] <- 0 
  
  srag.estudo.IDADE$Group.1 <- as.character( srag.estudo.IDADE$Group.1 )
  
  
  srag.estudo.IDADE.COVID19 <- aggregate( srag.estudo.covid$conta, 
                                          by = list( srag.estudo.covid$srag.estudo.cuts), sum , drop = FALSE)
  
  srag.estudo.IDADE.COVID19$x[ is.na( srag.estudo.IDADE.COVID19$x )] <- 0 
  
  srag.estudo.IDADE.COVID19$Group.1 <- as.character( srag.estudo.IDADE.COVID19$Group.1  )
  
  srag.estudo.OBITO.COVID19.IDADE <- aggregate( srag.estudo.obitocovid$conta, 
                                                by = list( srag.estudo.obitocovid$srag.estudo.cuts), sum , drop = FALSE)
  
  srag.estudo.OBITO.COVID19.IDADE$x[ is.na( srag.estudo.OBITO.COVID19.IDADE$x )] <- 0 
  
  srag.estudo.OBITO.COVID19.IDADE$Group.1 <- as.character( srag.estudo.OBITO.COVID19.IDADE$Group.1 )
  
  
  srag.estudo.OBITO.COVID19.ANO.IDADE <- aggregate( srag.estudo.obitocovid$conta, 
                                                    by = list( srag.estudo.obitocovid$srag.estudo.cuts), sum , drop = FALSE)
  
  srag.estudo.OBITO.COVID19.ANO.IDADE$x[ is.na( srag.estudo.OBITO.COVID19.ANO.IDADE$x )] <- 0
  
  srag.estudo.OBITO.COVID19.ANO.IDADE$Group.1 <- as.character( srag.estudo.OBITO.COVID19.ANO.IDADE$Group.1 )
  
  tab.2000.2001 <- data.frame( idade = srag.estudo.IDADE.COVID19$Group.1,
                               casos.obitos.covid19 = srag.estudo.OBITO.COVID19.ANO.IDADE$x,
                               casos.covid.total = srag.estudo.IDADE.COVID19$x,
                               casos.total = srag.estudo.IDADE$x,
                               perc1 = round( srag.estudo.OBITO.COVID19.ANO.IDADE$x / srag.estudo.IDADE.COVID19$x * 100, 3),
                               perc2 = round( srag.estudo.IDADE.COVID19$x / srag.estudo.IDADE$x * 100, 3) )
  
  tab.2000.2001.total <- data.frame( idade = "Total",
                                     casos.obitos.covid19 = sum( tab.2000.2001$casos.obitos.covid19),
                                     casos.covid.total = sum( tab.2000.2001$casos.covid.total),
                                     casos.total = sum( tab.2000.2001$casos.total ))
  
  tab.2000.2001.total$perc1 <- round(  tab.2000.2001.total$casos.obitos.covid19 / tab.2000.2001.total$casos.covid.total * 100 , 3 )
  tab.2000.2001.total$perc2 <- round(  tab.2000.2001.total$casos.covid.total / tab.2000.2001.total$casos.total * 100 , 3 )
  
  tab.2000.2001 <- rbind( tab.2000.2001, tab.2000.2001.total )
  
  names(tab.2000.2001) <- c("Age Group",
                            "Death and Final Classification by Covid19",
                            "Final Classification by Covid19",
                            "Total",
                            "Mortality by Covid19 (%)",
                            "Incidence of Covid19 (%)")
  
  nome.saida1 <- paste( "Mortality.Incidence.2020.2021 ", x , " ENG.RDS" )
  nome.saida2 <- paste( "Mortality.Incidence.2020.2021 ", x , " ENG.csv" )
  
  
  
  saveRDS(tab.2000.2001, nome.saida1 )
  
  write.csv2(tab.2000.2001, nome.saida2, row.names = FALSE )
  

  
  print( pos )
  
  pos <<- pos + 1
  
} )

setwd(link.princ)




# ---- flow  UF notification and UF residence

# UF NOTIFICA = SG_UF_NOT
# UF RESIDENCIA = SG_UF

fluxo.noti.resd <- aggregate( srag.estudo$conta, by = list( srag.estudo$SG_UF_NOT, srag.estudo$SG_UF ), sum )

fluxo.noti.resd <- fluxo.noti.resd[ order(fluxo.noti.resd$x, decreasing = T),]

names( fluxo.noti.resd ) <- c("UF NOTIFICA","UF RESIDENCIA","FREQ")


# --- output

setwd( link.saida )

saveRDS( fluxo.noti.resd, "UF.fluxo.noti.resd.RDS" )

setwd( link.entrada )

# ---







# --- NAO FOI <<<<


# ---- FLUXO entre CAPITAL NOTIFICA e MUNICIPIO RESIDENCIA

srag.estudo.capitais.notifica <- srag.estudo[ srag.estudo$CO_MUN_NOT %in% capitais$codigo6, ]

fluxo.capital.noti.resd <- aggregate( srag.estudo.capitais.notifica$conta, 
                                      by = list( srag.estudo.capitais.notifica$ID_MUNICIP,
                                                 srag.estudo.capitais.notifica$ID_MN_RESI ), sum )

fluxo.noti.resd <- fluxo.noti.resd[ order(fluxo.noti.resd$x, decreasing = T),]

tt <- fluxo.capital.noti.resd[fluxo.capital.noti.resd$Group.1 == "SAO PAULO",]

tt[ order(tt$x,decreasing = T),]










srag.estudo.ANO.IDADE <- aggregate( srag.estudo$conta, 
                                                  by = list( srag.estudo$Ano, 
                                                             srag.estudo$srag.estudo.cuts), sum )

srag.estudo.ANO.IDADE$Group.2 <- as.character( srag.estudo.ANO.IDADE$Group.2 )

srag.estudo.ANO.IDADE.2020 <- srag.estudo.ANO.IDADE[ srag.estudo.ANO.IDADE$Group.1 == 2020,]
srag.estudo.ANO.IDADE.2021 <- srag.estudo.ANO.IDADE[ srag.estudo.ANO.IDADE$Group.1 == 2021,]



srag.estudo.ANO.IDADE.COVID19 <- aggregate( srag.estudo.covid$conta, 
                                    by = list( srag.estudo.covid$Ano, 
                                               srag.estudo.covid$srag.estudo.cuts), sum )

srag.estudo.ANO.IDADE.COVID19$Group.2 <- as.character( srag.estudo.ANO.IDADE.COVID19$Group.2 )

srag.estudo.ANO.IDADE.2020.COVID19 <- srag.estudo.ANO.IDADE.COVID19[ srag.estudo.ANO.IDADE.COVID19$Group.1 == 2020,]
srag.estudo.ANO.IDADE.2021.COVID19 <- srag.estudo.ANO.IDADE.COVID19[ srag.estudo.ANO.IDADE.COVID19$Group.1 == 2021,]


srag.estudo.OBITO.COVID19.ANO.IDADE <- aggregate( srag.estudo.obitocovid$conta, 
                                             by = list( srag.estudo.obitocovid$Ano, 
                                                        srag.estudo.obitocovid$srag.estudo.cuts), sum )

srag.estudo.OBITO.COVID19.ANO.IDADE$Group.2 <- as.character( srag.estudo.OBITO.COVID19.ANO.IDADE$Group.2 )

srag.estudo.OBITO.COVID19.ANO.IDADE.2020 <- srag.estudo.OBITO.COVID19.ANO.IDADE[srag.estudo.OBITO.COVID19.ANO.IDADE$Group.1 == 2020,]
srag.estudo.OBITO.COVID19.ANO.IDADE.2021 <- srag.estudo.OBITO.COVID19.ANO.IDADE[srag.estudo.OBITO.COVID19.ANO.IDADE$Group.1 == 2021,]

# ---

srag.estudo.OBITO.COVID19.ANO.IDADE.2020.total <- data.frame( Group.1 = 2020,
                                                              Group.2 = "Total",
                                                              x = sum( srag.estudo.OBITO.COVID19.ANO.IDADE.2020$x[1:9] ))

srag.estudo.OBITO.COVID19.ANO.IDADE.2020 <- rbind( srag.estudo.OBITO.COVID19.ANO.IDADE.2020, srag.estudo.OBITO.COVID19.ANO.IDADE.2020.total )


srag.estudo.OBITO.COVID19.ANO.IDADE.2021.total <- data.frame( Group.1 = 2021,
                                                              Group.2 = "Total",
                                                              x = sum( srag.estudo.OBITO.COVID19.ANO.IDADE.2021$x[1:9] ))

srag.estudo.OBITO.COVID19.ANO.IDADE.2021 <- rbind( srag.estudo.OBITO.COVID19.ANO.IDADE.2021, srag.estudo.OBITO.COVID19.ANO.IDADE.2021.total )

# ---

srag.estudo.ANO.IDADE.2020.COVID19.total <- data.frame( Group.1 = 2020,
                                                        Group.2 = "Total",
                                                        x = sum( srag.estudo.ANO.IDADE.2020.COVID19$x[1:9] ))

srag.estudo.ANO.IDADE.2020.COVID19 <- rbind( srag.estudo.ANO.IDADE.2020.COVID19, srag.estudo.ANO.IDADE.2020.COVID19.total )


srag.estudo.ANO.IDADE.2021.COVID19.total <- data.frame( Group.1 = 2021,
                                                        Group.2 = "Total",
                                                        x = sum( srag.estudo.ANO.IDADE.2021.COVID19$x[1:9] ))

srag.estudo.ANO.IDADE.2021.COVID19 <- rbind( srag.estudo.ANO.IDADE.2021.COVID19, srag.estudo.ANO.IDADE.2021.COVID19.total )






# ---

srag.estudo.ANO.IDADE.2020.total <- data.frame( Group.1 = 2020,
                                                        Group.2 = "Total",
                                                        x = sum( srag.estudo.ANO.IDADE.2020$x[1:9] ))


srag.estudo.ANO.IDADE.2020 <- rbind( srag.estudo.ANO.IDADE.2020, srag.estudo.ANO.IDADE.2020.total )

srag.estudo.ANO.IDADE.2021.total <- data.frame( Group.1 = 2021,
                                                Group.2 = "Total",
                                                x = sum( srag.estudo.ANO.IDADE.2021$x[1:9] ))


srag.estudo.ANO.IDADE.2021 <- rbind( srag.estudo.ANO.IDADE.2021, srag.estudo.ANO.IDADE.2021.total )



tab.2020 <- data.frame( idade = srag.estudo.OBITO.COVID19.ANO.IDADE.2020$Group.2,
                        casos.obitos.covid19 = srag.estudo.OBITO.COVID19.ANO.IDADE.2020$x,
                        casos.covid.total = srag.estudo.ANO.IDADE.2020.COVID19$x,
                        casos.total = srag.estudo.ANO.IDADE.2020$x,
                        perc1 = round( srag.estudo.OBITO.COVID19.ANO.IDADE.2020$x / srag.estudo.ANO.IDADE.2020.COVID19$x * 100, 3 ),
                        perc2 = round( srag.estudo.ANO.IDADE.2020.COVID19$x / srag.estudo.ANO.IDADE.2020$x * 100, 3 ) )

tab.2021 <- data.frame( idade = srag.estudo.OBITO.COVID19.ANO.IDADE.2021$Group.2,
                        casos.obitos.covid19 = srag.estudo.OBITO.COVID19.ANO.IDADE.2021$x,
                        casos.covid.total = srag.estudo.ANO.IDADE.2021.COVID19$x,
                        casos.total = srag.estudo.ANO.IDADE.2021$x,
                        perc1 = round( srag.estudo.OBITO.COVID19.ANO.IDADE.2021$x / srag.estudo.ANO.IDADE.2021.COVID19$x * 100, 3 ),
                        perc2 = round( srag.estudo.ANO.IDADE.2021.COVID19$x / srag.estudo.ANO.IDADE.2021$x * 100, 3 ) )

# ---











# POR REGIAO

srag.estudo.semana.conta.sexo.REGIAO <- aggregate( srag.estudo$conta, by = list( srag.estudo$semana, srag.estudo$REGIAO, srag.estudo$CS_SEXO), sum )
srag.estudo.semana.conta.sexo.REGIAO$Group.1 <- as.character( srag.estudo.semana.conta.sexo.REGIAO$Group.1 )
srag.estudo.semana.conta.sexo.REGIAO$Group.1 <- ymd( srag.estudo.semana.conta.sexo.REGIAO$Group.1 )

names(srag.estudo.semana.conta.sexo.REGIAO) <- c("Semana","Região","Sexo","Casos")



png( "figura2.2.png", width = 10000, height = 6000, res = 600 )

pp2.2 <- ggplot(srag.estudo.semana.conta.sexo.REGIAO, aes(x = Semana, y = Casos , fill = Sexo )) +
  geom_bar(stat = "identity", width = 5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_date(breaks = as.Date( semana.epi.cuts[1:99], format = c("%d/%m/%Y") )) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )  +
  xlab("Epidemiological week") +
  ylab("Cases") +
  theme(strip.text.x = element_text(size = 18 )) +
  theme( axis.title.x = element_text(size = 18),
         axis.title.y = element_text(size = 18) )  +
  theme( axis.text=element_text(size=14),
         axis.title=element_text(size=14) ) +
  
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black") ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values=c("gray60","gray20")) +
  theme(legend.title=element_text(size=22), legend.text=element_text(size=20)) +
  facet_wrap( ~ Região ,ncol = 1, scales = "free_y") +
  theme(legend.position="bottom")

plot(pp2.2)

dev.off()






srag.estudo.semana.conta.UF <- aggregate( srag.estudo$conta, by = list( srag.estudo$semana, srag.estudo$SG_UF_NOT ), sum )



ggplot(srag.estudo.semana.conta.UF, aes(x=Group.1, y=x)) +
  geom_line()+
  geom_point() +
  facet_wrap( ~ Group.2 , ncol = 4 )


# ------------------------------------------------- Diferencia DATA INTER e DATA 1 sintomas

srag.estudo$DT_INTERNA

SELE.DATA.INTERNA <- srag.estudo[srag.estudo$HOSPITAL == "Sim",]


diff.data <- SELE.DATA.INTERNA$DT_INTERNA - SELE.DATA.INTERNA$DT_SIN_PRI

diff.data.df <- data.frame( DT_INTERNA = SELE.DATA.INTERNA$DT_INTERNA,
                            DT_SIN_PRI = SELE.DATA.INTERNA$DT_SIN_PRI,
                            diff.data = as.numeric(diff.data),
                            hosp = SELE.DATA.INTERNA$HOSPITAL,
                            UF.NOT = SELE.DATA.INTERNA$SG_UF_NOT,
                            FINAL = SELE.DATA.INTERNA$CLASSI_FIN )
table( diff.data.df$diff.data <0 )

diff.data.df.positivos.60 <- diff.data.df[diff.data.df$diff.data >= 0 &
                                            diff.data.df$diff.data <= 60  ,]



boxplot( diff.data.df.positivos.60$diff.data ~ diff.data.df.positivos.60$UF.NOT )


#
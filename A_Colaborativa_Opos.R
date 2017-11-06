#Cambio de directorio de trabajo
setwd("C:/Users/LC/Documents/Master/R/U3/A_Colaborativa")

#Creación de directorio de datos
if(!file.exists("./datos")) {dir.create("./datos")}

#Descarga de dataset de subvenciones de la Junta de Castilla y León
fileUrl <- "http://www.datosabiertos.jcyl.es/web/jcyl/risp/es/sector-publico/convocatoriaspublicas/1284165772128.csv"
download.file(fileUrl,destfile="./datos/oposiciones.csv",method="libcurl")

#Guardar fecha descarga
fechaDescarga <- date()

#Guardar en subven(dataframe) el contenido del dataset, descartando la primera linea
con <- file("./datos/oposiciones.csv","r")
opos <- read.csv2(con,skip=1,skipNul = FALSE)
close(con)

#Sustituir todos los valores "" por NA
is.na(opos) <- opos==''

#Guardado de fecha de descarga en dataframe
opos$FechaCarga <- date()

#Renombrado de columnas

colnames(opos)[3] <- "TipoConvocatoria"
colnames(opos)[6] <- "NumeroPlazasTurnoLibre"
colnames(opos)[7] <- "NumeroPlazasMinusvalia"
colnames(opos)[8] <- "NumeroPlazasPromociónInterna"
colnames(opos)[9] <- "NumeroPlazasPromociónInterna.Discapacidad"
colnames(opos)[18] <- "URLDocumentosAsociados"
colnames(opos)[32] <- "URLContenido"

#Convertir campos de fecha potenciales a tipo fecha

opos$FechaBOCYL <- as.character(opos$FechaBOCYL)
opos$FechaBOCYL <- as.Date(opos$FechaBOCYL,"%Y%m%d")
opos$Fecha.de.inicio <- as.character(opos$Fecha.de.inicio)
opos$Fecha.de.inicio <- as.Date(opos$Fecha.de.inicio,"%Y%m%d")
opos$FechaFinalizacion <- as.character(opos$FechaFinalizacion)
opos$FechaFinalizacion <- as.Date(opos$FechaFinalizacion,"%Y%m%d")
opos$ultimaActualizacion <- as.character(opos$ultimaActualizacion)
opos$ultimaActualizacion <- as.Date(opos$ultimaActualizacion,"%Y%m%d")

#Eliminación de campos de texto con poca posibilidad de análisis o vacios

opos$DocumentosTasas <- NULL
opos$Solicitud <- NULL
opos$Solicitud.1 <- NULL
opos$NombresDocumentos <- NULL
opos$NombresDocumentos <- NULL
opos$Plazo.de.presentación <-NULL
opos$Requisitos.necesarios <- NULL
opos$PROCEDIMIENTO.DE.SELECCIÓN <- NULL
opos$Información.adicional <- NULL
opos$Identificador <- NULL
opos$X <- NULL

#Reducción de texto en el campo "Titulo" por información redundante en otros camos (p.e. Organismo.gestor)
opos$Título <- toupper(gsub(" \\(.*$", "", opos$Título ))

#Reducción de texto en el campo "Clasificador" por información redundante en otros camos (p.e. FechaBOCYL)
opos$Clasificador <- toupper(gsub(",.*$", "", opos$Clasificador ))
opos$Clasificador <- toupper(gsub(" /.*$", "", opos$Clasificador ))

#Codificación numérica del campo tipo para facilitar su análisis con bucle que genera id subrogado
valores<-summary(opos$Tipo)
cont <- 1
while(cont <= length(valores))
{
  cat(paste(cont,"-",names(valores[cont]),'\n'))
  opos$Tipo <- gsub(names(valores[cont]),cont,opos$Tipo)
  cont = cont + 1
}
opos<-transform(opos,Tipo = as.numeric(Tipo))


#Borrado de info no numérica de campo NumeroPlazasMinusvalia
opos$NumeroPlazasMinusvalia <- gsub("[^0-9]", "", opos$NumeroPlazasMinusvalia)
opos$NumeroPlazasMinusvalia <- as.numeric(opos$NumeroPlazasMinusvalia)
opos<-transform(opos,NumeroPlazasMinusvalia = as.numeric(NumeroPlazasMinusvalia))

#Borrado de info no numérica de campo NumeroPlazasPromociónInterna.Discapacidad
opos$NumeroPlazasPromociónInterna.Discapacidad <- gsub("[^0-9]", "", opos$NumeroPlazasPromociónInterna.Discapacidad)
opos$NumeroPlazasPromociónInterna.Discapacidad <- as.numeric(opos$NumeroPlazasPromociónInterna.Discapacidad)
opos<-transform(opos,NumeroPlazasPromociónInterna.Discapacidad = as.numeric(NumeroPlazasPromociónInterna.Discapacidad))

#Estandarización del campo numérico Tasas
#Sustituir "No hay tasas" por 0
opos$Tasas <- gsub("No hay tasas",0,opos$Tasas)
#Convertir números "potenciales" en números
opos$Tasas <- gsub(",",".",opos$Tasas)
opos$Tasas <- gsub("[^0-9\\.]", "", opos$Tasas) 
opos$Tasas <- as.numeric(opos$Tasas)
opos<-transform(opos,Tasas = as.numeric(Tasas))


#Desdoble de campo NumeroCuentaCorriente en dos campos, Banco y NumeroCuentaCorriente  
#Creación de campo Banco
opos$Banco <- gsub( ":.*$", "", opos$NumeroCuentaCorriente )
#Limpieza y estandarización del campo cuenta corriente
opos$NumeroCuentaCorriente <- gsub("[^0-9]", "", opos$NumeroCuentaCorriente)
opos$NumeroCuentaCorriente <- paste(substr(opos$NumeroCuentaCorriente,1,4),"-",substr(opos$NumeroCuentaCorriente,5,8),"-",substr(opos$NumeroCuentaCorriente,9,10),"-",substr(opos$NumeroCuentaCorriente,10,19),sep="")
opos$NumeroCuentaCorriente <- gsub("NA-NA-NA-NA", NA, opos$NumeroCuentaCorriente) 

#Al ser un campo de texto, se desdobla en varios indicadores en base a si contienen ciertas palabras (DISCAPACITADO, FAMILIA NUMEROSA...)

library("stringr", lib.loc="~/R/win-library/3.4")
opos$Devolución.Exención.Tasas.Discapacitados <- str_detect(toupper(opos$Devolución.Exención.Tasas),"DISCAPACI")
opos$Devolución.Exención.Tasas.Fam.numerosa <- str_detect(toupper(opos$Devolución.Exención.Tasas),"FAMILIA.*.NUMEROSA")
opos$Devolución.Exención.Tasas.Prom.interna <- str_detect(toupper(opos$Devolución.Exención.Tasas),"PROMOCI.*.INTERN")   
#Se elimina el campo origen
opos$Devolución.Exención.Tasas <- NULL

#Tipificación de campo LugarDePresentacion mediante función
codificar_lugar <- function(cadena)
{
  if(str_detect(toupper(cadena),"PRESENCIAL") == TRUE) {"PRESENCIAL"} 
  else if (str_detect(toupper(cadena),"WWW.") == TRUE) {"WEB"} 
  else NA
  
}
opos$LugarDePresentacion <- as.character(opos$LugarDePresentacion)
opos["LugarDePresentacion"][is.na(opos["LugarDePresentacion"])] <- 0
opos$LugarDePresentacion <- lapply(opos$LugarDePresentacion,codificar_lugar)
opos$LugarDePresentacion <- gsub("NA", NA, opos$LugarDePresentacion)
library(RODBC)
library(neuralnet)
library(readxl)
library(data.table)


setwd("C:/Users/D3I6/Desktop/ZARATE_NN")   #Escribir path correcto.

EMR_Mm <- c(300,3000)
MFI_Mm <- c(1,24)
IMP_Mm <- c(0,3)


EMRNN_rds <- readRDS("Modelos/EMR_NN.rds")
IMPNN_rds <- readRDS("Modelos/IMP_NN.rds")
MFINN_rds <- readRDS("Modelos/MFI_NN.rds")


myconn <-odbcConnect("Infoplus.21", uid="ACONSIGLIO", pwd="password")

p=0
filtro <- 1-(1-0.80)**(60/3600)
filtro1 <- 1-(1-0.99)**(60/3600)
filtro2 <- 1-(1-0.99)**(60/3600)
predicfil <- 0
predicfil1 <- 0
predicfil2 <- 1973

while (p<1)
{
  
  #Datos EMR-----------------------------------------------------
  a <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TC241-1"')
  b <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FI210-19"')
  c <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TC221-1"')
  d <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TI221-6"')
  e <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TI222-3"')
  f <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "PC221-3"')
  g <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "PC225-4"')
  h <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "LC221-2"')
  i <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "LI222-2"')
  j <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "CALC-16"')
  k <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "CALC-22"')
  
  datos <- data.frame(TC241.1 = c(a$IP_INPUT_VALUE,150,250),
                      FI210.19 = c(b$IP_INPUT_VALUE,0,4000),
                      TC221.1 = c(c$IP_INPUT_VALUE,200,300),
                      TI221.6 = c(d$IP_INPUT_VALUE,150,350),
                      TI222.3 = c(e$IP_INPUT_VALUE,100,300),
                      PC221.3 = c(f$IP_INPUT_VALUE,0,800),
                      PC225.4 = c(g$IP_INPUT_VALUE,0,40),
                      LC221.2 = c(h$IP_INPUT_VALUE,0,100),
                      LI222.2 = c(i$IP_INPUT_VALUE,0,100),
                      CALC.16 = c(j$IP_INPUT_VALUE,0,100),
                      CALC.22 = c(k$IP_INPUT_VALUE,0,10000))
  
  normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
  datosN <- as.data.frame(lapply(datos, normalize))
  prediccion <- compute(EMRNN_rds,datosN[1,])
  predicted=prediccion$net.result * abs(diff(range(EMR_Mm)) + min(EMR_Mm))
  predicted <- (round(predicted, digits = 0))
  
  if(predicfil == 0)
  {
    predicfil <- predicted
  }else
        
        predicfil <- predicted * filtro + predicfil * (1- filtro)
        
  
  
  
  
  update_dato = ""
  update_dato = paste("UPDATE \"CALC-100\" SET ip_input_value = ",predicfil,", Qstatus(ip_input_value)='good';")
  escribir = sqlQuery(myconn,update_dato)


  #Datos MFI-----------------------------------------------------
  
  a <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TC241-1"')
  b <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TC210-3"')
  c <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC210-2"')
  d <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "WC204-32"')
  e <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC204-39"')
  f <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "WC210-1"')
  g <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC231-3"')
  h <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC301-8"')
  i <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC404-1"')
  j <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC239-1"')
  
  
 
  
  datos1 <- data.frame(TC241.1 = c(a$IP_INPUT_VALUE,150,250),
                       TC210.3 = c(b$IP_INPUT_VALUE,100,200),
                       FC210.2 = c(c$IP_INPUT_VALUE,0,900),
                       WC204.32 = c(d$IP_INPUT_VALUE,0,25),
                       FC204.39 = c(e$IP_INPUT_VALUE,0,15000),
                       WC210.1 = c(f$IP_INPUT_VALUE,0,14),
                       FC231.3 = c(g$IP_INPUT_VALUE,0,900),
                       FC301.8 = c(h$IP_INPUT_VALUE,0,240),
                       FC404.1 = c(i$IP_INPUT_VALUE,0,40),
                       FC239.1 = c(j$IP_INPUT_VALUE,0,120))
  
  normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
  datosN1 <- as.data.frame(lapply(datos1, normalize))
  prediccion1 <- compute(MFINN_rds,datosN1[1,])
  predicted1=prediccion1$net.result * abs(diff(range(MFI_Mm)) + min(MFI_Mm))
  predicted1 <- (round(predicted1, digits = 1))
  
  if(predicfil1 == 0)
  {
    predicfil1 <- predicted1
  }else
    
    predicfil1 <- predicted1 * filtro1 + predicfil1 * (1- filtro1)
  
  
  
  #Escribir datos en Infoplus MFI
  update_dato = ""
  update_dato = paste("UPDATE \"CALC-101\" SET ip_input_value = ",predicfil1,", Qstatus(ip_input_value)='good';")
  escribir = sqlQuery(myconn,update_dato)    
  
  
  #Datos IMP--------------------------------------------------------------
  a <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC204-39"')
  
  b<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TC204-1"')
  
  c<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC204-4"')
  
  d<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TC241-1"')
  
  e<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "SC205-2"')
  
  f<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TC210-3"')
  
  g<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC210-2"')
  
  h<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TI221-6"')
  
  i<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TI222-3"')
  
  j<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC251-12"')
  
  k<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "WC204-32"')
  
  l<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "WC210-1"')
  
  m<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC301-8"')
  
  n<-sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC404-1"')
  
  o<-sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC239-1"')
  
  
  
  
  
  datos1 <- data.frame(FC204.39 = c(a$IP_INPUT_VALUE,0,15000),
                       TC204.1 = c(b$IP_INPUT_VALUE,0,350),
                       FC204.4 = c(c$IP_INPUT_VALUE,0,10000),
                       TC241.1 = c(d$IP_INPUT_VALUE,150,250),
                       SC205.2 = c(e$IP_INPUT_VALUE,0,40),
                       TC210.3 = c(f$IP_INPUT_VALUE,0,900),
                       FC210.2 = c(g$IP_INPUT_VALUE,0,900),
                       TI221.6 = c(h$IP_INPUT_VALUE,150,350),
                       TI222.3 = c(i$IP_INPUT_VALUE,100,300),
                       FC251.12 = c(j$IP_INPUT_VALUE,0,250),
                       WC204.32 = c(k$IP_INPUT_VALUE,0,25),
                       WC210.1 = c(l$IP_INPUT_VALUE,0,14),
                       FC301.8 = c(m$IP_INPUT_VALUE,0,240),  
                       FC404.1 = c(n$IP_INPUT_VALUE,0,40),
                       FC239.1 = c(o$IP_INPUT_VALUE,0,120))

  
  normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
  datosN1 <- as.data.frame(lapply(datos1, normalize))
  prediccion2 <- neuralnet::compute(IMPNN_rds,datosN1[1,])
  predicted2=prediccion2$net.result * abs(diff(range(IMP_Mm)) + min(IMP_Mm))
  
  if(c < 2800)
  {
    predicted2 <- 0
  }
  
  if(predicfil2 == 1973)
  {
    predicfil2 <- predicted2
  }else
    
    predicfil2 <- predicted2 * filtro2 + predicfil2 * (1- filtro2)
  
  
  
  
  
  
  update_dato = ""
  update_dato = paste("UPDATE \"CALC-102\" SET ip_input_value = ",predicfil2,", Qstatus(ip_input_value)='good';")
  escribir = sqlQuery(myconn,update_dato)
  
Sys.sleep(10)
}


close(myconn)


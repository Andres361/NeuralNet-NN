library(neuralnet)
library(readxl)
library(RODBC)



EMR_Mm <- c(300,3000)
setwd("C:/Users/Desktop/NN/Modelos")

EMRNN_rds <- readRDS("C:/Users/Desktop/NN/Modelos/EMR_NN.rds")

myconn <-odbcConnect("Infoplus.21", uid="Usernames", pwd="password")

#Datos EMR-----------------------------------------------------
a <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TC241-1"')
b <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TC221-1"')
c <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TI221-6"')
d <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TI222-3"')
e <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "PC221-3"')
f <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "PC225-4"')
g <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "LC221-2"')
h <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "LI222-2"')
i <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "CALC-16"')
j <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "CALC-22"')

datos <- data.frame(TC241.1 = c(a$IP_INPUT_VALUE,150,250),
                    TC221.1 = c(b$IP_INPUT_VALUE,200,300),
                    TI221.6 = c(c$IP_INPUT_VALUE,150,350),
                    TI222.3 = c(d$IP_INPUT_VALUE,100,300),
                    PC221.3 = c(e$IP_INPUT_VALUE,0,800),
                    PC225.4 = c(f$IP_INPUT_VALUE,0,40),
                    LC221.2 = c(g$IP_INPUT_VALUE,0,100),
                    LI222.2 = c(h$IP_INPUT_VALUE,0,100),
                    CALC.16 = c(i$IP_INPUT_VALUE,0,100),
                    CALC.22 = c(j$IP_INPUT_VALUE,0,10000))

normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
datosN <- as.data.frame(lapply(datos, normalize))
prediccion <- compute(EMRNN_rds,datosN[1,])
predicted=prediccion$net.result * abs(diff(range(EMR_Mm)) + min(EMR_Mm))

update_dato = ""
update_dato = paste("UPDATE \"CALC-100\" SET ip_input_value = ",predicted,", Qstatus(ip_input_value)='good';")
escribir = sqlQuery(myconn,update_dato)

close(myconn)





library(neuralnet)
library(readxl)
library(RODBC)



FLU_Mm <- c(1,24)
setwd("C:/Users/D3I6/Desktop/ZARATE_NN/Modelos")

FLUNN_rds <- readRDS("C:/Users/Desktop/NN/Modelos/MFI_NN.rds")

myconn <-odbcConnect("Infoplus.21", uid="Usernames", pwd="password")
#Datos MFI--------------------------------------------------------------
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
                     FC404.1 = c(f$IP_INPUT_VALUE,0,40),
                     FC239.1 = c(f$IP_INPUT_VALUE,0,120))

normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
datosN1 <- as.data.frame(lapply(datos1, normalize))
prediccion1 <- neuralnet::compute(FLUNN_rds,datosN1[1,])
predicted1=prediccion1$net.result * abs(diff(range(FLU_Mm)) + min(FLU_Mm))
print(predicted1)

update_dato = ""
update_dato = paste("UPDATE \"CALC-101\" SET ip_input_value = ",predicted1,", Qstatus(ip_input_value)='good';")
escribir = sqlQuery(myconn,update_dato)

close(myconn)





library(RODBC)
library(neuralnet)
library(readxl)
library(data.table)





#LEER DATOS, MAXIMOS Y MINIMOS-----------------------------
FLU_Mm <- c(1,24)
setwd("C:/Users/Desktop/NN")
datos <- data.frame(read_excel("Datos/MFI.xlsx", sheet = "Hoja2"))


#NORMALIZACION DE DATOS
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
datosN <- as.data.frame(lapply(datos, normalize))

#Training and Test Data
trainset <- datosN[1:462,] 
testset <- datosN[463:578,]

	

net <- neuralnet(MFI ~ TC241.1 + TC210.3 + FC210.2 + FC204.39 + FC231.3 + FC301.8 + FC404.1 + WC204.32 + WC210.1 + FC239.1,
                 data=trainset,
                 act.fct = "logistic",
                 hidden =c(15,10,5), 
                 stepmax = 1e+7,
                 linear.output=T,
                 err.fct="sse",
                 lifesign = 'full',
                 threshold=0.01,
                 algorithm = "rprop+")


net$result.matrix[1]




results <- data.frame(actual = trainset$MFI, prediction = net$net.result)
print(results)





#Guardar neurona en formato RDS-----------------------------
GuardarMFI <- readline(prompt="Desea Guardar rds (y/n): ")
if (GuardarMFI == "y") 
saveRDS(net, file = "Modelos/MFI_NN.rds", ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)


#Usar Neurona en formato RDS--------------------------------
CargarMFI <- readline(prompt="Desea Cargar rds (y/n): ")
if (CargarMFI == "y")  
FLUNN_rds <- readRDS("Modelos/MFI_NN.rds")
#plot(FLUNN_rds)


#MODELO VALIDACION------------------------------------------
output <- compute(net, (testset[,1:11]))
output <- compute(FLUNN_rds, (testset[,1:11]))
results <- data.frame(actual = (testset$MFI * abs(diff(range(FLU_Mm))) + min(FLU_Mm)), prediction = (output$net.result * abs(diff(range(FLU_Mm))) + min(FLU_Mm)))
results
tabla <- data.frame(results)
testset$MFI

#INFERENCIA DATOS NORMALIZADOS------------------------------
TestLine <- 11
testset1 <- datosN[TestLine,]
prueba <- compute(FLUNN_rds, testset1[,1:11])
predicted=prueba$net.result * abs(diff(range(FLU_Mm))) + min(FLU_Mm)
print(data.frame(actual = datos[TestLine,11],Predicha = predicted ))







myconn <-odbcConnect("Infoplus.21", uid="ACONSIGLIO", pwd="password")
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

#k = (d / e)

#l = f / (g + i + c - j)


datos1 <- data.frame(TC241.1 = c(a$IP_INPUT_VALUE,150,250),
                     TC210.3 = c(b$IP_INPUT_VALUE,100,200),
                     #k = c(k$IP_INPUT_VALUE,0,1),
                     #l = c(l$IP_INPUT_VALUE,0,1),
                     FC210.2 = c(c$IP_INPUT_VALUE,0,900),
                     FC204.39 = c(d$IP_INPUT_VALUE,0,15000),
                     FC231.3 = c(e$IP_INPUT_VALUE,0,1),
                     FC301.8 = c(f$IP_INPUT_VALUE,0,240),
                     FC404.1 = c(g$IP_INPUT_VALUE,0,40),
                     WC204.32 = c(h$IP_INPUT_VALUE,0,25),
                     WC210.1 = c(i$IP_INPUT_VALUE,0,14),
                     FC239.1 = c(j$IP_INPUT_VALUE,0,120))  





  


normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
datosN1 <- as.data.frame(lapply(datos1, normalize))
prediccion1 <- neuralnet::compute(FLUNN_rds,datosN1[1,])
predicted1=prediccion1$net.result * abs(diff(range(FLU_Mm)) + min(FLU_Mm))
print(round(predicted1, digits = 2))





Sys.sleep(30)
#}

close(myconn)


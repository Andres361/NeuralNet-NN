library(neuralnet)
library(readxl)
library(data.table)
library(caret)

#LEER DATOS, MAXIMOS Y MINIMOS-----------------------------
EMR_Mm <- c(300,3000)
setwd("C:/Users/Desktop/NN")
datos <- data.frame(read_excel("Datos/EMR.xlsx", sheet = "Hoja1"))

#NORMALIZACION DE DATOS
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
maxmindf <- as.data.frame(lapply(datos, normalize))

#Training and Test Data
trainset <- maxmindf[1:280, ] 
testset <- maxmindf[281:358,]
testset <- na.omit(testset)

#NEURAL NETWORK-------------------------------------------
net <- neuralnet(Z.EMR ~ TC241.1 + FI210.19 + TC221.1 + TI221.6 + TI222.3 + PC221.3 + PC225.4 + LC221.2 + LI222.2 + CALC.16 + CALC.22, 
                 data=trainset,
                 act.fct = "logistic",
                 hidden =c(15,10,8), 
                 stepmax = 1e+7,
                 linear.output=T,
                 err.fct="sse",
                 lifesign = 'full',
                 threshold=0.01,
                 algorithm = "rprop+")
net$result.matrix[1]
#plot(net)
#Guardar neurona en formato RDS-----------------------------
GuardarEMR <- readline(prompt="Desea Guardar rds (y/n): ")
if (GuardarEMR == "y") 
saveRDS(net, file = "Modelos/EMR_NN.rds", ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)

#Usar Neurona en formato RDS--------------------------------
CargarEMR <- readline(prompt="Desea Cargar rds (y/n): ")
if (CargarEMR == "y") 
EMRNN_rds <- readRDS("Modelos/EMR_NN.rds")



#MODELO VALIDACION-----------------------------------------
#output <- compute(net, (testset[,1:10]))
output <- compute(EMRNN_rds, (testset[,1:11]))
results <- data.frame(actual = (testset$Z.EMR * abs(diff(range(EMR_Mm))) + min(EMR_Mm)), prediction = (output$net.result * abs(diff(range(EMR_Mm))) + min(EMR_Mm)))
tabla <- as.data.frame(results)
testset$Z.EMR
round(results, digits = 0)



#INFERENCIA DATOS NORMALIZADOS-------------------
TestLine <- 247
testset1 <- datos[TestLine,]
prueba <- compute(EMRNN_rds, testset1[,1:12])
predicted=prueba$net.result * abs(diff(range(EMR_Mm))) + min(EMR_Mm)
print(data.frame(actual = datos[TestLine,12],Predicha = predicted ))





myconn <-odbcConnect("Infoplus.21", uid="ACONSIGLIO", pwd="password")


  
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

print(round(predicted, digits = 0))




Sys.sleep(30)


close(myconn)



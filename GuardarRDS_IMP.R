library(neuralnet)
library(readxl)
library(data.table)


#LEER DATOS, MAXIMOS Y MINIMOS-----------------------------
IMP_Mm <- c(0,3)
setwd("C:/Users/Desktop/NN")
datos <- data.frame(read_excel("Datos/IMPACTO.xlsx", sheet = "Hoja5"))


#NORMALIZACION DE DATOS
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
datosN <- as.data.frame(lapply(datos, normalize))

#Training and Test Data
trainset <- datosN[1:183, ] 
testset <- datosN[184:227,]


#NEURAL NETWORK-------------------------------------------
 
net <- neuralnet(Z.IMPACTO ~ FC204.39 + TC204.1 + FC204.4 +TC241.1 + SC205.2 + TC210.3 + FC210.2 + TI221.6 + TI222.3 + FC251.12 + WC204.32 + WC210.1 +	FC301.8 +	FC404.1 +	FC239.1, 

                 data=trainset,
                 act.fct = "logistic",
                 hidden =c(20,15,8), 
                 stepmax = 1e+7,
                 linear.output=T,
                 err.fct="sse",
                 lifesign = 'full',
                 threshold=0.01,
                 constant.weights = NULL,
                 algorithm = "rprop+")
net$result.matrix[1]



#Guardar neurona en formato RDS-----------------------------
GuardarIMP <- readline(prompt="Desea Guardar rds (y/n): ")
if (GuardarIMP == "y") 
saveRDS(net, file = "Modelos/IMP_NN.rds", ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)

#Usar Neurona en formato RDS--------------------------------
CargarIMP <- readline(prompt="Desea Cargar rds (y/n): ")
if (CargarIMP == "y") 
IMPNN_rds <- readRDS("Modelos/IMP_NN.rds")


#MODELO VALIDACION-----------------------------------------
output <- compute(IMPNN_rds, (testset[,1:16]))
results <- data.frame(actual = (testset$Z.IMPACTO * abs(diff(range(IMP_Mm))) + min(IMP_Mm)), prediction = (output$net.result * abs(diff(range(IMP_Mm))) + min(IMP_Mm)))
results
tabla <- data.frame(results)
testset$Z.IMPACTO

#INFERENCIA DATOS NORMALIZADOS-------------------
TestLine <- 14
testset1 <- datos[TestLine,]
prueba <- compute(IMPNN_rds, testset1[,1:16])
predicted=prueba$net.result * abs(diff(range(IMP_Mm))) + min(IMP_Mm)
print(data.frame(actual = datos[TestLine,16],Predicha = predicted ))




myconn <-odbcConnect("Infoplus.21", uid="ACONSIGLIO", pwd="password")


  myconn <-odbcConnect("Infoplus.21", uid="ACONSIGLIO", pwd="password")

#Datos MFI--------------------------------------------------------------
a <- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC204-39"')
  
b<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "TC204-1"')
  
c<- sqlQuery(myconn, 'SELECT IP_INPUT_VALUE FROM "FC204-4"')
if (c < 2800)
{
  c <- 0
}
  
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
prediccion1 <- neuralnet::compute(IMPNN_rds,datosN1[1,])
predicted1=prediccion1$net.result * abs(diff(range(IMP_Mm)) + min(IMP_Mm))
print(round(predicted1, digits = 2))



update_dato = ""
update_dato = paste("UPDATE \"CALC-102\" SET ip_input_value = ",predicted1,", Qstatus(ip_input_value)='good';")
escribir = sqlQuery(myconn,update_dato)

Sys.sleep(30)
close(myconn)
#}










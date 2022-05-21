rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui debe cambiar los parametros por los que desea probar

param_basicos  <- list( "cp"=          -0.5,  #complejidad minima
                        "minsplit"=   1500,     #minima cantidad de registros en un nodo para hacer el split
                        "minbucket"=   500,     #minima cantidad de registros en una hoja
                        "maxdepth"=    6 )    #profundidad máxima del arbol

#va la matriz de perdida,  por columnas
matriz_perdida  <- matrix(c( 0, 120, 0.5,   1,0,1,   0.5, 120 ,0), nrow = 3)

#Aqui se debe poner la carpeta de SU computadora local
setwd("C:/Users/Administrator/Desktop/datamining")  #Establezco el Working Directory

#cargo los datos de 202011 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./datasets/paquete_premium_202011.csv")

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart("clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data = dtrain,
                 xval=0,
                 parms= list( loss= matriz_perdida),
                 control= param_basicos )  #aqui van los parametros del arbol

#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#Ahora aplico al modelo  a los datos de 202101  y genero la salida para kaggle

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, dapply , type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/60
dapply[ , Predicted  := as.numeric(prob_baja2 > 1/60) ]

#genero un dataset con las dos columnas que me interesan
entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./labo/exp/" ) 
dir.create( "./labo/exp/KA2021/" ) 

fwrite( entrega, 
        file= "./labo/exp/KA2021/K252_001.csv", 
        sep= "," )

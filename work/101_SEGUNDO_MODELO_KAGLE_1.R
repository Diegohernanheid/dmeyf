#Arbol elemental con libreria  rpart
require("data.table")
require("rpart")

#Establezco el Working Directory
#Aqui se debe poner la carpeta de la computadora local
setwd("C:/DIEGO_/MASTER_DM/2_CUAT_2021/DM_ECON_FINANZAS")  

#cargo los datos de 202009 que es donde voy a ENTRENAR el modelo
dtrain  = fread("./datasetsOri/paquete_premium_202009.csv")

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")
View(dtrain)  #Showing 1 to 11 of 235,354 entries, 158 total columns
View(dapply)  #Showing 1 to 11 of 238,986 entries, 158 total columns

#genero el modelo
# modelo  <- rpart("clase_ternaria ~ .",
  #              data = dtrain,
   #              xval=0,
    #             cp=        -0.3, 
     #            minsplit=  80,
      #           minbucket=  1,
       #        maxdepth=   8 )

### MODELO2: REGRESION LINEAL:
### NO LO USO, POR AHORA, PORQUE REQUIERE TRATAR PREVIAMENTE LOS N/A !!!!!
#library(MASS)
#formula_regresoras = formula(clase_ternaria ~ .)
#modelo2  <- lda(formula_regresoras, dtrain)


### MODELO 2 ARBOLES (O ARBOL + UN SEGUNDO ALGORITMO)!!!!!:
## PRIMERO UNO VARIABLES CATEGORICAS LABEL, PARA DE 3 ME QUEDEN 2:
# SEPARO POR UN LADO "CONTINUA" Y POR EL OTRO "BAJA+2" Y "BAJA+1" (LAS UNIFICO):
# ESTE TRABAJO LO REALIZO SOBRE EL DATASET DE ENTRENAMIENTO ORIGINAL COMPLETO (DATOS SEP-2020), A SABER:
# dtrain  = fread("./datasetsOri/paquete_premium_202009.csv")
# ENTONCES, GENERO NUEVAS LABELS (UNIFICANDO BAJA+1 Y BAJA+2 EN NUEVA VARIABLE "NO CONTINUA"):
dtrain2 = dtrain
dtrain2$CLASE_BINARIA_BAJA1y2_[dtrain2$clase_ternaria=="CONTINUA"] = 0
dtrain2$CLASE_BINARIA_BAJA1y2_[dtrain2$clase_ternaria!="CONTINUA"] = 1
dtrain2$CLASE_BINARIA_BAJA1y2_ = as.factor(dtrain2$CLASE_BINARIA_BAJA1y2_)
View(dtrain2)
### SEGUNDO: VOY A ENTRENAR UN PRIMER MODELO (ARBOL) SOLO PARA QUE ME SEPARE CONTINUA DE BAJAS (USANDO DE LABEL LA NUEVA VARIABLE BINARIA QUE CREAMOS ANTES)
## PERO ANTES, DEBERÉ SACAR LA COLUMNA DE LA VARIABLE TERNARIA, PARA NO SOBREENTRENAR AL MODELO CON INFO DUPLICADA:
dtrain3 = dtrain2[, -c(158)]
View(dtrain3) #PERFECTO!!!
### AHORA SI PUEDO ENTRENAR EL PRIMER MODELO, PARA SEPARAR CONTINUA DE BAJAS:
### DEFINO Y ENTRENO PRIMER MODELO: ARBOL SIMPLE:
names(dtrain3)
modelo_1  <- rpart("CLASE_BINARIA_BAJA1y2_ ~ .",
              data = dtrain3,
             xval=0,
            cp=        -0.3, 
           minsplit=  80,
          minbucket=  1,
          maxdepth=   8 )
### AHORA DEBO SEPARAR LAS SALIDAS QUE DIERON BAJAS DE ESTE PRIMER MODELO 
# Y USARLAS EN UN SEGUNDO MODELO PARA DIFERENCIAR ENTRE BAJAS+1 Y BAJAS+2:
###DEBO USAR/APLICAR AL dapply PARA LUEGO SEPARAR LAS BAJAS!!!!! XXXXXXX
## PERO ANTES DEBO TRANSFORMAR EL dapply AL MISMO FORMATO DE LOS DATOS DE ENTRENAMIENTO:
dapply_1 = dapply
dapply_1$CLASE_BINARIA_BAJA1y2_ = NA 
dapply_1 = dapply_1[, -c(158)]
View(dapply_1) # PERFECTO!
### AHORA SI APLICO EL PRIMER MODELO AL dapply_1:
####aplico al modelo  a los datos de 202011
# prediccion  <- predict( modelo_1, dapply , type = "prob") #aplico el modelo
prediccion  <- predict( modelo_1, dapply_1 , type = "prob") #aplico el modelo
#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 
View(prediccion)

dapply_1[ , prob_baja := prediccion[, "1"] ]
dapply_1[ , Predicted  := as.numeric(prob_baja > 0.025) ]
### AHORA QUE YA PRODUJE LA PREDICCION DE BAJAS (COLUMNA "1"), DEBO APLICAR UN SEGUNDO MODELO
# PARA SEPARAR LAS BAJAS EN BAJA+1 Y BAJA+2:
## SE GENERO:
salida_modelo1  <- dapply_1[   , list(numero_de_cliente, Predicted) ] #genero la salida
View(salida_modelo1)  ### CALIFICO AL 6% COMO BAJAS!!! (TIENE MUCHOS FALSOS POSITIVOS!!!)

### AHORA GENERO EL MODELO 2, PARA SEPARAR BAJAS+1 DE BAJAS+2:
## ANTES DEBO HACER UN MERGE, PARA CARGARLE A LA SALIDA DEL MODELO 1 LOS LABELS DE BAJA+1 Y BAJA+2:
entrada_modelo2_BAJAS = merge(salida_modelo1[salida_modelo1$Predicted==1], dtrain,by.x = "numero_de_cliente", by.y = "numero_de_cliente" )
#entrada_modelo2 = salida_modelo1 %>% left_join(dtrain, by = c("numero_de_cliente"="numero_de_cliente"))
View(entrada_modelo2_BAJAS)


###### ENTREGA PARA KAGGLE:
#entrega  <- dapply_1[   , list(numero_de_cliente, Predicted) ] #genero la salida
#View(entrega)
#########genero el archivo para Kaggle
#fwrite( entrega, file="./kaggle/K101_001.csv", sep="," )

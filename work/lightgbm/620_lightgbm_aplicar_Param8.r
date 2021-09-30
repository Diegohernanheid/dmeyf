#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

setwd("~/buckets/b1/")

#cargo el dataset donde voy a entrenar
dataset  <- fread("C:/DIEGO_/MASTER_DM/2_CUAT_2021/DM_ECON_FINANZAS/datasetsOri/paquete_premium_202009.csv")

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

campos_malos  <- c("ccajas_transacciones", "Master_mpagominimo")

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", campos_malos ) )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#Aqui se deben cargar los parametros
param_buenos1  <- list( objective= "binary",
                       num_iterations= 850,
                       learning_rate=  0.01,
                       min_data_in_leaf=  1850,
                       num_leaves= 16,
                       feature_fraction= 0.28,
                       max_bin= 31,
                       prob_corte= 0.025


param_buenos2  <- list( objective= "binary",
                       num_iterations= 3000,
                       learning_rate=  0.01,
                       min_data_in_leaf=  100,
                       num_leaves= 100,
                       feature_fraction= 0.50,
                       max_bin= 31,
                       prob_corte= 0.025
                       )
param_buenos3  <- list( objective= "binary",
                       num_iterations= 1500,
                       learning_rate=  0.005,
                       min_data_in_leaf=  1000,
                       num_leaves= 16,
                       feature_fraction= 0.28,
                       max_bin= 31,
                       prob_corte= 0.025
)
param_buenos4  <- list( objective= "binary",
                        num_iterations= 1750,
                        learning_rate=  0.005,
                        min_data_in_leaf=  1000,
                        num_leaves= 32,
                        feature_fraction= 0.15,
                        max_bin= 31,
                        prob_corte= 0.025
)
param_buenos  <- list( objective= "binary",
                    
                        prob_corte= 0.025,
                        boosting= "goss"
)


#genero el modelo
modelo  <- lgb.train( data= dtrain,
                      param= param_buenos
                    )


#aplico el modelo a los datos sin clase, 202011
dapply  <- fread("C:/DIEGO_/MASTER_DM/2_CUAT_2021/DM_ECON_FINANZAS/datasetsOri/paquete_premium_202011.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )


#Genero la entrega para Kaggle
entrega  <- as.data.table(list("numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion > param_buenos$prob_corte) ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file= "C:/DIEGO_/MASTER_DM/2_CUAT_2021/DM_ECON_FINANZAS/kaggle/lgb_aplicar_101_Param_8.csv", 
        sep= "," )

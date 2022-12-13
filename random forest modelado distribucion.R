


library(rsample)
library (maptools)
library(raster)

bat <- read.csv("./bat_thinned_file.csv", header = TRUE)
plot(bat[, 1:2], col = "red")
data("wrld_simpl")
plot(wrld_simpl, add = T)


# predictores 

wc <- raster::getData("worldclim", var = "bio", res = 10)

#?raster::getData
  
plot(wc[[c(1,2,3)]], nr = 3)

# bio 1: annual mean temperature 
# bio 2:  Mean Diurnal Range (Mean of monthly (max temp - min temp))
# bio 3: BIO3 = Isothermality (BIO2/BIO7) (×100)

# extraer datos climáticos de las ubicaciones de nuestras observaciones. 

clima_bat <- extract(wc, bat[,1:2]) 
head(clima_bat)

# añado columna de presencia 1 / ausencia 0 

pa <- c(rep(1, 669)) 

clima_bat <- cbind(clima_bat, pa)
head(clima_bat)

#### análisis exploratorio 

plot(bat[,1:2], cex = 0.5, col = "blue")
plot(wrld_simpl, add = T)


# BIo 1: Annual Mean Temperature 
# Bio 12: Annual precipitation 

plot(clima_bat[,1]/10, clima_bat[,12], xlab = "Bio1", ylab = "Bio12")


### modelamiento de presencia vs random expectation. 

# definicion random expectation: también llamada background, o random absence data, 
# es lo que obtendrías si la especie no tuviera ninguna preferencia por ninguna de 
# las variables predictora (u otras variables que no estén correlacionadas con las 
# variables predictoras).

library(dismo)

ext <- extent(SpatialPoints(bat[,1:2]))
ext  # la extensión de la distribución de la especie


# generamos 1000 muestras aleatorias de extent 
set.seed(0)
random_a <- sampleRandom(wc, 1000, ext = ext) # ext = para limitar el muestreo al
                                        # área dentro de la extensión. 

# generamos columna de presencia 1 ausencia 0 
pa <- c(rep(0, 1000))
random_a <- cbind(random_a, pa)
head(random_a)


## combinamos los datos de presencia con los de random absence: 

bat_completo <- rbind(clima_bat, random_a) 


# random forest clasificacion -----------------------------------------------------------
#install.packages("randomForest")
library(randomForest)

# definimos un factor de categoría de interés en predicción 

fpa <- as.factor(bat_completo[,"pa"])


modelo <- randomForest(x = bat_completo[,1:19], y= fpa)
modelo 

plot(modelo)

# importancia de variables 
varImpPlot(modelo)

# predicción 

mex <- getData("GADM", country = "MEX", level = 1)

plot(predict(wc, modelo, ext = mex))

# probabilidades de las clases: 

plot(predict(wc, modelo, ext=mex, type = "prob", index = 2))

# evaluar el modelo: NO LOGRO QUE CORRA 
pred <- predict(wc, modelo, ext = mex)
pred <- predict( modelo, type="prob")[,2]
eva <- evaluate(p = pred[which(pa == "1")], 
                a = pred[which(pa== "0")], modelo)



# random forest regresion -------------------------------------------------
library(rpart)


random_ar <- sampleRandom(wc, 3000, ext = ext) # ext = para limitar el muestreo al
# área dentro de la extensión. 

# generamos columna de presencia 1 ausencia 0 
par <- c(rep(0, 3000))
random_ar <- cbind(random_ar, par)

bat_completor <- rbind(clima_bat, random_ar) 



bat_completor <- bat_completor %>% 
                as.data.frame() 

bat_split <- initial_split(bat_completor, strata = pa)
train_data <- training(bat_split)
test_data <- testing(bat_split)

cart <- rpart(pa~., data=train_data)
printcp(cart)

plotcp(cart)

trf <- tuneRF(train_data[, 1:19], train_data[, 'pa'])
trf

mt <- trf[which.min(trf[,2]), 1]
mt

rrf <- randomForest(train_data[, 1:19], train_data[, 'pa'], mtry=mt)

rrf
plot(rrf)
varImpPlot(rrf)

#predictions
rp <- predict(wc, rrf, ext=mex)
plot(rp)

eva <- evaluate(train_data[train_data$pa==1, ], train_data[train_data$pa==0, ], rrf)
eva

plot(eva, 'ROC')

eva <- evaluate(test_data[test_data$pa==1, ], test_data[test_data$pa==0, ], rrf)
eva

plot(eva, 'ROC')


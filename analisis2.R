setwd("/Users/ramonosx/Documents/BIMBO/DATA")

library(data.table)
library(PerformanceAnalytics)
library(dplyr)
library(Metrics)

# Input data files are available in the "../input/" directory.

# Read in only required columns and force to numeric to ensure that subsequent 
# aggregation when calculating medians works
train <- fread('train.csv', 
               select = c('Agencia_ID', 'Cliente_ID', 'Producto_ID', 'Demanda_uni_equil'),
               colClasses=c(Agencia_ID="numeric", Cliente_ID="numeric",Producto_ID="numeric",Demanda_uni_equil="numeric"))

products <- fread('cluster_prods(20).csv', 
                  select = c('Producto_ID',	'Cluster'), 
                  colClasses = c(Producto_ID="numeric", Cluster="character"))

train <- merge(train, products, by = "Producto_ID")
train<- train  %>% sample_n(1000000)


## 50% of the sample size
smp_size <- floor(0.5 * nrow(train))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

train1 <- train[train_ind, ]
test1 <- train[-train_ind, ]

# set a table key to enable fast aggregations
setkey(train, Producto_ID, Agencia_ID, Cliente_ID)
setkey(products, Producto_ID)

setkey(train1, Producto_ID, Agencia_ID, Cliente_ID)
setkey(test1, Producto_ID, Agencia_ID, Cliente_ID)

#calculate the overall median and mean
median_train <- train1[, median(Demanda_uni_equil)]
mean_train<- train1[, mean(Demanda_uni_equil)]
log_mean<- train1[,  expm1(mean(log1p(Demanda_uni_equil)))] 


rmsle(median_train, test1$Demanda_uni_equil)
rmsle(mean_train, test1$Demanda_uni_equil)
rmsle(log_mean, test1$Demanda_uni_equil)


#by producto

logmean_cluster <- train1[, mean(Demanda_uni_equil), by = Cluster]

train1[, .(log1p_mean = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Producto_ID)] %>% 
  merge(test1, all.y = TRUE, by = "Producto_ID") -> merged_val #merge with validation set by product

mask = !(is.na(merged_val$log1p_mean))

rmsle(merged_val$Demanda_uni_equil[mask],merged_val$log1p_mean[mask])

lm_product <-lm(merged_val$Demanda_uni_equil~merged_val$log1p_mean)
summary(lm_product)

#By cluster

train1[, .(log1p_mean = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Cluster)] %>% 
  merge(test1, all.y = TRUE, by = "Cluster") -> merged_val #merge with validation set by product

mask = !(is.na(merged_val$log1p_mean))

rmsle(merged_val$Demanda_uni_equil[mask],merged_val$log1p_mean[mask])

lm_cluster <-lm(merged_val$Demanda_uni_equil~merged_val$log1p_mean)
summary(lm_cluster)

###ESTOS CLUSTERS DESCARTADOS (ver R-squared de Cluster vs el de products)

#by Cliente

train1[, .(log1p_mean = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Cliente_ID)] %>% 
  merge(test1, all.y = TRUE, by = "Cliente_ID") -> merged_val #merge with validation set by product

mask = !(is.na(merged_val$log1p_mean))

rmsle(merged_val$Demanda_uni_equil[mask],merged_val$log1p_mean[mask])

lm_cliente <-lm(merged_val$Demanda_uni_equil~merged_val$log1p_mean)
summary(lm_cliente)

# by agencia

train1[, .(log1p_mean = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Agencia_ID)] %>% 
  merge(test1, all.y = TRUE, by = "Agencia_ID") -> merged_val #merge with validation set by product

mask = !(is.na(merged_val$log1p_mean))

rmsle(merged_val$Demanda_uni_equil[mask],merged_val$log1p_mean[mask])

lm_agencia <-lm(merged_val$Demanda_uni_equil~merged_val$log1p_mean)
summary(lm_agencia)

#by producto y cliente

train1[, .(log1p_mean = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Producto_ID,Cliente_ID)] %>% 
  merge(test1, all.y = TRUE, by = c("Producto_ID","Cliente_ID")) -> merged_val #merge with validation set by product

mask = !(is.na(merged_val$log1p_mean))

rmsle(merged_val$Demanda_uni_equil[mask],merged_val$log1p_mean[mask])

lm_prodcliente <-lm(merged_val$Demanda_uni_equil~merged_val$log1p_mean)
summary(lm_prodcliente)

###BUENA CARA (R-squared = .67)

#by producto cliente agencia

train1[, .(log1p_mean = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Producto_ID,Agencia_ID,Cliente_ID)] %>% 
  merge(test1, all.y = TRUE, by = c("Producto_ID","Agencia_ID","Cliente_ID")) -> merged_val #merge with validation set by product

mask = !(is.na(merged_val$log1p_mean))

rmsle(merged_val$Demanda_uni_equil[mask],merged_val$log1p_mean[mask])

lm_prodClienteAgencia <-lm(merged_val$Demanda_uni_equil~merged_val$log1p_mean)
summary(lm_prodClienteAgencia)

###SUBE UN POCO EL R-squared (.68)

#promedios por separado y unidos en un solo modelo

train1[, .(Lmean_cliente = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Cliente_ID)] %>% 
  merge(test1, all.y = TRUE, by = c("Cliente_ID")) -> merged_val2

train1[, .(Lmean_producto = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Producto_ID)] %>% 
  merge(merged_val2, all.y = TRUE, by = c("Producto_ID")) -> merged_val2

train1[, .(Lmean_agencia = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Agencia_ID)] %>% 
  merge(merged_val2, all.y = TRUE, by = c("Agencia_ID")) -> merged_val2


lm_todas<-lm(merged_val2$Demanda_uni_equil~merged_val2$Lmean_agencia+merged_val2$Lmean_producto+merged_val2$Lmean_cliente)
summary(lm_todas)

###LOS PROMEDIOS POR SEPARADO NO EXPLICAN MUCHO

#probamos con interaccion

train1[, .(Lmean_ProdAgenClien = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Producto_ID,Agencia_ID,Cliente_ID)] %>% 
  merge(merged_val2, all.y = TRUE, by = c("Producto_ID","Agencia_ID","Cliente_ID")) -> merged_val2 


lm_todasInter<-lm(merged_val2$Demanda_uni_equil~merged_val2$Lmean_agencia+merged_val2$Lmean_producto+merged_val2$Lmean_cliente+merged_val2$Lmean_ProdAgenClien.y)
summary(lm_todasInter)

###YEEEYYYY SUBIMOS EL R-squared a .71

#ahora mas interacciones 

train1[, .(Lmean_ProdClien = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Producto_ID,Cliente_ID)] %>% 
  merge(merged_val2, all.y = TRUE, by = c("Producto_ID","Cliente_ID")) -> merged_val2 


lm_todasInter2<-lm(merged_val2$Demanda_uni_equil~
      merged_val2$Lmean_agencia+merged_val2$Lmean_producto+merged_val2$Lmean_cliente+merged_val2$Lmean_ProdAgenClien.y+merged_val2$Lmean_ProdClien)
summary(lm_todasInter2)

###Sube un poco el R .7123

##otra interaccion

train1[, .(Lmean_ProdAgen = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Producto_ID,Agencia_ID)] %>% 
  merge(merged_val2, all.y = TRUE, by = c("Producto_ID","Agencia_ID")) -> merged_val2 


lm_todasInter3<-lm(merged_val2$Demanda_uni_equil~
                     merged_val2$Lmean_agencia+merged_val2$Lmean_producto+
                     merged_val2$Lmean_cliente+merged_val2$Lmean_ProdAgenClien.y+
                     merged_val2$Lmean_ProdClien+merged_val2$Lmean_ProdAgen)
summary(lm_todasInter3)

##SUBE OTRO POQUITIN R = .7175

#ultima interaccion

train1[, .(Lmean_CLienAgen = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Cliente_ID,Agencia_ID)] %>% 
  merge(merged_val2, all.y = TRUE, by = c("Cliente_ID","Agencia_ID")) -> merged_val2 


lm_todasInter4<-lm(merged_val2$Demanda_uni_equil~
                     merged_val2$Lmean_agencia+merged_val2$Lmean_producto+
                     merged_val2$Lmean_cliente+merged_val2$Lmean_ProdAgenClien.y+
                     merged_val2$Lmean_ProdClien+merged_val2$Lmean_ProdAgen+merged_val2$Lmean_CLienAgen.y)
summary(lm_todasInter4)

### R = .7177 PARECE LO MAXIMO QUE SE PUEDE LOGRAR SOLO CON PROMEDIOS SIMPLES 

#ajustando por RMSLE

mask1 = !(is.na(merged_val2$Lmean_ProdAgenClien.y))


rmsleRes<-c(rmsle())


Lmean_cliente<-train1[, .(expm1(mean(log1p(Demanda_uni_equil)))), by = .(Cliente_ID)] 
Lmean_producto<-train1[, .(expm1(mean(log1p(Demanda_uni_equil)))), by = .(Producto_ID)]
Lmean_agencia<-train1[, .(expm1(mean(log1p(Demanda_uni_equil)))), by = .(Agencia_ID)]




#histogramas

mydata <- train2[, c(7,8,9,10,11)]

plot1<-plot(train$Cluster,train$Demanda_uni_equil)

train2 = subset(train, select = c(7,8,9,.....))

plot(train$Cluster,log(train$Demanda_uni_equil+1))
plot(train$Agencia_ID,log(train$Demanda_uni_equil+1))
plot(train$Cliente_ID,log(train$Demanda_uni_equil+1))

hist(train$Demanda_uni_equil)

hist(log(train$Demanda_uni_equil+1))
chart.Correlation(train)




#linnear model 

lm1 <- lm(train$Demanda_uni_equil~as.factor(train$Cluster)+as.factor(train$Agencia_ID)+as.factor(train$Cliente_ID))
summary(lm1)
anova(lm1)

lm2 <- lm(log(train2$Demanda_uni_equil+1)~as.factor(train$Cluster)+as.factor(train$Agencia_ID)+as.factor(train$Cliente_ID))
summary(lm2)
anova(lm2)

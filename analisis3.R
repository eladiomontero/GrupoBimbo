calcModelo <- function(n)
{
  trainp<- train  %>% sample_n(n)
  
  
  ## 50% of the sample size
  smp_size <- floor(0.8 * nrow(trainp))
  
  ## set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(trainp)), size = smp_size)
  
  train1 <- trainp[train_ind, ]
  test1 <- trainp[-train_ind, ]
  
  # set a table key to enable fast aggregations
  setkey(train, Producto_ID, Agencia_ID, Cliente_ID)
  setkey(products, Producto_ID)
  
  setkey(train1, Producto_ID, Agencia_ID, Cliente_ID)
  setkey(test1, Producto_ID, Agencia_ID, Cliente_ID)
  
  #variables por separado
  train1[, .(Lmean_cliente = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Cliente_ID)] %>% 
    merge(test1, all.y = TRUE, by = c("Cliente_ID")) -> merged_val2
  
  train1[, .(Lmean_producto = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Producto_ID)] %>% 
    merge(merged_val2, all.y = TRUE, by = c("Producto_ID")) -> merged_val2
  
  train1[, .(Lmean_agencia = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Agencia_ID)] %>% 
    merge(merged_val2, all.y = TRUE, by = c("Agencia_ID")) -> merged_val2
  
  #interacciones
  train1[, .(Lmean_ProdAgenClien = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Producto_ID,Agencia_ID,Cliente_ID)] %>% 
    merge(merged_val2, all.y = TRUE, by = c("Producto_ID","Agencia_ID","Cliente_ID")) -> merged_val2 
  
  train1[, .(Lmean_ProdClien = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Producto_ID,Cliente_ID)] %>% 
    merge(merged_val2, all.y = TRUE, by = c("Producto_ID","Cliente_ID")) -> merged_val2 
  
  train1[, .(Lmean_CLienAgen = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Cliente_ID,Agencia_ID)] %>% 
    merge(merged_val2, all.y = TRUE, by = c("Cliente_ID","Agencia_ID")) -> merged_val2 
  
  lm_todasInter4<-lm(merged_val2$Demanda_uni_equil~
                       merged_val2$Lmean_agencia+merged_val2$Lmean_producto+
                       merged_val2$Lmean_cliente+merged_val2$Lmean_ProdAgenClien+
                       merged_val2$Lmean_ProdClien+merged_val2$Lmean_ProdAgen+merged_val2$Lmean_CLienAgen)
  
  lm_todasInter4$r.squared
}

train <- fread('train.csv', 
               select = c('Agencia_ID', 'Cliente_ID', 'Producto_ID', 'Demanda_uni_equil'),
               colClasses=c(Agencia_ID="numeric", Cliente_ID="numeric",Producto_ID="numeric",Demanda_uni_equil="numeric"))

products <- fread('cluster_prods(20).csv', 
                  select = c('Producto_ID',	'Cluster'), 
                  colClasses = c(Producto_ID="numeric", Cluster="character"))

train <- merge(train, products, by = "Producto_ID")



## 50% of the sample size
smp_size <- floor(0.8 * nrow(train))

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


n<-seq(1000000,20000000,1000000)
r<-rep(0,length(n))

for(i in 1:length(n)){
  
  r[i]<-calcModelo(n[i])
  
}

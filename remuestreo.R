resampling <- function(n)
{
  
  trainp<- train  %>% sample_n(n)
  
  
  ## 50% of the sample size
  smp_size <- floor(0.8 * nrow(trainp))
  
  ## set the seed to make your partition reproductible
  #set.seed(123)
  train_ind <- sample(seq_len(nrow(trainp)), size = smp_size)
  
  train1 <- trainp[train_ind, ]
  test1 <- trainp[-train_ind, ]
  
  # set a table key to enable fast aggregations
  # setkey(train, Producto_ID, Agencia_ID, Cliente_ID)
  # setkey(products, Producto_ID)
  
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
  
  train1[, .(Lmean_ProdAgen = expm1(mean(log1p(Demanda_uni_equil)))), by = .(Producto_ID,Agencia_ID)] %>% 
    merge(merged_val2, all.y = TRUE, by = c("Producto_ID","Agencia_ID")) -> merged_val2
  
  lm_todasInter4<-lm(merged_val2$Demanda_uni_equil~
                       merged_val2$Lmean_agencia+merged_val2$Lmean_producto+
                       merged_val2$Lmean_cliente+merged_val2$Lmean_ProdAgenClien+
                       merged_val2$Lmean_ProdClien+merged_val2$Lmean_ProdAgen+merged_val2$Lmean_CLienAgen)
  
  
  lm_todasInter4
  
}

reps<-1000
n<-7500000
betas<-matrix(0,reps,8)
colnames(betas)<-c('intercept','agencia','producto','cliente','prodAgenCliente','prodCliente','prodAgencia','clienteAgen')

r<-rep(0,reps)

for(i in 1:reps){
  
  modelo<-resampling(n)
  betas[i,]<-modelo$coefficients
  r[i]<-summary(modelo)$r.squared
  print(i)
}

#summary(betas[,2])

promBetas<-apply(betas, 2, mean)

write.csv(betas, 'betas.csv')
write.csv(promBetas, 'promBetas.csv')


#comparacion
trainp<- train  %>% sample_n(5000000)

smp_size <- floor(0.8 * nrow(trainp))

## set the seed to make your partition reproductible
#set.seed(123)
train_ind <- sample(seq_len(nrow(trainp)), size = smp_size)

train1 <- trainp[train_ind, ]
test1 <- trainp[-train_ind, ]

x<-matrix(1,nrow(merged_val2),8)
x[,2]<-merged_val2$Lmean_agencia
x[,3]<-merged_val2$Lmean_producto
x[,4]<-merged_val2$Lmean_cliente
x[,5]<-merged_val2$Lmean_ProdAgenClien
x[,6]<-merged_val2$Lmean_ProdClien
x[,7]<-merged_val2$Lmean_ProdAgen
x[,8]<-merged_val2$Lmean_CLienAgen


demandauniqcalc<-x%*%promBetas
rmsle(merged_val2$Demanda_uni_equil[complete.cases(demandauniqcalc)],demandauniqcalc[complete.cases(demandauniqcalc)])

cov(merged_val2$Demanda_uni_equil[complete.cases(demandauniqcalc)],demandauniqcalc[complete.cases(demandauniqcalc)])/
 ( sd(merged_val2$Demanda_uni_equil[complete.cases(demandauniqcalc)])*sd(demandauniqcalc[complete.cases(demandauniqcalc)]))

summary(lm(merged_val2$Demanda_uni_equil[complete.cases(demandauniqcalc)]~demandauniqcalc[complete.cases(demandauniqcalc)]))

rmsle(merged_val2$Demanda_uni_equil[complete.cases(merged_val2$Lmean_producto)],merged_val2$Lmean_producto[complete.cases(merged_val2$Lmean_producto)])

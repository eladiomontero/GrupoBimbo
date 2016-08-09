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
  
  lm_todasInter4<-lm(merged_val2$Demanda_uni_equil~
                       merged_val2$Lmean_agencia+merged_val2$Lmean_producto+
                       merged_val2$Lmean_cliente+merged_val2$Lmean_ProdAgenClien+
                       merged_val2$Lmean_ProdClien+merged_val2$Lmean_ProdAgen+merged_val2$Lmean_CLienAgen)
  
  summary(lm_todasInter4)$r.squared
  
  }




n<-seq(1000000,20000000,1000000)
r<-rep(0,length(n))

for(i in 1:length(n)){
  
  r[i]<-calcModelo(n[i])
  print(paste("calculating with ", n[i]/1000000, " million, error: ", r[i]))
}

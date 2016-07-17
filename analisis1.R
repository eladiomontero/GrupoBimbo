library(data.table)

# Input data files are available in the "../input/" directory.

# Read in only required columns and force to numeric to ensure that subsequent 
# aggregation when calculating medians works
train <- fread('train.csv', 
               select = c('Agencia_ID', 'Cliente_ID', 'Producto_ID', 'Demanda_uni_equil'),
               colClasses=c(Agencia_ID="numeric", Cliente_ID="numeric",Producto_ID="numeric",Demanda_uni_equil="numeric"))

products <- fread('cluster_prods(20).csv', 
                  select = c('Producto_ID',	'Cluster'), 
                  colClasses = c(Producto_ID="numeric", Cluster="character"))

train2 <- merge(train, products, by = "Producto_ID")


# set a table key to enable fast aggregations
setkey(train, Producto_ID, Agencia_ID, Cliente_ID)
setkey(products, Producto_ID)
#calculate the overall median and mean
median <- train2[, median(Demanda_uni_equil)]
mean<- train2[, mean(Demanda_uni_equil)]

mean_cluster <- train2[, mean(Demanda_uni_equil), by = Cluster]
sd_cluster <- train2[, sd(Demanda_uni_equil), by = Cluster]

CV_cluster20 <- sd_cluster$V1/mean_cluster$V1




#linnear model 

lm1 <- lm(train2$Demanda_uni_equil~train2$Cluster)
summary(lm1)
anova(lm1)

lm2 <- lm(log(train2$Demanda_uni_equil+1)~train2$Cluster)
summary(lm2)
anova(lm2)


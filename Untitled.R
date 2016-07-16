setwd("/Users/ramonosx/Documents/BIMBO/DATA")
library(tm)

#cargando datis de los productos 
prods <- read.csv("producto_tabla.csv", header = TRUE)

text_prods<-read.csv("producto_tabla.csv", stringsAsFactors=FALSE)

text_prods <- text_prods[2:nrow(text_prods),]


#preparando los archivos para el text mining
text <- paste(text_prods$NombreProducto, collapse=" ")

prodcuts_source <- VectorSource(text)
corpus <- Corpus(prodcuts_source)


corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeWords, stopwords("spanish"))

#haciendo el analisis de frecuencia de palabras
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.9)


dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)

head(frequency)

freq <-data.frame(names(frequency),frequency)
freq<- freq[,2:3]
head(frequency[1:100])
write.csv(freq, file = "freq_prodcutos.csv")

#una vez seleccionadas las palabras 'claves' 
#se dicotomizan y se transforman en variables nuevas

test_words<-read.csv("test_words.csv", header = TRUE)

test_words<-read.csv("freq_prodcutos.csv", header = TRUE)
test_words <- data.frame(test_words[,1])

m <- matrix(0, ncol = nrow(test_words), nrow = nrow(prods))

m[t(grep(test_words[1,],prods[,2],ignore.case=TRUE)),]<-1

for(j in 1:nrow(test_words)){
  
  m[t(grep(test_words[j,],prods[,2],ignore.case=TRUE)),j]<-1
}


data <- data.frame(m)

colnames(data) <- t(test_words)

data<- data[-1,]

products<- cbind.data.frame(text_prods,data)

# Haciendo clusters de los productos
wss <- (nrow(data)-1)*sum(apply(data,2,var))
wss<-NULL

for (i in 1:20) wss[i] <- sum(kmeans(data, centers=i)$withinss)

plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(data, 20) 

productos<-cbind.data.frame(text_prods, fit$cluster)

write.csv(productos, file = "cluster_prods.csv")




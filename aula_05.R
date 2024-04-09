'''
Analise Multivariada
09/04/2024
Matrizes - continuacao

'''

S<-cov(iris[-5]) # tira a quinta variavel (por ser categorica)
S

autovalores<-eigen(S)$values # autovalores

# traco eh a soma dos elementos da diagonal principal da matriz
iris.mat <- as.matrix(iris[, -5])
is.matrix(iris.mat)
100*(autovalores/4.576) # 92% da var total esta em cima do primeiro autovalor

autovetores<-eigen(S)$vectors

t(iris.mat[13,])%*%autovetores[,1] # produto das observacoes da 13ª linha pelos elementos do primeiro autovetor

iris.cp<-iris.mat%*%autovetores
iris.cp[13,1]

autovetores

plot(iris[,1], col = c('red', 'blue', 'pink')[as.numeric(iris[,5])], pch = 19)

plot(iris[,3], col = c('red', 'blue', 'pink')[as.numeric(iris[,5])], pch = 19)

apply(iris[-5],2,mean) # media de cada coluna



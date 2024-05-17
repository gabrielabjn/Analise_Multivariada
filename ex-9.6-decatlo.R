# Exemplo 9.6 livro do Johnson -------------------------------------------------

R<-read.table(file='/home/ice/Downloads/E9-6.DAT')
R<-as.matrix(R)
R

# Solucao por Componentes Principais -------------------------------------------

decatlo.av<-eigen(R)
Lambda<-diag(decatlo.av$values)
E<-decatlo.av$vector

L<-E%*%sqrt(Lambda)
L # matriz de loadings (composta por vetores chamados fatores)

L.quad<-L[,1:4]**2 # loadings (colunas de L) ao quadrado

apply(L.quad,2,sum) # variabilidade associada a cada fator
apply(L.quad,1,sum) # h² - variabilidades de Z1,Z2,...,Zn (comunalidades)

# interpretacao: 88% da variancia de Z1 pode ser explicada pelos fatores em 
#comum (o restante eh variancia especifica)

sum(apply(L.quad,1,sum)) ==  sum(apply(L.quad,2,sum))

1-apply(L.quad,1,sum) # psi - simbolo tridente

x<-apply(L.quad,2,sum)
x<-as.matrix(x)

for(i in 1:4){
print(L.quad[,i]/x[i])
}

L.tL<-L[,1:4]%*%t(L[,1:4])
comunalidades<-diag(L.tL)
especificidades <- 1 - comunalidades

Qsi<-diag(especificidades)

R
R.rec<-L.tL + Qsi # matriz R recuperada

sum((R-R.rec)**2) # residuos

is.matrix(R)

# Solucao por Maxima Verossimilhanca -------------------------------------------

decatlo.af<-factanal(factors = 4, covmat = R, rotation = 'none')
# uniquenesses - especificidade

L.af<-decatlo.af$loadings
especificidades.af<-decatlo.af$uniquenesses
comunalidades.af<-1-especificidades.af

L.af.quad<-L.af**2
  
apply(L.af.quad,2,sum) # marginais dos fatores
apply(L.af.quad,1,sum) # marginais das variaveis Z1, Z2, ..., Z10
1-apply(L.af.quad,1,sum) # psi
R.af<-L.af%*%t(L.af)+diag(especificidades.af)

sum((R - R.af)**2)

# ------------------------------------

L.af**2 # valores que nao aparecem nao sao significativos
# alpha considerado foi 0.01

# comunalidade - soma o quadrado dos loadings por linha
# especificidade - soma o quadrado dos loadings por coluna


# matriz ortogonal - inversa coincide com a transposta


# 17 de maio -------------------------------------------------------

L.star<-varimax(L,eps=1e-5)
L.star

0.664^2/0.71
0.429^2/0.71

L.star$loadings%*%t(L.star$loadings)
colSums(L.star$loadings)

R<-as.matrix(R)
L.mv.rot<-factanal(covmat=R, factors=4, rotation = 'varimax')
L.mv.rot

colSums(L.mv.rot$loadings)

par(mfrow=c(2,2))
plot(L.mv.rot$loadings[,1],L.mv.rot$loadings[,2], type = 'n')
text(L.mv.rot$loadings[,1],L.mv.rot$loadings[,2], label = paste0('X', 1:10), cex  = 0.75)
plot(L.mv.rot$loadings[,1],L.mv.rot$loadings[,3], type = 'n')
text(L.mv.rot$loadings[,1],L.mv.rot$loadings[,3], label = paste0('X', 1:10), cex  = 0.75)







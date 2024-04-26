# aula do de 26 abr 2024
# parte 2 do curso (pos primeira prova)

x<-iris[-5]
xd<-scale(x,center = TRUE, scale = FALSE)
xd

t(xd)%*%xd*1/149
S<-cov(x) # da o mesmo que a expressao acima

D.meio<-diag(1/sqrt(diag(S)))
Z<-xd%*%D.meio
apply(Z,2,FUN = function(x)c(mean(x),sd(x)))
hist(Z[,1])
cor(x)
cov(Z)


A<-matrix(data = c(0.464,0.457,0.470,0.422,0.421,0.241,0.509,0.261,-0.525,-0.582), byrow = FALSE, nrow = 5)
(A**2)*100

plot(x = A[,1], y = A[,2],xlim = c(0,1), ylim = c(-1,1))
acoes<-c('AC', 'DP', 'UC', 'EXX', 'TX')
text(x = A[,1],y = A[,2], label = acoes, cex = 0.85, pos = 4)
arrows(x0 = 0, y0 = 0, x1 = A[,1], y1 = A[,2], length = 0.15)

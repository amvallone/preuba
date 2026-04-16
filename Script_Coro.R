library(estdaR)
# Comentario Online para el taller
data(us48)

w1queen <- nb2listw(poly2nb(us48))
t0 <- us48$X1969/mean(us48$X1969)
t1 <- us48$X1989/mean(us48$X1989)
t2 <- us48$X2009/mean(us48$X2009)

l.t0 <- lag.listw(w1queen,t0)
l.t1 <- lag.listw(w1queen,t1)
l.t2 <- lag.listw(w1queen,t2)


# Opción 1 
# Se divide en dos periodos y se gráfica el movimiento sucesivo
f.p <- as.data.frame(cbind(t1-t0,l.t1-l.t0))
s.p <- as.data.frame(cbind(t2-t1,l.t2-l.t1))
s.p <- as.data.frame(cbind(f.p[,1]+s.p[,1],f.p[,2]+s.p[,2]))
vtop<-cbind(rep(0,length(t0)),rep(0,length(t0)),f.p,rep("1",length(t0)))
vtop1 <- cbind(f.p,s.p,rep("2",length(t0)))
colnames(vtop)<-c("x0","y0","x1","y1","Time")
colnames(vtop1)<-c("x0","y0","x1","y1","Time")
vtop <- rbind(vtop,vtop1)

x.l.n <- min(c(vtop$x0,vtop$x1))
x.l.p <- max(c(vtop$x0,vtop$x1))
l.l.n <- min(c(vtop$y0,vtop$y1))
l.l.p <- max(c(vtop$y0,vtop$y1))





#####. hkjhkhkahfdklhalf

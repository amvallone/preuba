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

lisa <-ggplot(vtop)+
  geom_hline(yintercept=0,linetype=1)+
  geom_vline(xintercept=0,linetype=1)+
  geom_segment(aes(x=x0, xend=x1, y=y0, yend=y1,color=Time),size=0.4,arrow=arrow(length = unit(0.2, "cm")))+
  theme(
    legend.position="bottom",
    panel.border = element_rect(linetype = "solid", fill = NA),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line()
  )+xlab("X")+ylab("WX")+scale_color_brewer(palette="Set1",direction=-1)+
  annotate("text",x.l.n, l.l.p, label= "II")+
  annotate("text",x.l.p, l.l.p, label= "I")+
  annotate("text",x.l.p, l.l.n, label= "IV")+
  annotate("text",x.l.n, l.l.n, label= "III")


#Opción 2
# Se normalizan a cero en ambos periodos y se gráfica cada periodo con un color distinto
f.p <- as.data.frame(cbind(t1-t0,l.t1-l.t0))
s.p <- as.data.frame(cbind(t2-t1,l.t2-l.t1))
vtop<-cbind(rep(0,length(t0)),rep(0,length(t0)),f.p,rep("1",length(t0)))
vtop1 <- cbind(rep(0,length(t0)),rep(0,length(t0)),s.p,rep("2",length(t0)))
colnames(vtop)<-c("x0","y0","x1","y1","Time")
colnames(vtop1)<-c("x0","y0","x1","y1","Time")
vtop <- rbind(vtop,vtop1)

x.l.n <- min(c(vtop$x0,vtop$x1))
x.l.p <- max(c(vtop$x0,vtop$x1))
l.l.n <- min(c(vtop$y0,vtop$y1))
l.l.p <- max(c(vtop$y0,vtop$y1))

lisa <-ggplot(vtop)+
  geom_hline(yintercept=0,linetype=1)+
  geom_vline(xintercept=0,linetype=1)+
  geom_segment(aes(x=x0, xend=x1, y=y0, yend=y1,color=Time),size=0.4,arrow=arrow(length = unit(0.2, "cm")))+
  theme(
    legend.position="bottom",
    panel.border = element_rect(linetype = "solid", fill = NA),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line()
  )+xlab("X")+ylab("WX")+scale_color_brewer(palette="Set1",direction=-1)+
  annotate("text",x.l.n, l.l.p, label= "II")+
  annotate("text",x.l.p, l.l.p, label= "I")+
  annotate("text",x.l.p, l.l.n, label= "IV")+
  annotate("text",x.l.n, l.l.n, label= "III")

#Rose diagram

k <- 4 #fija el numero de cuadrantes en el rose diagrams el grafico solo funciona con 4
rotar <-ifelse(k==8,pi/4,0)
step <- (2*pi)/k
breaks <- seq(0,2*pi,step)
lmt <- (breaks * 180)/pi # rad to deg
symb<-c()
for (i in seq_len(length(lmt)-1)){
  symb <- c(symb, paste(lmt[i],lmt[i+1],sep="-"))
}
z<-atan2(vtop[,4],vtop[,3]) #get angles
z <- ifelse(z>0,1*z,(2*pi)+z) #avoid negatives rad
bins = rep(1, length(z))
i = 1L
for(b in breaks){
  bins[z > b] = i
  i = i + 1L
}
angle <- factor(bins, levels = seq_along(symb), labels = symb)
real<-hist(z,breaks=breaks,plot=FALSE)$counts

k <- 8 #fija el numero de cuadrantes en el rose diagrams.
rotar <-ifelse(k==8,pi/4,0)
step <- (2*pi)/k
breaks <- seq(0,2*pi,step)
lmt <- (breaks * 180)/pi # rad to deg
symb1<-c()
for (i in seq_len(length(lmt)-1)){
  symb1 <- c(symb1, paste(lmt[i],lmt[i+1],sep="-"))
}
d.plot <- data.frame(Time=factor(vtop$Time),angle=as.character(angle))
d.plot["angle1"] <- d.plot[,2]
d.plot[d.plot$Time==2 & d.plot$angle==symb[1],3] <- symb1[2]
d.plot[d.plot$Time==2 & d.plot$angle==symb[2],3] <- symb1[4]
d.plot[d.plot$Time==2 & d.plot$angle==symb[3],3] <- symb1[6]
d.plot[d.plot$Time==2 & d.plot$angle==symb[4],3] <- symb1[8]
d.plot[d.plot$Time==1 & d.plot$angle==symb[1],3] <- symb1[1]
d.plot[d.plot$Time==1 & d.plot$angle==symb[2],3] <- symb1[3]
d.plot[d.plot$Time==1 & d.plot$angle==symb[3],3] <- symb1[5]
d.plot[d.plot$Time==1 & d.plot$angle==symb[4],3] <- symb1[7]
lim=c(symb1[1],symb1[length(symb1):2])

rose<-ggplot(d.plot,aes(angle1,fill=Time),xlab=" ",ylab=" ") +geom_bar(width=1,colour="black",size=0.1) +scale_x_discrete(name="",limit=lim,labels=NULL)+scale_y_continuous(name="",breaks=seq(0,max(table(angle)),5),labels=seq(0,max(table(angle)),5))+coord_polar(start=rotar)+theme(legend.position="bottom",panel.grid.major = element_line( color="gray50",linetype="solid"),panel.background = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank())+scale_fill_brewer(palette="Set1",direction=-1)+ geom_vline(xintercept = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5), colour = "grey32")+annotate("text",0.5,21,label="0-90")+annotate("text",2.5,21,label="270-360")+annotate("text",4.5,21,label="180-270")+annotate("text",6.5,21,label="90-180")

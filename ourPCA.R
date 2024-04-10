dd <- read.table("ObesityWithKnnNeighbors.csv",header=T, sep=",");
#
# VISUALISATION OF DATA
#
# PRINCIPAL COMPONENT ANALYSIS OF CONTINcUOUS VARIABLES, WITH Dictamen PROJECTED AS ILLUSTRATIVE
#

#set a list of numerical variables (with no missing values)
numeriques<-which(sapply(dd,is.numeric))
numeriques

dcon<-dd[,numeriques]
sapply(dcon,class)

# PRINCIPAL COMPONENT ANALYSIS
pc1 <- prcomp(dcon, scale=TRUE)
class(pc1)
attributes(pc1)
print(pc1)
str(pc1)


# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?
pc1$sdev
inerProj<- pc1$sdev^2
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
barplot(pinerEix)

#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])
percInerAccum<-100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2]
percInerAccum

# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)
nd = 6
print(pc1)
attributes(pc1)
pc1$rotation

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS
View(pc1$x)
dim(pc1$x)
dim(dcon)
dcon[2000,]
pc1$x[2000,]
Psi = pc1$x[,1:nd]
dim(Psi)
Psi[2000,]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES
iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

# PLOT OF INDIVIDUALS

#select your axis
#eje1<-2
eje1<-1
#eje2<-3
eje2<-2

plot(Psi[,eje1],Psi[,eje2], type="n")
text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#Projection of variables
Phi = cor(dcon,Psi)
View(Phi)

#select your axis
X<-Phi[,eje1]
Y<-Phi[,eje2]
plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)
#zooms
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)

# PROJECTION OF ILLUSTRATIVE qualitative variables on individuals' map
# PROJECCI? OF INDIVIDUALS DIFFERENTIATING THE Dictamen
# (we need a numeric Dictamen to color)
varcat=factor(dd[,1])
plot(Psi[,1],Psi[,2],col=varcat)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(factor(varcat)),pch=1,col=c(1,2), cex=0.6)

#select your qualitative variable
k<-1 #dictamen in credsco

varcat<-factor(dd[,k])
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean)
text(fdic1,fdic2,labels=levels(varcat),col="yellow", cex=0.7)

#Now we project both cdgs of levels of a selected qualitative variable without
#representing the individual anymore

plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#nominal qualitative variables
dcat<-c(1,5,6,9,10,12,15:17)
#divide categoricals in several graphs if joint representation saturates
#build a palette with as much colors as qualitative variables
#alternative
colors<-rainbow(length(dcat))
c<-1
for(k in dcat){
  seguentColor<-colors[c]
  fdic1 = tapply(Psi[,eje1],dd[,k],mean)
  fdic2 = tapply(Psi[,eje2],dd[,k],mean)
 
  text(fdic1,fdic2,labels=levels(factor(dd[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(dd)[dcat],pch=1,col=colors, cex=0.6)

#determine zoom level
#use the scale factor or not depending on the position of centroids
# ES UN FACTOR D'ESCALA PER DIBUIXAR LES FLETXES MES VISIBLES EN EL GRAFIC
#fm = round(max(abs(Psi[,1])))
fm=20
#represent numerical variables in background
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1), ylim=c(-3,1))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")
#add projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
text(X,Y,labels=etiq,col="gray", cex=0.9)

#add centroids
c<-1
for(k in dcat){
  seguentColor<-colors[c]
  fdic1 = tapply(Psi[,eje1],dd[,k],mean)
  fdic2 = tapply(Psi[,eje2],dd[,k],mean)
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
  text(fdic1,fdic2,labels=levels(factor(dd[,k])),col=seguentColor, cex=0.9)
  c<-c+1
}
legend("bottomleft",names(dd)[dcat],pch=1,col=colors, cex=0.6)


Dendograms script
dd <- read.csv("ObesityWithKnnNeighborsS.csv", sep=",", stringsAsFactors = TRUE);

#set a list of numerical variables
names(dd)
dcon <- data.frame (Age,Height,Weight,FCVC,NCP,CH2O,FAF,TUE)
dim(dcon)

library(cluster)
#dissimilarity matrix
actives<-c(1:17)
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)
distMatrix<-dissimMatrix^2
h1 <- hclust(distMatrix,method="ward.D")  # NOTICE THE COST
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot
plot(h1, labels = FALSE)
c2 <- cutree(h1,3) #3 es el numero de clases que quiero cortar
#class sizes
table(c2)

potencials<-c(3,4,6,7,10,11)
pairs(dcon[,potencials],col=c2)

#Profiling plots

#Dictamen	<- as.factor(Dictamen)
#levels(Dictamen) <- c(NA, "positiu","negatiu")


#Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum,P){
  #freq dis of fac
  nk <- as.vector(table(P));
  n <- sum(nk);
  #mitjanes x grups
  xk <- tapply(Xnum,P,mean);
  #valors test
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk)));
  #p-values
  pxk <- pt(txk,n-1,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}




ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali);
  n <- sum(taula);
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2], byrow=TRUE); 	 
  dpf <- pf - pjm;
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj)));
  #i hi ha divisions iguals a 0 dona NA i no funciona
  zkj <- dpf
  zkj[dpf!=0]<-dpf[dpf!=0]/dvt[dpf!=0];
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}


#source("file")
#dades contain the dataset
dades<-dd
#dades<-dd[filtro,]
#dades<-df
K<-dim(dades)[2]
par(ask=TRUE)


#P must contain the class variable
#P<-dd[,3]
P<-c2
#P<-dd[,18]
nameP<-"classe"
#P<-df[,33]

nc<-length(levels(factor(P)))
nc
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dades)))
nameP<-"Class"
n<-dim(dades)[1]

for(k in 1:K){
  if (is.numeric(dades[,k])){
	print(paste("Anàlisi per classes de la Variable:", names(dades)[k]))
    
	boxplot(dades[,k]~P, main=paste("Boxplot of", names(dades)[k], "vs", nameP ), horizontal=TRUE)
    
	barplot(tapply(dades[[k]], P, mean),main=paste("Means of", names(dades)[k], "by", nameP ))
	abline(h=mean(dades[[k]]))
	legend(0,mean(dades[[k]]),"global mean",bty="n")
	print("Estadístics per groups:")
	for(s in levels(as.factor(P))) {print(summary(dades[P==s,k]))}
	o<-oneway.test(dades[,k]~P)
	print(paste("p-valueANOVA:", o$p.value))
	kw<-kruskal.test(dades[,k]~P)
	print(paste("p-value Kruskal-Wallis:", kw$p.value))
	pvalk[,k]<-ValorTestXnum(dades[,k], P)
	print("p-values ValorsTest: ")
	print(pvalk[,k]) 	 
  }else{
	if(class(dd[,k])=="Date"){
  	print(summary(dd[,k]))
  	print(sd(dd[,k]))
  	#decide breaks: weeks, months, quarters...
  	hist(dd[,k],breaks="weeks")
	}else{
  	#qualitatives
  	print(paste("Variable", names(dades)[k]))
  	table<-table(P,dades[,k])
  	#   print("Cross-table")
  	#   print(table)
  	rowperc<-prop.table(table,1)
 	 
  	colperc<-prop.table(table,2)
  	#  print("Distribucions condicionades a files")
  	# print(rowperc)
 	 
  	#ojo porque si la variable es true o false la identifica amb el tipus Logical i
  	#aquest no te levels, por tanto, coertion preventiva
 	 
  	dades[,k]<-as.factor(dades[,k])
 	 
 	 
  	marg <- table(as.factor(P))/n
  	print(append("Categories=",levels(as.factor(dades[,k]))))
 	 
  	#from next plots, select one of them according to your practical case
  	plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
  	paleta<-rainbow(length(levels(dades[,k])))
  	for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
 	 
  	#with legend
  	plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
  	paleta<-rainbow(length(levels(dades[,k])))
  	for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
  	legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
 	 
  	#condicionades a classes
  	print(append("Categories=",levels(dades[,k])))
  	plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
  	paleta<-rainbow(length(levels(dades[,k])))
  	for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
 	 
  	#with legend
  	plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
  	paleta<-rainbow(length(levels(dades[,k])))
  	for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
  	legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
 	 
  	#amb variable en eix d'abcisses
  	marg <-table(dades[,k])/n
  	print(append("Categories=",levels(dades[,k])))
  	plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
  	#x<-plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), xaxt="n")
  	#text(x=x+.25, y=-1, adj=1, levels(CountryName), xpd=TRUE, srt=25, cex=0.7)
  	paleta<-rainbow(length(levels(as.factor(P))))
  	for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c]) }
 	 
  	#with legend
  	plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
  	for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c])}
  	legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
 	 
  	#condicionades a columna
  	plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
  	paleta<-rainbow(length(levels(as.factor(P))))
  	for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c]) }
 	 
  	#with legend
  	plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
  	for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c])}
  	legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
 	 
  	table<-table(dades[,k],P)
  	print("Cross Table:")
  	print(table)
  	print("Distribucions condicionades a columnes:")
  	print(colperc)
 	 
  	#diagrames de barres apilades                                    	 
 	 
  	paleta<-rainbow(length(levels(dades[,k])))
  	barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
 	 
  	barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
  	legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
 	 
  	#diagrames de barres adosades
  	barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta )
 	 
  	barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta)
  	legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
 	 
  	print("Test Chi quadrat: ")
  	print(chisq.test(dades[,k], as.factor(P)))
 	 
  	print("valorsTest:")
  	print( ValorTestXquali(P,dades[,k]))
  	#calcular els pvalues de les quali
	}
  }
}#endfor

#descriptors de les classes més significatius. Afegir info qualits
for (c in 1:length(levels(as.factor(P)))) {
  if(!is.na(levels(as.factor(P))[c])){
	print(paste("P.values per class:",levels(as.factor(P))[c]));
	print(sort(pvalk[c,]), digits=3)
  }
}

#afegir la informacio de les modalitats de les qualitatives a la llista de pvalues i fer ordenacio global

#saving the dataframe in an external file
#write.table(dd, file = "credscoClean.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

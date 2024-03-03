rm(list=ls()) # limpiar los datos del RStudio
dev.off() # limpiar los plots del RStudio
#install.packages("plyr") # This package is useful as a data manipulator 
#install.packages("ggplot2") #This package is useful for visualizing data
library(plyr)    
library(ggplot2) 

# cargar base de datos
setwd("F:/q6/dm")
dd <- read.table("ObesityDatasetAmbMissingValues.csv",header=T, sep=",");

#-------------------------------------------------------------
# versio no fusionada
fullvariables<-c(4,6)
aux<-dd[,fullvariables]
dim(aux)
names(aux)

# divide in rows that had missing incomes or not on the target variable to be imputed
aux1 <- aux[!is.na(dd$Weight),]
dim(aux1)
aux2 <- aux[is.na(dd$Weight),]
dim(aux2)

any(is.na(aux1))

#Find nns for aux2
knn.ing = knn(aux1,aux2,dd$Weight[!is.na(dd$Weight)])

#-------------------------------------------------------------
# versio fusionada

uncompleteVars<-c(5,9,10,13,15,16)
#auxu<-dd[,uncompleteVars]
#dim(auxu)
#names(auxu)
#better if you sort them by increasing number of missing values

fullVariables<-c(4,6)
aux<-dd[,fullVariables]
dim(aux)
names(aux)

for (k in uncompleteVars){
  aux1 <- aux[!is.na(dd[,k]),]
  dim(aux1) 
  aux2 <- aux[is.na(dd[,k]),]
  dim(aux2)
  
  RefValues<- dd[!is.na(dd[,k]),k]
  #Find nns for aux2
  knn.values = knn(aux1,aux2,RefValues)   
  
  #CARE: neither aux1 nor aux2 can contain NAs
  
  
  #CARE: knn.ing is generated as a factor. 
  #Be sure to retrieve the correct values
  
  dd[is.na(dd[,k]),k] = as.numeric(as.character(knn.values))
  fullVariables<-c(fullVariables, k)
  aux<-dd[,fullVariables]
}

dim(dd)
summary(dd)

#dd[dd == ""] <- NA
write.csv(dd, "ObesityWithKnnNeighbors.csv")


# ------------------------------------------------------------
# funcionalitats alternatives (independents de la execucio del knn)

# resumen de cada variable de la dd (incluye median, quantile...)
summary(dd)

# barplot -> variables descriptives
barplot(table(dd$Gender))

# histogram -> variables qualitatives
d1 <- subset(dd, dd$Gender=="Male")
d2 <- subset(dd, dd$Gender=="Female")
hist(dd$Weight, breaks = 10, xlim=c(30,200)) 
hist(d1$Weight, breaks = 10, xlim=c(30,200))
hist(d2$Weight, breaks = 10, xlim=c(30,200))

head(dd)

boxplot(dd$Weight~dd$family_history_with_overweight)
d3 <- subset(dd, dd$family_history_with_overweight=="no")
d4 <- subset(dd, dd$family_history_with_overweight=="yes")
par(mfrow=c(1,2))
hist(d3$Weight, breaks = 10, xlim=c(30,200))
hist(d4$Weight, breaks = 10, xlim=c(30,200))

StackedBar = ggplot(dd, aes(dd$Weight, fill = Gender)) + 
  geom_bar(stat=dd$Weight fun.y="mean") + 
  scale_fill_brewer(palette = "Set2") + ggtitle("Peso genero")
StackedBar


# load database
dd <- read.table("ObesityDatasetAmbMissingValues.csv",header=T, sep=",");

#-------------------------------------------------------------
# version without fusion
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

uncompleteVars<-c(2,4,7,8,13,14)
#auxu<-dd[,uncompleteVars]
#dim(auxu)
#names(auxu)
#better if you sort them by increasing number of missing values

fullVariables<-c(3,11)
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

columns_to_round <- c("Age", "Weight")
dd[columns_to_round] <- apply(dd[columns_to_round], 2, round, digits = 0)

# Save the obesity dataset in the ObesityWithKnnNeighbours.csv file
write.csv(dd, "ObesityWithKnnNeighbors.csv", row.names = FALSE)

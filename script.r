# limpiar los datos del RStudio -> rm(list=ls())
# limpiar los plots del RStudio -> dev.off()

# cargar base de datos
setwd("C:/Users/oscar.garries/Downloads/")
dd <- read.table("ObesityDatasetAmbMissingValues.csv",header=T, sep=",");

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

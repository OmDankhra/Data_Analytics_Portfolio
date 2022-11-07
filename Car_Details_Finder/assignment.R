#OM DANKHRA
#40998226
#ASSIGNMENT 1

#PART 1 A
myCarList <- read.csv("assignment1.csv", sep = ";")
print(myCarList)
head(myCarList, 6)

#PART 1 B
myCarList[-c(1),]
print(ncol(myCarList))
print(nrow(myCarList))

#PART 1 C
lapply(myCarList,class)
myCarList$MPG <- as.numeric(myCarList$MPG)
myCarList$Cylinders <- as.numeric(myCarList$Cylinders)
myCarList$Displacement <- as.numeric(myCarList$Displacement)
myCarList$Horsepower <- as.numeric(myCarList$Horsepower)
myCarList$Weight <- as.numeric(myCarList$Weight)
myCarList$Acceleration <- as.numeric(myCarList$Acceleration)
myCarList$Model<- as.numeric(myCarList$Model)
myCarList$Origin<- as.factor(myCarList$Origin)
lapply(myCarList,class)

#PART 1 D
m <- max(myCarList$MPG, na.rm = TRUE)
m
retval <- subset(myCarList, MPG == m)
print(retval)

retval <- subset(myCarList, Horsepower > 100)
write.csv(retval,"Horsepower.csv")
print(retval)

retval <- subset(myCarList, Acceleration < 15)
write.csv(retval,"Acceleration.csv")
print(retval)

h = as.numeric(myCarList[,2], na.rm = TRUE)
hist(h, breaks = 9, main = "MPG Histogram", col = "red", xlab = "MPG")

#PART 2 A
set.seed(75)
aVec  = sample(0:500, 50, replace=TRUE)
aVec
bVec  = sample(0:500, 50, replace=TRUE)
bVec

#PART 2 B
result <- bVec[1:50-1] - aVec[2:50]
matrix(result, 7, 7, byrow = FALSE)

#PART 2 C
b <- cos(bVec[1:50-1])
a <- sin(aVec[2:50])
result <- b/a
result

#PART 2 D
s <- 1
i <- seq(s, (aVec[1:50-1]), aVec[2:50])
result <- sum(exp(-aVec[1:50-1])/aVec[2:50])
result

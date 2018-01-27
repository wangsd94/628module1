setwd("E:/wisc/628/module 1")
BodyFat <- read.csv("BodyFat.csv")
model = lm(BODYFAT~.,data=BodyFat[,c(-1,-3,-7)])  #we can not measure density directly
summary(model)
plot(model,which=4)
abline(h=4/(252-13),lty=2)
layout(matrix(1:4,ncol=2))
plot(model)
layout(1)
BodyFat[39,]
BodyFat[86,]
BodyFat[175,]

model = lm(BODYFAT~.,data=BodyFat[-39,c(-1,-3,-7)])  #we can not measure density directly
summary(model)
plot(model,which=4)
abline(h=4/(251-13),lty=2)
layout(matrix(1:4,ncol=2))
plot(model)
layout(1)
BodyFat[42,]
BodyFat[31,]

model = lm(BODYFAT~.,data=BodyFat[c(-39,-42),c(-1,-3,-7)])  #we can not measure density directly
summary(model)
plot(model,which=4)
abline(h=4/(251-13),lty=2)
layout(matrix(1:4,ncol=2))
plot(model)
layout(1)
BodyFat[41,]
BodyFat[221,]


setwd("E:/wisc/628/module 1")
BodyFat <- read.csv("BodyFat.csv")
model = lm(BODYFAT~.,data=BodyFat[,c(-1,-3)])  #we can not measure density directly
plot(model,which=4)
abline(h=4/(252-13),lty=2)
layout(matrix(1:4,ncol=2))
plot(model)
layout(1)
BodyFat[39,]
BodyFat[86,]
BodyFat[175,]

model = lm(BODYFAT~.,data=BodyFat[-39,c(-1,-3)])
plot(model,which=4)
abline(h=4/(251-13),lty=2)
layout(matrix(1:4,ncol=2))
plot(model)
layout(1)
BodyFat[42,]
BodyFat[31,]

model = lm(BODYFAT~.,data=BodyFat[c(-39,-42),c(-1,-3)])
plot(model,which=4)
abline(h=4/(250-13),lty=2)
layout(matrix(1:4,ncol=2))
plot(model)
layout(1)
BodyFat[41,]
BodyFat[221,]



library(car)
outlierTest(model)
BodyFat[224,]

summary(model)
#mallow cp
X <- BodyFat[c(-39,-42,-182),c(-1,-2,-3)]
Y <- BodyFat[c(-39,-42,-182),1]
library(leaps)
library(faraway)
g <- leaps(X,Y)
Cpplot(g)
g <- leaps(X,Y,nbest = 1)
Cpplot(g)


model.AIC <- step(model, k=2)
## BODYFAT ~ AGE + HEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST



model.BIC <- step(model, k=log(250))
## BODYFAT ~ AGE + ABDOMEN + WRIST

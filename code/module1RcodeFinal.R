setwd( "/Users/jing/Desktop/628")
BodyFat <- read.csv("BodyFat.csv")
#data cleaning
BodyFat[,6] = BodyFat[,6]*2.54
BodyFat$IDNO[which(BodyFat$BODYFAT<=0)]
BodyFat[182,]
model0 = lm(BODYFAT~.,data=BodyFat[-182,c(-1,-3)])  
summary(model0)
layout(1)
plot(model0,which=4)
abline(h=4/(251-14),lty=2)
layout(matrix(1:4, ncol=2))
plot(model0)
BodyFat[42,]
summary(model1 <- lm(BODYFAT~., data=BodyFat[c(-182,-42),c(-1,-3)]))
layout(1)
plot(model1,which=4)
abline(h=4/(250-14),lty=2)
layout(matrix(1:4, ncol=2));plot(model1)
BodyFat[39,]
model2 <- lm(BODYFAT~., data=BodyFat[-c(39,42,182),c(-1,-3)])
layout(matrix(1:4, ncol=2))
plot(model2)
bodyfat<-BodyFat[-c(39,42,182),c(-1,-3)]
bodyfat[c(86,224,221,207),]
#mallow cp
X <- model.matrix(model2)[,-1]
Y <- BodyFat[-c(39,42,182),2]
library(leaps) # for leaps()
library(faraway) # for Cpplot()
layout(1)
g <- leaps(X,Y, nbest=1)
Cpplot(g)
cp.choice1 <- c(1,3,6,7,14)+3
summary(model.cp <- lm(BODYFAT ~ ., data=BodyFat[-c(39,42),c(2,cp.choice1)]))
layout(matrix(1:4, ncol=2));plot(model.cp)
g1<- leaps(X,Y, nbest=1, method="adjr2")
layout(1);plot(g1$adjr2)
(g1$which)[which(g1$adjr2 == max(g1$adjr2)),]
r2.choice <- c(4,7:13,16:17)
summary(model.r2 <- lm(BODYFAT ~ ., data=BodyFat[-c(39,42,182),c(2,r2.choice)]))
layout(matrix(1:4, ncol=2));plot(model.r2)
#AIC
BodyFat = read.csv("BodyFat.csv")


model.clean <- lm(BODYFAT ~ ., data=BodyFat[-182, c(-1,-3)])
plot(model.clean, which=4)
abline( h = 4/(251-14),lty=2)
layout(matrix(1:4, ncol=2))
plot(model.clean)

model.clean <- lm(BODYFAT ~ ., data=BodyFat[-c(42,182), c(-1,-3)])
plot(model.clean, which=4)
abline( h = 4/(250-14),lty=2)
layout(matrix(1:4, ncol=2))
plot(model.clean)

model.clean <- lm(BODYFAT ~ ., data=BodyFat[-c(39,42,182), c(-1,-3)])
plot(model.clean, which=4)
abline( h = 4/(249-14),lty=2)
layout(matrix(1:4, ncol=2))
plot(model.clean)
BodyFat[c(86,207,221,224),]

bodyfat = BodyFat[c(-39,-42,-182),c(-1,-3)]
model = lm(BODYFAT~.,data=bodyfat)
<<<<<<< HEAD
model.AIC <- step(model, k=2)
model1 = lm(BODYFAT ~ AGE + ADIPOSITY + NECK + CHEST + ABDOMEN + HIP + FOREARM + WRIST, data=bodyfat)
summary(model1); 
anova(model1)$`Mean Sq`[9]
qqnorm(model1$residuals)
qqline(model1$residuals, col = "red")
shapiro.test(model1$residuals)
plot(model.BIC$fitted.values,model.AIC$residuals, main = "Residuals Plot")
#BIC
model.BIC <- step(model, k=log(249))
model2 = lm(BODYFAT ~ AGE + ADIPOSITY + CHEST + ABDOMEN  + WRIST, data = bodyfat)
summary(model2); anova(model2)$`Mean Sq`[6]
qqnorm(model2$residuals); qqline(model2$residuals, col = "red")
shapiro.test(model2$residuals)
plot(model.BIC$fitted.values,model.BIC$residuals, main = "Residuals Plot")
#confident interval
coef.BIC = coef(summary(model2))
coef.BIC[,1] + qnorm(p = 0.975) * coef.BIC[,2]
confint(model2)
#prediction
model2.pre = data.frame(predict(model2,interval="confidence"))
model2.pre$x = seq(nrow(model2.pre))
plot(fit ~ x, data = model2.pre, ylim = range(c(model2.pre$lwr,model2.pre$upr)), main = "Predicted Values with Interval")
with(model2.pre, polygon(c(x,rev(x)), c(lwr,rev(upr)),col = "grey75", border = FALSE))
matlines(model2.pre[,4], model2.pre[,-4], lwd=c(2,1,1),lty=1,col=c("blue","red","red")) 
plot(bodyfat$BODYFAT, type = "l", col = "red", main = "Real Vales vs Predicted Values")
lines(model2.pre$fit, col = "blue")
legend("topleft", legend=c("real values", "predicted values"),col=c("red", "blue"), lty=1, cex=0.8)
library(ggplot2);options(repr.plot.width=8, repr.plot.height=2);ggplot()+geom_line(aes(c(1:249,1:249),c(model.BIC.pre$fit,bodyfat$BODYFAT),color=as.factor(rep(c("Predicted value","True value"),each=249))))+
  theme(legend.title=element_blank(),axis.title=element_blank())+ggtitle("Predicted Value VS True Value")

=======
summary(model)

model.AIC <- step(model, k=2)
model1 = lm(BODYFAT ~ AGE + ADIPOSITY + NECK + CHEST + ABDOMEN + HIP + FOREARM + WRIST, data=bodyfat)
summary(model1)
anova(model1)

model.BIC <- step(model, k=log(249))
model2 = lm(BODYFAT ~ AGE + ADIPOSITY + CHEST + ABDOMEN + WRIST, data = bodyfat)
summary(model2)
anova(model2)
>>>>>>> origin/master

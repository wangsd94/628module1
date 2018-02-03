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
summary(model)

model.AIC <- step(model, k=2)
model1 = lm(BODYFAT ~ AGE + ADIPOSITY + NECK + CHEST + ABDOMEN + HIP + FOREARM + WRIST, data=bodyfat)
summary(model1)
anova(model1)

model.BIC <- step(model, k=log(249))
model2 = lm(BODYFAT ~ AGE + ADIPOSITY + CHEST + ABDOMEN + WRIST, data = bodyfat)
summary(model2)
anova(model2)

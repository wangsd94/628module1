BodyFat = read.csv("BodyFat.csv")
bodyfat = BodyFat[c(-39,-42,-182),c(-1,-3)]
model = lm(BODYFAT~.,data=bodyfat)
summary(model)


model.AIC <- step(model, k=2)
model1 = lm(BODYFAT ~ AGE + ADIPOSITY + NECK + CHEST + ABDOMEN + HIP + FOREARM + WRIST, data=bodyfat)
summary(model1)
anova(model1)
## BODYFAT ~ AGE + ADIPOSITY + NECK + CHEST + ABDOMEN + HIP + FOREARM + WRIST


model.BIC <- step(model, k=log(247))
model2 = lm(BODYFAT ~ AGE + ADIPOSITY + CHEST + ABDOMEN + WRIST, data = bodyfat)
summary(model2)
anova(model2)
## BODYFAT ~ AGE + ADIPOSITY + CHEST + ABDOMEN + WRIST
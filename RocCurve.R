#ROC Curve
library(pROC)
detach(df)
df <- read.csv("2016_clean.csv")
df$best <- ifelse(df$HR <=32, TRUE, FALSE)
attach(df)
m6 <- glm(best ~ GpC + GC + Freedom + Region,family = binomial, data = df)
prob=predict(m6,type=c("response"))

df$prob=prob
g6 <- roc(best ~ prob, data = df)
plot(g6)
auc(g6)

probM3=predict(m3,type=c("response"))
g3 <- roc(best ~probM3, data = df)
plot(g3)
auc(g3)

g5 <- roc(best ~ model.probsM5, data = df)
plot(g5)
auc(g5)
detach(df)



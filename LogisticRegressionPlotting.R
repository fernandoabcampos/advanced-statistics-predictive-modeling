df <- read.csv("2016_clean.csv")
df$best <- ifelse(df$HR <= 32, TRUE, FALSE)
detach(df)
attach(df)
m6 <- glm(best ~ GpC + GC + Freedom + Region, family = binomial, data = df)
AIC(m6)

model.probs = predict(m6, type="response")

model.pred = rep(FALSE, nrow(df)) #1st crear un vector de N FALSE elementos.
threshold <- 0.8 # suponiendo un umbral de discriminación del 80%
model.pred[model.probs >= threshold] = TRUE # Transformar a TRUE predicciones que superan probabilidad del umbral.

# Crear la matriz de confusión
table(model.pred, best)

predict(m6, type="response")

model.pred
options(scipen=999)

new.df <- df
new.df$predicted = model.probs
#newdat <- data.frame(hp=seq(min(mtcars$hp), max(mtcars$hp),len=100))
#newdat$vs = predict(fit, newdata=newdat, type="response")
plot(best~Country, data=df, col="red4")
lines(best ~ Country, new.df, col="green4", lwd=2)
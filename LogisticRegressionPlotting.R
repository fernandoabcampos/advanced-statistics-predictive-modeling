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

#options(scipen=999)

new.df <- df
df$predicted = model.probs
#newdat <- data.frame(hp=seq(min(mtcars$hp), max(mtcars$hp),len=100))
#newdat$vs = predict(fit, newdata=newdat, type="response")

detach(df)
attach(new.df)

with(new.df, plot(HR ~ predicted, lwd = 2))
threshold
df.filtered <- new.df[which(new.df$predicted >=0.85),]


threshold<-0.85
df.filtered85 <- new.df[which(new.df$predicted >=0.85),]
detach(df.filtered)
attach(df.filtered85)
par(las=1)
with(df.filtered, plot(HR ~ predicted, col = ifelse(HR > 32,'red','green'), pch = 19))
text(HR ~ predicted, labels=paste(HR, "-", Country), cex= 0.6, pos=1, lty=0.1)

par(las=1)
with(df.filtered, plot(HR ~ predicted, col = ifelse(HR > 32,'red','green'), pch = 19))
text(HR ~ predicted, labels=paste(HR, "-", Country), cex= 0.6, pos=1, lty=0.1)



model.probsM5 = predict(m5, type="response")
model.predM5 = rep(FALSE, nrow(df)) #1st crear un vector de N FALSE elementos.
model.predM5[model.probsM5 >= threshold] = TRUE # Transformar a TRUE predicciones que superan probabilidad del umbral.
detach(df.filtered)
attach(df)
table(model.predM5, best)


df$predictedM5 = model.probsM5
detach(df)
df.filteredM5 <- df[which(df$predictedM5 >=0.8),]
attach(df.filteredM5)
with(df.filteredM5, plot(HR ~ predictedM5, col = ifelse(HR > 32,'red','green'), pch = 19))
text(HR ~ predictedM5, labels=paste(HR, "-", Country), cex= 0.4)


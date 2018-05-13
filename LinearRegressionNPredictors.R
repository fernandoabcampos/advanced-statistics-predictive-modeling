
df <- read.csv("2016_clean.csv")
head(df)
attach(df)
names(df)

model.quant <- lm(formula =  HS ~ GpC + LE + GC, data = df)
model.quant
summary(model.quant)$r.sq
summary(model.quant)

?lm

df2 = within(df, Region <- relevel(Region, ref = "Western Europe"))
str(df)
str(df2)

model.qualit <- lm(formula =  df2$HS ~ df2$GpC + df2$LE + df2$GC + df2$Region, data = df2)
coef(model.qualit)
coef(model.quant)

(Intercept) 

summary(model.quant)
summary(model.qualit)$r.sq
summary(model.qualit)


models <- c("Apartado 1.1", "Apartado 1.2")
ajuste <- c(summary(model.quant)$r.sq, summary(model.qualit)$r.sq)
kable(data.frame(variables=models,'Bondad del Ajuste'=ajuste))

library('kableExtra')
models <- c("Apartado 1.1", "Apartado 1.2")
ajuste <- c(summary(model.quant)$r.sq, summary(model.qualit)$r.sq)
dfE <- data.frame(variables=models,'Bondad del Ajuste'=as.vector(ajuste))
kable(dfE)





library(ggplot2)
par( mfrow=c(1,3))
ggplot(df, aes(x = GpC, y = HS)) + 
  geom_point(aes(color = GpC)) + xlab("GpC") + 
  ggtitle("(a) Happiness Score & Gross per Capita")

ggplot(df, aes(x = LE, y = HS)) + 
  geom_point(aes(color = LE)) + xlab("LE") + 
  ggtitle("(d) Happiness Score & Life Expectancy")

ggplot(df, aes(x = GC, y = HS)) + 
  geom_point(aes(color = GC)) + xlab("GC") + 
  ggtitle("(c) Happiness Score & Government Corruption")
par( mfrow=c(1,1))

contrasts(Region)

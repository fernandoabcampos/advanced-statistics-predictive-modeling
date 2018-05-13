
df <- read.csv("2016_clean.csv")
head(df)
attach(df)

model <- lm(formula =  HS ~ GpC + LE + GC, data = df)
model
summary(model)$r.sq


ggplot(df, aes(x = , y = `Premature Death`)) + geom_point(aes(color = )) + 
  xlab("Percentage of population under \n age 65 without health insurance") + 
  ylab("Years of potential life lost before \n age 75 per 100,000 population") + 
  ggtitle("(b) Premature Death & Uninsured")

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

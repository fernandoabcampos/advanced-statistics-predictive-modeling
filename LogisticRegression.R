# Logistic Regression exercises
df <- read.csv("2016_clean.csv")
attach(df)

m3 <- glm(best ~ GpC + GC,family = binomial, data = df)

summary (glm.fits)

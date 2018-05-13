
df <- read.csv("2016_clean.csv")
head(df)
twoPredictorModel <- lm(ROLL ~ UNEM + HGRAD, datavar)
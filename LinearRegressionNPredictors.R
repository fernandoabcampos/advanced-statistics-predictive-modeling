
df <- read.csv("2016_clean.csv")
head(df)
nPredictor <- lm(df$HS ~ df$GpC + df$LE + df$GC, df)
nPredictor

# Logistic Regression exercises
df <- read.csv("2016_clean.csv")
attach(df)
df$best <- ifelse(df$HR <=32, TRUE, FALSE)
m3 <- glm(best ~ GpC + GC,family = binomial, data = df)
m3
summary(m3)


coef(m3)[2]

coef(summary(m3))[,4]
summ <- summary(m3)
str(summ, max = 1)


coef(summary(m3))[row.names(coef(summary(m3))) %in% "GC" , 4]
coef(summary(m3))[row.names(coef(summary(m3))) %in% "GpC" , 4]


new.df <- data.frame(GpC = 1.5, GC = 0.35)
predict(m3, new.df, type = "response")




#https://stats.stackexchange.com/questions/52475/how-are-the-p-values-of-the-glm-in-r-calculated/52476

####
#Now we’ll calculate Z score and p-Value for the variables in the model.

#z <- summary(test)$coefficients/summary(test)$standard.errors
#z

##          (Intercept)  sesmiddle   seshigh     write
## general     2.445214 -1.2018081 -2.261334 -2.705562
## vocation    4.484769  0.6116747 -1.649967 -5.112689

#p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
#p <- (1 - pnorm(abs(z), 0, 1))*2
#p

##           (Intercept) sesmiddle    seshigh        write
## general  0.0144766100 0.2294379 0.02373856 6.818902e-03
## vocation 0.0000072993 0.5407530 0.09894976 3.176045e-07

#exp(coef(test))

##          (Intercept) sesmiddle   seshigh     write
## general     17.32582 0.5866769 0.3126026 0.9437172
## vocation   184.61262 1.3382809 0.3743123 0.8926116

#The p-Value tells us that ses variables are not significant.  Now we’ll explore the entire data set, and analyze if we can remove any variables which do not add to model performance.

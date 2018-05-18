#ROC Curve
library(pROC)
df$best <- ifelse(df$HR <=32, 1, 0)
attach(df)
roc_obj <- roc(best, model.probsM5)
auc(roc_obj)



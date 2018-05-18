#ROC Curve
library(pROC)
df$best <- ifelse(df$HR <=32, 1, 0)
attach(df)
roc_obj <- roc(best, model.probsM5)
auc(roc_obj)


roc_df <- data.frame(
  TPR=rev(roc_obj$sensitivities), 
  FPR=rev(1 - roc_obj$specificities), 
  labels=roc_obj$response, 
  scores=roc_obj$predictor)






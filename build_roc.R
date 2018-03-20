library(ROCR)
library(caret)

plot_sensitivity_and_acc = function(preds, obs){
  
  preds = prediction( preds,  obs)
  par(mfrow=c(1,2))
  plot(performance(preds, "sens" , x.measure = "cutoff"), col = 'red', ylab= NULL)
  par(new=T)
  plot(performance(preds, "spec" , x.measure = "cutoff"),add = TRUE, col = 'blue', xlab = NULL)
  axis(side = 4,  at = .5, labels = 'Specificity', padj = 1 )
  
  plot(performance(preds, "acc" , x.measure = "cutoff"), col = 'red', ylab= NULL)
}

plot_roc = function(pred1, pred2, pred3){
  par(mfrow=c(1,1))
  perf = performance( pred1, "tpr", "fpr" )
  perf2 = performance(pred2, "tpr", "fpr")
  perf3 = performance(pred3, "tpr", "fpr")
  sens1 = performance(pred1,"sens","spec")
  sens2 = performance(pred2,"sens","spec")
  sens3 = performance(pred3,"sens","spec")
  auc1 = performance(pred1,"auc")
  auc2 = performance(pred2,"auc")
  auc3 = performance(pred3,"auc")
  plot( perf, col='Red')
  plot(perf2, add = T, col='Blue')
  plot(perf3, add = T, col='Green')
  legend(0.7,
         0.3,
         c(sprintf("%3.3f glm",auc1@y.values[[1]]),
           sprintf("%3.3f rf, cutoff = 0.50",auc2@y.values[[1]]),
           sprintf("%3.3f rf, cutoff = 0.45",auc3@y.values[[1]])),
         lty=c(1,1),
         lwd=c(2.5,2.5),
         c(col="red",
           col="blue",
           col="green"),
         title = "AUC")
  
}

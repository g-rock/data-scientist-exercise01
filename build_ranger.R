library(caret)
library(randomForest)
library(ranger)
library(dplyr)

build_ranger = function(formula, train){
  
  # Set up a grid of search values to test different ranger models
  mtry = c(1:7)
  trees = c(25, 50, 100, 250, 500)
  model.metrics = tibble(mtry = integer(),
                         trees = integer(),
                         train_accuracy = double())
  
  for (i in mtry){
    for (j in trees){
      print(paste("Building Ranger model with mtry=", i, "and num.trees=", j))
      
      fit = ranger(f, 
                   data=train,
                   mtry = i,
                   num.trees = j,
                   importance = 'impurity',
                   probability = T,
                   classification = T,
                   sample.fraction = 0.8)
      
      model.metrics = add_row(
        model.metrics,
        mtry = i,
        trees = j,
        train_accuracy = 1 - fit$prediction.error)
      
      # this is our best model using train accuracy
      if (i == 3 & j == 500){
        best_model = fit
      }
      
    }
  }
  
  return(best_model)

}

plot_rf_importance = function(model){
  var.imp = tibble(var= names(model$variable.importance), imp = model$variable.importance)
  ggplot(data=var.imp, aes(x = reorder(var, imp), y = imp)) +
    geom_bar(mapping = , stat="identity", fill = "dodgerblue3", color="black") + 
    ggtitle('Variable Importance: Gini Impurity') +
    xlab('Variables') +
    ylab('Relative Importance')+
    coord_flip()
}

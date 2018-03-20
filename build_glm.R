library(caret)

build_glm = function(formula, train){
  print("Building logistic model with 10 fold cross validation")
  train_control<- trainControl(method="cv", number=10)
  
  model.glm = train(f, data=train, trControl=train_control, method="glm",family=binomial())
  
  return(model.glm)
}

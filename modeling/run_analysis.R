source('modeling/prepare.R')
source('modeling/build_glm.R')
source('modeling/build_ranger.R')
source('modeling/build_roc.R')
library(caret)

set.seed(11)
dat = prepare()

target = 'over_50k'
cols = colnames(dat)

predictors = cols[!cols %in% c(target) ]

# Split data into train, validation, test

trainidx = createDataPartition(dat[,target], p=.5, list = F)
train = dat[trainidx, ]
nontrain = dat[-trainidx,]
validationidx = createDataPartition(nontrain[,target], p=.25, list=F)
x.validation = nontrain[validationidx, -which(names(nontrain) == c(target))]
y.validation = nontrain[validationidx, target]
x.test = nontrain[-validationidx, -which(names(nontrain) == c(target))]
y.test = nontrain[-validationidx, target]

# Down sample to get equal groups of salary < 50k & salary > 50k
# This step reduces overfitting of the models we will build 
ds = downSample(train[,predictors], train[,target], list = F)
names(ds)[names(ds) == 'Class'] = "over_50k"

f = formula(paste(target, paste(predictors, collapse = '+' ), sep = '~'))

model_metrics = tibble(Model = character(), Accuracy = character(), TPR = character(), TNR = character())
add_model_metric = function(metric_table, model_name, confMatrix){
  model_metrics = add_row(model_metrics,
                          Model = model_name,
                          Accuracy = sprintf("%3.3f",confMatrix$overall[['Accuracy']]),
                          TPR = sprintf("%3.3f", confMatrix$byClass[['Sensitivity']]),
                          TNR = sprintf("%3.3f", confMatrix$byClass[['Specificity']]))
  
  return(model_metrics)
}

model.glm = build_glm(formula = f, train = ds)

# [ Logisitc Regression 0.50 cutoff threshold ]
# Assess results by validation Accuracy, Sensitivity, Specificity, ROC

glm.Results = predict(model.glm, x.validation, type = 'prob')
glm.Results$preds = ifelse(glm.Results$under > 0.50, 'under', 'over')
glm.Results$obs = y.validation


validate.glmConfMatrix = confusionMatrix(glm.Results$preds, glm.Results$obs)
print(validate.glmConfMatrix)
model_metrics = add_model_metric(model_metrics, "Logistic Regression w/ 0.50 cutoff", validate.glmConfMatrix)

plot_sensitivity_and_acc(glm.Results$under, glm.Results$obs)

# ------------------------------------ # 

# [ Logisitc Regression 0.45 cutoff threshold ]

test.glm.Results = predict(model.glm, x.test, type = 'prob')
test.glm.Results$preds = ifelse(test.glm.Results$under > 0.45, 'under', 'over')
test.glm.Results$obs = y.test

test.glmConfMatrix = confusionMatrix(test.glm.Results$preds, test.glm.Results$obs)
print(test.glmConfMatrix)
model_metrics = add_model_metric(model_metrics, "Logistic Regression w/ 0.45 cutoff", test.glmConfMatrix)
            
plot_sensitivity_and_acc(test.glm.Results$under, test.glm.Results$obs)

# ------------------------------------ # 

# [ Ranger Random Forest Classifier 0.50 cutoff threshold ]

model.ranger = build_ranger(formula = f, train = ds)

# Assess ranger random forest results by validation Accuracy, Sensitivity, Specificity, ROC
validate.ranger.Results = as.data.frame(predict(model.ranger, x.validation)$predictions)
validate.ranger.Results$preds = ifelse(validate.ranger.Results$under > 0.50, 'under', 'over')
validate.ranger.Results$obs = y.validation

validate.rangerConfMatrix = confusionMatrix(validate.ranger.Results$preds, validate.ranger.Results$obs)
print(validate.rangerConfMatrix)

model_metrics = add_model_metric(model_metrics, "Random Forest w/ 0.50 cutoff", validate.rangerConfMatrix)

plot_sensitivity_and_acc(validate.ranger.Results$under, validate.ranger.Results$obs)

# ------------------------------------ # 

# [ Ranger Random Forest Classifier 0.45 cutoff threshold ]

test.ranger.Results = as.data.frame(predict(model.ranger, x.test)$predictions)
test.ranger.Results$preds = ifelse(test.ranger.Results$under > 0.45, 'under', 'over')
test.ranger.Results$obs = y.test

test.rangerConfMatrix = confusionMatrix(test.ranger.Results$preds, test.ranger.Results$obs)
print(test.rangerConfMatrix)
model_metrics = add_model_metric(model_metrics, "Random Forest w/ 0.45 cutoff", test.rangerConfMatrix)

plot_sensitivity_and_acc(test.ranger.Results$under, test.ranger.Results$obs)


# [ Plot AUC of each model ]

pred1 = prediction( glm.Results$under, glm.Results$obs )
pred2 = prediction( test.glm.Results$under, test.glm.Results$obs )
pred3 = prediction( validate.ranger.Results$under, validate.ranger.Results$obs )
pred4 = prediction ( test.ranger.Results$under, test.ranger.Results$obs )

plot_roc(pred1, pred2, pred3, pred4)
 
save(model_metrics, file = "markdown_data/model_comparisons.rda")

# [ An interesting chart for #7 on README.md using most important variables from ranger model ]

chart = dat %>%
  filter(relationship %in% c("Husband", "Wife", "Unmarried")) %>%
  filter(education %in% c("Bachelors", "Masters", "Doctorate", "Some-college", "Prof-school")) %>%
  filter(age >= 25 & age <= 55) %>%
  filter(hours_week >= 40) %>%
  group_by_(.dots=c("over_50k", "relationship")) %>%
  count()

bar = ggplot() +
  geom_bar(data=chart, aes(y = n, x = relationship, fill = over_50k), 
           stat="identity",
           position='stack')  +
  ggtitle("College graduates, age 25-55, working 40+ hours a week")



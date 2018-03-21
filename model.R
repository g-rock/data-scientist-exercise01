source('prepare.R')
source('build_glm.R')
source('build_ranger.R')
source('build_roc.R')
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
model.glm = build_glm(formula = f, train = ds)

# Assess logistic regression results by validation Accuracy, Sensitivity, Specificity, ROC
glm.Results = predict(model.glm, x.validation, type = 'prob')
glm.Results$preds = ifelse(glm.Results$under > 0.50, 'under', 'over')
glm.Results$obs = y.validation


glmConfMatrix = confusionMatrix(glm.Results$preds, glm.Results$obs)
print(glmConfMatrix)
model_metrics = add_row(model_metrics, Model = "Logistic Regression",
                        Accuracy = sprintf("%3.3f",glmConfMatrix$overall[['Accuracy']]),
                        TPR = sprintf("%3.3f", glmConfMatrix$byClass[['Sensitivity']]),
                        TNR = sprintf("%3.3f", glmConfMatrix$byClass[['Specificity']]))

plot_sensitivity_and_acc(glm.Results$under, glm.Results$obs)

model.ranger = build_ranger(formula = f, train = ds)

# Assess ranger random forest results by validation Accuracy, Sensitivity, Specificity, ROC
validate.ranger.Results = as.data.frame(predict(model.ranger, x.validation)$predictions)
validate.ranger.Results$preds = ifelse(validate.ranger.Results$under > 0.50, 'under', 'over')
validate.ranger.Results$obs = y.validation

ranger1ConfMatrix = confusionMatrix(validate.ranger.Results$preds, validate.ranger.Results$obs)
print(ranger1ConfMatrix)

model_metrics = add_row(model_metrics,
                      Model = "Random Forest w/ 0.50 cutoff",
                      Accuracy = sprintf("%3.3f",ranger1ConfMatrix$overall[['Accuracy']]),
                      TPR = sprintf("%3.3f",ranger1ConfMatrix$byClass[['Sensitivity']]),
                      TNR = sprintf("%3.3f",ranger1ConfMatrix$byClass[['Specificity']]))

plot_sensitivity_and_acc(validate.ranger.Results$under, validate.ranger.Results$obs)

# Assess ranger random forest results with cutoff threshold = 0.45
test.ranger.Results = as.data.frame(predict(model.ranger, x.test)$predictions)
test.ranger.Results$preds = ifelse(test.ranger.Results$under > 0.45, 'under', 'over')
test.ranger.Results$obs = y.test

ranger2ConfMatrix = confusionMatrix(test.ranger.Results$preds, test.ranger.Results$obs)
print(ranger2ConfMatrix)
model_metrics = add_row(model_metrics,
                      Model = "Random Forest w/ 0.45 cutoff",
                      Accuracy = sprintf("%3.3f", ranger2ConfMatrix$overall[['Accuracy']]),
                      TPR = sprintf("%3.3f",ranger2ConfMatrix$byClass[['Sensitivity']]),
                      TNR = sprintf("%3.3f",ranger2ConfMatrix$byClass[['Specificity']]))
plot_sensitivity_and_acc(test.ranger.Results$under, test.ranger.Results$obs)

pred = prediction( glm.Results$under, glm.Results$obs )
pred2 = prediction( validate.ranger.Results$under, validate.ranger.Results$obs )
pred3 = prediction ( test.ranger.Results$under, test.ranger.Results$obs )

plot_roc(pred, pred2, pred3)
 
save(model_metrics, file = "model_comparisons.rda")

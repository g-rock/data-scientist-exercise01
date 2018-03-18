library(caret)
library(dplyr)
source('prepare.R')

dat = prepare()

target = 'over_50k'
cols = colnames(dat)
predictors = cols[!cols %in% c("id", target)]

# split
trainidx = createDataPartition(dat[,target], p=.8, list = FALSE)
train = dat[trainidx, ]
x.test = dat[-trainidx, -which(names(dat) == target)]
y.test = dat[-trainidx, target]


library(tidyverse)
library(Amelia)

# [DATA PREP]

prepare = function(){
  
  # coerce '?' found in csv to <NA>
  dat = read.csv("flattened.csv", na.strings="?")
  cols = colnames(dat)
  factors = cols[!cols %in% c("_id", "capital_loss", "capital_gain", "age", "hours_week")]
  
  for (factor in factors){
    dat[,factor] <- as.factor(dat[, factor])
  }
  
  dat = dat[complete.cases(dat),]
  
  drops <- c("id","country")
  dat = dat[ , !(names(dat) %in% drops)]
  
  levels(dat$over_50k) = c("under", "over")
  
  return(dat)
}

# [EXPLORATORY ANALYSIS]

dat = prepare()

# missmap(dat, main = "Missing values vs observed")

# A function that produces a bar plot of a given categorical predictor's distribution
# and stacks each bar by the target: over_50k 
plot_target_barplot = function(predictor){
  
  ggplot(data = dat) +
    geom_bar(mapping = aes(x = dat[,predictor], fill = over_50k)) +
    theme(text = element_text(size=8), axis.text.x=element_text(angle=90, hjust=1)) +
    ggtitle(paste(predictor, "in 1996 Census"))
  
}

target_barplot("hours_week")
target_barplot("age")
target_barplot("workclass")
target_barplot("education")
target_barplot("marital_status")
target_barplot("occupation")
target_barplot("relationship")
target_barplot("race")
target_barplot("sex")
# target_barplot("country")


library(corrplot)
c <- cor(dat[,c("capital_gain", "age", "hours_week", "capital_loss")])
corrplot(c, method = 'color', title="Numeric correlation ", tl.srt = 45, tl.cex=1, mar=c(0,0,1,0))

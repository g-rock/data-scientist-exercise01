library(tidyverse)

# [DATA PREP]

prepare = function(){
  
  # coerce '?' found in csv to <NA>
  dat = read.csv("flattened.csv", na.strings="?")
  cols = colnames(dat)
  factors = cols[!cols %in% c("_id", "capital_loss", "capital_gain")]
  
  for (factor in factors){
    dat[,factor] <- as.factor(dat[, factor])
  }
  
  return(dat)
}

# [EXPLORATORY ANALYSIS]

dat = prepare()

# A function that produces a bar plot of a given categorical predictor's distribution
# and stacks each bar by the target: over_50k 
target_barplot = function(predictor){
  
  ggplot(data = dat) +
    geom_bar(mapping = aes(x = dat[,predictor], fill = over_50k)) +
    theme(text = element_text(size=8), axis.text.x=element_text(angle=90, hjust=1)) +
    ggtitle(paste(predictor, "Distribution in the Census Dataset"))
  
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
target_barplot("country")

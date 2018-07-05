# -- Setup -------

# Libraries
library(fst)
library(tidyverse)

# Link to data folder
data_folder <- "C:/Kaggle/2018-06-23 - Santander/data"

# -- Get the data -------
train_raw <- read_fst(paste0(data_folder,"/processed_data/train_raw.fst"))

# -- Analysis of target -------

# Histogram
source("./R/aux_scripts/histo_box_plot.R")
histo_box_plot(
  data=train_raw,
  target="f677d4d13"
)

# Key stats
summary(train_raw$target)

# -- Missing values in the features -------

# Total missings across all (0 apparently)
sum(is.na(train_raw%>%select(-ID)))

# -- Most correlated features with the target -------
source("./R/aux_scripts/gini_coefficient.R")

correlations_spearman <- train_raw %>%
  select(-ID, -target) %>%
  cor(train_raw$target, method = "spearman") %>%
  as.data.frame() %>%
  rename(corr_spearman = V1)

correlations_pearson <- train_raw %>%
  select(-ID, -target) %>%
  cor(train_raw$target, method = "pearson") %>%
  as.data.frame() %>%
  rename(corr_pearson = V1) 

correlations <-
  cbind.data.frame(
    feature = row.names(correlations_spearman)
    ,correlations_spearman
    ,correlations_pearson
  ) %>% 
  mutate(
    abs_corr_spearman = abs(corr_spearman)
    ,abs_corr_pearson = abs(corr_pearson)
  )
  
head(correlations %>% dplyr::arrange(-abs_corr_spearman))
head(correlations %>% dplyr::arrange(-abs_corr_pearson))

# system.time({
#   for(i in c("X77eb013ca","a60027bb4","X3adf5e2b5","X186b87c05","f8b733d3f","X715fa74a4","b2541a277")){
#     x <- gini_coefficient(data=train_raw,target = "target",prediction = i)
#     print(i)
#     print(x)
#   }
# })
#     gini_coefficient(data=train_raw,target = "target",prediction = "X77eb013ca")
#     gini_coefficient(data=train_raw,target = "target",prediction = "b2541a277")



# -- Unknown proportions ------- ### There are no NAs!

# Calculate number of NAs for each feature
num_NAs <- sapply(train_raw,function(x){sum(is.na(x))})

# Get the ones with some NAs
num_NAs_gt0 <- num_NAs[num_NAs > 0]

# -- Vars with no variance -------

for_zero_var <- lapply(train_raw, function(x) length(unique(x)))
zero_var_cols <- for_zero_var[(for_zero_var == 1)]
unlist(zero_var_cols[1:3])

# -- check where max (or min) are over a certain threshold over the median -------

ranges_large <- sapply(
  train_raw %>% select(-ID,target)
  ,function(x){
    max(
      abs(max(x)/mean(x))
      ,abs(min(x)/mean(x))
    )
  }
)
ranges_large <- ranges_large[(order(ranges_large))]
ranges_large <- ranges_large[!is.nan(ranges_large)]

plot(ranges_large)

ranges_small <- sapply(
  train_raw %>% select(-ID,target)
  ,function(x){
    min(
      abs(max(x) /  mean(x))
      ,abs(min(x) / mean(x))
    )
  }
)
ranges_small <- ranges_small[(order(ranges_small))]
plot(ranges_small)

# -- vars with significant lack of variability / lots of weight at one value -------

low_var_feats <- nearZeroVar(
  train_raw
  ,freqCut = 98.5/1.5
  ,uniqueCut = 0.1
  ,saveMetrics = FALSE
  ,names = TRUE
)
  
setdiff(low_var_feats,names(zero_var_cols))
setdiff(names(zero_var_cols),low_var_feats)




  
# -- Setup -------

# Libraries
require(bit64)
require(fst)
require(data.table)
require(h2o)
require(caret)

# Link to data folder
data_folder <- "C:/Kaggle/2018-06-23 - Santander/data"

# Read in data from source
train_raw <- read_fst(paste0(data_folder,"/processed_data/train_raw.fst"))
test_raw <- read_fst(paste0(data_folder,"/processed_data/train_raw.fst"))

# -- Data Processing -------

# Identify low variance columns
low_var_feats <- lapply(
  train_raw
  ,function(x){
    is_near_zero_var(
      x
      ,primary_ratio_cut_off = 75
      ,primary_proportion_cut_off = 0.98
    )
  }
)

low_var_feats <- unlist(low_var_feats)
low_var_feats <- names(low_var_feats[(low_var_feats==TRUE)])
length(low_var_feats)

# Remove low var columns
modelling_all <- train_raw %>%
  dplyr::select(-one_of(low_var_feats))

# Create local train and validation
set.seed(1)

modelling_all[["split_var"]] = runif(nrow(modelling_all))

modelling_train <- modelling_all %>%
  dplyr::filter(split_var > 0.8)

modelling_validate <- modelling_all %>%
  dplyr::filter(split_var <= 0.8)

# -- Save stuff -------

write_fst(x = modelling_train,path = paste0(data_folder,"/processed_data/modelling_train.fst"))
write_fst(x = modelling_validate,path = paste0(data_folder,"/processed_data/modelling_validate.fst"))
write_rds(
  names(modelling_all%>%dplyr::select(-ID,target))
  ,paste0(data_folder,"/processed_data/feature_names.rds")
)



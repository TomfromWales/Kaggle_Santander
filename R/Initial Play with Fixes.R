# -- Setup -------

set.seed(1)

# Libraries
require(bit64)
require(fst)
require(data.table)
require(h2o)

# Link to data folder
data_folder <- "C:/Kaggle/2018-06-23 - Santander/data"

# -- Basic data admin -------

# Read in data from source
#train_raw <- fread(paste0(data_folder,"/source_data/train.csv"),data.table = FALSE,header = TRUE)
#test_raw <- fread(paste0(data_folder,"/source_data/test.csv"),data.table = FALSE,header = TRUE)
#test_raw <- read.csv(paste0(data_folder,"/source_data/test.csv"),header = TRUE)
# test_names <- names(test_raw)
# train_names <- names(train_raw)
# new_test_names = ifelse(
#   substr(test_names,1,1) == "X"
#   ,substr(test_names,2,length(test_names))
#   ,test_names
# )
# length(setdiff(train_names,new_test_names))
#names(test_raw) <- new_test_names

# Store as fst for faster use going forward
#write_fst(train_raw,paste0(data_folder,"/processed_data/train_raw.fst"))
#write_fst(test_raw,paste0(data_folder,"/processed_data/test_raw.fst"))

# Read in data


# -- EDA -------

# Histogram of target
source("./R/aux_scripts/histo_box_plot.R")
histo_box_plot(
  data=train_raw,
  target="target"
)

# -- Shit gbm -------
source("./R/aux_scripts/create_modelling_formula.R")
h2o.init()

# Split into a simple training and validation frame
train_raw$rand <- runif(nrow(train_raw))
training_df <- 
  train_raw %>%
  dplyr::filter(
    rand < 0.8
  )
validation_df <-
  train_raw %>%
  dplyr::filter(
    rand >= 0.8
  )

# Send data to h2o
as.h2o(training_df,destination_frame = "training_h2o")
as.h2o(validation_df,destination_frame = "validation_h2o")
  
# Build model
h2o.gbm(
  training_frame = h2o.getFrame("training_h2o")
  ,validation_frame = h2o.getFrame("validation_h2o")
  ,y = "target"
  ,x = setdiff(names(train_raw),c("ID","target"))
  ,ntrees = 80
  ,max_depth = 8
  ,learn_rate = 0.1
  ,distribution = "gamma"
  ,sample_rate = 0.8
  ,min_rows = 25
  ,model_id = "shit_gbm_h2o"
)    

h2o.saveModel(h2o.getModel("shit_gbm_h2o"),paste0(data_folder,"/models/"))

# -- Predictions for submission -------

h2o.init()

# Load model
h2o.loadModel(paste0(data_folder,"/models/shit_gbm_h2o"))

# Load test set
test_raw <- read_fst(paste0(data_folder,"/processed_data/test_raw.fst"))

# Send data to h2o
as.h2o(test_raw,"test_raw_h2o")

# Get preds
preds <- h2o.predict(h2o.getModel("shit_gbm_h2o"),h2o.getFrame("test_raw_h2o"),ntrees=57)
preds_for_submission <- cbind.data.frame(ID = test_raw[["ID"]],target = as.numeric(as.matrix(preds)))

# Get rescaled preds
train_raw_avg = mean(train_raw$target)
test_preds_avg = mean(preds_for_submission$target)
preds_for_submission_set2 <- preds_for_submission %>%
  mutate(
    target = target / test_preds_avg * train_raw_avg
  )

# Export
write.csv(preds_for_submission,paste0(data_folder,"/submissions/initial_play_submission_set1_with_fixes.csv"),row.names = FALSE)
write.csv(preds_for_submission_set2,paste0(data_folder,"/submissions/initial_play_submission_set2_with_fixes.csv"),row.names = FALSE)




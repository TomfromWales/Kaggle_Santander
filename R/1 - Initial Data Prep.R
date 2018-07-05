# -- Setup -------
# Libraries
require(bit64)
require(fst)
require(data.table)
require(h2o)

# Link to data folder
data_folder <- "C:/Kaggle/2018-06-23 - Santander/data"

# -- Basic data admin -------
# Read in data from source
train_raw <- fread(paste0(data_folder,"/source_data/train.csv"),data.table = FALSE,header = TRUE)
train_raw <- read.csv(paste0(data_folder,"/source_data/train.csv"),header = TRUE)
test_raw <- fread(paste0(data_folder,"/source_data/test.csv"),data.table = FALSE,header = TRUE)
test_raw <- read.csv(paste0(data_folder,"/source_data/test.csv"),header = TRUE)
# test_names <- names(test_raw)
# train_names <- names(train_raw)
# new_test_names = ifelse(
#   substr(test_names,1,1) == "X"
#   ,substr(test_names,2,length(test_names))
#   ,test_names
# )
# length(setdiff(train_names,new_test_names))
# names(test_raw) <- new_test_names

# Store as fst for faster use going forward
write_fst(train_raw,paste0(data_folder,"/processed_data/train_raw.fst"))
write_fst(test_raw,paste0(data_folder,"/processed_data/test_raw.fst"))



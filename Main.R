#### Config and Loading ####
library(pacman)
my_seed <- 7
p_load(h2o, reticulate, tensorflow, keras, tidyverse)

#### Functions ####
# Evaluation function
rmsle <- function(actual, predicted){
  ((log(actual) - log(predicted))^2) %>% mean %>% sqrt
}

#### H2o Start ####
h2o.init(nthreads = 7, max_mem_size = "16G")
h2o.removeAll()

#### Load Data ####
train_dat <- h2o.importFile(path= "https://www.dropbox.com/s/bawlkeolef1bse2/train_dat.csv?dl=1", 
                        sep= ",", header= T)
test_dat <- h2o.importFile(path= "https://www.dropbox.com/s/rbjatpuk5x7dios/test_dat.csv?dl=1", 
                       sep= ",", header= T)

#### Split dataset ####
splits <- h2o.splitFrame(train_dat, c(0.6, 0.2), seed = my_seed)
train <- h2o.assign(splits[[1]], "train.hex")
valid <- h2o.assign(splits[[2]], "valid.hex")
test <- h2o.assign(splits[[3]], "test.hex")
sampled_train = train

#### Config ####
response <- "y_train"
predictors <- setdiff(names(train), response)

layer1 <- seq(1, 300, 50)
layer2 <- seq(1, 300, 50)
layer3 <- seq(1, 300, 50)
layer4 <- seq(1, 300, 50)

layers <- expand.grid(layer1, layer2, layer3, layer4) %>% split(1:nrow(.)) %>% unname

hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden=layers,
  input_dropout_ratio=seq(0,0.9, 0.05),
  l1=seq(0,1e-4,1e-6),
  l2=seq(0,1e-4,1e-6)
)
hyper_params


## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
run_id <- Sys.time() %>% str_replace_all('-|:| ', '_')
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 600, max_models = 500, seed=my_seed, stopping_rounds=5, stopping_tolerance= 1e-2)
dl_random_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = run_id,
  training_frame=sampled_train,
  validation_frame=valid,
  x=predictors,
  y=response,
  epochs=30,
  stopping_metric="RMSLE",
  stopping_tolerance=1e-2, ## stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=20,
  score_validation_samples=500, ## downsample validation set for faster scoring
  score_duty_cycle=0.025, ## don't score more than 2.5% of the wall time
  max_w2=10, ## can help improve stability for Rectifier
  hyper_params = hyper_params,
  search_criteria = search_criteria
)

grid <- h2o.getGrid(run_id,sort_by="RMSLE",decreasing=FALSE)

#### Read grid.csv, write to grid.csv, and save ####
saved_path <- h2o.saveGrid(grid_directory = paste0(getwd(), '/DeepLearningModels'), grid_id = grid_id, save_params_references = F, export_cross_validation_predictions = F)

grid

# To load a certain grid, run:
grid <- h2o.loadGrid(paste0(getwd(), '/DeepLearningModels/', run_id))

best_model <- h2o.getModel(grid@model_ids[[1]]) ## model with lowest logloss
best_model

best_params <- best_model@allparameters
best_params$activation
best_params$hidden
best_params$input_dropout_ratio
best_params$l1
best_params$l2

#### Predict ####
predictions <- h2o.predict(best_model, valid)
rmsle(valid$y_train, predictions)

#### Save data ####
surname1 <- "Shi" # first team member surname
surname2 <- "Janiszewski" # second team member surname
file_name <- paste0(surname1, "_", surname2,".csv")
getwd()
write.table(pred_submit, file= file_name, row.names = FALSE, col.names  = FALSE)

#### Shutdown H2o (use at end of day) ####
h2o.shutdown(prompt = TRUE)

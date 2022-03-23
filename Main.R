#### Config and Loading ####
library(pacman)
my_seed <- 12345
p_load(h2o, reticulate, tensorflow, keras, tidyverse, DescTools)

#### Functions ####
# Evaluation function
rmsle <- function(actual, predicted){
  ((log(actual) - log(predicted))^2) %>% mean %>% sqrt
}

#### Load Data ####
load_train <- read.csv("https://www.dropbox.com/s/bawlkeolef1bse2/train_dat.csv?dl=1", 
                        sep= ",", header= T)
load_test <- read.csv("https://www.dropbox.com/s/rbjatpuk5x7dios/test_dat.csv?dl=1", 
                       sep= ",", header= T)

load_test %>% apply(2, max)
load_train %>% apply(2, max)

# Area columns
sf_columns = load_train %>% select(contains('SF')) %>% colnames %>% paste(collapse=',')
bath_columns = load_train %>% select(contains('Bath')) %>% colnames%>% paste(collapse=',')

prep_dat <- function(dat, testset=F, scale=T){
  if(testset==T){
    dat$y_train <- 0
  }else{
    # Drop values that are too high/low for testset
    dat <- dat %>%
      filter(
        LotArea <= max(load_test$LotArea),
        YearBuilt <= max(load_test$YearBuilt),
        TotalBsmtSF <= max(load_test$TotalBsmtSF),
        X1stFlrSF <= max(load_test$X1stFlrSF),
        X2ndFlrSF <= max(load_test$X2ndFlrSF),
        LowQualFinSF <= max(load_test$LowQualFinSF),
        GrLivArea <= max(load_test$GrLivArea),
        GarageArea <= max(load_test$GarageArea),
        OpenPorchSF <= max(load_test$OpenPorchSF),
        X3SsnPorch <= max(load_test$X3SsnPorch),
        ScreenPorch <= max(load_test$ScreenPorch),
        
        LotArea >= min(load_test$LotArea),
        OverallCond >= min(load_test$OverallCond),
        OverallQual >= min(load_test$OverallQual),
        YearBuilt >= min(load_test$YearBuilt),
        X1stFlrSF >= min(load_test$X1stFlrSF),
        GrLivArea >= min(load_test$GrLivArea),
        KitchenAbvGr >= min(load_test$KitchenAbvGr),
        TotRmsAbvGrd >= min(load_test$TotRmsAbvGrd),
      )
  }
  
  dat <- dat %>%
    # Create new variables
    mutate(
      age = YrSold - YearBuilt,
      YearRemodAdd = replace(YearRemodAdd, YearRemodAdd == 1950, 0),
      renewed = 1*(YearRemodAdd > YearBuilt),
      house_vs_lot = X1stFlrSF / LotArea,
      live_vs_lot = GrLivArea / LotArea,
      year_month = YrSold + (MoSold / 12),
      remodeled_before_sold = 1*((YrSold - YearRemodAdd) < 3),
      new_building = 1*((YrSold - YearBuilt) < 3),
      y_new = y_train / GrLivArea,
      
      # Total variables
      total_area = TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+WoodDeckSF+OpenPorchSF,
      total_bath = BsmtFullBath+BsmtHalfBath+FullBath+HalfBath,
      
      # has... variables
      has_basement = 1*(TotalBsmtSF > 0),
      has_firepalce = 1*(Fireplaces > 0),
      has_garage =1*(GarageArea > 0),
      has_porch_deck = 1*((WoodDeckSF +OpenPorchSF+EnclosedPorch+ X3SsnPorch+ ScreenPorch) > 0),
      has_MiscVal = 1*(MiscVal > 0),
      has_LowQualFinSF = 1*(LowQualFinSF > 0),
      
      # Replacements for dropped variables
      has_PoolArea = 1*(PoolArea > 0),
      has_X3SsnPorch = 1*(X3SsnPorch > 0),
    ) #%>%
    # Winsorize
    # mutate(
    #   OverallQual = replace(OverallQual, OverallQual <= 1, 3), # 2
    #   LotArea = replace(LotArea, LotArea > 35000, 35000), # 10
    #   OverallCond = replace(OverallCond, OverallCond <= 2, 3),
    #   OpenPorchSF = replace(OpenPorchSF, OpenPorchSF > 400, 400),
    # ) #%>%
    # Drop columns that have too few non-0 entries

  # Drop columns that are not necessary
  dat <- dat %>% select(-c(
        PoolArea, # 2 in test with x>0
        X3SsnPorch, # 3 in test with x>0
      )) 
      
  
  if(scale == T){
    x <- dat %>% select(-c(y_train, y_new), ) %>% scale %>% as_tibble
    y <- dat %>% select(y_train, y_new)
    dat <- cbind(x, y)
  }

  return(dat)
}

#### Manual tests ####
# Which values should be maximum, minimum for training set
load_test %>% apply(2, max)
load_train %>% apply(2, max)

load_test %>% apply(2, min)
load_train %>% apply(2, min)

# Test that all training values are larger than minimum of test values
all(load_train%>% select(y_train) %>% apply(2, max) > load_test %>% apply(2, min))

# Which values are often different from 0
load_test  %>% apply(2, function(x) sum(x > 0))

# Prepare data
plot_dat <- prep_dat(load_train, scale=F, testset=F)
plot_dat_test <- prep_dat(load_test, scale=F, testset=T)
train_dat <- prep_dat(load_train, scale=T, testset=F)
test_dat <- prep_dat(load_test, testset=T, scale=T)

# How much data left
nrow(plot_dat)

# Tests of how high values are
(plot_dat$LotArea > 40000) %>% sum
(plot_dat_test$LotArea > 40000) %>% sum

#### LMs ####
#### Plots ####
pdf('densities.pdf')
for(cname in colnames(train_dat)){
  p <- ggplot(plot_dat %>% as_tibble, aes_string(x=cname)) + geom_histogram(aes(y=..density..), alpha=0.5, 
                                                                           position="identity")+
    geom_density(alpha=.2) 
  p %>% plot
}
dev.off()

pdf('univariates_y.pdf')
for(cname in colnames(plot_dat)){
  p <- ggplot(plot_dat %>% as_tibble, aes_string(x=cname, y='y_train')) + geom_smooth() +
    scale_x_continuous(limits = quantile(plot_dat %>% pull(cname), c(0.01, 0.99)))
  p %>% plot
}
dev.off()

pdf('univariates_ynew.pdf')
for(cname in colnames(plot_dat)){
  p <- ggplot(plot_dat %>% as_tibble, aes_string(x=cname, y='y_new')) + geom_smooth() +
    scale_x_continuous(limits = quantile(plot_dat %>% pull(cname), c(0.01, 0.99)))
  p %>% plot
}
dev.off()

#### Split dataset ####
#### H2o Start ####
h2o.init(nthreads = 7, max_mem_size = "16G")
h2o.removeAll() 


splits <- h2o.splitFrame(train_dat, c(0.6, 0.2), seed = my_seed)
train <- h2o.assign(splits[[1]], "train.hex")
valid <- h2o.assign(splits[[2]], "valid.hex")
test <- h2o.assign(splits[[3]], "test.hex")
sampled_train = train

#### Config ####
response <- "y_train"
predictors <- setdiff(names(train), response)

layer1 <- seq(0, 250, 25)
layer2 <- seq(0, 250, 25)
layer3 <- seq(0, 250, 25)
layer4 <- seq(0, 250, 25)

layers <- expand.grid(layer1, layer2, layer3, layer4) %>% split(1:nrow(.)) %>% unname

hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden=layers,
  input_dropout_ratio=seq(0,0.9, 0.05),
  l1=seq(0,1e-3,1e-6),
  l2=seq(0,1e-3,1e-6)
)

## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
run_id <- Sys.time() %>% str_replace_all('-|:| ', '_')
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 30*60, max_models = 999999999, seed=my_seed, stopping_rounds=500, stopping_tolerance= 1e-7)
dl_random_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = run_id,
  training_frame=sampled_train,
  validation_frame=valid,
  x=predictors,
  y=response,
  epochs=700,
  stopping_metric="RMSLE",
  stopping_tolerance=1e-7, ## stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=500,
  #score_validation_samples=500, ## downsample validation set for faster scoring
  score_duty_cycle=0.025, ## don't score more than 2.5% of the wall time
  max_w2=10, ## can help improve stability for Rectifier
  hyper_params = hyper_params,
  search_criteria = search_criteria
)

grid <- h2o.getGrid(run_id,sort_by="RMSLE",decreasing=FALSE)

#### Read grid.csv, write to grid.csv, and save ####
saved_path <- h2o.saveGrid(grid_directory = paste0(getwd(), '/DeepLearningModels'), grid_id = run_id, save_params_references = F, export_cross_validation_predictions = F)

grid@summary_table %>% head

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

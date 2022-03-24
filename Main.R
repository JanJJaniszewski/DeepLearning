#### Config and Loading ####
library(pacman)
my_seed <- 12345
p_load(h2o, reticulate, tensorflow, keras, tidyverse, DescTools)

#### Functions ####
# Evaluation function
rmsle <- function(actual, predicted){
  ((log(actual) - log(predicted))^2) %>% mean %>% sqrt
}

scale2 <- function(dat, trained) {
  # Obtained from https://stat.ethz.ch/pipermail/r-help/2011-July/283983.html
  x <- dat %>% select(-c(y_train, y_new, uncentered_GrLivArea))
  y <- dat %>% select(y_train, y_new, uncentered_GrLivArea)
  x <- as.matrix(x)
  x <- sweep(x, 2L, attr(trained, "scaled:center"), FUN = "-")
  x <- sweep(x, 2L, attr(trained, "scaled:scale"), FUN = "/")
  attr(x, "scaled:center") <- attr(trained, "scaled:center")
  attr(x, "scaled:scale") <- attr(trained, "scaled:scale")
  x <- x %>% as_tibble
  dat <- cbind(x,y)
  return(dat)
}

unscale <- function(dat, trained) {
  # Obtained from https://stat.ethz.ch/pipermail/r-help/2011-July/283983.html
  x <- dat %>% select(-c(y_train, y_new, uncentered_GrLivArea))
  y <- dat %>% select(y_train, y_new, uncentered_GrLivArea)
  x <- as.matrix(x)
  x <- sweep(x, 2L, attr(trained, "scaled:scale"), FUN = "*")
  x <- sweep(x, 2L, attr(trained, "scaled:center"), FUN = "+")
  attr(x, "scaled:center") <- attr(trained, "scaled:center")
  attr(x, "scaled:scale") <- attr(trained, "scaled:scale")
  x <- x %>% as_tibble
  dat <- cbind(x,y)
  return(dat)
}


#### Load Data ####
load_train <- read.csv("https://www.dropbox.com/s/bawlkeolef1bse2/train_dat.csv?dl=1", 
                        sep= ",", header= T)
load_test <- read.csv("https://www.dropbox.com/s/rbjatpuk5x7dios/test_dat.csv?dl=1", 
                       sep= ",", header= T)

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
  
  
  garage_area_per_car <- median(dat$GarageArea / ifelse(dat$GarageCars == 0, 1, dat$GarageCars))
  dat <- dat %>%
    # Create new variables
    mutate(
      
      # New variables
      age = YrSold - YearBuilt,
      live_vs_lot = GrLivArea / LotArea,
      remodeled_before_sold = 1*((YrSold - YearRemodAdd) < 3),
      new_building = 1*((YrSold - YearBuilt) < 3),
      rooms_per_SF = TotRmsAbvGrd / GrLivArea,
      y_new = y_train / GrLivArea,
      uncentered_GrLivArea = GrLivArea,
      low_lot_area = LotArea < 2800,
      

      
      # Total variables
      total_min_GRLivArea = (TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+WoodDeckSF+OpenPorchSF-GrLivArea)/GrLivArea,
      total_bath = BsmtFullBath+BsmtHalfBath+FullBath+HalfBath,
      total_porch = WoodDeckSF +OpenPorchSF+EnclosedPorch+ X3SsnPorch+ ScreenPorch,
      
      # has... variables
      has_remod = 1*((YearRemodAdd > YearBuilt) & (YearRemodAdd > 1950)),
      has_2nd_floor = 1*(X2ndFlrSF > 0),
      has_firepalce = 1*(Fireplaces > 0),
      has_porch_deck = 1*((WoodDeckSF +OpenPorchSF+EnclosedPorch+ X3SsnPorch+ ScreenPorch) > 0),
      has_LowQualFinSF = 1*(LowQualFinSF > 0),
      has_WoodDeckSF = 1*(WoodDeckSF>0),
      has_garage =1*(GarageArea > 0), 
      
      # Replacements for dropped variables
      has_PoolArea = 1*(PoolArea > 0),
      has_X3SsnPorch = 1*(X3SsnPorch > 0),
      has_LowQualFinSF = 1*(LowQualFinSF> 0),
      has_EnclosedPorch = 1*(EnclosedPorch> 0),
      has_OpenPorchSF = 1*(OpenPorchSF> 0),
      has_ScreenPorch = 1*(ScreenPorch> 0),
      has_MiscVal = 1*(MiscVal > 0),
      year_month = YrSold + (MoSold / 12),
      big_garage = (GarageArea / ifelse(GarageCars==0, 1, GarageCars)) / garage_area_per_car, # GarageCars
      
      has_basement = 1*(TotalBsmtSF > 0), # Basement
      first_min_basement = (has_basement * X1stFlrSF - TotalBsmtSF) / X1stFlrSF, # Basement
      first_min_basement = replace(first_min_basement, first_min_basement<0, 0),
      has_smaller_basement = 1*(first_min_basement > 0), # Basement
      
      # High values
      high_lot_area = LotArea > 20000,
      high_WoodDeckSF = WoodDeckSF > 300,
      high_total_porch = total_porch > 400,
      high_first_min_basement = first_min_basement > 0.3,
    ) %>%
    # Winsorize
    mutate(
      LotArea = replace(LotArea, LotArea > 20000, 20000), # 10
      # total_area = replace(total_area, total_area < 1500, 1500), # 10
      WoodDeckSF = replace(WoodDeckSF, WoodDeckSF > 300, 300), # 10
      total_porch = replace(total_porch, total_porch>400, 400),
      first_min_basement = replace(first_min_basement, first_min_basement>0.3, 0.3),
    )

  if(scale == T){
    # Drop columns that are not necessary
    dat <- dat %>% select(-c(
      PoolArea, # 2 in test with x>0
      X3SsnPorch, # 3 in test with x>0
      LowQualFinSF, # No pattern recognizable
      TotalBsmtSF,
      EnclosedPorch,
      OpenPorchSF,
      ScreenPorch,
      MiscVal,
      GarageCars,
      YrSold,
    )) 
    
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

# Scaling
# Training
train_dat0 <- prep_dat(load_train, scale=T, testset=F)
x <- train_dat0 %>% select(-c(y_train, y_new, uncentered_GrLivArea))
y <- train_dat0 %>% select(y_train, y_new, uncentered_GrLivArea)
scale_base <- x %>% scale
x <- scale_base %>% as_tibble
train_dat <- cbind(x,y)

apply(train_dat0, 2, mean)
apply(test_dat0, 2, mean)

# Test
test_dat0 <- prep_dat(load_test, testset=T, scale=T) 
test_dat <- test_dat0 %>% scale2(.,scale_base)

apply(test_dat, 2, mean)
apply(train_dat, 2, mean)
#plot_dat_test %>% cor %>% round(2) %>% View

# How much data left
nrow(plot_dat)

#### Plots ####
pdf('densities.pdf')
for(cname in colnames(plot_dat)){
  p <- ggplot(plot_dat %>% as_tibble, aes_string(x=cname)) +
  #+ geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
    geom_density(alpha=.2, fill='green') +
    geom_density(data=plot_dat_test, fill="purple", alpha=.2)
  p %>% plot
}
dev.off()

pdf('univariates_y.pdf')
for(cname in colnames(plot_dat)){
  p <- ggplot(plot_dat %>% as_tibble, aes_string(x=cname, y='y_train')) + 
    geom_point() +geom_smooth(alpha=.5) +
    scale_x_continuous(limits = quantile(plot_dat %>% pull(cname), c(0.01, 0.99)))
  p %>% plot
}
dev.off()

pdf('univariates_ynew.pdf')
for(cname in colnames(plot_dat)){
  p <- ggplot(plot_dat %>% as_tibble, aes_string(x=cname, y='y_new')) + 
    geom_point() +geom_smooth(alpha=.5) +
    scale_x_continuous(limits = quantile(plot_dat %>% pull(cname), c(0.01, 0.99)))
  p %>% plot
}
dev.off()

#### Split dataset ####
#### H2o Start ####
h2o.init(nthreads = 6, max_mem_size = "20G")
h2o.removeAll()  

train_dat <- train_dat %>% as.h2o
pred_dat <- test_dat %>% as.h2o

splits <- h2o.splitFrame(train_dat, c(0.7, 0.2), seed = my_seed)
train <- h2o.assign(splits[[1]], "train.hex")
valid <- h2o.assign(splits[[2]], "valid.hex")
test <- h2o.assign(splits[[3]], "test.hex")

#### Config ####
#response <- "y_train"
response <- 'y_new'
predictors <- train %>% as_tibble %>% select(-starts_with('y_'), -contains('uncentered_')) %>% names

layer_most <- c(seq(5, 80, 5), 100, 150, 200, 250)
layer_many <- c(seq(5, 55, 10), 100, 150, 200, 250)
layer_fewer <- c(5, 10, 20, 40, 50, 150, 250)
layer_few <- c(5, 10, 20, 50, 100, 250)

layers5 <- expand.grid(layer_many, layer_fewer, layer_fewer, layer_fewer, layer_many) %>% split(1:nrow(.)) %>% unname
length(layers5)
layers4 <- expand.grid(layer_many, layer_many, layer_many, layer_many) %>% split(1:nrow(.)) %>% unname
length(layers4)
layers3 <- expand.grid(layer_most, layer_most, layer_most) %>% split(1:nrow(.)) %>% unname
length(layers3)
layers2 <- expand.grid(layer_most, layer_most) %>% split(1:nrow(.)) %>% unname
length(layers2)

layers <- c(layers2, layers3, layers4, layers5)

hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden=layers,
  input_dropout_ratio=seq(0,0.9, 0.05),
  l1=seq(0,0.01,0.0001),
  l2=seq(0,0.01,0.0001)
)

## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
run_id <- Sys.time() %>% str_replace_all('-|:| ', '_')
print(run_id)
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 12*60*60, stopping_tolerance = 0.001, stopping_rounds = 20, seed=my_seed)
dl_random_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = run_id,
  training_frame=train,
  validation_frame=valid,
  x=predictors,
  y=response,
  epochs=500,
  stopping_metric="RMSLE",
  stopping_tolerance=0.005, ## stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=20,
  score_duty_cycle=0.025, ## don't score more than 2.5% of the wall time
  max_w2=10, ## can help improve stability for Rectifier
  hyper_params = hyper_params,
  search_criteria = search_criteria,
  parallelism = 1
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
h2o.saveModel(best_model, path='BestModels')
best_params$activation
best_params$hidden
best_params$input_dropout_ratio
best_params$l1
best_params$l2

#### Predict ####
predictions <- h2o.predict(best_model, test) * test['uncentered_GrLivArea']
predictions_final <- (h2o.predict(best_model, pred_dat) * pred_dat['uncentered_GrLivArea']) %>% as.vector

prediction_table %>% filter(preds < min(prediction_table$y_train))

predictions_final %>% as_tibble %>% ggplot(aes(x=predict)) + geom_density() +
  geom_density(data = predictions %>% as_tibble)

prediction_table <- valid %>% as_tibble %>%
  mutate(preds = predictions %>% as.vector, new_preds = new_preds %>% as.vector,
  rmsle = ((log(y_train) - log(preds))^2),
  lower_price = y_train < 1e+05,
  pred_quant = ntile(preds, 10)
  )

prediction_table %>% group_by(pred_quant) %>% summarise(mean(y_train / preds))
ggplot(data=prediction_table, aes(x=preds, y=y_train)) + geom_point()
ggplot(data=prediction_table, aes(x=y_train, y=rmsle)) + geom_point()
ggplot(data=prediction_table, aes(x=y_new, y=rmsle)) + geom_point()

pdf('predictions_ynew.pdf')
for(cname in colnames(prediction_table)){
  p <- ggplot(prediction_table, aes_string(x=cname)) + 
    geom_point() +geom_smooth(alpha=.5, aes(y=y_new)) +
    geom_smooth(alpha=.5, aes(y=preds))
    scale_x_continuous(limits = quantile(plot_dat %>% pull(cname), c(0.01, 0.99)))
  p %>% plot
}
dev.off()

rmsle(test$y_train, predictions)

#### Save data ####
surname1 <- "Shi" # first team member surname
surname2 <- "Janiszewski" # second team member surname
file_name <- paste0(surname1, "_", surname2,".csv")
getwd()
write.table(predictions_final, file= file_name, row.names = FALSE, col.names  = FALSE)

#### Shutdown H2o (use at end of day) ####
h2o.shutdown(prompt = TRUE)


# Create a model based 80% of the training data. Keep 20% for valiation
TT <- NROW(train_dat)
samplee <- sample(1:TT, size= 0.8*TT, replace= F)
trainn <- train_dat[samplee, ]
validd <- train_dat[-samplee, ]



index_y <- which(colnames(validd)== "y_train")

# keras.backend.clear_session()
# k_clear_session()

model1 <- keras_model_sequential() %>% 
  layer_dense(units = 60, activation = "relu", 
              input_shape = dim(trainn[,-index_y])[2] ) %>% 
  layer_dense(units = 1) 

model1 %>% summary()

model1 %>% compile( optimizer = "rmsprop", loss = "mse", metrics = c("mae") )

num_epochs <- 50
batch_sizee <- 2^6
minn <- as.matrix(trainn[,index_y]) %>% min

model1 %>% fit(as.matrix(trainn[,-index_y]), y = as.matrix(trainn[,index_y]),
               epochs = num_epochs, batch_size = batch_sizee, verbose = 1)
eval_dl <- model1 %>% evaluate(as.matrix(validd[,-index_y]), as.matrix(validd[,index_y]), 
                               verbose = 1)

pred_dl <- predict(model1, as.matrix(validd[,-index_y]) )
pred_dl <- ifelse(pred_dl < 0, minn, pred_dl)
# plot(pred_dl, validd[, index_y])
rmsle(pred_dl, validd[, index_y]) 
# plot(pred_dl, validd[, index_y])

# Create the actual submission

# Fit the model based on all data
model1 %>% fit(as.matrix(train_dat[,-index_y]), y = as.matrix(train_dat[,index_y]),
               epochs = num_epochs, batch_size = batch_sizee, verbose = 1)

# create the prediction
pred_submit <- predict(model1, as.matrix(test_dat) )
# Cap the predictions
pred_submit <- ifelse(pred_submit < 0, minn, pred_submit)
str(pred_submit)
head(pred_submit)

surname1 <- "surname1" # first team member surname
surname2 <- "surname2" # second team member surname
file_name <- paste0(surname1, "_", surname2,".csv")
getwd()
write.table(pred_submit, file= file_name, row.names = FALSE, col.names  = FALSE)

# Check your file
checkk <- read.table(file=file_name)
str(checkk)





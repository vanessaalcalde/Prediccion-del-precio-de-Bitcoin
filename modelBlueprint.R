base::library(magrittr)

# Flags de los hiperparametros
FLAGS <- keras::flags(
  keras::flag_numeric("nodes_layer_2", 16, "Nodes in layer 2"),
  keras::flag_numeric("nodes_layer_3", 8, "Nodes in layer 3"),
  keras::flag_string("activation_layer_2", "relu", "Activation in layer 2"),
  keras::flag_string("activation_layer_3", "relu", "Activation in layer 3")
  
)

# Arquitectura de la red
input <- tfdatasets::layer_input_from_dataset(datapriceTrain %>% dplyr::select(-close))
output <- input %>% 
  keras::layer_dense_features(
    tfdatasets::dense_features(spec)
  ) %>%
  keras::layer_dense(
    units = FLAGS$nodes_layer_2,
    activation = FLAGS$activation_layer_2
  ) %>%
  keras::layer_dense(
    units = FLAGS$nodes_layer_3,
    activation = FLAGS$activation_layer_3
  ) %>%
  keras::layer_dense(
    units = 1,
    activation = "relu"
  ) 

model <- keras::keras_model(input, output)
summary(model)

model %>% 
  keras::compile(
    loss = "mse",
    optimizer = keras::optimizer_rmsprop(),
    metrics = base::list("mean_absolute_error")
  )

# Entrenamiento del modelo.
modelFit <- model %>%
  keras::fit(
    x = dplyr::select(datapriceTrain, -close),
    y = datapriceTrain[["close"]],
    epochs = 3000,
    validation_split = 0.2,
    callbacks = base::list(
      keras::callback_early_stopping(
        monitor = "val_loss",
        patience = 3000
      )
    ),
    verbose = 2
  )

# Prediccion
base::c(loss, mae) %<-% (keras::evaluate(model, 
                        dplyr::select(datapriceTest, -close), 
                        datapriceTest[["close"]], verbose = 2))
base::sprintf("%.2f", mae * 1000)
test_predict <- base::as.numeric(stats::predict(model, 
                                                dplyr::select(datapriceTest, 
                                                              -close)))

RMSE <- base::sqrt(base::mean((test_predict - datapriceTest[["close"]])^2))
RMSE


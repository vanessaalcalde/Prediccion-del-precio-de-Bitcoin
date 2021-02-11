base::library(magrittr)
base::source("DatabaseManager.R")

base::set.seed(1234)
datapriceTest  <- dplyr::slice_sample(databaseCoinsNeuralNetwork, prop = 0.3)
datapriceTrain <- dplyr::anti_join(databaseCoinsNeuralNetwork, datapriceTest)

# Estandarizamos todas las variables para que tengan media 0 y desvio 1.
spec <- tfdatasets::feature_spec(
  datapriceTrain,
  close ~ . 
) %>% 
  tfdatasets::step_numeric_column(
    tfdatasets::all_numeric(),
    normalizer_fn = tfdatasets::scaler_standard()
  ) %>% 
  keras::fit()

layer <- keras::layer_dense_features(
  feature_columns = tfdatasets::dense_features(spec), 
  dtype = tf[["float32"]]
)
layer(datapriceTrain)

# Ejecutar Modelo.
model_runs <- tfruns::tuning_run(
  file = "modelBlueprint.R", 
  flags = base::list(
    nodes_layer_1 = base::c(16, 8),
    activation_layer_1 = base::c("relu", "linear")
  ),
  confirm = FALSE,
  runs_dir = "runs"
)

# Resultados.
utils::View(model_runs)
tfruns::ls_runs(order = metric_val_accuracy)

# RMSE 546.5264
# flag_nodes_layer_2 16
# flag_nodes_layer_3 8
# flag_activation_layer_2 relu
# flag_activation_layer_3 relu
tfruns::view_run(run_dir = "runs/2020-12-04T13-04-12Z")

# RMSE 610.5063
# flag_nodes_layer_2 8
# flag_nodes_layer_3 8
# flag_activation_layer_2 relu
# flag_activation_layer_3 relu
tfruns::view_run(run_dir = "runs/2020-12-04T14-03-53Z")

# RMSE 485.0457
# flag_nodes_layer_2 16
# flag_nodes_layer_3 8
# flag_activation_layer_2 linear
# flag_activation_layer_3 relu
tfruns::view_run(run_dir = "runs/2020-12-04T15-02-16Z")

# RMSE 2657.04
# flag_nodes_layer_2 8
# flag_nodes_layer_3 8
# flag_activation_layer_2 linear
# flag_activation_layer_3 linear
tfruns::view_run(run_dir = "runs/2020-12-04T16-04-57Z")

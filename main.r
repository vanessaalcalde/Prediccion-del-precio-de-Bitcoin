base::library(magrittr)
base::source("DatabaseManager.R")


# Seleccionamos aleatoriamente 70% para train y 30% para test.
base::set.seed(1234)
datapriceTest <- dplyr::slice_sample(databaseCoins, prop = 0.3)
datapriceTrain <- dplyr::anti_join(databaseCoins, datapriceTest)


# Algoritmo de fuerza bruta utilizado para encontrar el modelo con el  
# minimo RMSE.
# Buscamos todas las combinaciones posibles entre n variables.  
combinacionesPosibles <- tidyr::crossing(
   var1 = 0:1, 
   var2 = 0:1,
   var3 = 0:1,
   var4 = 0:1,
   var5 = 0:1,
   var6 = 0:1,
   var7 = 0:1,
   var8 = 0:1,
   var9 = 0:1,
   var10 = 0:1,
   var11 = 0:1,
   var12 = 0:1
   # var13 = 0:1,
   # var14 = 0:1,
   # var15 = 0:1
   # var17 = 0:1, 
   # var18 = 0:1,
   # var19 = 0:1,
   # var20 = 0:1
  )

# El grupo de variables para buscar todas las posibles combinaciones  
# surge de la prueba y el error, y la literatura existente. 
variablesLag <- base::c(
  "trend",
  "closeLag1",
  "closeLag2",
  "closeLag3",
  "closeLag4",
  "closeLag5",
  #"closeLag6",
  #"closeLag7",
  #"closeLag8",
  "volLag1",
  "volLag2",
  "volLag3",
  "volLag4",
  "volLag5",
  #"volLag6"
  # "ethOpen",
  # "ethVol",
  "ltcOpen" 
  #"ltcVol"  
  #"bchOpen"  , 
  #"bchVol"   ,
  #"etcOpen"  , 
  #"etcVol"   ,
  #"linkOpen" , 
  #"linkVol"  ,
  #"repOpen"  , 
  #"repVol"  
  )



baggingEvalFirst  <- "baggingClose <- randomForest::randomForest(close ~"
baggingEvalSecond <- ",mtry ="
baggingEvalEnd    <- ",replace = TRUE,importance = TRUE,ntree = 100L,nPerm = 1L,data = datapriceTrain)"

regressionTreeEvalFirst <- "regressionTreeClose = tree::tree(formula =close~"
regressionTreeEvalEnd   <- ",data= datapriceTrain)"

baggingRMSEDataprice <- base::c()
baggingModelsFit     <- base::c()

regressionTreeRMSEDataprice <- base::c()
regressionTreeModelsFit     <- base::c()

totalCombinaciones <- base::nrow(combinacionesPosibles)

# Recorremos todas las combinaciones posibles.
for (i in base::c(1:totalCombinaciones)) {
  
  eval <- ""
  totalVariables <- 0
  
  for ( variable in variablesLag[combinacionesPosibles[i,]==1]) {
    if (eval == "") {
      variable = base::paste(" ", variable)
    } else {
      variable = base::paste("+ ", variable)  
    }
    eval = base::paste(eval, variable)
    totalVariables = totalVariables + 1
  }
  
  if (eval!="") {
    
    baggingEval = base::paste(baggingEvalFirst, eval, baggingEvalSecond,
                              totalVariables, baggingEvalEnd)
    # Se ejecuta una instancia de ajuste por eval.
    base::eval(base::parse(text=baggingEval))
    predDataprice = stats::predict(baggingClose, datapriceTest, interval = "prediction")
    # RMSE Bagging.
    baggingRMSEDataprice[base::length(baggingRMSEDataprice) + 1] = base::sqrt(
                                                      base::sum((predDataprice -
                                                      datapriceTest$close )^2)/
                                                      base::length(datapriceTest$close))
    # Guardamos el Modelo Bagging.
    baggingModelsFit[length(baggingModelsFit) + 1] = baggingEval

    regressionTreeEval = paste(regressionTreeEvalFirst,eval,regressionTreeEvalEnd)
    
    base::eval(base::parse(text=regressionTreeEval))
    # RMSE Regression Tree.
    regressionTreeRMSEDataprice[base::length(regressionTreeRMSEDataprice) + 1] = base::sqrt(
                                                    base::mean((datapriceTest$close - 
                                                    stats::predict(regressionTreeClose,
                                                                   datapriceTest))^2))
    regressionTreeModelsFit[length(regressionTreeModelsFit) + 1] = regressionTreeEval
    
    # Estado actual del algoritmo. 
    base::print((i/totalCombinaciones))
  }
}


# Dataframe con el resultado RMSE y los modelos de todas las posibles 
# combinaciones ejecutadas.
# El que tiene menor RMSE en cada dataframe es el algoritmo optimo 
# para cada tipo de modelado.
infoBagging = base::data.frame("model" = baggingModelsFit,
                               "RMSE" = baggingRMSEDataprice)
infoBagging[base::which.min(infoBagging$RMSE),]

infoRegressionTree = base::data.frame("model" = regressionTreeModelsFit, 
                                      "RMSE" = regressionTreeRMSEDataprice)
infoRegressionTree[base::which.min(infoRegressionTree$RMSE),]

# Regression Tree analisis.
# Cantidad de modelos con igual RMSE en Regression Tree.
infoRegressionTree %>% dplyr::group_by(RMSE) %>% dplyr::summarise(n = dplyr::n())

# Se poda el Arbol que tiene menor RMSE
closeTree <- tree::tree(formula =close~ closeLag1, data= datapriceTrain)
base::summary(closeTree)
base::plot(closeTree)
graphics::text(closeTree, pretty = 0L)

cvTree <- tree::cv.tree(
  object = closeTree
)

base::plot(cvTree[["size"]], cvTree[["dev"]], type = "b")

treePruned <- tree::prune.tree(
  tree = closeTree,
  best = 5L
)

base::plot(treePruned)
graphics::text(treePruned, pretty = 0L)

# Grafico del Arbol
rpartTree = rpart::rpart(formula =close~ closeLag1 ,data= datapriceTrain)
rpart.plot::rpart.plot(rpartTree)

# Bagging analisis.
baggingClose <- randomForest::randomForest(close ~ trend +  closeLag1 + 
                                           closeLag2 +  closeLag3 +  volLag3 +
                                           volLag5 +  ltcOpen ,mtry= 7,replace = TRUE,
                                           importance = TRUE,ntree = 100L,
                                           nPerm = 1L,data = datapriceTrain)

# Matriz de importancia de variables. 
baggingClose[["importance"]]
randomForest::importance(baggingClose)
randomForest::varImpPlot(baggingClose, main="")
# MSE
baggingClose[["mse"]]
graphics::hist(baggingClose[["mse"]])

randomForest:::plot.randomForest(baggingClose)

# Error Rate OOB
yhat_bag <- stats::predict(baggingClose, newdata = datapriceTest)

base::plot(yhat_bag, datapriceTest[["close"]])
graphics::abline(0, 1)

bagErrorTesteMse <-base::mean((yhat_bag - datapriceTest[["close"]])^2)
sqrt(bagErrorTesteMse)



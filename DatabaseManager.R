base::library(magrittr)

# Bitcoin precio diario desde 2020/9/9 
.btcPrice <-  utils::read.csv("Coinbase_BTCUSD_d.csv")

# Principales criptomonedas diarias desde 2020/9/9 
.ethPrice  <-  utils::read.csv("Coinbase_ETHUSD_d.csv")
.ltcPrice  <-  utils::read.csv("Coinbase_LTCUSD_d.csv")
.bchPrice  <-  utils::read.csv("Coinbase_BCHUSD_d.csv")
.etcPrice  <-  utils::read.csv("Coinbase_ETCUSD_d.csv")
.linkPrice <-  utils::read.csv("Coinbase_LINKUSD_d.csv")
.repPrice  <-  utils::read.csv("Coinbase_REPUSD_d.csv")
.xlmPrice  <-  utils::read.csv("Coinbase_XLMUSD_d.csv")
.xrpPrice  <-  utils::read.csv("Coinbase_XRPUSD_d.csv")

# Google trends de busqueda "Bitcoin" desde 2020/9/9
.googleTrendsBitcoin <- utils::read.csv("GoogleTrends_Bitcoin_d.csv")
.googleTrendsBitcoin <- base::data.frame("index"= base::c(1:233), 
                                          "bitcoin"= .googleTrendsBitcoin$bitcoin[1:233], 
                                          "day" = .googleTrendsBitcoin$Day[1:233])
.googleTrendsBitcoin <- .googleTrendsBitcoin %>% 
                        dplyr::arrange(dplyr::desc(index))


# Merge de bases
# Se toma los "precios del open" en el momento n.
# La "cantidad de volumen" en el momento n-1.
# La variable explicada "precio del close de bitcoin" esta en el momento n.
# Se agregan los lags de 1 a 10 del precio del bitcion y volumen de bitcoin. 

databaseCoins <-  base::data.frame(
  "trend" =  .googleTrendsBitcoin$bitcoin[2:233],
  "close" = .btcPrice$Close[1:232], 
  # "open" = .btcPrice$Open[1:232],
  # "ethOpen"  = .ethPrice$Open[1:232], 
  # "ethVol"   = .ethPrice$Volume.ETH[2:233],
  "ltcOpen"  = .ltcPrice$Open[1:232],
  # "ltcVol"   = .ltcPrice$Volume.LTC[2:233],
  # "bchOpen"  = .bchPrice$open[1:232], 
  # "bchVol"   = .bchPrice$Volume.BCH[2:233],
  # "etcOpen"  = .etcPrice$open[1:232], 
  # "etcVol"   = .etcPrice$Volume.ETC[2:233],
  # "linkOpen" = .linkPrice$open[1:232], 
  # "linkVol"  = .linkPrice$Volume.LIN[2:233],
  # "repOpen"  = .repPrice$open[1:232], 
  # "repVol"   = .repPrice$Volume.REP[2:233],
  "closeLag1" = .btcPrice$Close[2:233],
  "closeLag2" = .btcPrice$Close[3:234],
  "closeLag3" = .btcPrice$Close[4:235],
  "closeLag4" = .btcPrice$Close[5:236],
  "closeLag5" = .btcPrice$Close[6:237],
  # "closeLag6" = .btcPrice$Close[7:238],
  # "closeLag7" = .btcPrice$Close[8:239],
  # "closeLag8" = .btcPrice$Close[9:240],
  # "closeLag9" = .btcPrice$Close[10:241],
  # "closeLag10" = .btcPrice$Close[11:242],
  "volLag1" = .btcPrice$Volume.BTC[2:233],
  "volLag2" = .btcPrice$Volume.BTC[3:234],
  "volLag3" = .btcPrice$Volume.BTC[4:235],
  "volLag4" = .btcPrice$Volume.BTC[5:236],
  "volLag5" = .btcPrice$Volume.BTC[6:237]
  # "volLag6" = .btcPrice$Volume.BTC[7:238],
  # "volLag7" = .btcPrice$Volume.BTC[8:239],
  # "volLag8" = .btcPrice$Volume.BTC[9:240],
  # "volLag9" = .btcPrice$Volume.BTC[10:241],
  # "volLag10" = .btcPrice$Volume.BTC[11:242]
)

databaseCoinsNeuralNetwork <- databaseCoins %>% dplyr::select(close, 
                                                              trend, 
                                                              closeLag1, 
                                                              closeLag2, 
                                                              closeLag3,
                                                              volLag3, 
                                                              volLag5, 
                                                              ltcOpen)


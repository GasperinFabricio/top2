# plumber.R
#* @apiTitle API de Previsão de Defeitos

modelo <- lm(NUMDEFECTS ~ sumLOC_TOTAL, data = readxl::read_excel("dataset_KC1_classlevel_numdefect.xlsx"))

#* @param loc Número de linhas de código
#* @get /prever
function(loc){
  loc <- as.numeric(loc)
  pred <- predict(modelo, newdata = data.frame(sumLOC_TOTAL = loc))
  return(list(sumLOC_TOTAL = loc, NUMDEFECTS_PREDITOS = pred))
}

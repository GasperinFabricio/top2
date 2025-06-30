library(readxl)
modelo <- lm(NUMDEFECTS ~ sumLOC_TOTAL, data = read_excel("dataset_KC1_classlevel_numdefect.xlsx"))
saveRDS(modelo, "modelo_predict_defects.rds")

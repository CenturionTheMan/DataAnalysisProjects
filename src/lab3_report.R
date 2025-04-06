# sprawdzanie normalności zmiennej: X=TFD,	Y=yellow bag

library("readODS")
library("moments") 

LoadDataFromSheet <- function(sheet) {
  read_ods(
    "./data/ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
    sheet = sheet,
    skip = 1,
  )
}

SpaceToUnderscore <- function(x) {
  gsub("\\s", "_", x)
}



TFD_T <- LoadDataFromSheet("TFD_T_Total_Fixation_Duration")
TFD_R <- LoadDataFromSheet("TFD_R_Total_Fixation_Duration")
TFD_Y <- LoadDataFromSheet("TFD_Y_Total_Fixation_Duration_")

YBagData <- list(
  'T' = TFD_T$`Y bag`,
  'R' = TFD_R$`Y bag`,
  'Y' = TFD_Y$`Y bag`
)

# =====================================================
# H0 -> rozkłady są normalne, H1-> rozkładady nie są normalne
# =====================================================
Create3ColumnHist <- function(figTitle, path, t, r, y) {
  png(path, width=900, height=600, res=150)
  
  time_layout <- matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE)
  layout(time_layout)
  
  par(oma=c(0,0,2,0))
  
  hist(x = t, col = "White", main = "Gogle przezroczyste", xlab = "Czas skupienia na żółtej torbie [s]", ylab = "Częstość")
  hist(x = r, col = "Red", main="Gogle czerwone", xlab = "Czas skupienia na żółtej torbie [s]", ylab = "Częstość")
  hist(x = y, col = "Yellow", main="Gogle żółte", xlab = "Czas skupienia na żółtej torbie [s]", ylab = "Częstość")
  
  mtext(figTitle, outer=TRUE, cex=1.5, font=2)
  
  dev.off()
}

SaveFrameToCsv <- function(dataframe, path) {
  write.csv(dataframe, paste0("./res_tables/", path), row.names = FALSE)
}



HandleNormalityForData <- function(t,r,y, hist_title_sub, hist_path_sub, frame_sub)
{
  Create3ColumnHist(paste0("Histogramy czasu skupienia na żółtej torbie", hist_title_sub), 
                    paste0("./res_plots/historgram_dla_TFD_yBag", hist_path_sub, ".png"), 
                    t, r, y)
  
  alpha <- 0.05
  
  shapiroData <- data.frame(
    "kolor_gogli" = c('przezroczysty', 'czerwony', 'żółty'),
    "shapiro_p" = c(shapiro.test(t)$p, shapiro.test(r)$p, shapiro.test(y)$p),
    "czy_normalny" = c(
      ifelse(shapiro.test(t)$p > alpha, "normalny", "nienormalny"),
      ifelse(shapiro.test(r)$p > alpha, "normalny", "nienormalny"),
      ifelse(shapiro.test(y)$p > alpha, "normalny", "nienormalny")
    )
  )
  SaveFrameToCsv(shapiroData, paste0("yBag_shapiro", frame_sub, ".csv")) 
}


HandleNormalityForData(
  t = YBagData$T, r = YBagData$R, y = YBagData$Y,
  hist_title_sub = " (bez przekształceń)",
  hist_path_sub = "_default",
  frame_sub = "_default"
)

HandleNormalityForData(
  t = YBagData$T^0.5, r = YBagData$R^0.5, y = YBagData$Y^0.5,
  hist_title_sub = " (przekształcenie x^0.5)",
  hist_path_sub = "_x^0.5",
  frame_sub = "_x^0.5"
)

HandleNormalityForData(
  t = YBagData$T^0.25, r = YBagData$R^0.25, y = YBagData$Y^0.25,
  hist_title_sub = " (przekształcenie x^0.25)",
  hist_path_sub = "_x^0.25",
  frame_sub = "_x^0.25"
)

HandleNormalityForData(
  t = log(YBagData$T), r = log(YBagData$R), y = log(YBagData$Y),
  hist_title_sub = " (przekształcenie logx)",
  hist_path_sub = "_log(x)",
  frame_sub = "_log(x)"
)


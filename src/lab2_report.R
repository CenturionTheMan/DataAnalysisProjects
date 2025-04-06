library("readODS")


SpaceToUnderscore <- function(x) {
  gsub("\\s", "_", x)
}



LoadDataFromSheet <- function(sheet) {
  read_ods(
    "./data/ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
    sheet = sheet,
    skip = 1,
  )
}

dTime <- list(
  t = LoadDataFromSheet("T_TIME"),
  r = LoadDataFromSheet("R_TIME"),
  y = LoadDataFromSheet("Y_TIME")
)


dSex = list(
  t = LoadDataFromSheet("T_SEX"),
  r = LoadDataFromSheet("R_SEX"),
  y = LoadDataFromSheet("Y_SEX")
)

dAge = list(
  t = LoadDataFromSheet("T_AGE"),
  r = LoadDataFromSheet("R_AGE"),
  y = LoadDataFromSheet("Y_AGE")
)

dHSTestResults = list(
  t = LoadDataFromSheet("T_H&STEST_RESULTS"),
  r = LoadDataFromSheet("R_H&STEST_RESULTS"),
  y = LoadDataFromSheet("Y_H&STEST_RESULTS")
)

dExperience = list(
  t = LoadDataFromSheet("T_EXPERIENCE"),
  r = LoadDataFromSheet("R_EXPERIENCE"),
  y = LoadDataFromSheet("Y_EXPERIENCE")
)



# CREATE TABLES
SaveTableToCSV <- function(fileName, df){
  write.csv(df, paste0("./res_tables/", fileName), row.names = FALSE, quote=FALSE)
}

tableMeasuresNames <- c("średnia", "odchylenie std.", "mediana", "1. kwartyl", "3. kwartyl", "minimum", "maksimum")
getTrainMeasures <- function(x){
  tmp <- c(
    mean(x),
    sd(x),
    median(x),
    quantile(x = x, probs=0.25),
    quantile(x = x, probs=0.75),
    min(x),
    max(x)
  )
  return(tmp)
}

tableTime <- data.frame(
  Miara = tableMeasuresNames,
  'T' = getTrainMeasures(dTime$t$`Time [s]`),
  'R' = getTrainMeasures(dTime$r$`Time [s]`),
  'Y' = getTrainMeasures(dTime$y$`Time [s]`)
)
SaveTableToCSV("summaryTime.csv", tableTime)

tableAge <- data.frame(
  Miara = tableMeasuresNames,
  'T' = getTrainMeasures(dAge$t$`AGE [y]`),
  'R' = getTrainMeasures(dAge$r$`AGE [y]`),
  'Y' = getTrainMeasures(dAge$y$`AGE [y]`)
)
SaveTableToCSV("summaryAge.csv", tableAge)

tableExperience <- data.frame(
  Miara = tableMeasuresNames,
  'T' = getTrainMeasures(ifelse(dExperience$t$`Do you have any professional experience beyond intership?`=="YES",1,0)),
  'R' = getTrainMeasures(ifelse(dExperience$r$`Do you have any professional experience beyond intership?`=="YES",1,0)),
  'Y' = getTrainMeasures(ifelse(dExperience$y$`Do you have any professional experience beyond intership?`=="YES",1,0))
)
SaveTableToCSV("summaryExperience.csv", tableExperience)

tableHSTestResults <- data.frame(
  Miara = tableMeasuresNames,
  'T' = getTrainMeasures(dHSTestResults$t$`TEST 0-10 points`),
  'R' = getTrainMeasures(dHSTestResults$r$`TEST 0-10 points`),
  'Y' = getTrainMeasures(dHSTestResults$y$`TEST 0-10 points`)
)
SaveTableToCSV("summaryHSTestResults.csv", tableHSTestResults)

tableSex <- data.frame(
  Miara = tableMeasuresNames,
  'T' = getTrainMeasures(as.numeric(as.factor(dSex$t$`M / F / O`))),
  'R' = getTrainMeasures(as.numeric(as.factor(dSex$r$`M / F / O`))),
  'Y' = getTrainMeasures(as.numeric(as.factor(dSex$y$`M / F / O`)))
)
SaveTableToCSV("summarySex.csv", tableSex)

# HISTOGRAMY

CreateHist <- function(figTitle, xAxisName, t,r,y){
  png(paste0("./res_plots/", SpaceToUnderscore(figTitle), '.png'), width=800, height=600, res=150)
  
  time_layout <- matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE)
  layout(time_layout)
  
  par(oma=c(0,0,2,0))
  
  hist(x = t, col = "White", main = "Gogle przezroczyste", xlab = xAxisName, ylab = "Częstość")
  hist(x = r, col = "Red", main="Gogle czerwone", xlab = xAxisName, ylab = "Częstość")
  hist(x = y, col = "Yellow", main="Gogle zółte", xlab = xAxisName, ylab = "Częstość")
  
  mtext(figTitle, outer=TRUE, cex=1, font=2)
  
  dev.off()
} 

CreateHist(
  figTitle= "Histogram dla czasu",
  xAxisName = "Czas noszenia gogli [s]",
  t = dTime$t$`Time [s]`,
  r = dTime$r$`Time [s]`,
  y = dTime$y$`Time [s]`
)

CreateHist(
  figTitle= "Histogram dla wieku",
  xAxisName = "Wiek [lata]",
  t = dAge$t$`AGE [y]`,
  r = dAge$r$`AGE [y]`,
  y = dAge$y$`AGE [y]`
)

CreateHist(
  figTitle= "Histogram dla wyników testu H&S",
  xAxisName = "Wynik testu [0-10]",
  t = dHSTestResults$t$`TEST 0-10 points`,
  r = dHSTestResults$r$`TEST 0-10 points`,
  y = dHSTestResults$y$`TEST 0-10 points`
)

CreateHist(
  figTitle= "Histogram dla doświadczenia zawodowego",
  xAxisName = "Doświadczenie [0-1]",
  t = ifelse(dExperience$t$`Do you have any professional experience beyond intership?`=="YES",1,0),
  r = ifelse(dExperience$r$`Do you have any professional experience beyond intership?`=="YES",1,0),
  y = ifelse(dExperience$y$`Do you have any professional experience beyond intership?`=="YES",1,0)
)


CreateHist(
  figTitle= "Histogram dla płci (F=1, M=2, O=3)",
  xAxisName = "Płeć [1/2/3]",
  t = as.numeric(as.factor(dSex$t$`M / F / O`)),
  r = as.numeric(as.factor(dSex$r$`M / F / O`)),
  y = as.numeric(as.factor(dSex$y$`M / F / O`))
)

#PUDEŁKOWE
CreateBoxplot <- function(figTitle, yAxisName, t,r,y){
  png(paste0("./res_plots/", SpaceToUnderscore(figTitle), '.png'), width=800, height=600, res=150)
  
  time_layout <- matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE)
  layout(time_layout)
  
  par(oma=c(0,0,2,0))
  
  boxplot(x = t, col = "White", main = "Gogle przezroczyste", ylab = yAxisName)
  boxplot(x = r, col = "Red", main="Gogle czerwone",ylab = yAxisName)
  boxplot(x = y, col = "Yellow", main="Gogle zółte", ylab = yAxisName)
  
  mtext(figTitle, outer=TRUE, cex=1, font=2)
  
  dev.off()
} 

CreateBoxplot(
  figTitle= "Wykres pudełkowy dla czasu",
  yAxisName = "Czas noszenia gogli [s]",
  t = dTime$t$`Time [s]`,
  r = dTime$r$`Time [s]`,
  y = dTime$y$`Time [s]`
)

CreateBoxplot(
  figTitle= "Wykres pudełkowy dla wieku",
  yAxisName = "Wiek [lata]",
  t = dAge$t$`AGE [y]`,
  r = dAge$r$`AGE [y]`,
  y = dAge$y$`AGE [y]`
)

CreateBoxplot(
  figTitle= "Wykres pudełkowy dla wyników testu H&S",
  yAxisName = "Wynik testu [0-10]",
  t = dHSTestResults$t$`TEST 0-10 points`,
  r = dHSTestResults$r$`TEST 0-10 points`,
  y = dHSTestResults$y$`TEST 0-10 points`
)



############################# TFD -> yellow bag
dTFD = list(
  t =  LoadDataFromSheet("TFD_T_Total_Fixation_Duration"),
  r = LoadDataFromSheet("TFD_R_Total_Fixation_Duration"),
  y = LoadDataFromSheet("TFD_Y_Total_Fixation_Duration_")
)

tableTFD_yBag_Results <- data.frame(
  Miara = tableMeasuresNames,
  'T' = getTrainMeasures(dTFD$t$`Y bag`),
  'R' = getTrainMeasures(dTFD$r$`Y bag`),
  'Y' = getTrainMeasures(dTFD$y$`Y bag`)
)
SaveTableToCSV("summaryTFD_yBag.csv", tableTFD_yBag_Results)

CreateHist(
  figTitle= "Histogram dla czasu skupienia na żółtej torbie",
  xAxisName = "Czas [s]",
  t = dTFD$t$`Y bag`,
  r = dTFD$r$`Y bag`,
  y = dTFD$y$`Y bag`
)

CreateBoxplot(
  figTitle= "Wykres pudełkowy dla czasu skupienia na żółtej torbie",
  yAxisName = "Czas [s]",
  t = dTFD$t$`Y bag`,
  r = dTFD$r$`Y bag`,
  y = dTFD$y$`Y bag`
)


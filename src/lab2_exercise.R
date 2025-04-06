library("readODS")

dataT <- read_ods(
  "./data/ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "T_TIME",
  skip = 1,
)

dataR <- read_ods(
  "./data/ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "R_TIME",
  skip = 1,
)

dataY <- read_ods(
  "./data/ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods",
  sheet = "Y_TIME",
  skip = 1,
)


#head(data)
#tail(data)
#colnames(data)
#dim(data)
#str(data)
#summary(data)
#summary(data$`T task [s]`)
#median(data$`T task [s]`)
#sd(data$`T task [s]`)
#var(data$`T task [s]`)
#class(data$`T task [s]`)


time_layout <- matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE)
layout(time_layout)

hist(x = dataT$`Time [s]`,
     main = "Histogram zmiennej T_TIME",
     xlab = "Czas noszenia przezroczystych gogli [s]",
     ylab = "Częstość",
     col = "White",
     col.main = "Green")

hist(x = dataR$`Time [s]`,
     main = "Histogram zmiennej R_TIME",
     xlab = "Czas noszenia przezroczystych gogli [s]",
     ylab = "Częstość",
     col = "Red",
     col.main = "Green")

hist(x = dataY$`Time [s]`,
     main = "Histogram zmiennej Y_TIME",
     xlab = "Czas noszenia przezroczystych gogli [s]",
     ylab = "Częstość",
     col = "Yellow",
     col.main = "Green")

# -------------------------------- BOX
#png("my_plot.png") -> open file

time_layout <- matrix(c(1,2,3), nrow=3, ncol=1, byrow = TRUE)
layout(time_layout)


boxplot(x = dataT$`Time [s]`,
        col = "White",
        horizontal = TRUE,
        xlab = "(T) Czas noszenia przezroczystych gogli [s]")

boxplot(x = dataR$`Time [s]`,
        col = "Red",
        horizontal = TRUE,
        xlab = "(R) Czas noszenia przezroczystych gogli [s]")


boxplot(x = dataY$`Time [s]`,
        col = "Yellow",
        horizontal = TRUE,
        xlab = "(Y) Czas noszenia przezroczystych gogli [s]")


#dev.off() -> save file




# Wymagane pakiety
load_or_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

load_or_install("readODS")
load_or_install("dplyr")
load_or_install("tidyr")
load_or_install("rstatix")
load_or_install("ggpubr")

# Nazwy arkuszy i pliku
var_sheets <- c(
  "TFD_T_Total_Fixation_Duration", 
  "TFD_R_Total_Fixation_Duration", 
  "TFD_Y_Total_Fixation_Duration_"
)
# Kolory gogli, w kolejności wynikającej z arkuszy tj. var_sheets
goggle_cols <- c("Transparent", "Red", "Yellow")
file_path <- "data/ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods"

# Funkcja do wczytywania i łączenia arkuszy
read_and_combine_ods_sheets <- function(file_path, sheets, skip_rows = 1, include_source = TRUE) {
  all_data <- lapply(sheets, function(sheet) {
    read_ods(file_path, sheet = sheet, skip = skip_rows)
  })
  names(all_data) <- sheets
  if (include_source) {
    bind_rows(all_data, .id = "source_sheet")
  } else {
    bind_rows(all_data)
  }
}

# Wczytanie danych
ttff <- read_and_combine_ods_sheets(file_path, var_sheets)

# Mapowanie arkusza na kolor gogli
goggles_map <- setNames(goggle_cols, var_sheets)
# Stworzenie zmiennej goggles z kolorem gogli
ttff <- ttff %>%
  mutate(goggles = recode(source_sheet, !!!goggles_map))

# Zamiana nazw na "bezpieczne" tzn. spacje na kropki
names(ttff) <- make.names(names(ttff))

# Wykres kwantyl-kwantyl
goggle_colors <- c(
  "Transparent" = "skyblue",
  "Red" = "firebrick",
  "Yellow" = "goldenrod"
)

# Operator + dodaje kolejne warstwy do wykresu
ggqqplot(
  ttff, 
  x = "Y.bag", 
  facet.by = "goggles", 
  title="TFD dla obiektu Y bag", 
  color="goggles",
) + 
  scale_color_manual(values = goggle_colors) +
  scale_fill_manual(values = goggle_colors)



#=========================== ZABAWA DALEJ ===========================

x <- rnorm(50, mean = 0, sd = 2)
y <- rnorm(30, mean = 1, sd = 1)
#var.test(x,y)
xy <- c(x,y)
grupa <- as.factor(c(rep("A", length(x)), rep("B", length(y))))
random_df <- data.frame( dane = xy, grupa = grupa)
bartlett.test(dane ~ grupa, random_df)

random_df %>% levene_test(dane ~ grupa, center = mean)

red_googles_amount <- ttff %>%
  filter(goggles == "Red") %>%
  nrow() %>% 
  print()
chisq.test(c(54, 57, 58)) #szukać tylko obserwacji niepustych

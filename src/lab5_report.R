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

# Nazwy arkuszy i pliku
exp_sheets <- c("T_EXPERIENCE", "R_EXPERIENCE", "Y_EXPERIENCE")
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


# Wczytanie danych o TTFF
ttff <- read_and_combine_ods_sheets(file_path, var_sheets)

# Mapowanie arkusza na kolor gogli
goggles_map <- setNames(goggle_cols, var_sheets)
# Stworzenie zmiennej goggles z kolorem gogli
ttff <- ttff %>%
  mutate(goggles = recode(source_sheet, !!!goggles_map))

# Zamiana nazw na "bezpieczne" tzn. spacje na kropki
names(ttff) <- make.names(names(ttff))

# Wczytanie danych
experience <- read_and_combine_ods_sheets(file_path, exp_sheets)

# Zmiana nazwy zmiennej z doświadczeniem na krótszą oraz zamiana na zmienną kategoryczną (factor)
experience <- experience  %>%
  rename(
    experience = `Do you have any professional experience beyond intership?`,
  )  %>%
  mutate(
    experience = as.factor(experience)
  )
# Zamiana nazw na "bezpieczne" tzn. spacje na kropki
names(experience) <- make.names(names(experience))

# Połączenie danych
merged_data <- inner_join(
  experience,
  ttff,
  by = c("participant.nr" = "Participant.nr")
)

# 4. Zweryfikuj normalność rozkładów w grupach
shapiro_full <- merged_data %>%
  group_by(experience) %>%
  shapiro_test(Y.bag)
shapiro_full


# 5. Zweryfikuj homogeniczność wariancji
# rozkład NORMALNY więc flinger
# !!! TODO -> DO POPRAWY!!!!!!!!
var.test(Y.bag ~ experience, data = merged_data)


# 6. Wybierz właściwy test do porywnania położenia dwóch grup - osób doświadczonych i niedoświadczonych
# H0: Nie ma różnicy w czasie skupienia na żółtej torbie (TFD-Y bag) pomiędzy grupami z doświadczeniem i bez doświadczenia.
# H1: Grupa z doświadczaniem ma dłuższy czas skupienia na żółtej torbie (TFD-Y bag) niż grupa bez doświadczenia.
merged_data %>% 
  wilcox_test(Y.bag ~ experience, alternative = 'less')


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
SaveTableToCSV <- function(fileName, df){
  write.csv(df, paste0("./res_tables/", fileName), row.names = FALSE, quote=FALSE)
}
tableExp <- data.frame(
  Miara = tableMeasuresNames,
  'ZExp' = getTrainMeasures(merged_data %>% filter(experience == "YES") %>% pull(Y.bag)),
  'BezExp' = getTrainMeasures(merged_data %>% filter(experience == "NO") %>% pull(Y.bag)) 
)
print(tableExp)
SaveTableToCSV("summaryExp_ybag.csv", tableExp)

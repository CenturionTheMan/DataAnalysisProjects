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

var_sheets <- c(
  "TFD_T_Total_Fixation_Duration", 
  "TFD_R_Total_Fixation_Duration", 
  "TFD_Y_Total_Fixation_Duration_"
)
goggle_cols <- c("Transparent", "Red", "Yellow")
file_path <- "data/ALL_DATA_TRANSPARENT_YELLOW_RED GOGLES_ON_CONSTRUCTION_SITE.ods"

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

ttff <- read_and_combine_ods_sheets(file_path, var_sheets)

goggles_map <- setNames(goggle_cols, var_sheets)
ttff <- ttff %>%
  mutate(goggles = recode(source_sheet, !!!goggles_map))

names(ttff) <- make.names(names(ttff))

goggle_colors <- c(
  "Transparent" = "skyblue",
  "Red" = "firebrick",
  "Yellow" = "goldenrod"
)

png("./res_plots/qqplot_dla_TFD_yBag.png", width=900, height=600, res=150)
ggqqplot(
  ttff, 
  x = "Y.bag", 
  facet.by = "goggles", 
  title="TFD dla obiektu Y bag", 
  color="goggles",
) + 
  scale_color_manual(values = goggle_colors) +
  scale_fill_manual(values = goggle_colors)

dev.off()


#=================== RÓWNOLICZNOSC ===================
tmp <- ttff %>% select(Y.bag, goggles)

ttff_for_chi = c(
  R = ttff %>% filter(goggles == "Red") %>% select(Y.bag) %>% unlist(),
  T = ttff %>% filter(goggles == "Transparent") %>% select(Y.bag) %>% unlist(),
  Y = ttff %>% filter(goggles == "Yellow") %>% select(Y.bag) %>% unlist()
)


#=================== RÓWNOŚĆ WARIANCJI ===================
#H₀ (hipoteza zerowa): wariancje we wszystkich grupach są równe.
#H₁ (hipoteza alternatywna): co najmniej jedna grupa ma inną wariancję.
#Ponieważ wartość p = 0.240 jest większa niż standardowy poziom istotności (np. 0.05), nie ma podstaw do odrzucenia hipotezy zerowej.
#Oznacza to, że nie stwierdzono istotnych różnic między wariancjami w analizowanych grupach.

ttff %>%
  levene_test(Y.bag ~ goggles) %>%
  mutate(
    p = paste0("Wartość p = ", p),
  ) %>%
  select(p) %>%
  print()
  
  
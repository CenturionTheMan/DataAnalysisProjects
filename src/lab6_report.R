load_or_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

load_or_install("ggpubr")
load_or_install("tidyverse")
load_or_install("rstatix")
load_or_install("knitr")
load_or_install("readODS")
load_or_install("dplyr")
load_or_install("tidyr")
load_or_install("rlang")


summary_stats <- function(data, dv, iv) {
  cat("\nStatystyki opisowe")
  
  data %>%
    group_by({{ iv }}) %>%
    get_summary_stats({{ dv }}, type = "mean_sd") %>%
    kable() %>%
    print()
  
  dv_str <- as_label(enquo(dv))
  iv_str <- as_label(enquo(iv))
  
  p <- ggboxplot(data, x = iv_str, y = dv_str, add = "jitter")
  print(p)
}

check_normality <- function(data, dv, iv) {
  cat("\nTest Shapiro-Wilka")
  
  dv_str <- as_label(enquo(dv))
  iv_str <- as_label(enquo(iv))
  
  result <- data %>%
    group_by({{ iv }}) %>%
    shapiro_test({{ dv }})
  
  print(kable(result))
  
  p <- ggqqplot(data, x = dv_str, facet.by = iv_str)
  print(p)
  
  return(result)
}

check_homogeneity_of_variance <- function(data, dv, iv, center) {
  cat("\nTest homogeniczności wariancji (Levene, center = ", center, "):")
  
  formula <- as.formula(paste(as_label(enquo(dv)), "~", as_label(enquo(iv))))
  data %>% levene_test(formula = formula, center = center) %>% kable() %>% print()
}


check_assumptions <- function(data, dv, iv, alpha=0.05) {
  normality <- data %>% check_normality({{ dv }}, {{ iv }}) 
  non_normal <- any(normality$p < alpha)
  center <- if (non_normal) "median" else "mean"
  data %>% check_homogeneity_of_variance({{ dv }}, {{ iv }}, center = center)
}

#### FUNCTIONS END ####

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

ttff %>% summary_stats(dv = Y.bag, iv = goggles)
ttff %>% check_assumptions(dv = Y.bag, iv=goggles)
ttff %>% anova_test(Y.bag ~ goggles) %>% print()
ttff %>% tukey_hsd(Y.bag ~ goggles) %>% kable() %>% print()




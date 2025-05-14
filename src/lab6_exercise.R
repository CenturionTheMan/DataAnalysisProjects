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

load_or_install("dplyr")
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
#### PLANT GROWTH ####
data("PlantGrowth")
PlantGrowth %>% summary_stats(dv = weight, iv = group)
PlantGrowth %>% check_assumptions(dv = weight, iv=group)
PlantGrowth %>% anova_test(weight ~ group) %>% print()
PlantGrowth %>% tukey_hsd(weight ~ group) %>% kable() %>% print()

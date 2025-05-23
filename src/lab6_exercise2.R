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
df_rats <- read.table(
  "data/ratfeed.dat",
  header=FALSE,
  skip=25,
  colClasses = c("numeric", "factor", "factor"),
  col.names=c("rat_weight_gain", "diet_amount", "diet_type")
)
df_rats %>% summary_stats(dv = rat_weight_gain, iv = diet_type)
df_rats %>% check_assumptions(dv = rat_weight_gain, iv=diet_type)
df_rats %>% anova_test(rat_weight_gain ~ diet_type) %>% print()
df_rats %>% tukey_hsd(rat_weight_gain ~ diet_type) %>% kable() %>% print()


# zad 3
df_rats %>% welch_anova_test(rat_weight_gain ~ diet_type) %>% print()
df_rats %>% games_howell_test(rat_weight_gain ~ diet_type) %>% kable() %>% print()


# zad 4
load_or_install("gapminder")
load_or_install("WRS2")

head(gapminder)
distinct(gapminder, year)
gapminder_2007 <- gapminder %>%
  filter(year == 2007 & continent != "Oceania") %>% 
  droplevels()

gapminder_2007 %>% summary_stats(dv = lifeExp, iv = continent)
gapminder_2007 %>% check_assumptions(dv = lifeExp, iv = continent)

#kruskal
gapminder_2007 %>% kruskal_test(lifeExp ~ continent) %>% print()
gapminder_2007 %>% dunn_test(lifeExp ~ continent) %>% kable() %>% print()


# odporne
WRS2::t1way(lifeExp ~ continent, gapminder_2007)
WRS2::lincon(lifeExp ~ continent, gapminder_2007)



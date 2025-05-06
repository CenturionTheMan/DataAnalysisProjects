load_or_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

load_or_install("dplyr")
load_or_install("tidyr")
load_or_install("rstatix")
load_or_install("lawstat")

df_rats <- read.table(
  "data/ratfeed.dat",
  header=FALSE,
  skip=25,
  colClasses = c("numeric", "factor", "factor"),
  col.names=c("rat_weight_gain", "diet_amount", "diet_type")
)
summary(df_rats)

df_rats %>%
  group_by(diet_amount) %>%
  shapiro_test(rat_weight_gain) %>% 
  print()

ggqqplot(
  df_rats, 
  x = "rat_weight_gain", 
  facet.by = "diet_amount", 
  title="Waga od ilosci", 
  color="diet_amount",
)


# wariancja
df_rats %>% 
  levene_test(rat_weight_gain ~ diet_amount, center=mean) %>% 
  print()

#zad4
df_rats %>% 
  t_test(rat_weight_gain ~ diet_amount, var.equal = TRUE)

#zad5
df_rats %>% 
  wilcox_test(rat_weight_gain ~ diet_amount)

#zad6
df_rats %>% 
  t_test(rat_weight_gain ~ diet_amount)

#zad7
brunner.munzel.test(
  df_rats %>% filter(diet_amount == 1) %>% pull(rat_weight_gain),
  df_rats %>% filter(diet_amount == 2) %>% pull(rat_weight_gain),
)

#zad8
df_rats %>% 
  t_test(rat_weight_gain ~ diet_amount, var.equal = TRUE, alternative="greater")


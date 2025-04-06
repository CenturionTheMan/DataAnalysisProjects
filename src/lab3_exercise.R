library("moments")


#time_layout <- matrix(1, nrow=1, ncol=1, byrow = TRUE)
#layout(time_layout)

df <- read.csv("./data/nutrient.csv")
nrow(df)



# zad 1
skewness(df)
kurtosis(df)


#zad 3
df <- df[df[,"iron"] != 0, ] 
nrow(df)

#zad 2
print("POW 0.5")
skewness(df^0.5)
kurtosis(df^0.5)

print("POW 0.25")
skewness(df^0.25)
kurtosis(df^0.25)

print("LOG")
skewness(log(df))
kurtosis(log(df))

print("LOG 10")
skewness(log10(df))
kurtosis(log10(df))

df_changed <- df^0.25

#zad3 kontynuacja
plot(x=df_changed$iron, y = df_changed$calcium)


df$ss_calcium <- df$calcium^(1/4)
df$ss_iron = df$iron^(1/4)
pairs( df[, c("ss_calcium", "ss_iron", "calcium", "iron")])


#zad6
cor(x=df$calcium, y=df$iron, method='pearson')
cor(x=df_changed$calcium, y=df_changed$iron, method='pearson')

cor(x=df$calcium, y=df$iron, method='spearman')
cor(x=df_changed$calcium, y=df_changed$iron, method='spearman')

cor(x=df$calcium, y=df$iron, method='kendall')
cor(x=df_changed$calcium, y=df_changed$iron, method='kendall')



#zad 7
df$ss_calcium <- df$calcium^(1/4)
shapiro.test(df$ss_calcium)
# jezeli p-value < alpha to odrzucamy H0 -> w testach normlanosci H0 mówi ze rozkład jest normalny
# jeżeli p-value >= alpha to mówimy że nie ma podstaw dla odrzucenia H0


#zad 8
df$ss_iron <- df$iron^(1/4)
shapiro.test(df$iron)
shapiro.test(df$ss_iron)


#zad 9
library(MVN)

df$ss_calcium <- df$calcium^(1/4)
df$ss_iron = df$iron^(1/4)
hz_result <- mvn(
  data = df[, c("ss_calcium", "ss_iron")],
  mvnTest = "hz"
)
print(hz_result)

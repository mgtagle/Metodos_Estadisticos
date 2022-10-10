# Análisis de varianza
# ANOVA
# 05/10/2022

library(repmis)

paraje <- source_data("https://www.dropbox.com/s/fbrwxypacjgeayj/Datos_Rascon_Anova.csv?dl=1")


tapply(paraje$DAP, paraje$Paraje, mean)
tapply(paraje$DAP, paraje$Paraje, var)

boxplot(paraje$DAP ~ paraje$Paraje,
        xlab = "Paraje",
        ylab = "DAP (cm)",
        col = "green")
# Homogeneidad de varianzas barlett.test

bartlett.test(paraje$DAP, paraje$Paraje)
# H0 = las varianzas son homogeneas
# H1 = las varianzas no son homogeneas

# Normalidad de la variable DAP

shapiro.test(paraje$DAP)
hist(paraje$DAP)

library(dplyr)

chinatu <- paraje %>% 
  filter(Paraje == "Chinatu")
trinidad <- paraje %>% 
  filter(Paraje == "Trinidad")

shapiro.test(chinatu$DAP)
shapiro.test(trinidad$DAP)

par.aov <- aov(paraje$DAP ~ paraje$Paraje)
summary(par.aov)

# Como la prueba de ANOVA (aov) me dice que hay diferencias
# significativas, entoneces procedo con la prueba de Tukey.
# la prueba de Tukey identificará donde estan las diferencias


TukeyHSD(par.aov)

plot(TukeyHSD(par.aov))

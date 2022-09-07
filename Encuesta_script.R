# MAGT
# Importar encuesta

Enc1 <- read.csv("Encuesta_codificada.csv", header=T)

# convertir variables a factores
Enc1$Entrev <- as.factor(Enc1$Entrev)
levels(Enc1$Entrev) # revisar si son factores

Enc1$Genero <- as.factor(Enc1$Genero)
levels(Enc1$Genero)
Enc1$Carrera <- as.factor(Enc1$Carrera)
levels(Enc1$Carrera)
Enc1$Semestre <- as.factor(Enc1$Semestre)
levels(Enc1$Semestre)

#  Función para transformar caracteres a fechas

library(lubridate)
# Enc1$Fecha <- as_date(Enc1$Fecha) #Pendiente de arreglar

# ¿Cuál fue el porcentaje de entrevistados por género?
gen <- table(Enc1$Genero)
round(gen/length(Enc1$Genero)*100,1)

# ¿Qué equipo entrevistador tuvo más encuestas?

ent <- table(Enc1$Entrev)
prop.table(ent)*100
pie(prop.table(ent)*100)

# ¿Cuál es el porcentaje de alumnos entrevistados por carrera?

table(Enc1$Carrera)
car <- table(Enc1$Carrera)
prop.table(car)*100

# ¿Cuántos alumnos participaron por semestre?

sem <- table(Enc1$Semestre)
prop.table(sem)*100

# ¿Cuál es el rango de edad de los participantes?
range(Enc1$Edad)

# ¿Cómo te entersaste de la facultad?

conFCF <- table(Enc1$oi_1)
prop.table(conFCF)*100
pie(prop.table(conFCF)*100)

# ¿Fué tu primera opción la FCF?
op <- table(Enc1$oi_2)
prop.table(op)*100

# ¿Presentaste en otra facultad?
of <- table(Enc1$oi_2a)
prop.table(of)*100

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
Enc1$oi_4a <- as.factor(Enc1$oi_4a)

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

paleta <- c("red", "gray", "blue", "yellow", "green")
conFCF <- table(Enc1$oi_1)
prop.table(conFCF)*100
pie(prop.table(conFCF)*100,
    col=palette("Tableau 10"))

# ¿Fué tu primera opción la FCF?
op <- table(Enc1$oi_2)
prop.table(op)*100

# ¿Presentaste en otra facultad?
of <- table(Enc1$oi_2a)
prop.table(of)*100

# Estuviste inscrito en otra facultad
oi_4 <- table(Enc1$oi_4)
prop.table(oi_4)*100

# Donde etaba inscrito
oi_4a <- table(Enc1$oi_4a)
prop.table(oi_4a)*100
oi_4a

# cONVIVENCIA
ef_1 <- table(Enc1$ef_1)
prop.table(ef_1)*100

ins1 <- table(Enc1$ins_1)
prop.table(ins1)*100

ap_1 <- table(Enc1$ap_1)
prop.table(ap_1)*100

ap_2 <- table(Enc1$ap_2)
prop.table(ap_2)*100


ap_3 <- table(Enc1$ap_3)
prop.table(ap_3)*100


# Guardar la BD con los datos nuevos en formato csv
write.csv(Enc1, "Encuesta_codificada.csv")


# paleta de colores
# https://estadisticamente.com/paletas-de-colores-en-r/
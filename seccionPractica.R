# Libreria especial para hacer carga automática de librerías
install.packages("easypackages")
library("easypackages")

# Listado de librerías requeridas
lib_req <- c("ggplot2", "dplyr", "readxl", "utils")

# Verificación, instalación y carga de librerías
easypackages::packages(lib_req)

# Buscar la ruta del archivo de Excel
file.choose()

# Copiar la ruta de la consola y guardar en la variable
ruta_excel<- "C:\\Users\\MAYCOL\\Documents\\Universidad\\Cuarto Semestre\\Estadistica\\Ejercicio-practico1\\salarios.xlsx"

# Leer archivo de excel mediante la funcion
tabla <- read_excel(ruta_excel)

# Convertir columna salario a númerica
tabla$salario <- as.numeric(tabla$salario)

# DESCRIPTIVAS ---------------------------------------------------------------

# Resumen de los datos
total <- summary(tabla)
total

# Otra medida descriptiva: Varianza y Desviación estándar por salarios
varianza <- var(tabla$salario) 
desviacionEstandar <- sd(tabla$salario)   


#Tablas Frecuencias---------------------------------------------------------
# Tabla de frecuencias para la variable cuantitativa salario
tabla_frecuencia_salarios <- table(tabla$salario)
tabla_frecuencia_salarios

# Tabla de frecuencias para variable cuantitativa continua 'salario'
# Agrupar en 5 intervalos la variable 'salario' de tabla
intervalos_salarios <-  cut(tabla$salario, breaks = 10, include.lowest = TRUE, right = FALSE)

# Calcular la tabla de frecuencias completa
tabla_frec <- as.data.frame(table(intervalos_salarios)) %>%
  rename(Intervalo = intervalos_salarios, Frec_Abs = Freq) %>%
  mutate(Frec_Rel = Frec_Abs / sum(Frec_Abs),
         Frec_Acum = cumsum(Frec_Abs),
         Frec_Rel_Acum = cumsum(Frec_Rel))

# Tabla final
tabla_frec

#GRÁFICOS-------------------------------------------------------------------
# Graficar histograma de 'salarios'
hist(tabla$salario, col = "lightblue", main = "Histograma de Salario", xlab = "Salario", ylab = "Frecuencia")

# Graficar histograma a partir de la tabla de frecuencias
barplot(tabla_frec$Frec_Abs, 
        names.arg = tabla_frec$Intervalo,
        space = 0,
        col = "lightblue", 
        main = "Histograma de Salario a partir de la T. Frecuencias", 
        xlab = "Salario", 
        ylab = "Frecuencia", 
        border = "black")

# Ojiva (frecuencia acumulada de 'salario')
plot(tabla_frec$Frec_Acum, type = "o", col = "red", main = "Ojiva de Salario", xlab = "Intervalos de Salario", ylab = "Frecuencia acumulada")

#Discreta vs Continua
#Distribución Frecuencia Relativa -- Frecuencia, Frecuencia Relativa
tabla_salario1 <- as.data.frame(table(tabla$salario)) %>%
  rename(salario = Var1, Frecuencia = Freq) %>%
  mutate(Frec_R = Frecuencia / sum(Frecuencia))

#Visualización grafica de la distribución de Frecuencia Relativa
ggplot(tabla_salario1, aes(x = as.numeric(salario), y = Frec_R)) +
  geom_point(size = 4, color = "blue") +
  geom_segment(aes(xend = as.numeric(salario), yend = 0), color = "blue", linetype = "dashed") +
  labs(title = "Función de Distribución de Salario",
       x = "Salario",
       y = "Frecuencia Relativa") +
  theme_minimal()

# Función de densidad para 'salario'
ggplot(tabla, aes(x = salario)) +
  geom_density(fill = "skyblue", alpha = 0.5, color = "blue") +
  labs(title = "Función de Densidad de Salario",
       x = "Salario Mensual",
       y = "Densidad") +
  theme_minimal()

#Usando acumulada
# Distribución acumulada para (salario) Frecuencia, Frecuencia Acumulada y Frecuencia R A
tabla_salario2 <- as.data.frame(table(tabla$salario)) %>%
  rename(salario = Var1, Frecuencia = Freq) %>%
  mutate(Frec_Acum = cumsum(Frecuencia), 
         Frec_Rel_Acum = Frec_Acum / sum(Frecuencia))

# Distribución acumulada de salario-Represenación grafica de la ditribución acumulada
plot(as.numeric(tabla_salario2$salario), tabla_salario2$Frec_Rel_Acum, type = "o", col = "blue",
     main = "Distribución Acumulada de Salario",
     xlab = "Salario Mensual", ylab = "Frecuencia Relativa Acumulada",
     pch = 16, lwd = 2)

# Distribución acumulada para (salario) -- Función piso
dis_ecdf <- ecdf(tabla$salario)  # Función de distribución acumulada empírica
plot(dis_ecdf, col = "red", main = "Distribución Acumulada de disp (Volumen total)",
     xlab = "Volumen total", ylab = "Frecuencia Acumulada",
     lwd = 2)

#Con ggplot -- Otra representación grafica, de la ditribución acumulada para salario
ggplot(tabla, aes(x = salario)) +
  stat_ecdf(geom = "step", color = "red", size = 1) +
  labs(title = "Distribución Acumulada de Salario",
       x = "Salario",
       y = "Frecuencia Acumulada") +
  theme_minimal()

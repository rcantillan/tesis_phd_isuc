
library(mfx)


#unir todos los data frames
edges_full <- rbind(edges_2009, edges_2015, edges_2020)


# Diferencias absolutas
edges_full$hombres_perc_dist <- abs(edges_full$hombres_perc_i - edges_full$hombres_perc_j)
edges_full$mujeres_perc_dist <- abs(edges_full$mujeres_perc_i - edges_full$mujeres_perc_j)
edges_full$superior_perc_dist <- abs(edges_full$superior_perc_i - edges_full$superior_perc_j)
edges_full$promedio_edad_dist <- abs(edges_full$promedio_edad_i - edges_full$promedio_edad_j)
edges_full$promedio_horas_dist <- abs(edges_full$promedio_horas_i - edges_full$promedio_horas_j)
edges_full$promedio_ingreso_dist <- abs(edges_full$promedio_ingreso_i - edges_full$promedio_ingreso_j)
edges_full$tamaño_combinado <- abs(edges_full$tamano_i + edges_full$tamano_j)
edges_full$promedio_ingreso_dist <- sqrt(edges_full$promedio_ingreso_dist)
edges_full$tamaño_combinado <- log(edges_full$tamaño_combinado)


edges_full <- edges_full %>% 
  filter(ocup_i != 0 & ocup_i != 11 & ocup_i != 31) %>%
  filter(ocup_j != 0 & ocup_j != 11 & ocup_j != 31) %>%
  filter(ocup_i != ocup_j)



# Modelo binomial negativo por año (con coeficientes estandarizados)
## Modelo 2009
m1 <- MASS::glm.nb(peso ~ 
                     tamaño_combinado +  
                     mujeres_perc_dist +
                     superior_perc_dist +   
                     promedio_edad_dist +
                     promedio_horas_dist +
                     promedio_ingreso_dist +
                     mujeres_perc_dist*superior_perc_dist,
                   data = subset(edges_full, ano == 2009),
                   maxit = 1000)

me_m1 <- negbinmfx(m1, data=subset(edges_full, ano == 2009), robust=T, clustervar1 = "ocup_i")

m1 <- MASS::glm.nb(peso ~ 
                     scale(tamaño_combinado) +  
                     scale(mujeres_perc_dist) +
                     scale(superior_perc_dist) +   
                     scale(promedio_edad_dist) +
                     scale(promedio_horas_dist) +
                     scale(promedio_ingreso_dist) +
                     scale(mujeres_perc_dist)*scale(superior_perc_dist),
                   data = subset(edges_full, ano == 2009),
                   maxit = 1000)
summary(m1)
coefs_m1 <- coef(m1)

## modelo 2015
m2 <- MASS::glm.nb(peso ~ 
                     tamaño_combinado +  
                     mujeres_perc_dist +
                     superior_perc_dist +   
                     promedio_edad_dist +
                     promedio_horas_dist +
                     promedio_ingreso_dist +
                     mujeres_perc_dist*superior_perc_dist,
                   data = subset(edges_full, ano == 2015),
                   maxit = 2000)

summary(m2)
me_m2 <- negbinmfx(m2, data=subset(edges_full, ano == 2015), robust=T, clustervar1 = "ocup_i")


m2 <- MASS::glm.nb(peso ~ scale(tamaño_combinado) +  
                     scale(mujeres_perc_dist) +
                     scale(superior_perc_dist) +   
                     scale(promedio_edad_dist) +
                     scale(promedio_horas_dist) +
                     scale(promedio_ingreso_dist) +
                     scale(mujeres_perc_dist)*scale(superior_perc_dist),
                   data = subset(edges_full, ano == 2015),
                   maxit = 1000)
summary(m2)
coefs_m2 <- coef(m2)

## Modelo 2020
m3 <- MASS::glm.nb(peso ~ 
                     tamaño_combinado +  
                     mujeres_perc_dist +
                     superior_perc_dist +   
                     promedio_edad_dist +
                     promedio_horas_dist +
                     promedio_ingreso_dist +
                     mujeres_perc_dist*superior_perc_dist,
                   data = subset(edges_full, ano == 2020),
                   maxit = 1000)

me_m3 <- negbinmfx(m3, data=subset(edges_full, ano == 2020), robust=T, clustervar1 = "ocup_i")


m3 <- MASS::glm.nb(peso ~ scale(tamaño_combinado) +  
                     scale(mujeres_perc_dist) +
                     scale(superior_perc_dist) +   
                     scale(promedio_edad_dist) +
                     scale(promedio_horas_dist) +
                     scale(promedio_ingreso_dist) +
                     scale(mujeres_perc_dist)*scale(superior_perc_dist),
                   data = subset(edges_full, ano == 2020),
                   maxit = 1000)

summary(m3)
coefs_m3 <- coef(m3)




library(texreg)

library(texreg)

texreg(list(m1, m2, m3),  
       
       # Nombres de modelos
       custom.model.names = c("Modelo 2009", "Modelo 2010", "Modelo 2011"),
       
       # Nombre variable dependiente  
       dep.var.caption = "Variable Dependiente: Flujo de Trabajadores",
       
       # Leyendas
       caption = "Modelos Regresión Binomial Negativa",
       label = c("2009", "2010", "2011"),
       
       # Opciones de formato tabla
       dcolumn = TRUE,
       booktabs = TRUE)




# Crear un dataframe con valores de ejemplo para las variables de interés
datos_ejemplo <- expand.grid(
  mujeres_perc_dist = seq(0, 100, length.out = 100),
  promedio_educ_dist = seq(0, 13, length.out = 100),
  tamaño_combinado = median(edges_full$tamaño_combinado),  # Agrega tamaño_combinado con un valor arbitrario
  promedio_edad_dist = median(edges_full$promedio_edad_dist),  # Agrega promedio_edad_dist con un valor arbitrario
  promedio_horas_dist = median(edges_full$promedio_horas_dist, na.rm=T),
  promedio_ingreso_dist = median(edges_full$promedio_ingreso_dist, na.rm=T),
  ano = "2012"  # Puedes cambiar el año según tus necesidades
)

# Predecir valores de peso utilizando el modelo
datos_ejemplo$peso_predicho <- predict(m1, newdata = datos_ejemplo, type = "response")


library(ggplot2)
library(reshape2)

# Crear un dataframe con valores de ejemplo para las variables de interés
datos_ejemplo <- expand.grid(
  mujeres_perc_dist = seq(0, 100, length.out = 100),
  promedio_educ_dist = seq(0, 13, length.out = 100),
  tamaño_combinado = median(edges_full$tamaño_combinado),
  promedio_edad_dist = median(edges_full$promedio_edad_dist, na.rm=T),
  promedio_horas_dist = median(edges_full$promedio_horas_dist, na.rm=T),
  promedio_ingreso_dist = median(edges_full$promedio_ingreso_dist, na.rm=T),
  ano = "2012"
)

# Predecir valores de peso utilizando el modelo
datos_ejemplo$peso_predicho <- predict(m1, newdata = datos_ejemplo, type = "response")

# Convertir el dataframe a formato largo para ggplot
datos_largos <- melt(datos_ejemplo, id.vars = c("mujeres_perc_dist", "promedio_educ_dist"), variable.name = "Variable")


# Crear el gráfico de contorno con escala de colores ajustada
ggplot(datos_ejemplo, aes(x = mujeres_perc_dist, y = promedio_educ_dist, fill = peso_predicho)) +
  geom_tile() +
  labs(title = "Interacción mujeres_perc_dist:promedio_educ_dist en Peso",
       x = "mujeres_perc_dist",
       y = "promedio_educ_dist",
       fill = "Peso predicho") +
  scale_fill_viridis_c() +  # Utilizar paleta de colores "viridis"
  theme_minimal()




library(plot3D)

# Crear el gráfico 3D
scatter3D(
  datos_ejemplo$mujeres_perc_dist, 
  datos_ejemplo$promedio_educ_dist, 
  datos_ejemplo$peso_predicho,
  color = "blue", 
  pch = 19, 
  cex = 2, 
  phi = 20,  # Ajusta este valor según sea necesario
  theta = 30,  # Ajusta este valor según sea necesario
  ticktype = "detailed", 
  xlab = "mujeres_perc_dist", 
  ylab = "promedio_educ_dist", 
  zlab = "Peso predicho"
)

library(plotly)
plot_ly(x=datos_ejemplo$mujeres_perc_dist, 
        y=datos_ejemplo$promedio_educ_dist, 
        z=datos_ejemplo$peso_predicho, 
        type="scatter3d", mode="markers", color=datos_ejemplo$peso_predicho) %>%
  layout(scene = list(
    xaxis = list(title = "mujeres_perc_dist"),
    yaxis = list(title = "promedio_educ_dist"),
    zaxis = list(title = "Peso predicho")
  ))

plot_ly(datos_ejemplo, x = ~mujeres_perc_dist, y = ~promedio_educ_dist, z = ~peso_predicho, type = "scatter3d") %>%
  layout(scene = list(
    xaxis = list(title = "mujeres_perc_dist"),
    yaxis = list(title = "promedio_educ_dist"),
    zaxis = list(title = "Peso predicho")
  ))

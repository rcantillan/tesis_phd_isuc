### librerías
library(igraph)
library(ggraph)
library(dplyr)
library(haven)
library(readxl)
library(cowplot)
library(tidyverse)
library(questionr)
library(igraph)
library(here)
library(netmem)
library(occupar)
library(data.table)
library(infomapecology)
library(devtools)
library(tidygraph)
library(sjlabelled)
library(viridis)
library(reshape2)
library(quanteda.textmodels)
library(MASS)
library(netglm)
library(broom)
library(ivreg)
library(texreg)
library(margins)
#install.packages("pglm")
#devtools::install_github("timonelmer/netglm")
library(pglm)


### Datos

## 2009
hlaboral_2009 <- read_dta(here("paper1/data/EPS/2009/hlaboral.dta"))
ind_attr_2009 <- read_dta(here("paper1/data/EPS/2009/entrevistado.dta"))

# seleccionar variables
ind_2009 <- ind_attr_2009 %>% dplyr::select(folio_n20,a8,a9,a12n) #sexo,edad,nivel educativo
s_2009<-hlaboral_2009%>%dplyr::select(folio_n20, oficio, b12, b12t, b13, orden) #ingreso,horas semanales

ind_2009$a12n

# unir, filtrar, contar. 
laboral_2009 <- inner_join(ind_2009, s_2009, by = "folio_n20") %>%
  dplyr::select(folio_n20, sexo = a8, edad = a9, educ = a12n, ocup = oficio, 
                ingreso = b12, ingreso_tramo = b12t, horas = b13, orden) %>%
  filter(sexo %in% c(1, 2)) %>%
  mutate(
    educ = ifelse(educ %in% c(88, 99), NA, educ),
    ingreso = ifelse(ingreso %in% c(8, 9), NA, ingreso),
    horas = ifelse(horas %in% c(888, 999), NA, horas),
    superior = ifelse(educ %in% c(12,13), 1, 0)
  ) %>%
  group_by(ocup) %>%
  summarise(
    tamano = n(),
    hombres_perc = sum(sexo == 1) / sum(!is.na(sexo)) * 100,
    mujeres_perc = sum(sexo == 2) / sum(!is.na(sexo)) * 100,
    superior_perc= sum(superior == 1) / sum(!is.na(superior)) * 100,
    #promedio_educ = mean(educ, na.rm = TRUE),
    promedio_edad = mean(edad, na.rm = TRUE),
    promedio_horas = mean(horas, na.rm = TRUE),
    promedio_ingreso = median(ingreso, na.rm = TRUE)
  )


##### overlap ocupaciones
# renombrar columnas
#colnames(s_2009)<-c("from", "to", "orden")
#m_2009 <- create_am(s_2009[,1:2])

# edgelist to 
m1 <- netmem::edgelist_to_matrix(cbind(s_2009$folio_n20,s_2009$oficio), bipartite = T)
m1t <-t(m1)
overlap_2009 <- m1t%*%t(m1t)

# borrar x en nombres de filas
rownames(overlap_2009) <- gsub('X', '', rownames(overlap_2009))
#rownames(overlap_2009)
#colnames(overlap_2009)

# Eliminar las filas y columnas de nombre NA 
filas_a_eliminar <- 338
overlap_2009 <- overlap_2009[-filas_a_eliminar, ]
overlap_2009 <- overlap_2009[,-filas_a_eliminar]

# Convertir la matriz en un edgelist
edgelist_2009 <- melt(overlap_2009, varnames = c("ocup_i", "ocup_j"), value.name = "peso")
glimpse(edgelist_2009)

#### unir (datos para binomial negativo)
edges_2009 <- edgelist_2009 %>%
  left_join(laboral_2009, by = c("ocup_i" = "ocup")) %>%
  rename_at(vars(-ocup_i, -ocup_j, -peso), ~ paste0(., "_i")) 

edges_2009 <- edges_2009 %>% 
  left_join(laboral_2009, by = c("ocup_j" = "ocup")) %>%
  rename_at(vars(-ocup_i, -ocup_j, -peso, -tamano_i, -hombres_perc_i,
                 -mujeres_perc_i, -superior_perc_i, -promedio_edad_i,
                 -promedio_horas_i, -promedio_ingreso_i), ~ paste0(., "_j")) %>%
  mutate(ano=2009)

# Extracción de los nombres de filas y columnas 
occupation_names <- rownames(overlap_2009)

# Calcular los marginales (tamaños de ocupaciones)
calculate_occupation_margins <- function(overlap_2009) {
  row_margins <- rowSums(overlap_2009)
  col_margins <- colSums(overlap_2009)
  return(list(row_margins, col_margins))
}

# Calcula la matriz de vínculos ponderados no dirigidos
calculate_weighted_adjacency <- function(overlap_2009, occupation_names) {
  n <- nrow(overlap_2009)
  weighted_adjacency <- matrix(0, nrow = n, ncol = n)
  
  margins <- calculate_occupation_margins(overlap_2009)
  row_margins <- margins[[1]]
  col_margins <- margins[[2]]
  
  rownames(weighted_adjacency) <- occupation_names
  colnames(weighted_adjacency) <- occupation_names
  
  for (i in 1:n) {
    for (j in 1:n) {
      c_ijp <- overlap_2009[i, j]
      c_jip <- overlap_2009[j, i]
      o_i <- row_margins[i]
      o_j <- col_margins[j]
      
      # Calcula E_ijp utilizando la fórmula
      e_ijp <- (c_ijp + c_jip) / (o_i + o_j)
      
      weighted_adjacency[i, j] <- e_ijp
    }
  }
  
  return(weighted_adjacency)
}

# Aplicar función
weighted_matrix <- calculate_weighted_adjacency(overlap_2009, occupation_names)

# Revisar matriz. 
#print(weighted_matrix)

# Proporción de casillas que son 0 en la matriz de adyacencia
proportion_zero_edges <- sum(weighted_matrix == 0) / length(weighted_matrix) *100
print(paste("Proporción de casillas que son 0:", round(proportion_zero_edges, 2), "%"))

### edgelist
# to edgelist con igraph 
wedgelist_2009 <- graph.adjacency(weighted_matrix, weighted=TRUE)
wedgelist_2009 <- get.data.frame(wedgelist_2009)
wedgelist_2009 <- wedgelist_2009%>%filter(from!=to) # Remove self loops
wedgelist_2009$ano<-2009
#save(wedgelist_2009, file="wedgelist_2009.RData")
#wedgelist_2009<-wedgelist_2009%>%filter(from!=to) # Remove self loops
#cl<-wedgelist_2009 %>% select(from, to)
#wgraph_2009<-graph_from_adjacency_matrix(overlap_2009, weighted=TRUE, diag=F)
#wgraph_2009


#save(wedgelist_2009, file = "/home/rober/Desktop/wedgelist_2009.RData")
#save(laboral_2009, file = "/home/rober/Desktop/laboral_2009.RData")
#write.csv(wedgelist_2009, file = "/home/rober/Desktop/wedgelist_2009.csv", row.names = FALSE)
#write.csv(laboral_2009, file = "/home/rober/Desktop/laboral_2009.csv", row.names = FALSE)
#write.csv(edges_full_2009, file = "/home/rober/Desktop/edges_full_2009.csv", row.names = FALSE)
#
### crear variable de segregación compartida y salario compartido. 
laboral_2009$dissim_index = abs(laboral_2009$hombres_perc - laboral_2009$mujeres_perc)/2

### disimilitud 
# Cargar dataframe
# Función Índice de disimilitud normalizado
dissimilarity_index <- function(hombres_perc, mujeres_perc){
  
  absolut_diff <- abs(hombres_perc - mujeres_perc) 
  index <- absolut_diff/100
  
  return(index)
  
}

# Aplicar la función 
laboral_2009$dissim_index <- apply(laboral_2009[, c("hombres_perc","mujeres_perc")], 1, 
                         FUN = function(x) dissimilarity_index(x[1],x[2]))

# Índice de disimilitud total  
#dissimilarity <- sum(df$dissim_index)/nrow(df)
#print(paste("El índice de disimilitud total es:", round(dissimilarity, 3)))


glimpse(laboral_2009)
glimpse(wedgelist_2009)


# Calcular el promedio ponderado de los salarios medianos para cada ocupación y período
laboral_2009$ocup <- as.character(laboral_2009$ocup)
laboral_2009$ano<-2009
merged_data <- left_join(wedgelist_2009, laboral_2009, by = c("from" = "ocup", "ano"))


# Calcular la disimilitud compartida ponderada para cada ocupación y período
result <- merged_data %>%
  group_by(to, ano) %>%
  summarise(linked_dissimilarity    = sum(weight * dissim_index, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            hombres_perc_linked     = sum(weight * hombres_perc, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            mujeres_perc_linked     = sum(weight * mujeres_perc, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            superior_perc_linked    = sum(weight * superior_perc, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            promedio_edad_linked    = sum(weight * promedio_edad, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            promedio_horas_linked   = sum(weight * promedio_horas, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            promedio_ingreso_linked = sum(weight * promedio_ingreso, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            .groups = 'drop')

laboral_2009 <- left_join(laboral_2009, result, by=c("ocup" = "to"))

ggplot(laboral_2009, aes(x = dissim_index, y = linked_dissimilarity)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(x = "dissim_index", y = "linked_dissimilarity")

m1<-lm(dissim_index ~ linked_dissimilarity + 
         mujeres_perc+ 
         superior_perc +
         promedio_ingreso, data=laboral_2009)

summary(m1)

glimpse(laboral_2009)


## 2012 
hlaboral_2012 <- read_dta(here("paper1/data/EPS/2012/hlaboral.dta"))
ind_attr_2012 <- read_dta(here("paper1/data/EPS/2012/entrevistado.dta"))

ind_2012 <- ind_attr_2012 %>% dplyr::select(folio_n20,a8,a9,a12n) #sexo,edad,nivel educativo
s_2012<-hlaboral_2012%>%dplyr::select(folio_n20, b5_cod, b12m, b12t, b13, orden) #ingreso,horas semanales

# unir, filtrar, contar. 
laboral_2012 <- inner_join(ind_2012, s_2012, by = "folio_n20") %>%
  dplyr::select(folio_n20, sexo = a8, edad = a9, educ = a12n, ocup = b5_cod, 
                ingreso = b12m, ingreso_tramo = b12t, horas = b13, orden) %>%
  filter(sexo %in% c(1, 2)) %>%
  mutate(
    educ = ifelse(educ %in% c(88, 99), NA, educ),
    ingreso = ifelse(ingreso %in% c(8, 9), NA, ingreso),
    horas = ifelse(horas %in% c(888, 999), NA, horas),
    superior = ifelse(educ %in% c(12,13), 1, 0)
  ) %>%
  group_by(ocup) %>%
  summarise(
    tamano = n(),
    hombres_perc = sum(sexo == 1) / sum(!is.na(sexo)) * 100,
    mujeres_perc = sum(sexo == 2) / sum(!is.na(sexo)) * 100,
    superior_perc= sum(superior == 1) / sum(!is.na(superior)) * 100,
    #promedio_educ = mean(educ, na.rm = TRUE),
    promedio_edad = mean(edad, na.rm = TRUE),
    promedio_horas = mean(horas, na.rm = TRUE),
    promedio_ingreso = median(ingreso, na.rm = TRUE)
  )


##### overlap ocupaciones
# edgelist to 
m1 <- netmem::edgelist_to_matrix(cbind(s_2012$folio_n20,s_2012$b5_cod), bipartite = T)
m1t <- t(m1)
overlap_2012 <- m1t%*%t(m1t)

# borrar x en nombres de filas
rownames(overlap_2012) <- gsub('X', '', rownames(overlap_2012))

# Eliminar las filas y columnas de nombre NA 
filas_a_eliminar <- 338
overlap_2012 <- overlap_2012[-filas_a_eliminar, ]
overlap_2012 <- overlap_2012[,-filas_a_eliminar]

# Convertir la matriz en un edgelist
edgelist_2012 <- melt(overlap_2012, varnames = c("ocup_i", "ocup_j"), value.name = "peso")
glimpse(edgelist_2012)


edges_2012 <- edgelist_2012 %>%
  left_join(laboral_2012, by = c("ocup_i" = "ocup")) %>%
  rename_at(vars(-ocup_i, -ocup_j, -peso), ~ paste0(., "_i")) 

edges_2012 <- edges_2012 %>% 
  left_join(laboral_2012, by = c("ocup_j" = "ocup")) %>%
  rename_at(vars(-ocup_i, -ocup_j, -peso, -tamano_i, -hombres_perc_i,
                 -mujeres_perc_i, -superior_perc_i, -promedio_edad_i,
                 -promedio_horas_i, -promedio_ingreso_i), ~ paste0(., "_j")) %>%
  mutate(ano=2012)


# Extracción de los nombres de filas y columnas 
occupation_names <- rownames(overlap_2012)

# Calcular los marginales (tamaños de ocupaciones)
calculate_occupation_margins <- function(overlap_2012) {
  row_margins <- rowSums(overlap_2012)
  col_margins <- colSums(overlap_2012)
  return(list(row_margins, col_margins))
}

# Calcula la matriz de vínculos ponderados no dirigidos
calculate_weighted_adjacency <- function(overlap_2012, occupation_names) {
  n <- nrow(overlap_2012)
  weighted_adjacency <- matrix(0, nrow = n, ncol = n)
  
  margins <- calculate_occupation_margins(overlap_2012)
  row_margins <- margins[[1]]
  col_margins <- margins[[2]]
  
  rownames(weighted_adjacency) <- occupation_names
  colnames(weighted_adjacency) <- occupation_names
  
  for (i in 1:n) {
    for (j in 1:n) {
      c_ijp <- overlap_2012[i, j]
      c_jip <- overlap_2012[j, i]
      o_i <- row_margins[i]
      o_j <- col_margins[j]
      
      # Calcula E_ijp utilizando la fórmula
      e_ijp <- (c_ijp + c_jip) / (o_i + o_j)
      
      weighted_adjacency[i, j] <- e_ijp
    }
  }
  
  return(weighted_adjacency)
}

# Aplicar función
weighted_matrix <- calculate_weighted_adjacency(overlap_2012, occupation_names)

# Revisar matriz. 
#print(weighted_matrix)

# Proporción de casillas que son 0 en la matriz de adyacencia
proportion_zero_edges <- sum(weighted_matrix == 0) / length(weighted_matrix) *100
print(paste("Proporción de casillas que son 0:", round(proportion_zero_edges, 2), "%"))

### edgelist
# to edgelist con igraph 
wedgelist_2012 <- graph.adjacency(weighted_matrix, weighted=TRUE)
wedgelist_2012 <- get.data.frame(wedgelist_2012)
wedgelist_2012 <- wedgelist_2012%>%filter(from!=to) # Remove self loops
wedgelist_2012$ano<-2012

#save(wedgelist_2009, file="wedgelist_2009.RData")
#wedgelist_2009<-wedgelist_2009%>%filter(from!=to) # Remove self loops
#cl<-wedgelist_2009 %>% select(from, to)
#wgraph_2009<-graph_from_adjacency_matrix(overlap_2009, weighted=TRUE, diag=F)
#wgraph_2009

### crear variable de segregación compartida y salario compartido. 
### disimilitud 
# Cargar dataframe

# Función Índice de disimilitud normalizado
dissimilarity_index <- function(hombres_perc, mujeres_perc){
  
  absolut_diff <- abs(hombres_perc - mujeres_perc) 
  index <- absolut_diff/100
  
  return(index)
  
}

# Aplicar la función 
laboral_2012$dissim_index <- apply(laboral_2012[, c("hombres_perc","mujeres_perc")], 1, 
                                   FUN = function(x) dissimilarity_index(x[1],x[2]))

# Índice de disimilitud total  
#dissimilarity <- sum(df$dissim_index)/nrow(df)
#print(paste("El índice de disimilitud total es:", round(dissimilarity, 3)))

glimpse(laboral_2012)
glimpse(wedgelist_2012)


# Calcular el promedio ponderado de los salarios medianos para cada ocupación y período
laboral_2012$ocup <- as.character(laboral_2012$ocup)
laboral_2012$ano<-2012
merged_data <- left_join(wedgelist_2012, laboral_2012, by = c("from" = "ocup", "ano"))


# Calcular la disimilitud compartida ponderada para cada ocupación y período
result <- merged_data %>%
  group_by(to, ano) %>%
  summarise(linked_dissimilarity    = sum(weight * dissim_index, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            hombres_perc_linked     = sum(weight * hombres_perc, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            mujeres_perc_linked     = sum(weight * mujeres_perc, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            superior_perc_linked    = sum(weight * superior_perc, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            promedio_edad_linked    = sum(weight * promedio_edad, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            promedio_horas_linked   = sum(weight * promedio_horas, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            promedio_ingreso_linked = sum(weight * promedio_ingreso, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            .groups = 'drop')

# join
laboral_2012 <- left_join(laboral_2012, result, by=c("ocup" = "to"))

ggplot(laboral_2012, aes(x = dissim_index, y = linked_dissimilarity)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(x = "dissim_index", y = "linked_dissimilarity")

m1<-lm(dissim_index ~ linked_dissimilarity + 
         mujeres_perc + 
         superior_perc +
         promedio_ingreso, data=laboral_2012)

summary(m1)
glimpse(laboral_2012)


## 2015 

## 2015
hlaboral_2015 <- read_dta(here("paper1/data/EPS/2015/MODULOB_historia_laboral.dta"))
ind_attr_2015 <- read_dta(here("paper1/data/EPS/2015/MODULOA_entrevistado.dta"))

ind_2015 <- ind_attr_2015 %>% dplyr::select(folio_n20,a8,a9,a12n) #sexo,edad,nivel educativo
s_2015<-hlaboral_2015%>%dplyr::select(folio_n20, b5_cod, b12, b12t, b13, orden) #ingreso,horas semanales


# unir, filtrar, contar. 
laboral_2015 <- inner_join(ind_2015, s_2015, by = "folio_n20") %>%
  dplyr::select(folio_n20, sexo = a8, edad = a9, educ = a12n, ocup = b5_cod, 
                ingreso = b12, ingreso_tramo = b12t, horas = b13, orden) %>%
  filter(sexo %in% c(1, 2)) %>%
  mutate(
    educ = ifelse(educ %in% c(88, 99), NA, educ),
    ingreso = ifelse(ingreso %in% c(8, 9), NA, ingreso),
    horas = ifelse(horas %in% c(888, 999), NA, horas),
    superior = ifelse(educ %in% c(12,13), 1, 0)
  ) %>%
  group_by(ocup) %>%
  summarise(
    tamano = n(),
    hombres_perc = sum(sexo == 1) / sum(!is.na(sexo)) * 100,
    mujeres_perc = sum(sexo == 2) / sum(!is.na(sexo)) * 100,
    superior_perc= sum(superior == 1) / sum(!is.na(superior)) * 100,
    #promedio_educ = mean(educ, na.rm = TRUE),
    promedio_edad = mean(edad, na.rm = TRUE),
    promedio_horas = mean(horas, na.rm = TRUE),
    promedio_ingreso = median(ingreso, na.rm = TRUE)
  )

##### overlap ocupaciones
# edgelist to 
m1 <- netmem::edgelist_to_matrix(cbind(s_2015$folio_n20,s_2015$b5_cod), bipartite = T)
m1t <-t(m1)
overlap_2015 <- m1t%*%t(m1t)

# borrar x en nombres de filas
rownames(overlap_2015) <- gsub('X', '', rownames(overlap_2015))

# Eliminar las filas y columnas de nombre NA 
filas_a_eliminar <- 356
overlap_2015 <- overlap_2015[-filas_a_eliminar, ]
overlap_2015 <- overlap_2015[,-filas_a_eliminar]


# Convertir la matriz en un edgelist
edgelist_2015 <- melt(overlap_2015, varnames = c("ocup_i", "ocup_j"), value.name = "peso")
glimpse(edgelist_2015)



edges_2015 <- edgelist_2015 %>%
  left_join(laboral_2015, by = c("ocup_i" = "ocup")) %>%
  rename_at(vars(-ocup_i, -ocup_j, -peso), ~ paste0(., "_i")) 

edges_2015 <- edges_2015 %>% 
  left_join(laboral_2015, by = c("ocup_j" = "ocup")) %>%
  rename_at(vars(-ocup_i, -ocup_j, -peso, -tamano_i, -hombres_perc_i,
                 -mujeres_perc_i, -superior_perc_i, -promedio_edad_i,
                 -promedio_horas_i, -promedio_ingreso_i), ~ paste0(., "_j")) %>%
  mutate(ano=2015)



# Extracción de los nombres de filas y columnas 
occupation_names <- rownames(overlap_2015)

# Calcular los marginales (tamaños de ocupaciones)
calculate_occupation_margins <- function(overlap_2015) {
  row_margins <- rowSums(overlap_2015)
  col_margins <- colSums(overlap_2015)
  return(list(row_margins, col_margins))
}

# Calcula la matriz de vínculos ponderados no dirigidos
calculate_weighted_adjacency <- function(overlap_2015, occupation_names) {
  n <- nrow(overlap_2015)
  weighted_adjacency <- matrix(0, nrow = n, ncol = n)
  
  margins <- calculate_occupation_margins(overlap_2015)
  row_margins <- margins[[1]]
  col_margins <- margins[[2]]
  
  rownames(weighted_adjacency) <- occupation_names
  colnames(weighted_adjacency) <- occupation_names
  
  for (i in 1:n) {
    for (j in 1:n) {
      c_ijp <- overlap_2015[i, j]
      c_jip <- overlap_2015[j, i]
      o_i <- row_margins[i]
      o_j <- col_margins[j]
      
      # Calcula E_ijp utilizando la fórmula
      e_ijp <- (c_ijp + c_jip) / (o_i + o_j)
      
      weighted_adjacency[i, j] <- e_ijp
    }
  }
  
  return(weighted_adjacency)
}

# Aplicar función
weighted_matrix <- calculate_weighted_adjacency(overlap_2015, occupation_names)

# Revisar matriz. 
#print(weighted_matrix)

# Proporción de casillas que son 0 en la matriz de adyacencia
#proportion_zero_edges <- sum(weighted_matrix == 0) / length(weighted_matrix) *100
#print(paste("Proporción de casillas que son 0:", round(proportion_zero_edges, 2), "%"))

### edgelist
# to edgelist con igraph 
wedgelist_2015 <- graph.adjacency(weighted_matrix, weighted=TRUE)
wedgelist_2015 <- get.data.frame(wedgelist_2015)
wedgelist_2015 <- wedgelist_2015%>%filter(from!=to) # Remove self loops
wedgelist_2015$ano <- 2015
#save(wedgelist_2009, file="wedgelist_2009.RData")
#wedgelist_2009<-wedgelist_2009%>%filter(from!=to) # Remove self loops
#cl<-wedgelist_2009 %>% select(from, to)
#wgraph_2009<-graph_from_adjacency_matrix(overlap_2009, weighted=TRUE, diag=F)
#wgraph_2009


### crear variable de segregación compartida y salario compartido. 
### disimilitud 
# Cargar dataframe

# Función Índice de disimilitud normalizado
dissimilarity_index <- function(hombres_perc, mujeres_perc){
  
  absolut_diff <- abs(hombres_perc - mujeres_perc) 
  index <- absolut_diff/100
  
  return(index)
  
}

# Aplicar la función 
laboral_2015$dissim_index <- apply(laboral_2015[, c("hombres_perc","mujeres_perc")], 1, 
                                   FUN = function(x) dissimilarity_index(x[1],x[2]))

# Índice de disimilitud total  
#dissimilarity <- sum(df$dissim_index)/nrow(df)
#print(paste("El índice de disimilitud total es:", round(dissimilarity, 3)))

glimpse(laboral_2015)
glimpse(wedgelist_2015)


# Calcular el promedio ponderado de los salarios medianos para cada ocupación y período
laboral_2015$ocup <- as.character(laboral_2015$ocup)
laboral_2015$ano<-2015
merged_data <- left_join(wedgelist_2015, laboral_2015, by = c("from" = "ocup", "ano"))


# Calcular la disimilitud compartida ponderada para cada ocupación y período
result <- merged_data %>%
  group_by(to, ano) %>%
  summarise(linked_dissimilarity    = sum(weight * dissim_index, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            hombres_perc_linked     = sum(weight * hombres_perc, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            mujeres_perc_linked     = sum(weight * mujeres_perc, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            superior_perc_linked    = sum(weight * superior_perc, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            promedio_edad_linked    = sum(weight * promedio_edad, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            promedio_horas_linked   = sum(weight * promedio_horas, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            promedio_ingreso_linked = sum(weight * promedio_ingreso, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            .groups = 'drop')

# join
laboral_2015 <- left_join(laboral_2015, result, by=c("ocup" = "to"))

ggplot(laboral_2015, aes(x = dissim_index, y = linked_dissimilarity)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(x = "dissim_index", y = "linked_dissimilarity")

m1<-lm(dissim_index ~ linked_dissimilarity + 
         mujeres_perc + 
         superior_perc +
         promedio_ingreso, data=laboral_2015)

summary(m1)
glimpse(laboral_2015)


## 2020
hlaboral_2020 <- read_dta(here("paper1/data/EPS/2020/Bases_EPS2020_continuidad/01_Vivos_continuidad/MODULO_Historia Laboral_VIVOS_in.dta"))
ind_attr_2020 <- read_dta(here("paper1/data/EPS/2020/Bases_EPS2020_continuidad/01_Vivos_continuidad/MODULO_Entrevistado_VIVOS_in.dta"))

ind_2020 <- ind_attr_2020 %>% dplyr::select(folio_n20,a8,a9,a12_n) #sexo,edad,nivel educativo
s_2020<-hlaboral_2020%>%dplyr::select(folio_n20, CIUO_08_4dig, b12, b13, orden) #ingreso,horas semanales

# unir, filtrar, contar. 
laboral_2020 <- inner_join(ind_2020, s_2020, by = "folio_n20") %>%
  dplyr::select(folio_n20, sexo = a8, edad = a9, educ = a12_n, ocup = CIUO_08_4dig, 
                ingreso = b12, horas = b13, orden) %>%
  filter(sexo %in% c(1, 2)) %>%
  mutate(
    educ = ifelse(educ %in% c(88, 99), NA, educ),
    ingreso = ifelse(ingreso %in% c(8, 9), NA, ingreso),
    horas = ifelse(horas %in% c(888, 999), NA, horas),
    superior = ifelse(educ %in% c(12,13), 1, 0)
  ) %>%
  group_by(ocup) %>%
  summarise(
    tamano = n(),
    hombres_perc = sum(sexo == 1) / sum(!is.na(sexo)) * 100,
    mujeres_perc = sum(sexo == 2) / sum(!is.na(sexo)) * 100,
    superior_perc= sum(superior == 1) / sum(!is.na(superior)) * 100,
    #promedio_educ = mean(educ, na.rm = TRUE),
    promedio_edad = mean(edad, na.rm = TRUE),
    promedio_horas = mean(horas, na.rm = TRUE),
    promedio_ingreso = median(ingreso, na.rm = TRUE)
  )


##### overlap ocupaciones
# edgelist to 
m1 <- netmem::edgelist_to_matrix(cbind(s_2020$folio_n20,s_2020$CIUO_08_4dig), bipartite = T)
m1t <-t(m1)
overlap_2020 <- m1t%*%t(m1t)

# borrar x en nombres de filas
rownames(overlap_2020) <- gsub('X', '', rownames(overlap_2020))

# Eliminar las filas y columnas de nombre NA 
filas_a_eliminar <- 352
overlap_2020 <- overlap_2020[-filas_a_eliminar, ]
overlap_2020 <- overlap_2020[,-filas_a_eliminar]

# Convertir la matriz en un edgelist
edgelist_2020 <- melt(overlap_2020, varnames = c("ocup_i", "ocup_j"), value.name = "peso")
glimpse(edgelist_2020)


edges_2020 <- edgelist_2020 %>%
  left_join(laboral_2020, by = c("ocup_i" = "ocup")) %>%
  rename_at(vars(-ocup_i, -ocup_j, -peso), ~ paste0(., "_i")) 

edges_2020 <- edges_2020 %>% 
  left_join(laboral_2020, by = c("ocup_j" = "ocup")) %>%
  rename_at(vars(-ocup_i, -ocup_j, -peso, -tamano_i, -hombres_perc_i,
                 -mujeres_perc_i, -superior_perc_i, -promedio_edad_i,
                 -promedio_horas_i, -promedio_ingreso_i), ~ paste0(., "_j")) %>%
  mutate(ano=2020)


# Extracción de los nombres de filas y columnas 
occupation_names <- rownames(overlap_2020)

# Calcular los marginales (tamaños de ocupaciones)
calculate_occupation_margins <- function(overlap_2020) {
  row_margins <- rowSums(overlap_2020)
  col_margins <- colSums(overlap_2020)
  return(list(row_margins, col_margins))
}

# Calcula la matriz de vínculos ponderados no dirigidos
calculate_weighted_adjacency <- function(overlap_2020, occupation_names) {
  n <- nrow(overlap_2020)
  weighted_adjacency <- matrix(0, nrow = n, ncol = n)
  
  margins <- calculate_occupation_margins(overlap_2020)
  row_margins <- margins[[1]]
  col_margins <- margins[[2]]
  
  rownames(weighted_adjacency) <- occupation_names
  colnames(weighted_adjacency) <- occupation_names
  
  for (i in 1:n) {
    for (j in 1:n) {
      c_ijp <- overlap_2020[i, j]
      c_jip <- overlap_2020[j, i]
      o_i <- row_margins[i]
      o_j <- col_margins[j]
      
      # Calcula E_ijp utilizando la fórmula
      e_ijp <- (c_ijp + c_jip) / (o_i + o_j)
      
      weighted_adjacency[i, j] <- e_ijp
    }
  }
  
  return(weighted_adjacency)
}

# Aplicar función
weighted_matrix <- calculate_weighted_adjacency(overlap_2020, occupation_names)

# Revisar matriz. 
#print(weighted_matrix)

# Proporción de casillas que son 0 en la matriz de adyacencia
#proportion_zero_edges <- sum(weighted_matrix == 0) / length(weighted_matrix) *100
#print(paste("Proporción de casillas que son 0:", round(proportion_zero_edges, 2), "%"))


### edgelist
# to edgelist con igraph 
wedgelist_2020 <- graph.adjacency(weighted_matrix, weighted=TRUE)
wedgelist_2020 <- get.data.frame(wedgelist_2020)
wedgelist_2020 <- wedgelist_2020%>%filter(from!=to) # Remove self loops
wedgelist_2020$ano <- 2020

#save(wedgelist_2009, file="wedgelist_2009.RData")
#wedgelist_2009<-wedgelist_2009%>%filter(from!=to) # Remove self loops
#cl<-wedgelist_2009 %>% select(from, to)
#wgraph_2009<-graph_from_adjacency_matrix(overlap_2009, weighted=TRUE, diag=F)
#wgraph_2009

### crear variable de segregación compartida y salario compartido. 
### disimilitud 
# Cargar dataframe

# Función Índice de disimilitud normalizado
dissimilarity_index <- function(hombres_perc, mujeres_perc){
  
  absolut_diff <- abs(hombres_perc - mujeres_perc) 
  index <- absolut_diff/100
  
  return(index)
  
}

# Aplicar la función 
laboral_2020$dissim_index <- apply(laboral_2020[, c("hombres_perc","mujeres_perc")], 1, 
                                   FUN = function(x) dissimilarity_index(x[1],x[2]))

# Índice de disimilitud total  
#dissimilarity <- sum(df$dissim_index)/nrow(df)
#print(paste("El índice de disimilitud total es:", round(dissimilarity, 3)))

glimpse(laboral_2020)
glimpse(wedgelist_2020)


# Calcular el promedio ponderado de los salarios medianos para cada ocupación y período
laboral_2020$ocup <- as.character(laboral_2020$ocup)
laboral_2020$ano<-2020
merged_data <- left_join(wedgelist_2020, laboral_2020, by = c("from" = "ocup", "ano"))


# Calcular la disimilitud compartida ponderada para cada ocupación y período
result <- merged_data %>%
  group_by(to, ano) %>%
  summarise(linked_dissimilarity    = sum(weight * dissim_index, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            hombres_perc_linked     = sum(weight * hombres_perc, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            mujeres_perc_linked     = sum(weight * mujeres_perc, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            superior_perc_linked    = sum(weight * superior_perc, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            promedio_edad_linked    = sum(weight * promedio_edad, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            promedio_horas_linked   = sum(weight * promedio_horas, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            promedio_ingreso_linked = sum(weight * promedio_ingreso, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            .groups = 'drop')

# join
laboral_2020 <- left_join(laboral_2020, result, by=c("ocup" = "to"))

ggplot(laboral_2020, aes(x = dissim_index, y = linked_dissimilarity)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(x = "dissim_index", y = "linked_dissimilarity")

m1<-lm(dissim_index ~ linked_dissimilarity + 
         mujeres_perc + 
         superior_perc +
         promedio_ingreso, data=laboral_2020)

summary(m1)
glimpse(laboral_2020)


## unir data frames
laboral <- rbind(laboral_2009, laboral_2015, laboral_2020)

# Cargar paquetes
library(plm)
library(AER)

laboral_completos <- laboral[complete.cases(laboral), ]


# Subconjunto con %in%
laboral <- filter(laboral, !(ocup %in% c("110","11","0",NA)))

ggplot(laboral, aes(x = linked_dissimilarity, y = dissim_index)) + 
  geom_point(size = .1) + 
  geom_smooth(method = lm) + 
  labs(x = "Segregación compartida (linked dissimilarity)", y = "Segregación de género (índice de disimilitud)")

laboral_completos$ano.y<-NULL
laboral_completos <- laboral_completos %>% rename(ano=ano.x)
glimpse(laboral_completos)


# OLS
m1<-lm(dissim_index ~ 
         linked_dissimilarity + 
         superior_perc_linked +
         #hombres_perc_linked +
         superior_perc + 
         promedio_ingreso + 
         promedio_horas +
         factor(ano),
       data=laboral_completos)

summary(m1)



# modelo IV

m_iv <- ivreg(dissim_index ~ 
                promedio_horas + mujeres_perc + promedio_ingreso + factor(ano) # otras exógenas
              | 
                linked_dissimilarity  #endógena
              |
                superior_perc_linked,  # instrumento
              data = laboral_completos)

summary(m_iv)


iv3<-ivreg(dissim_index ~ 
             linked_dissimilarity + # endógena
             superior_perc_linked   # instrumento
              | 
             promedio_horas + mujeres_perc + promedio_ingreso + factor(ano), 
      data = laboral_completos)

summary(iv3)
coef(iv3)

summary(iv3, vcov = sandwich, df = laboral, diagnostics = TRUE)
summary(iv3, vcov = sandwich::sandwich, df = Inf)


coef(iv3, component = "stage1")
confint(iv3, component = "stage1")


## extract components from ivreg stage 1
c1 = coef(iv3, component = 'stage1')
v1 = vcov(iv3, component = 'stage1')
se1 = sqrt(diag(v1))
t1 = c1/se1
p1 = 2*pt(-abs(t1),df=iv3$df.residual1)


# ols sobre endógena (first stage)
ols <- lm(linked_dissimilarity ~ 
            superior_perc_linked +
            promedio_horas + 
            mujeres_perc + 
            promedio_ingreso + 
            factor(ano), 
          data = laboral_completos)

summary(ols)

# Crear la variable instrumentada (linked_dissimilarity_hat)
laboral_completos$linked_dissimilarity_hat <- ivreg(
  formula = linked_dissimilarity ~ 
    superior_perc_linked,
  data = laboral_completos
)$fitted.values


# Modelo con Efectos Fijos
fe <- plm(dissim_index ~ 
            linked_dissimilarity_hat + 
            superior_perc + promedio_ingreso + promedio_horas +
                factor(ano),
              index = c("ano"), 
              model = "within",
              data = laboral_completos)

summary(m2)
glimpse(laboral_completos)


# tabla 
texreg(list(m1, fe))


# Plot
par(mfrow = c(2, 3))
for (variable in c("hombres_perc_linked", "mujeres_perc_linked", "superior_perc_linked", "promedio_ingreso_linked", "promedio_horas_linked", "linked_dissimilarity")) {
  hist(laboral_completo[[variable]], main = variable, xlab = variable, col = "lightblue", border = "black")
}


laboral_completos$ano <- as.factor(laboral_completos$ano)
ggplot(laboral_completos, aes(x = linked_dissimilarity_hat, y = dissim_index)) +
  geom_point(size=.3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
       x = "Segregación de género compartida",
       y = "Segregación de género (índice de disimilitud)") +
  theme_minimal()


# Modelo de diferenciación estructural ------------------------------------------------
# unir todos los data frames
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

summary(edges_full$superior_perc_dist)


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


datos_ejemplo$

# Crear un dataframe con valores de ejemplo para las variables de interés
datos_ejemplo <- expand.grid(
  mujeres_perc_dist = seq(0, 100, length.out = 100),
  superior_perc_dist = seq(0, 100, length.out = 100),
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
  superior_perc_dist = seq(0, 100, length.out = 100),
  tamaño_combinado = median(edges_full$tamaño_combinado),
  promedio_edad_dist = median(edges_full$promedio_edad_dist, na.rm=T),
  promedio_horas_dist = median(edges_full$promedio_horas_dist, na.rm=T),
  promedio_ingreso_dist = median(edges_full$promedio_ingreso_dist, na.rm=T),
  ano = "2009"
)

# Predecir valores de peso utilizando el modelo
datos_ejemplo$peso_predicho <- predict(m1, newdata = datos_ejemplo, type = "response")

# Convertir el dataframe a formato largo para ggplot
#datos_largos <- melt(datos_ejemplo, id.vars = c("mujeres_perc_dist", "promedio_educ_dist"), variable.name = "Variable")


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








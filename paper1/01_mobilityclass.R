

# install InfoMap 
#package.list=c("emln","attempt", "cowplot", "igraph", "ggalluvial","magrittr","vegan", "dplyr","readr","ggplot2","stringr","tibble","tidyr","rlang","igraph","bipartite")
#loaded <-  package.list %in% .packages()
#package.list <-  package.list[!loaded]
#installed <-  package.list %in% .packages(TRUE)
#if (!all(installed)) install.packages(package.list[!installed], repos="http://cran.rstudio.com/")

# Install infomapecology 
#devtools::install_github('Ecological-Complexity-Lab/emln', force=T)
#devtools::install_github('Ecological-Complexity-Lab/infomap_ecology_package', force=T)

#library(infomapecology)
#setwd('/home/rober/Documents/doctorado-UC/tesis_phd_isuc/paper1')
#install_infomap()
#check_infomap() # Make sure file can be run correctly. Should return TRUE

install.packages("segregation")




# Librerías 
library(igraph)
library(ggraph)
library(dplyr)
library(haven)
library(readxl)
library(cowplot)
library(tidyverse)
library(questionr)
library(igraph)
library(netmem)
library(occupar)
library(data.table)
library(infomapecology)
library(infomapecology)
library(devtools)
library(tidygraph)
library(sjlabelled)
#install.packages("here")
library(here)
library(segregation)
library(patchwork)




# 2009 
hlaboral_2009 <- read_dta("/home/rober/Documents/doctorado-UC/tesis_phd_isuc/data/EPS/2009/hlaboral.dta")
s_2009<-hlaboral_2009%>%dplyr::select(folio_n20, oficio, orden)
hlaboral_2009$oficio


# add attributes
## 2009
hlaboral_2009 <- read_dta(here("/home/rober/Documents/doctorado-UC/tesis_phd_isuc/data/EPS/2009/hlaboral.dta"))
ind_attr_2009 <- read_dta(here("/home/rober/Documents/doctorado-UC/tesis_phd_isuc/data/EPS/2009/entrevistado.dta"))

# seleccionar variables
ind_2009 <- ind_attr_2009 %>% dplyr::select(folio_n20,a8,a9,a12n) #sexo,edad,nivel educativo
s_2009<-hlaboral_2009%>%dplyr::select(folio_n20, oficio, b12, b12t, b13, orden) #ingreso,horas semanales

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
    tamano          = n(),
    hombres_n       = sum(sexo == 1),
    mujeres_n       = sum(sexo == 2),
    superior_n      = sum(superior == 1),
    nosuperior_n    = sum(superior == 0),
    hombres_prop    = sum(sexo == 1)/ sum(!is.na(sexo)),
    mujeres_prop    = sum(sexo == 2)/ sum(!is.na(sexo)),
    superior_prop   = sum(superior == 1) / sum(!is.na(superior)),
    nosuperior_prop = sum(superior == 0) / sum(!is.na(superior))
  )

# 2009 -------------------------------------------------------
# renombrar columnas
colnames(s_2009)<-c("from", "to", "orden")
#m_2009 <- create_am(s_2009[,1:2])


# edgelist to 
m1 <- netmem::edgelist_to_matrix(cbind(s_2009$from,s_2009$to), bipartite = T)
m1t <-t(m1)
overlap_2009 <- m1t%*%t(m1t)

# borrar x en nombres de filas
rownames(overlap_2009) <- gsub('X', '', rownames(overlap_2009))
#rownames(overlap_2009)
#colnames(overlap_2009)

# Eliminar las filas y columnas de nombre NA 
filas_a_eliminar <- c(337,338)
overlap_2009 <- overlap_2009[-filas_a_eliminar, ] # filas 
overlap_2009 <- overlap_2009[,-filas_a_eliminar] # columnas


# data.table object 
#s_2009$orden<-NULL
#s_2009<-s_2009%>%drop_na("to")
#s_2009<-create_edgelist(s_2009)


# to edgelist con igraph 
wedgelist_2009  <- graph.adjacency(overlap_2009,weighted=TRUE)
wedgelist_2009 <- get.data.frame(wedgelist_2009)
#save(wedgelist_2009, file="wedgelist_2009.RData")
wedgelist_2009<-wedgelist_2009%>%filter(from!=to) # Remove self loops
cl<-wedgelist_2009 %>% select(from, to)
#wgraph_2009<-graph_from_adjacency_matrix(overlap_2009, weighted=TRUE, diag=F)
#wgraph_2009


# overlap_2009
# wedgelist_2009 (convert to `data.table` format)
#overlap_2009 <- as.data.table(overlap_2009)

# fit infomap --------------------------------------------------------
setwd('/home/rober/Documents/doctorado-UC/tesis')
#install_infomap()
#check_infomap()


network_object <- create_monolayer_network(wedgelist_2009, directed = T, bipartite = F)

res_dir <- run_infomap_monolayer(network_object, 
                                 infomap_executable='Infomap',
                                 flow_model = 'directed',
                                 silent=F,
                                 trials=500,
                                 signif = T, 
                                 nsim = 50,
                                 shuff_method = 'r00',
                                 #two_level=-2, 
                                 seed=200952)


res_rawdir <- run_infomap_monolayer(network_object, 
                                    infomap_executable='Infomap',
                                    flow_model = 'rawdir',
                                    silent=F,
                                    trials=500, 
                                    two_level=-1, 
                                    seed=200952,
                                    signif=F)


res_dir_modules <- res_dir$modules %>% drop_na()
res_rawdir_modules <- res_rawdir$modules %>% drop_na()
#create_infomap_linklist(network_object)

plot_modular_matrix(res_dir)
plots <- plot_signif(res_dir, plotit = T)
#plots

# create modularity  -----------------------------------------------

# LinkRank 
lr.modularity <- function(g,
                          partition, 
                          damping = .85, 
                          pr.algo = 'prpack',
                          weights = NULL) {
  
  ## g           = graph (igraph object)
  ## partition   = graph partition (numeric vector of memberships or "communities" object)
  ## damping     = damping factor (1 - teleportation prob.)
  ## pr.algo     = algorithm to calculate Perron vector,
  ##               possible options are "prpack", "arpack", and "power"
  ## weights     = If this is NULL and the graph has a weight edge attribute
  ##               then that is used. If weights is a numerical vector then
  ##               it used, even if the graph has a weights edge attribute.
  ##               If this is NA, then no edge weights are used (even if the
  ##               graph has a weight edge attribute)
  
  # check args
  if (!is.igraph(g)) 
    stop('graph is not an i.graph object')
  
  if (damping > 1 | damping < 0) 
    stop('damping factor has to be between zero and one!')
  
  # get algorithm name to calculate Perron vector
  pr.algo <- match.arg(pr.algo, c('prpack','arpack','power'))
  
  # no of nodes
  n <- vcount(g)
  # node sequence
  v.seq <- seq_len(n)
  
  # get membership vector
  if (class(partition) == 'communities') {
    
    pp <- membership(partition)
    
  } else {
    
    if (!is.numeric(partition))
      stop("'partition' has to be a 'communities' object or a numeric vector!")
    pp <- partition
    
  }
  
  # check dimensions
  if (length(pp) != n) 
    stop('Length of membership vector differs from number of nodes!')
  
  # get adjacency matrix & out-degree
  if (is.vector(weights) & length(weights) > 1) {
    
    # check args
    if (ecount(g) != length(weights))
      stop("'weights' differes in length from ecount!")
    if (!is.numeric(weights))
      stop("'weights' must be 'NA','NULL', or a numeric vector!")
    
    edge_attr(g, 'tmp') <- weights
    A <- get.adjacency(g, type = 'both', attr = 'tmp')
    
    out.deg <- igraph::strength(g, mode = 'out', weights = weights)
    
  } else if (is.null(weights)) {
    
    if ('weight' %in% edge_attr_names(g)) {
      
      A <- get.adjacency(g, type='both', attr='weight')
      out.deg <- igraph::strength(g, mode = 'out')
      
    }  else {
      
      A <- get.adjacency(g, type='both')
      out.deg <- igraph::degree(g, mode = 'out')
      
    }
    
  } else if (is.na(weights)) {
    
    A <- get.adjacency(g, type='both')
    out.deg <- igraph::degree(g, mode = 'out')
    
  } else {
    
    stop("'weights' option has to be 'NA','NULL', or a numeric vector!")
    
  }
  
  # dead-end nodes
  dangling <- out.deg == 0
  
  # row-normalize A (recycle vector)
  G.temp <- A / out.deg
  # equivalent to sweep(A, 1, out.deg, FUN='/')
  
  # set rows for dead-end nodes to zero
  if (sum(dangling) > 0) {
    G.temp[dangling,] <- 0
  }
  
  # add teleportation probabilities
  Tmat <- Matrix::Matrix(1/n * (damping * dangling + 1 - damping), 
                         nrow = n, ncol = n)
  G <- damping * G.temp + Tmat
  
  # get Perron vector (PageRank)
  p.vec <- page_rank(g, damping = damping, algo = pr.algo, weights = weights)$vector
  
  # LinkRank matrix
  Q <- G * p.vec -  tcrossprod(p.vec)
  # equivalent to sweep(G, 1, p.vec, '*') -  tcrossprod(p.vec)
  
  # get LinkRank Modularity by summing over within-community weights
  return(sum(Q[outer(pp, pp, '==')]))
  
}


#class(res_dir$edge_list)
#class(wgraph_2009)
memb<-res_dir$modules
memb<-memb%>%dplyr::select(node_name, module_level1)

# join con membresía 
wedgelist_2009<-as_tbl_graph(wedgelist_2009, directed = T)

wedgelist_2009 <- wedgelist_2009 %>%
  left_join(memb, by = c("name" = "node_name"))

#borrar nodos sin memb
wedgelist_2009<-wedgelist_2009%>%activate(nodes)%>%filter(!is.na(module_level1))

# link_rank
lr.modularity(wedgelist_2009,
              V(wedgelist_2009)$module_level1, 
              damping = .85, 
              pr.algo = 'prpack',
              weights = E(wedgelist_2009)$weight)



# degree dist
hist(igraph::degree(wedgelist_2009),
     xlab = "k",
     ylab = "Frequency",
     main = "Distribución de grado ocupacional 2009",
     col = "skyblue")


#igraph::degree(wedgelist_2009, mode = "all")
#igraph::degree(wedgelist_2009, mode = "in")
#igraph::degree(wedgelist_2009, mode = "out")
#wedgelist_2009


# CIUO match ----------------------------------------------

#x<-as.tibble(get_labels(s_2020$CIUO_08_4dig,  values = "p"))
#x<-str_split(x$value, "] ", simplify = TRUE)
#x<-as.data.frame(x)
#x$V1 <- gsub('[^[:alnum:] ]', '', x$V1)
#colnames(x)<-c("name", "name2")

ciuo<-read_excel("/home/rober/Downloads/corrtab08-88.xls",sheet=1,col_names= TRUE,col_types=NULL,na="",skip= 0)
ciuo<-ciuo%>%dplyr::select(name2="CIUO-88 Título SP", name= "Código CIUO-88")%>%distinct(.keep_all = TRUE)
wedgelist_2009 <- wedgelist_2009 %>% left_join(ciuo, by = c("name"))


# Calcula los grados de los nodos
wedgelist_2009<-wedgelist_2009 %>%activate(nodes)%>%
  mutate(degree=centrality_degree(mode="all"))


# Ordena el data frame por grado de manera descendente
wedgelist_2009 <- wedgelist_2009 %>% activate(nodes)%>% arrange(desc(degree))

# Toma las tres ocupaciones con los grados más altos
top_occupations <- wedgelist_2009 %>% 
  activate(nodes) %>%
  as_tibble()%>%
  head(n = 3) 

# Crea un vector de colores para la leyenda
legend_colors <- rainbow(length(top_occupations))
top_occupations$name_degree <- paste(top_occupations$name2, top_occupations$degree, sep = " / ")

# Etiqueta de leyenda con las tres ocupaciones principales
legend_labels <- top_occupations$name_degree

# Plotea la distribución de grados
hist(igraph::degree(wedgelist_2009,  mode = "all"),
     xlab = "degree",
     ylab = "Frequency",
     main = "",
     col = "skyblue")

# Agrega la leyenda
legend("topright", legend = legend_labels, fill = legend_colors)


# plot. --------------------------------------------------------
library(ggraph)
library(ggforce)
library(ggrepel)
library(igraph)
library(graphlayouts)
library(colourvalues)
#install.packages("oaqc")

# Graph
g <- as_tbl_graph(wedgelist_2009) %>% filter(igraph::degree(wedgelist_2009) > 0) 
#g <- simplify(g)
#g <- as.undirected(g)
#comunidades <- unique(V(g)$module_level1)  

# Dataframe original 
nodes <- wedgelist_2009 %>% activate(nodes) %>% select(name, module_level1, degree) %>% as_tibble()
df <- nodes


# Función para generar coordenadas
generate_coordinates <- function(community_df, scale_factor = 1) {
  
  nodes <- community_df$name 
  
  # Calcular ángulo aleatorio
  angle <- runif(1, 0, 2 * pi)
  
  # Aleatorizar ligeramente el radio y el desplazamiento
  radius <- runif(1, 0.5, 1) * runif(1, 0.8, 1.2)
  
  # Introducir ruido pequeño para variabilidad entre nodos de la misma comunidad
  noise_factor_x <- 0.2  # Ajusta este factor según la intensidad de variabilidad deseada en la coordenada x
  noise_factor_y <- 0.2  # Ajusta este factor según la intensidad de variabilidad deseada en la coordenada y
  
  noise_x <- noise_factor_x * runif(length(nodes), -1, 1)
  noise_y <- noise_factor_y * runif(length(nodes), -1, 1)
  
  # Añadir más desplazamiento entre nodos de la misma comunidad
  displacement_factor_x <- 20.5  # Ajusta este factor para controlar la distancia entre nodos de la misma comunidad en x
  displacement_factor_y <- 20.5  # Ajusta este factor para controlar la distancia entre nodos de la misma comunidad en y
  
  # Generar coordenadas en función del ángulo con desplazamiento y ruido
  x <- scale_factor * community_df$module_level1 * radius * cos(angle) + displacement_factor_x * noise_x
  y <- scale_factor * community_df$module_level1 * radius * sin(angle) + displacement_factor_y * noise_y
  
  return(data.frame(name = nodes, x = x, y = y)) 
}

# Dataframe de salida
positions <- data.frame(name = character(),
                        x = numeric(),
                        y = numeric())

# Generar coordenadas por comunidad con un factor de escala adicional
scale_factor <- 8  # Ajusta este factor según la separación deseada entre comunidades

for(com in unique(df$module_level1)){
  
  com_df <- filter(df, module_level1 == com)  
  coords <- generate_coordinates(com_df, scale_factor)
  
  positions <- bind_rows(positions, coords)  
}

# Graficar
# Leer grafo
#g <- wedgelist_2009 %>%
#  as_tbl_graph() 

# Unir coordenadas a nodos del grafo
laboral_2009 <- laboral_2009 %>% select(name, tamano, hombres_n, mujeres_n, superior_n,
                                        nosuperior_n, hombres_prop, mujeres_prop, superior_prop,  
                                        nosuperior_prop)

laboral_2009$name <- as.character(laboral_2009$name)

g <- g %>%   
  activate(nodes) %>%
  left_join(positions, by = "name") %>%
  left_join(laboral_2009, by = "name")

# estadisticas a nivel de modulo
a <- g %>% 
  activate(nodes) %>%
  group_by(module_level1) %>%
  as_tibble() %>%
  summarize(sum_tamano = sum(tamano),
            sum_mujeres_n = sum(mujeres_n),
            sum_hombres_n = sum(hombres_n),
            sum_superior_n = sum(superior_n),
            sum_nosuperior_n = sum(nosuperior_n)) %>%
  mutate(modulo_prop_mujeres    = sum_mujeres_n / sum_tamano,
         modulo_prop_hombres    = sum_hombres_n / sum_tamano,
         modulo_prop_superior   = sum_superior_n / sum_tamano,
         modulo_prop_nosuperior = sum_nosuperior_n / sum_tamano) %>%
  ungroup()

# unir
g <- g %>%   
  activate(nodes) %>%
  left_join(a, by = "module_level1") 


glimpse(g)

# crear medidas de segregación -------------------------------------------------
seg <- g %>% 
  activate(nodes) %>% 
  select(name, module_level1, tamano, hombres_n, mujeres_n) %>% 
  as_tibble()

#glimpse(seg)

# Datos en formato largo
seg <- seg %>% 
  pivot_longer(cols = c(hombres_n, mujeres_n), 
               names_to = "sexo", 
               values_to = "n")

# Segregación a nivel de ocupación 
mutual_total(seg, "sexo", "name", weight = "n")
name_ml <- mutual_local(seg, "sexo", "name", weight = "n", wide = TRUE) %>% as_tibble()
names(name_ml) <- c("name", "ls_name", "p_name")
hist(name_ml$ls_name)


# Segregación a nivel de cluster
mutual_total(seg, "sexo", "module_level1", weight = "n")  
module_ml <- mutual_local(seg, "sexo", "module_level1", weight = "n", wide = TRUE) %>% as_tibble()
names(module_ml) <- c("module_level1", "ls_module", "p_module")
hist(module_ml$ls_module)


# join
g <- g %>% 
  activate(nodes) %>%
  left_join(name_ml, by="name") %>%  # medidas locales
  left_join(module_ml, by = "module_level1") # medidas de componentes
glimpse(g)

# segcurve plot
p<-segcurve(seg, "sexo", "name", weight = "n") 
segcurve(seg, "sexo", "name", weight = "n", segment = "module_level1") +
  theme(legend.position = "none")
glimpse(p)


# Crear el gráfico con ambas curvas suavizadas, reducir el tamaño y agregar la línea en 45 grados
ggplot(p$data, aes(y = 1:nrow(p$data))) +
 # geom_abline(intercept = 0, linetype = "dashed", color = "black") +
  geom_abline(intercept = 23, slope = 214, linetype = "dashed") +
  geom_smooth(aes(x = cumul_prob_1, color = "Hombres"), se = FALSE, size = .3) +
  geom_smooth(aes(x = cumul_prob_2, color = "Mujeres"), se = FALSE, size = .3) +
  labs(x = "Cumulative Probability", y = "index") +
  scale_color_manual(name = "Catgeories", values = c("Hombres" = "blue", "Mujeres" = "red", "Equidad" = "black"))
  theme_minimal() +
  guides(shape = guide_legend("Category", override.aes = list(fill = c("red", "blue", "black"))))

  
?segplot
segplot(seg, "sexo", "name", weight = "n",order = "segregation")
segplot(seg, "sexo", "name", weight = "n", order = "segregation", secondary_plot = "segregation")



## correlations ---------------------------------------------------------------
library(corrr)

g %>% 
  activate(nodes) %>%
  as_tibble()%>%
  select(modulo_prop_mujeres, modulo_prop_superior, ls_name, ls_module) %>% 
  correlate()

# plot -------------------------------------------------------------------------

# round
g<-g %>% 
  activate(nodes) %>%
  mutate_at(vars(ls_name, ls_module, p_name, p_module,
                 hombres_prop, mujeres_prop, superior_prop, 
                 nosuperior_prop, 
                 modulo_prop_mujeres,   
                 modulo_prop_hombres,   
                 modulo_prop_superior,  
                 modulo_prop_nosuperior), ~ round(., digits = 2))%>%
  mutate(ls_module_label = paste("ls:", ls_module)) %>%
  mutate(p_module_label = paste("p:", p_module)) %>%
  mutate(p_module_mujeres = paste("pm:", modulo_prop_mujeres)) %>% 
  mutate(p_module_superior= paste("ps:", modulo_prop_superior)) %>%
  mutate(ls_pms_module = paste(ls_module_label,p_module_mujeres,p_module_superior))




g<-g %>% 
  activate(nodes) %>%
  mutate(module_level1 = as.factor(module_level1))

glimpse(g)

#glimpse(g)
# Plotear el grafo con las posiciones especificadas
# Obtener los nodos con mayor grado en cada comunidad
top_nodes <- g %>%
  group_by(module_level1) %>%
  top_n(3, degree) %>%
  ungroup() %>% as_tibble()


grupos_a_etiquetar <- c("ls: 0.27 pm: 0.77 ps: 0.15", 
                        "ls: 0.41 pm: 0.03 ps: 0.03",
                        "ls: 0.35 pm: 0.82 ps: 0.12", 
                        "ls: 0.36 pm: 0.04 ps: 0.04",
                        # "ls: 0.77 pm: 0.98 ps: 0.39", 
                        "ls: 0.13 pm: 0.17 ps: 0",
                        "ls: 0.18 pm: 0.71 ps: 0.12",
                        "ls: 0.19 pm: 0.13 ps: 0.14")

# Convertir ls_module_label a factor
g_filtered<-g%>%
  activate(nodes) %>%
  #dplyr::mutate(ls_pms_module = as.factor(ls_pms_module)) %>%
  filter(ls_pms_module %in% grupos_a_etiquetar) %>%
  as_tibble()

# Seleccionar la segunda fila (por ejemplo)
#fila <- g_filtered %>%
#  filter(module_level1 == 20 & name == "3320") %>%
#  mutate(name = case_when(name == 3320 ~ 3321))
#
#g_filtered <- rbind(g_filtered, fila)

# plot
ggraph(g, layout = "manual", x = g$x, y = g$y) +
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
  geom_node_point(aes(size = degree, fill = as.factor(module_level1)), colour = "#FFFFFF", shape = 21, stroke = 0.3) +
  scale_size_continuous(range = c(2, 10)) +  # Ajusta el rango según tus preferencias
  geom_mark_hull(
    aes(x, y, 
        group = module_level1, 
        fill  = module_level1,
        label = ls_pms_module),
    data = g_filtered, 
    concavity = 0.2,
    expand = unit(2.5, "mm"),
    alpha = 0.25
  ) + 
  theme_void() +
  theme(legend.position = "none")


glimpse(g)



# descriptivos ocupaciones 
df <- g %>% activate(nodes)%>%as_tibble()
df <- arrange(df, ls_name)

deciles <- quantile(df$ls_name, probs = seq(0,1,0.1))

inf_decil <- subset(df, ls_name <= deciles[2])

sup_decil <- subset(df, ls_name >= deciles[9])

#Comparación de promedios
summary(inf_decil$superior_prop)
summary(sup_decil$superior_prop)

Frecuencias absolutas y relativas de factores cualitativos
table(inf_decil$sector_economico)

prop.table(table(sup_decil$nivel_educativo))



ggplot(df, aes(mujeres_prop, superior_prop)) +
  geom_point() +
  geom_smooth(method = lm)






# Plot ------------------------------------------------------------------

# Crear el objeto tbl_graph a partir de los datos de los nodos y las aristas
tbl_graph <- as_tbl_graph(wedgelist_2009, directed = FALSE)

# Obtener el vector de clasificación a comunidades
communities <- tbl_graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  select(name, module_level1) %>%
  unique()

# Convertir el objeto tbl_graph a un objeto igraph
graph <- as.igraph(tbl_graph)

# Crear la lista de comunidades separadas
community_list <- split(communities$name, communities$module_level1)

# Dividir la lista de comunidades en dos grupos
num_communities <- length(community_list)
group1 <- community_list[1:10]
group2 <- community_list[11:22]

# Asignar colores a las comunidades de cada grupo
group1_colors <- rainbow(length(group1))
group2_colors <- rainbow(length(group2))

# Definir el inicio de cada rango de nombres para los títulos
start_index_group1 <- 1
start_index_group2 <- 11

# Función para crear el gráfico de una lista de comunidades
create_plot <- function(community_list, community_colors, start_index) {
  plot_list <- lapply(seq_along(community_list), function(i) {
    sub_nodes <- community_list[[i]]
    sub_graph <- induced_subgraph(graph, sub_nodes)
    
    sub_layout <- layout_with_fr(sub_graph)
    
    node_degrees <- degree(sub_graph)  # Calcular el grado de los nodos
    
    top_nodes <- names(sort(node_degrees, decreasing = TRUE)[1:3])  # Obtener los nombres de los 3 nodos más importantes
    
    ggraph(sub_graph, layout = sub_layout) +
      geom_edge_link(color = "grey", size = 0.5) +
      geom_node_point(aes(fill = factor(module_level1), size = node_degrees), shape = 21, color = "white") +
      geom_node_label(aes(label = ifelse(name %in% top_nodes, substr(name2, 1, 35), "")), 
                      fill = "white", color = "black", repel = TRUE,
                      size = 3, max.overlaps=50) +  # Ajustar tamaño de las etiquetas
      theme_void() +
      scale_fill_manual(values = community_colors[i]) +
      scale_size_continuous(range = c(2, 6)) +
      labs(title = paste("Mobility Class ", start_index + (i - 1))) +  # Ajustar título
      guides(fill = FALSE, size = FALSE)
  })
  
  return(plot_list)
}

# Crear los gráficos para cada grupo de comunidades
plot_list_group1 <- create_plot(community_list = group1, community_colors = group1_colors, start_index = start_index_group1)
plot_list_group2 <- create_plot(community_list = group2, community_colors = group2_colors, start_index = start_index_group2)

# Combinar los gráficos de cada grupo en una fila
combined_plot_group1 <- cowplot::plot_grid(plotlist = plot_list_group1, ncol = 3, align = "v", rel_heights = rep(1, length(plot_list_group1)))
combined_plot_group2 <- cowplot::plot_grid(plotlist = plot_list_group2, ncol = 3, align = "v", rel_heights = rep(1, length(plot_list_group2)))

# Mostrar los gráficos combinados
print(combined_plot_group1)
print(combined_plot_group2)

# flujo entre cluster----------------------------------------------------------

library(sna)
library(network)


attr<-wedgelist_2009%>%activate(nodes)%>%select(name, module_level1)%>%as_tibble()
cl <- network(cl)
bm<-blockmodel(cl, attr$module_level1)
#bm$cluster.method
#plot(bm$block.model)
cbm<-bm$block.model
cbm <- graph.adjacency(cbm,weighted=TRUE)
cbm <- get.data.frame(cbm)



cbm$to<-str_replace_all(cbm$to,"Block", "cm")
cbm$from<-str_replace_all(cbm$from,"Block", "cm")

# plot
ggplot(cbm,aes(x=factor(to, level=c("cm 1", "cm 2", "cm 3", "cm 4", "cm 5" ,"cm 6", "cm 7", "cm 8", "cm 9", "cm 10", 
                                      "cm 11","cm 12", "cm 13", "cm 14", "cm 15", "cm 16", "cm 17", "cm 18", "cm 19", 
                                      "cm 20","cm 21", "cm 22")), 
               y=factor(from, levels=c("cm 1", "cm 2", "cm 3", "cm 4", "cm 5" ,"cm 6", "cm 7", "cm 8", "cm 9", "cm 10", 
                                     "cm 11","cm 12", "cm 13", "cm 14", "cm 15", "cm 16", "cm 17", "cm 18", "cm 19", 
                                     "cm 20","cm 21", "cm 22")))) +
  scale_y_discrete(limits=rev) +
  scale_x_discrete(position = "top") +
  geom_tile(aes(fill=weight))+
  #scale_fill_gradient(low="white", high="black") +
  #scale_fill_gradientn(colours = (40)) +
  scale_fill_viridis_c(option = "magma",   direction = -1, limits = c(0.005, 1)) +
  labs(x ="", y = "") + 
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, size=13),
        axis.text.y = element_text(size=13))+
  guides(fill=guide_legend(title="Density", reverse = TRUE))
  


# 2020 ------------------------------------------------------------------
detach("package:xUCINET", unload = TRUE)
detach("package:sna", unload = TRUE)
detach("package:network", unload = TRUE)


# 2020
hlaboral_2020 <- read_dta("data/EPS/2020/Bases_EPS2020_continuidad/01_Vivos_continuidad/MODULO_Historia Laboral_VIVOS_in.dta")
s_2020<-hlaboral_2020%>%dplyr::select(folio_n20, CIUO_08_4dig, orden)

# renombrar columnas
colnames(s_2020)<-c("from", "to", "orden")

# edgelist to 
m1 <- netmem::edgelist_to_matrix(cbind(s_2020$from,s_2020$to), bipartite = T)
m1t <-t(m1)
overlap_2020 <- m1t%*%t(m1t)

# borrar x en nombres de filas
#rownames(overlap_2020) <- gsub('X', '', rownames(overlap_2020))
#rownames(overlap_2009)
#colnames(overlap_2009)

# Eliminar las filas y columnas de nombre NA 
filas_a_eliminar <- 332
overlap_2020 <- overlap_2020[-filas_a_eliminar, ]
overlap_2020 <- overlap_2020[,-filas_a_eliminar]

# to edgelist con igraph 
wedgelist_2020  <- graph.adjacency(overlap_2020,weighted=TRUE)
wedgelist_2020 <- get.data.frame(wedgelist_2020)
wedgelist_2020<-wedgelist_2020%>%filter(from!=to) # Remove self loops
cl<-wedgelist_2020 %>% select(from, to)%>%drop_na()

#wgraph_2020<-graph_from_adjacency_matrix(overlap_2020, weighted=TRUE, diag=F)
#wgraph_2020


# overlap_2020
# wedgelist_2020 (convert to `data.table` format)
#overlap_2020 <- as.data.table(overlap_2020)

# fit infomap --------------------------------------------------------
setwd('/home/rober/Documents/doctorado-UC/tesis')
#install_infomap()
#check_infomap()

wedgelist_2020 <- wedgelist_2020 %>% filter(from != 8888 & from != 7777, to != 8888 & to != 7777)
network_object <- create_monolayer_object(wedgelist_2020, directed = T, bipartite = F)
res_dir <- run_infomap_monolayer(network_object, 
                                 infomap_executable='Infomap',
                                 flow_model = 'directed',
                                 silent=F,
                                 trials=500, 
                                 #two_level=-2, 
                                 seed=200952)

res_rawdir <- run_infomap_monolayer(network_object, 
                                    infomap_executable='Infomap',
                                    flow_model = 'rawdir',
                                    silent=F,
                                    trials=500, 
                                    #two_level=-1, 
                                    seed=200952,
                                    signif=F)


#?run_infomap_monolayer

res_dir_modules <- res_dir$modules %>% drop_na()
res_rawdir_modules <- res_rawdir$modules %>% drop_na()
#create_infomap_linklist(network_object)



# create modularity  -----------------------------------------------
#class(res_dir$edge_list)
#class(wgraph_2009)
memb<-res_rawdir$modules
memb<-memb%>%dplyr::select(node_name, module_level1)

# join con membresía 
wedgelist_2020<-as_tbl_graph(wedgelist_2020, directed = T)

wedgelist_2020 <- wedgelist_2020 %>%
  left_join(memb, by = c("name" = "node_name"))

#borrar nodos sin memb
wedgelist_2020<-wedgelist_2020%>%activate(nodes)%>%filter(!is.na(module_level1))

# link_rank
lr.modularity(wedgelist_2020,
              V(wedgelist_2020)$module_level1, 
              damping = .85, 
              pr.algo = 'prpack',
              weights = E(wedgelist_2020)$weight)

# degree dist
hist(degree(wedgelist_2020),
     xlab = "k",
     ylab = "Frequency",
     main = "Distribución de grado ocupacional 2009",
     col = "skyblue")

#degree(wedgelist_2020, mode = "all")
#degree(wedgelist_2020, mode = "in")
#degree(wedgelist_2020, mode = "out")
#wedgelist_2009


# CIUO match ----------------------------------------------

#x<-as.tibble(get_labels(s_2020$CIUO_08_4dig,  values = "p"))
#x<-str_split(x$value, "] ", simplify = TRUE)
#x<-as.data.frame(x)
#x$V1 <- gsub('[^[:alnum:] ]', '', x$V1)
#colnames(x)<-c("name", "name2")

ciuo<-read_excel("/home/rober/Downloads/corrtab08-88.xls",sheet=1,col_names= TRUE,col_types=NULL,na="",skip= 0)
ciuo<-ciuo%>%dplyr::select(name2="CIUO-08 Título SP", name= "Código CIUO-08")%>%distinct(.keep_all = TRUE)
wedgelist_2020 <- wedgelist_2020 %>% left_join(ciuo, by = c("name")) 

# Calcula los grados de los nodos
wedgelist_2020<-wedgelist_2020 %>%activate(nodes)%>%
  mutate(degree=centrality_degree(mode="all"))

# Ordena el data frame por grado de manera descendente
wedgelist_2020 <- wedgelist_2020 %>% activate(nodes)%>% arrange(desc(degree))

# Toma las tres ocupaciones con los grados más altos
top_occupations <- wedgelist_2020 %>% 
  activate(nodes) %>%
  as_tibble()%>%
  head(n = 3) 

# Crea un vector de colores para la leyenda
legend_colors <- rainbow(length(top_occupations))

top_occupations$name_degree <- paste(top_occupations$name2, top_occupations$degree, sep = " / ")

# Etiqueta de leyenda con las tres ocupaciones principales
legend_labels <- top_occupations$name_degree

# Plotea la distribución de grados
hist(degree(wedgelist_2020),
     xlab = "degree",
     ylab = "Frequency",
     main = "",
     col = "skyblue")

# Agrega la leyenda
legend("topright", legend = legend_labels, fill = legend_colors)




# Plot ------------------------------------------------------------------

# Crear el objeto tbl_graph a partir de los datos de los nodos y las aristas
tbl_graph <- as_tbl_graph(wedgelist_2020, directed = FALSE)

# Obtener el vector de clasificación a comunidades
communities <- tbl_graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  select(name, module_level1) %>%
  unique()

# Convertir el objeto tbl_graph a un objeto igraph
graph <- as.igraph(tbl_graph)

# Crear la lista de comunidades separadas
community_list <- split(communities$name, communities$module_level1)

# Dividir la lista de comunidades en dos grupos
num_communities <- length(community_list)
group1 <- community_list[1:10]
group2 <- community_list[11:23]

# Asignar colores a las comunidades de cada grupo
group1_colors <- rainbow(length(group1))
group2_colors <- rainbow(length(group2))

# Definir el inicio de cada rango de nombres para los títulos
start_index_group1 <- 1
start_index_group2 <- 11

# Función para crear el gráfico de una lista de comunidades
create_plot <- function(community_list, community_colors, start_index) {
  plot_list <- lapply(seq_along(community_list), function(i) {
    sub_nodes <- community_list[[i]]
    sub_graph <- induced_subgraph(graph, sub_nodes)
    
    sub_layout <- layout_with_fr(sub_graph)
    
    node_degrees <- degree(sub_graph)  # Calcular el grado de los nodos
    
    top_nodes <- names(sort(node_degrees, decreasing = TRUE)[1:3])  # Obtener los nombres de los 3 nodos más importantes
    
    ggraph(sub_graph, layout = sub_layout) +
      geom_edge_link(color = "grey", size = 0.5) +
      geom_node_point(aes(fill = factor(module_level1), size = node_degrees), shape = 21, color = "white") +
      geom_node_label(aes(label = ifelse(name %in% top_nodes, substr(name2, 1, 35), "")), 
                      fill = "white", color = "black", repel = TRUE,
                      size = 3, max.overlaps=100) +  # Ajustar tamaño de las etiquetas
      theme_void() +
      scale_fill_manual(values = community_colors[i]) +
      scale_size_continuous(range = c(2, 6)) +
      labs(title = paste("Mobility Class ", start_index + (i - 1))) +  # Ajustar título
      guides(fill = FALSE, size = FALSE)
  })
  
  return(plot_list)
}

# Crear los gráficos para cada grupo de comunidades
plot_list_group1 <- create_plot(community_list = group1, community_colors = group1_colors, start_index = start_index_group1)
plot_list_group2 <- create_plot(community_list = group2, community_colors = group2_colors, start_index = start_index_group2)

# Combinar los gráficos de cada grupo en una fila
combined_plot_group1 <- cowplot::plot_grid(plotlist = plot_list_group1, ncol = 3, align = "v", rel_heights = rep(1, length(plot_list_group1)))
combined_plot_group2 <- cowplot::plot_grid(plotlist = plot_list_group2, ncol = 3, align = "v", rel_heights = rep(1, length(plot_list_group2)))

# Mostrar los gráficos combinados
print(combined_plot_group1)
print(combined_plot_group2)


# flujo entre cluster----------------------------------------------------------

library(sna)
library(network)

attr<-wedgelist_2020%>%activate(nodes)%>%select(name, module_level1)%>%as_tibble()%>%drop_na()
cl <- network(cl, loops = T)
bm<-blockmodel(cl, attr$module_level1)
#bm$cluster.method
#plot(bm$block.model)
cbm<-bm$block.model
cbm <- graph.adjacency(cbm,weighted=TRUE)
cbm <- get.data.frame(cbm)

cbm$to<-str_replace_all(cbm$to,"Block", "cm")
cbm$from<-str_replace_all(cbm$from,"Block", "cm")

# plot
ggplot(cbm,aes(x=factor(to, level=c("cm 1", "cm 2", "cm 3", "cm 4", "cm 5" ,"cm 6", "cm 7", "cm 8", "cm 9", "cm 10", 
                                    "cm 11","cm 12", "cm 13", "cm 14", "cm 15", "cm 16", "cm 17", "cm 18", "cm 19", "cm 20",
                                    "cm 21", "cm 22", "cm 23")), 
               y=factor(from, levels=c("cm 1", "cm 2", "cm 3", "cm 4", "cm 5" ,"cm 6", "cm 7", "cm 8", "cm 9", "cm 10", 
                                       "cm 11","cm 12", "cm 13", "cm 14", "cm 15", "cm 16", "cm 17", "cm 18", "cm 19", "cm 20",
                                       "cm 21", "cm 22", "cm 23")))) +
  scale_y_discrete(limits=rev) +
  scale_x_discrete(position = "top") +
  geom_tile(aes(fill=weight))+
  #scale_fill_gradient(low="white", high="black") +
  #scale_fill_gradientn(colours = (40)) +
  scale_fill_viridis_c(option = "magma",   direction = -1, limits = c(0.001, 1)) +
  labs(x ="", y = "") + 
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, size=13),
        axis.text.y = element_text(size=13))+
  guides(fill=guide_legend(title="Density", reverse = TRUE))









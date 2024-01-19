

# install InfoMap 
package.list=c("emln","attempt", "cowplot", "igraph", "ggalluvial","magrittr","vegan", "dplyr","readr","ggplot2","stringr","tibble","tidyr","rlang","igraph","bipartite")
loaded <-  package.list %in% .packages()
package.list <-  package.list[!loaded]
installed <-  package.list %in% .packages(TRUE)
if (!all(installed)) install.packages(package.list[!installed], repos="http://cran.rstudio.com/")

# Install infomapecology 
devtools::install_github('Ecological-Complexity-Lab/emln', force=T)
devtools::install_github('Ecological-Complexity-Lab/infomap_ecology_package', force=T)

library(infomapecology)
setwd('/home/rober/Documents/doctorado-UC/tesis_phd_isuc/paper1')
install_infomap()
check_infomap() # Make sure file can be run correctly. Should return TRUE


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
library(devtools)
library(tidygraph)
library(sjlabelled)



# 2009 
hlaboral_2009 <- read_dta("data/EPS/2009/hlaboral.dta")
s_2009<-hlaboral_2009%>%dplyr::select(folio_n20, oficio, orden)
hlaboral_2009$oficio

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
filas_a_eliminar <- 338
overlap_2009 <- overlap_2009[-filas_a_eliminar, ]
overlap_2009 <- overlap_2009[,-filas_a_eliminar]


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
install_infomap()
check_infomap()
library(infomapecology)
library(tidyverse)
library(igraph)

network_object <- create_monolayer_network(wedgelist_2009, directed = T, bipartite = F)
library(bipartite)

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
plots

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



igraph::degree(wedgelist_2009, mode = "all")
igraph::degree(wedgelist_2009, mode = "in")
igraph::degree(wedgelist_2009, mode = "out")
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
hist(igraph::degree(wedgelist_2009),
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
#install.packages("oaqc")

# Graph
wg <- as_tbl_graph(wedgelist_2009) %>% filter(igraph::degree(wedgelist_2009) > 0) 

wg <- igraph::simplify(wg)
wg
V(wg)$grp <- V(wg)$module_level1
V(wg)$grp <- as.factor(V(wg)$grp)


wg<- as.undirected(wg, mode = "each")
bb <- layout_as_backbone(wg, keep = 0.5)
bb <- layout_as_backbone(wg)

E(wg)$col <- F
E(wg)$col[bb$backbone] <- T

ggraph(wg,
       layout = "manual",
       x = bb$xy[, 1],
       y = bb$xy[, 2]) +
  geom_edge_link0(aes(col = col), width = 0.2) +
  geom_node_point(aes(fill = grp), shape = 21, size = 3) +
  geom_mark_hull(
    aes(x, y, group = grp, fill = grp),
    concavity = 4,
    expand = unit(2, "mm"),
    alpha = 0.25
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 1))) +
  theme_graph()+
  theme(legend.position = "none")


# Función layout por comunidad
# Load graph data
library(tidygraph) 
wg <- create_graph(...) # load/create graph in wg

# Simplify 
wg <- simplify(wg)




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









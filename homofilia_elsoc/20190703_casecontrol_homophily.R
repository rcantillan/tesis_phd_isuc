
## TO DO:
## Calcular modelo loglineales de 3 variables: educa ego, alter y padres de ego.
## Calcular random sorting models con interaccion entre homofilia educacional y
## movilidad educativa entre padres e hijo.

Sys.getlocale('LC_CTYPE')
Sys.setlocale(category = 'LC_ALL', 'es_ES.UTF-8')

#Packages
library(tidyverse)
library(GLMMadaptive)
library(survival)
library(R.utils)
require('survival')
library(questionr)
library(car)

#Clean up
rm(list=ls())

#Set up user
user <- "C:/Users/Matias Bargsted/Dropbox/"

#Open ELSOC - 2 waves
setwd(paste0(user, "Proyecto Fondecyt Redes Sociales/LT Homofilia"))
load("Homofilia educacional/Data/elsoc_network_recodes.RData")

# Recodificar religión
rel_str <- "1='Católico'; 2='Evangélico'; c(3,4,6)='Otra religión'; 5='Católico'; #5='Creyente, No adherente';
-999:-888=NA; 7:9='Ninguna / Ateo'"
el$religid_w02 <- car::recode(el$m38_w02, rel_str, as.factor=TRUE) 

##NOTA: weights are problematic: seleccionan misma persona con mucha mayor frecuencia.
d <- length(el$idencuesta)
table(table(sample(el$idencuesta, d, replace=T, prob = (1/el$ponderador02_w01))))
table(table(sample(el$idencuesta, d, replace=T)))
#Si se usa ponderador, usar de la ola 1, no 2 (este tiene NA's)

#Distancia Social Observada entre Ego-Alter
vars_to_keep <- c("idencuesta", "m0_edad_w02", "educaR_w02", "religid_w02", 
                  "hombre_w02", "tamred",  "alter_rela_01", "alter_rela_02", 
                  "alter_rela_03", "alter_rela_04", "alter_rela_05",
                  "esup_p", "esup_m", "both_uni",
                  "educ_papa", "educ_mama",
                  "mov_padre", "mov_madre") 
elsoc_egos <- el[names(el) %in% vars_to_keep]

educa_obs_dist    <- array(NA, dim=list(nrow(elsoc_egos), 5))
eduni_obs_dist    <- array(NA, dim=list(nrow(elsoc_egos), 5))
edtec_obs_dist    <- array(NA, dim=list(nrow(elsoc_egos), 5))
edmedcom_obs_dist <- array(NA, dim=list(nrow(elsoc_egos), 5))
edmedinc_obs_dist <- array(NA, dim=list(nrow(elsoc_egos), 5))
edbas_obs_dist    <- array(NA, dim=list(nrow(elsoc_egos), 5))
edad_obs_dist     <- array(NA, dim=list(nrow(elsoc_egos), 5))
relig_obs_dist    <- array(NA, dim=list(nrow(elsoc_egos), 5))
sexo_obs_dist     <- array(NA, dim=list(nrow(elsoc_egos), 5))

### Distancia ego-alter observados

#Distancia educativa
for(k in 1:5) educa_obs_dist[,k] <- abs(as.numeric(el$educaR_w02) -
                                          as.numeric(eval(parse(
                                            text=paste0("el$alter_ed_0", k)))))
educa_obs_dist <- data.frame(educa_obs_dist)
names(educa_obs_dist) <- paste0("educa_alter",1:5)

## Educa Universitaria 
for(k in 1:5) eduni_obs_dist[,k] <- ifelse(
  el$educaR_w02=="Ed. Universitaria" &
    eval(parse(text=paste0("el$alter_ed_0", k)))=="Ed. Universitaria", 1, 0)
eduni_obs_dist <- data.frame(eduni_obs_dist)
names(eduni_obs_dist) <- paste0("eduni_alter",1:5)

## Educa Tecnica
for(k in 1:5) edtec_obs_dist[,k] <- ifelse(
  el$educaR_w02=="Ed. Tecnica superior" &
    eval(parse(text=paste0("el$alter_ed_0", k)))=="Ed. Tecnica superior", 1, 0)
edtec_obs_dist <- data.frame(edtec_obs_dist)
names(edtec_obs_dist) <- paste0("edtec_alter",1:5)

## Educa Media Completa
for(k in 1:5) edmedcom_obs_dist[,k] <- ifelse(
  el$educaR_w02=="Ed. Media completa" &
    eval(parse(text=paste0("el$alter_ed_0", k)))=="Ed. Media completa", 1, 0)
edmedcom_obs_dist <- data.frame(edmedcom_obs_dist)
names(edmedcom_obs_dist) <- paste0("edmedcom_alter",1:5)

## Educa Media Incompleta
for(k in 1:5) edmedinc_obs_dist[,k] <- ifelse(
  el$educaR_w02=="Ed. Media incompleta" &
    eval(parse(text=paste0("el$alter_ed_0", k)))=="Ed. Media incompleta", 1, 0)
edmedinc_obs_dist <- data.frame(edmedinc_obs_dist)
names(edmedinc_obs_dist) <- paste0("edmedinc_alter",1:5)

## Educa Básica
for(k in 1:5) edbas_obs_dist[,k] <- ifelse(
  el$educaR_w02=="Ed. Basica o menos" &
    eval(parse(text=paste0("el$alter_ed_0", k)))=="Ed. Basica o menos", 1, 0)
edbas_obs_dist <- data.frame(edbas_obs_dist)
names(edbas_obs_dist) <- paste0("edbas_alter",1:5)

## Edad
for(k in 1:5) edad_obs_dist[,k] <- abs(el$m0_edad_w02 -
                                         eval(parse(
                                           text=paste0("el$r13_edad_0", k))))
edad_obs_dist <- data.frame(edad_obs_dist)
names(edad_obs_dist) <- paste0("edad_alter",1:5)

## Religion
for(k in 1:5) relig_obs_dist[,k] <- ifelse(
  el$religid_w02 == eval(parse(text=paste0("el$alter_relig_0", k))), 1, 0)
relig_obs_dist <- data.frame(relig_obs_dist)
names(relig_obs_dist) <- paste0("relig_alter",1:5)

## Sexo
for(k in 1:5) sexo_obs_dist[,k] <- ifelse(
  el$hombre_w02 == eval(parse(text=paste0("el$alter_hombre_0", k))), 1, 0)
sexo_obs_dist <- data.frame(sexo_obs_dist)
names(sexo_obs_dist) <- paste0("sexo_alter",1:5)

hom_obs <- data.frame(cbind(educa_obs_dist, elsoc_egos, eduni_obs_dist, 
                            edtec_obs_dist, edmedcom_obs_dist, 
                            edmedinc_obs_dist, edbas_obs_dist, 
                            edad_obs_dist, relig_obs_dist, sexo_obs_dist))
hom_obs$case <- 1


###------- SIMULATION --------------#########

#Preparativos simulacion de alteres
educa_sim_dist    <- array(NA, dim=list(nrow(elsoc_egos), 5))
eduni_sim_dist    <- array(NA, dim=list(nrow(elsoc_egos), 5))
edtec_sim_dist    <- array(NA, dim=list(nrow(elsoc_egos), 5))
edmedcom_sim_dist <- array(NA, dim=list(nrow(elsoc_egos), 5))
edmedinc_sim_dist <- array(NA, dim=list(nrow(elsoc_egos), 5))
edbas_sim_dist    <- array(NA, dim=list(nrow(elsoc_egos), 5))
edad_sim_dist     <- array(NA, dim=list(nrow(elsoc_egos), 5))
relig_sim_dist    <- array(NA, dim=list(nrow(elsoc_egos), 5))
sexo_sim_dist     <- array(NA, dim=list(nrow(elsoc_egos), 5))

#Weight CASEN
w_casen <- read.csv("Homofilia educacional/Results/weight_casen.csv", sep=";")

#Simulation parameters
set.seed(44322)
n <- 50
d <- length(el$idencuesta)

#Matrices with results
res_lin <- list(betas    = array(NA, dim=list(n, 5)),
                varcomp  = array(NA, dim=list(n, 1)),
                fitindex = array(NA, dim=list(n, 3)))
res_lin_nk <- res_lin

res_lin_int <- list(betas = array(NA, dim=list(n, 9)),
                varcomp   = array(NA, dim=list(n, 4)),
                fitindex  = array(NA, dim=list(n, 3)))
res_lin_int_nk <- res_lin_int


system.time(
  for(i in 1:n){
    
    #Post-stratificaction weight
    age_recode <- "18:30=18; 31:45=31; 46:60=46; else=61"
    el$age_grp <- car::recode(el$m0_edad_w02, age_recode)
    el$educ_grp <- el$educaR_w02
    el$educ_grp[el$educaR_w02=="Ed. Basica o menos"] <- "Ed. Media incompleta"
    el$educ_grp <- factor(el$educ_grp, exclude=NULL)
    w <- data.frame(xtabs(~educ_grp + m0_sexo_w02 + age_grp, data=el))
    #names(w) <- c("educaR_w02", "m0_sexo_w02", "Freq")
    w$prop <- w$Freq / sum(w$Freq)
    w$prop_target <- w_casen[,4]
    w$N_target <- w$prop_target*sum(w$Freq)
    w$w <- w$N_target/w$Freq
    w$m0_sexo_w02 <- as.numeric(levels(w$m0_sexo_w02))[w$m0_sexo_w02]
    el <- merge(el, w, by=c("educ_grp", "m0_sexo_w02", "age_grp"))
    
    #Sample random alters from observed egos
    #NOTA: Usar 'join' en lugar de 'merge', ya que este ultimo ordena las filas segun
    #'idencuesta', y por ende altera la seleccion aleatoria de alteres.
    nonties1 <- data.frame(idencuesta = sample(el$idencuesta, d, replace=T, el$w))
    nonties1 <- plyr::join(nonties1, elsoc_egos, by="idencuesta")
    

        #Number of repeated Alters
    #table(table(nonties1$idencuesta)
    
    #Observed and simulated distribution of education 
    #prop.table(table(nonties1$educaR_w02))
    #prop.table(wtd.table(el$educaR_w02, weights = el$w))
    
    #Observed and simulated distribution of age 
    #age_recode <- "18:30=18; 31:45=31; 46:60=46; else=61"
    #nonties1$age_grp <- recode(nonties1$m0_edad_w02, age_recode)
    #el$age_grp <- recode(el$m0_edad_w02, age_recode)
    #prop.table(table(nonties1$age_grp))
    #prop.table(table(el$age_grp))
    #prop.table(wtd.table(el$age_grp, weights = el$w))
    
    nonties2 <- data.frame(idencuesta=sample(el$idencuesta, d, replace=T, el$w))
    nonties2 <- plyr::join(nonties2, elsoc_egos, by="idencuesta")
    
    nonties3 <- data.frame(idencuesta=sample(el$idencuesta, d, replace=T, el$w))
    nonties3 <- plyr::join(nonties3, elsoc_egos, by="idencuesta")
    
    nonties4 <- data.frame(idencuesta=sample(el$idencuesta, d, replace=T, el$w))
    nonties4 <- plyr::join(nonties4, elsoc_egos, by="idencuesta")
    
    nonties5 <- data.frame(idencuesta=sample(el$idencuesta, d, replace=T, el$w))
    nonties5 <- plyr::join(nonties5, elsoc_egos, by="idencuesta")
    
    #Delete variables for next loop
    el$Freq <- NULL
    el$prop <- NULL
    el$prop_target <- NULL
    el$N_target <- NULL
    el$w <- NULL
    
    ### Distancia ego-alter simuladaos
    
    ## Distancia Educacion Lineal
    for(k in 1:5) educa_sim_dist[,k] <- abs(as.numeric(elsoc_egos$educaR_w02) -
                                              as.numeric(eval(parse(
                                                text=paste0("nonties",k, 
                                                            "$educaR_w02")))))
    educa_sim_dist <- data.frame(educa_sim_dist)
    names(educa_sim_dist) <- paste0("educa_alter",1:5)
    
    ##Educa Universitaria
    for(k in 1:5) eduni_sim_dist[,k] <- ifelse(
      elsoc_egos$educaR_w02=="Ed. Universitaria" &
        eval(parse(text=paste0("nonties",k,"$educaR_w02")))=="Ed. Universitaria", 1, 0)
    eduni_sim_dist <- data.frame(eduni_sim_dist)
    names(eduni_sim_dist) <- paste0("eduni_alter",1:5)
    
    ##Educa Tecnica
    for(k in 1:5) edtec_sim_dist[,k] <- ifelse(
      elsoc_egos$educaR_w02=="Ed. Tecnica superior" &
        eval(parse(text=paste0("nonties",k,"$educaR_w02")))=="Ed. Tecnica superior", 1, 0)
    edtec_sim_dist <- data.frame(edtec_sim_dist)
    names(edtec_sim_dist) <- paste0("edtec_alter",1:5)
    
    ##Educa Media Completa
    for(k in 1:5) edmedcom_sim_dist[,k] <- ifelse(
      elsoc_egos$educaR_w02=="Ed. Media completa" &
        eval(parse(text=paste0("nonties",k,"$educaR_w02")))=="Ed. Media completa", 1, 0)
    edmedcom_sim_dist <- data.frame(edmedcom_sim_dist)
    names(edmedcom_sim_dist) <- paste0("edmedcom_alter",1:5)
    
    ##Educa media incompleta
    for(k in 1:5) edmedinc_sim_dist[,k] <- ifelse(
      elsoc_egos$educaR_w02=="Ed. Media incompleta" &
        eval(parse(text=paste0("nonties",k,"$educaR_w02")))=="Ed. Media incompleta", 1, 0)
    edmedinc_sim_dist <- data.frame(edmedinc_sim_dist)
    names(edmedinc_sim_dist) <- paste0("edmedinc_alter",1:5)
    
    ##Educa Basica
    for(k in 1:5) edbas_sim_dist[,k] <- ifelse(
      elsoc_egos$educaR_w02=="Ed. Basica o menos" &
        eval(parse(text=paste0("nonties",k,"$educaR_w02")))=="Ed. Basica o menos", 1, 0)
    edbas_sim_dist <- data.frame(edbas_sim_dist)
    names(edbas_sim_dist) <- paste0("edbas_alter",1:5)
    
    ## Edad
    for(k in 1:5) edad_sim_dist[,k] <- abs(elsoc_egos$m0_edad_w02 - 
                                             as.numeric(eval(parse(
                                               text=paste0("nonties", k, "$m0_edad_w02")))))
    edad_sim_dist <- data.frame(edad_sim_dist)
    names(edad_sim_dist) <- paste0("edad_alter",1:5)
    
    ## Religion
    for(k in 1:5) relig_sim_dist[,k] <- ifelse(
      elsoc_egos$religid_w02 == eval(parse(text=paste0("nonties",k,"$religid_w02"))), 1, 0)
    relig_sim_dist <- data.frame(relig_sim_dist)
    names(relig_sim_dist) <- paste0("relig_alter",1:5)
    
    ## Sexo
    for(k in 1:5) sexo_sim_dist[,k] <- ifelse(
      elsoc_egos$hombre_w02 == eval(parse(text=paste0("nonties",k,"$hombre_w02"))), 1, 0)
    sexo_sim_dist <- data.frame(sexo_sim_dist)
    names(sexo_sim_dist) <- paste0("sexo_alter",1:5)
    
    hom_sim <- data.frame(cbind(elsoc_egos, educa_sim_dist, eduni_sim_dist, 
                                edtec_sim_dist, edmedcom_sim_dist, edmedinc_sim_dist,
                                edbas_sim_dist, edad_sim_dist, relig_sim_dist, 
                                sexo_sim_dist))
    hom_sim$case <- 0
    
    #Eliminar alteres simulados si info de alter no fue observada
    vwm <- names(hom_sim)[!names(hom_sim) %in% c("idencuesta","case")]
    for(l in 1:length(vwm)) hom_sim[vwm[l]][is.na(hom_obs[vwm[l]])]<- NA
    
    #Merge
    homo <- plyr::rbind.fill(hom_obs, hom_sim)
    #Reshape Data 
    homoL <- reshape(homo, varying = list(paste0("educa_alter", 1:5),
                                          paste0("eduni_alter", 1:5),
                                          paste0("edtec_alter", 1:5),
                                          paste0("edmedcom_alter", 1:5),
                                          paste0("edmedinc_alter", 1:5),
                                          paste0("edbas_alter", 1:5),
                                          paste0("edad_alter", 1:5),
                                          paste0("relig_alter", 1:5),
                                          paste0("sexo_alter", 1:5),
                                          paste0("alter_rela_0", 1:5)),
                     v.names=c("educa_alter","eduni_alter", "edtec_alter", 
                               "edmedcom_alter", "edmedinc_alter", "edbas_alter", 
                               "edad_alter", "relig_alter", "sexo_alter", 
                               "rela_alter"), direction="long")
    homoL <- homoL[order(homoL$idencuesta, homoL$case),]
    #Drop lots of NA's - though with or without N of regresion is equal.
    homoL <- subset(homoL, !is.na(sexo_alter))
    head(homoL, 20)
    
    #Case regression
    #res.clogit <- clogit(case ~ eduni_alter + edad_alterMC + relig_alter + 
    #                     sexo_alter + strata(idencuesta), homoL)
    #case_reg_hom[i,] <- res.clogit$coefficients
    
    #Mean Center Social Distances
    homoL$educa_alterMC <- homoL$educa_alter - mean(homoL$educa_alter, na.rm=T)
    homoL$eduni_alterMC <- homoL$eduni_alter - mean(homoL$eduni_alter, na.rm=T)
    homoL$edtec_alterMC <- homoL$edtec_alter - mean(homoL$edtec_alter, na.rm=T)
    homoL$edmedcom_alterMC <- homoL$edmedcom_alter - mean(homoL$edmedcom_alter, na.rm=T)
    homoL$edmedinc_alterMC <- homoL$edmedinc_alter - mean(homoL$edmedinc_alter, na.rm=T)
    homoL$edbas_alterMC <- homoL$edbas_alter - mean(homoL$edbas_alter, na.rm=T)
    homoL$edad_alterMC <- homoL$edad_alter - mean(homoL$edad_alter, na.rm=T)
    homoL$relig_alterMC <- homoL$relig_alter - mean(homoL$relig_alter, na.rm=T)
    homoL$sexo_alterMC <- homoL$sexo_alter - mean(homoL$sexo_alter, na.em=T)
    
    ######################################################################
    # ##Correlation between education of ego-alter by father's education
    # plot(1:5, sapply(1:5, function(x) {
    #   cor(homoL$educa_alter[homoL$educ_papa==x],
    #       homoL$case[homoL$educ_papa==x], use="complete.obs")
    # }), type="b")

    ##Correlation between education of ego-alter by educational mobility
    plot(-4:4, sapply(-4:4, function(x) {
      cor(homoL$educa_alter[homoL$mov_padre==x],
        homoL$case[homoL$mov_padre==x], use="complete.obs")
    }), type="b", ylab="Proxy Homofilia")
    
    ######################################################################
    # #Linear Educational Distance - All Ties
    # m_lin <- mixed_model(case ~ 1 + educa_alterMC + edad_alterMC +
    #                        relig_alterMC + sexo_alterMC,
    #                      random =~ 1 | idencuesta,
    #                      data=subset(homoL, !is.na(educa_alterMC)),
    #                      family = binomial, nAGQ=5)
    # res_lin[["betas"]][i,] <- fixef(m_lin)
    # res_lin[["varcomp"]][i,] <- as.vector((m_lin$D))
    # res_lin[["fitindex"]][i,] <- c(m_lin$logLik, AIC(m_lin), BIC(m_lin))
    # 
    # #Linear Educational Distance - Non kin Ties
    # m_lin_nk <- update(m_lin, data=subset(homoL, rela_alter!="Familiar"))
    # res_lin_nk[["betas"]][i,] <- fixef(m_lin_nk)
    # res_lin_nk[["varcomp"]][i,] <- as.vector((m_lin_nk$D))
    # res_lin_nk[["fitindex"]][i,] <- c(m_lin_nk$logLik, AIC(m_lin_nk),
    #                                   BIC(m_lin_nk))

    #Linear Educational Distance - All Ties
    #Pendietne: modelo base para non-kin only.
    
    m_lin_int0 <- mixed_model(case ~ 1 + 
                                educa_alterMC + 
                                edad_alterMC + 
                                relig_alterMC + 
                                sexo_alterMC,
                              random =~ educa_alterMC | idencuesta,
                              data=subset(homoL, !is.na(educa_alterMC)),
                              family = binomial, nAGQ=5)
    
    m_lin_int1 <- mixed_model(case ~ 1 + 
                                educa_alterMC*educ_papa + 
                                educa_alterMC*mov_padre + 
                                edad_alterMC +
                                relig_alterMC + 
                                sexo_alterMC,
                              random =~ educa_alterMC | idencuesta,
                              data=subset(homoL, !is.na(educa_alterMC)),
                              family = binomial, nAGQ=5)
    m_lin_int2 <- mixed_model(case ~ 1 + 
                               educa_alterMC*educ_papa + 
                               educa_alterMC*mov_padre + 
                               educa_alterMC*I(mov_padre^2) + 
                               edad_alterMC +
                               relig_alterMC + 
                               sexo_alterMC,
                             random =~ educa_alterMC | idencuesta,
                             data=subset(homoL, !is.na(educa_alterMC)),
                             family = binomial, nAGQ=5)
    m_lin_int3 <- mixed_model(case ~ 1 + 
                                educa_alterMC*educ_papa + 
                                educa_alterMC:mov_padre + 
                                educa_alterMC:I(mov_padre^2) + 
                                educa_alterMC:I(mov_padre^3) + 
                                edad_alterMC +
                                relig_alterMC + 
                                sexo_alterMC,
                             random =~ educa_alterMC | idencuesta,
                             data=subset(homoL, !is.na(educa_alterMC)),
                             family = binomial, nAGQ=5)
    m_lin_intBS <- mixed_model(case ~ 1 + 
                                educa_alterMC*educ_papa + 
                                educa_alterMC:bs(mov_padre, knots=c(-1,1), degree=1) + 
                                edad_alterMC +
                                relig_alterMC + 
                                sexo_alterMC,
                              random =~ educa_alterMC | idencuesta,
                              data=subset(homoL, !is.na(educa_alterMC)),
                              family = binomial, nAGQ=5)
    # res_lin_int[["betas"]][i,] <- fixef(m_lin_int)
    # res_lin_int[["varcomp"]][i,] <- as.vector(m_lin_int$D)
    # res_lin_int[["fitindex"]][i,] <- c(m_lin_int$logLik, AIC(m_lin_int), 
    #                                    BIC(m_lin_int))
    
    # #Linear Educational Distance - Non kin Ties
    # m_lin_int_nk <- update(m_lin_int, data=subset(homoL, rela_alter!="Familiar" &
    #                                                 !is.na(educa_alterMC)))
    # res_lin_int_nk[["betas"]][i,] <- fixef(m_lin_int_nk)
    # res_lin_int_nk[["varcomp"]][i,] <- as.vector(m_lin_int_nk$D)
    # res_lin_int_nk[["fitindex"]][i,] <- c(m_lin_int_nk$logLik, AIC(m_lin_int_nk), 
    #                                       BIC(m_lin_int_nk))
  }
)

#save Homophilty Estimactes
he = list(res_lin, res_lin_nk,
          res_lin_int, res_lin_int_nk)
          
save(he, file="Homofilia educacional/Results/homo_results.Rdata")

#Process Hirarchical Logit Coef's
model <- res_lin_int_nk[["betas"]]
b <- apply(model, 2, mean, na.rm=T)
se <- apply(model, 2, sd, na.rm=T)
t=b/se
coef_mat <- cbind(b,se,t, pval=(1-pnorm(abs(t)))*2)
print(coef_mat, digits=3)

coef_glmm_hom <- gather(data.frame(glmm_reg_hom), key="Tipo_Homofilia")
head(coef_glmm_hom)

ggplot(coef_glmm_hom, aes(value)) + 
  geom_density() + 
  geom_vline(aes(xintercept=mean(value)), color="blue") + 
  facet_wrap(~Tipo_Homofilia, nrow=3, scales = "free") +
    theme_bw()

#Process Clogit coefficients
coef_hom <- data.frame(wrap(case_reg_hom, map=list(NA, 2)))
head(coef_hom)
names(coef_hom) <- names(res.clogit$coefficients)
survey_est_uni <- prop.table(table(el$educaR_w02))[5]
coef_hom$modifier <- rep(survey_est_uni+modifier/100, each=n)
coef_hom <- gather(coef_hom, key="Var", value="Coefficient", 
                   eduni_alter:sexo_alter, -modifier)

hum.names <- as_labeller(c(`eduni_alter` = "Education", 
                           `edad_alterMC` = "Age",
                           `relig_alter` = "Religion",
                           `sexo_alter` = "Gender"))

ggplot(coef_hom, aes(exp(Coefficient), color=factor(round(modifier,2)))) + 
  facet_wrap(~Var, scales = "free", labeller=hum.names) + geom_density() + 
  theme_bw() + scale_color_discrete(name="Universitary \nEducation") + 
  labs(x="Odds Ratio")
ggsave("Homofilia educacional/Results/homo_odds.jpg",
       units="cm", width=20, height=14)  

#Number of repeated random Alters in last simulation
table(table(nonties1$idencuesta))
#Education  homophily
prop.table(table(homoL$eduni_alter, homoL$case),2)
#Distribucion marginal de alteres simulados de multiples variables
summary(nonties1$m0_edad_w02)
prop.table(table(nonties1$religid_w02))
prop.table(table(nonties1$hombre_w02))
prop.table(table(nonties1$educaR_w02))

#GLMM Logit
#Recode Age
age_str <- "18:24='18-24'; 25:34='25-34'; 35:44='35-44'; 45:54='45-54'; 55:64='55-64'; 65:74='65-74'; 75:hi='75 o +'; -999:-888=NA"
homoL$edad_w02R <- car::recode(homoL$m0_edad_w02, age_str, as.factor=T) 
homoL$edad_w02R <- relevel(homoL$edad_w02R, "45-54")  ##Largest category

m0 <- mixed_model(case ~ eduni_alter + edad_alter + relig_alter +
                    sexo_alter, random =~ 1 | idencuesta, 
                    data=homoL, family = binomial, nAGQ=20)
summary(m0)

#Data wrangling
##Exploring moderation effects
#Mean center
homoL$educa_alterMC <- homoL$educa_alter - mean(homoL$educa_alter, na.rm=T)
homoL$eduni_alterMC <- homoL$eduni_alter - mean(homoL$eduni_alter, na.rm=T)
homoL$edad_alterMC <- homoL$edad_alter - mean(homoL$edad_alter, na.rm=T)
homoL$relig_alterMC <- homoL$relig_alter - mean(homoL$relig_alter, na.rm=T)
homoL$sexo_alterMC <- homoL$sexo_alter - mean(homoL$sexo_alter, na.em=T)

m1 <- mixed_model(case ~ 0 + 
                    eduni_alterMC + 
                    edad_alterMC + 
                    relig_alterMC + 
                    sexo_alterMC, 
                  random =~ 0 + eduni_alterMC + edad_alterMC | idencuesta, 
                  data=subset(homoL, !is.na(eduni_alter)), 
                  family = binomial, 
                  nAGQ=11, iter_EM=60)
summary(m1)

##Compare marginal distribution of egos and random alters
nonties <- cbind(rbind(elsoc_egos,elsoc_egos,elsoc_egos,elsoc_egos,elsoc_egos),
                 rbind(nonties1, nonties2, nonties3, nonties4, nonties5))
##Compare fitted values from log-linear model with marginal distribution of controls.
##Check why educa_alterMC has more missing data than eduni_alterMC
## How to contrast distribution of alter with equal group size

## Check for homophily interactions
#Age and Education
summary(clogit(case ~ educa_alter * edad_alter + relig_alter + 
                 sexo_alter + strata(idencuesta), homoL))
#Age and religion
summary(clogit(case ~ educa_alter * relig_alter * edad_alter + 
                 sexo_alter + strata(idencuesta), homoL))

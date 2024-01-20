library(readstata13)
library(lme4)
library(gnm)
library(car)
library(dplyr)
library(polycor)
library(ggplot2)
library(texreg)
library(xtable)
library(tidyverse)

setwd("C:/Users/Fondecyt R. Sociales/Dropbox/Proyecto Fondecyt Redes Sociales/LT Homofilia/Homofilia educacional/Replicación Smith McPherson Smith-Lovin 2014")
rm(list=ls())

user<- "C:/Users/Fondecyt R. Sociales/Dropbox/"

#Fijar Ruta
setwd(paste0(user,"Proyecto Fondecyt Redes Sociales/LT Homofilia/Homofilia educacional"))

#Open ELSOC - 2 waves
el <- read.dta13("Data/ELSOC_wide_2016_2017_v1.00_Stata14.dta", convert.factors = F)



# Codigicación de Ego -----------------------------------------------------

#Sexo - ola 1 y 2
el$hombre_w01 <- as.numeric(el$m0_sexo_w01==1)
el$hombre_w02 <- as.numeric(el$m0_sexo_w02==1)
table(el$hombre_w01, el$hombre_w02)

#Edad - ola 1 y 2 continua
el$edad_w01 <- el$m0_edad_w01
el$edad_w02 <- el$m0_edad_w02
cor(el$edad_w01, el$edad_w02)

#Edad en tramos
age_str <- "0:17='17 o -'; 18:24='18-24'; 25:34='25-34'; 35:44='35-44'; 45:54='45-54'; 55:64='55-64'; 65:74='65-74'; 75:hi='75 o +'; -999:-888=NA"
el$edad_w01R <- car::recode(el$edad_w01, age_str, as.factor=T) 
el$edad_w02R <- car::recode(el$edad_w02, age_str, as.factor=T) 

#Educacion ego similar a categorias de Alter - usando ola 1
el$educaR_w01 <- NA
el$educaR_w01[el$m01_w01>=1 & el$m01_w01<=3] <- 1 
el$educaR_w01[el$m01_w01==4] <- 2 
el$educaR_w01[el$m01_w01==5] <- 3
el$educaR_w01[el$m01_w01>=6 & el$m01_w01<=7] <- 4
el$educaR_w01[el$m01_w01>=8] <- 5
el$educaR_w01 <- factor(el$educaR_w01, labels=c("Ed. Basica o menos", "Ed. Media incompleta",
                                                "Ed. Media completa", "Ed. Tecnica superior",
                                                "Ed. Universitaria"))

#Educacion ego similar a categorias de Alter - usando ola 2
el$educaR_w02 <- NA
el$educaR_w02[el$m01_w02>=1 & el$m01_w02<=3] <- 1 
el$educaR_w02[el$m01_w02==4] <- 2 
el$educaR_w02[el$m01_w02==5] <- 3
el$educaR_w02[el$m01_w02>=6 & el$m01_w02<=7] <- 4
el$educaR_w02[el$m01_w02>=8] <- 5
el$educaR_w02 <- factor(el$educaR_w02, labels=c("Ed. Basica o menos", "Ed. Media incompleta",
                                                "Ed. Media completa", "Ed. Tecnica superior",
                                                "Ed. Universitaria"))
table(el$educaR_w01, el$educaR_w02)
polychor(el$educaR_w01, el$educaR_w02)

#Movilidad Educacional **** OJO SE CODIFICAN TODOS LOS NA=0
#Padre con educacion superior
el$esup_p <- car::recode(el$m27, "9:10=1; else=0")
with(el,table(esup_p))

#Madre
el$esup_m <- car::Recode(el$m28, "9:10=1; else=0")
with(el,table(el$esup_m))

#Criterio de dominancia: al menos un padre con educacion superior
el$both_uni <- ifelse(el$esup_p==1 | el$esup_m==1, 1, 0)
table(el$both_uni) #244 casos algun padre con educacion superior
table(el$esup_p,el$esup_m)

#Educacion de los Padres en 5 categorias
el$educ_papa <- car::recode(el$m27, "1:3=1; 4=2; 5=3; 6:7=4; 8:10=5; else=NA")
el$educ_mama <- car::recode(el$m28, "1:3=1; 4=2; 5=3; 6:7=4; 8:10=5; else=NA")
table(el$educ_papa, exclude=NULL)
table(el$educ_mama, exclude=NULL)
tmpX <- with(el, data.frame(educaR_w01, educaR_w02, factor(both_uni), 
                            factor(esup_p), factor(esup_m), 
                            factor(educ_papa), factor(educ_mama)))
hetcor(tmpX, use="pairwise.complete.obs")

##Educational mobility
el$mov_padre <- as.numeric(el$educaR_w02) - el$educ_papa
el$mov_madre <- as.numeric(el$educaR_w02) - el$educ_mama
table(el$mov_padre)
table(el$mov_madre)


#Religion
# Recodificar religión
rel_str <- "1='Católico'; 2='Evangélico'; c(3,4,6)='Otra religión'; 5='Creyente, No adherente';
-999:-888=NA; 7:9='Ninguna / Ateo'"
el$religid_w01 <- car::recode(el$m38_w01, rel_str, as.factor=TRUE) 
el$religid_w02 <- car::recode(el$m38_w02, rel_str, as.factor=TRUE) 
with(el, table(religid_w01, religid_w02))
with(el, polychor(religid_w01=="Católico", religid_w02=="Católico"))
with(el, polychor(religid_w01=="Evangélico", religid_w02=="Evangélico"))
with(el, polychor(religid_w01=="Ninguna / Ateo", religid_w02=="Ninguna / Ateo"))

#practicancia religiosa
el$practica_w01 <- el$m39_w01
el$practica_w01[el$practica_w01<0] <- NA
el$practica_w02 <- el$m39_w02
el$practica_w02[el$practica_w02<0] <- NA
with(el, polychor(practica_w02,practica_w01))

#Proxy densidad de la red - 1=mayoria se cono  - 3=ninguno o casi ninguno se conoce
el$red_density <- el$r14
el$red_density[el$r14<0] <- NA

#Tamaño red
el$tamred <- el$r13_nredes

#Cantidad de amigos cercanos
#1=No tiene amigos ni amigas
#2=De 1 a 2
#3=De 3 a 5
#4=De 6 a 10
#5=Mas de 10
el$n_amigos <- el$r15
el$n_amigos[el$r15<0] <- NA
#Correlacion n_amigos y tamaño de red
with(el, prop.table(table(n_amigos, tamred),2))
with(el, polychor(n_amigos, tamred))


#-----CODIFICACIÓN DE VARIABLES DEL CONFIDENTES------#

#Sexo Confidentes
el$alter_hombre_01 <- as.numeric(el$r13_sexo_01==1)
el$alter_hombre_02 <- as.numeric(el$r13_sexo_02==1)
el$alter_hombre_03 <- as.numeric(el$r13_sexo_03==1)
el$alter_hombre_04 <- as.numeric(el$r13_sexo_04==1)
el$alter_hombre_05 <- as.numeric(el$r13_sexo_05==1)

#Tipo de relacion
tip_rela <- "1='Esposo/a'; 2:3='Familiar'; 4:5='Amigo/a'; -999:-888=NA"
el$alter_rela_01 <- car::recode(el$r13_relacion_01, tip_rela, as.factor=T)
el$alter_rela_02 <- car::recode(el$r13_relacion_02, tip_rela, as.factor=T)
el$alter_rela_03 <- car::recode(el$r13_relacion_03, tip_rela, as.factor=T)
el$alter_rela_04 <- car::recode(el$r13_relacion_04, tip_rela, as.factor=T)
el$alter_rela_05 <- car::recode(el$r13_relacion_05, tip_rela, as.factor=T)
with(el, table(alter_rela_01) + table(alter_rela_02) + table(alter_rela_03) + 
       table(alter_rela_04) + table(alter_rela_05))

#Mismo vecindario o barrio
tip_barrio <- "1='Mismo barrio'; 2='Otro barrio'; -999:-888=NA"
el$alter_barrio_01 <- car::recode(el$r13_barrio_01, tip_barrio, as.factor=T)
el$alter_barrio_02 <- car::recode(el$r13_barrio_02, tip_barrio, as.factor=T)
el$alter_barrio_03 <- car::recode(el$r13_barrio_03, tip_barrio, as.factor=T)
el$alter_barrio_04 <- car::recode(el$r13_barrio_04, tip_barrio, as.factor=T)
el$alter_barrio_05 <- car::recode(el$r13_barrio_05, tip_barrio, as.factor=T)
table(el$alter_barrio_01) + table(el$alter_barrio_02) + table(el$alter_barrio_03) +
  table(el$alter_barrio_04) + table(el$alter_barrio_05)

#Edad Continua - caseswith less than 10 are left missing

el$alter_agec_01 <- car::recode(el$r13_edad_01, "0:10=NA") 
el$alter_agec_02 <- car::recode(el$r13_edad_02, "0:10=NA")
el$alter_agec_03 <- car::recode(el$r13_edad_03, "0:10=NA")
el$alter_agec_04 <- car::recode(el$r13_edad_04, "0:10=NA")
el$alter_agec_05 <- car::recode(el$r13_edad_05, "0:10=NA")
table(el$alter_agec_01) + table(el$alter_agec_02) + table(el$alter_agec_03) +
  table(el$alter_agec_04) + table(el$alter_agec_05)

#Edad Confidentes
age_str <- "0:17='17 o -'; 18:24='18-24'; 25:34='25-34'; 35:44='35-44'; 45:54='45-54';
55:64='55-64'; 65:74='65-74'; 75:hi='75 o +'; -999:-888=NA"
el$alter_age_01 <- car::recode(el$r13_edad_01, age_str, as.factor=T) 
el$alter_age_02 <- car::recode(el$r13_edad_02, age_str, as.factor=T) 
el$alter_age_03 <- car::recode(el$r13_edad_03, age_str, as.factor=T) 
el$alter_age_04 <- car::recode(el$r13_edad_04, age_str, as.factor=T) 
el$alter_age_05 <- car::recode(el$r13_edad_05, age_str, as.factor=T) 
table(el$alter_age_01) + table(el$alter_age_02) + table(el$alter_age_03) +
  table(el$alter_age_04) + table(el$alter_age_05)

#Educacion Confidentes
el$alter_ed_01 <- el$r13_educ_01; el$alter_ed_01[el$r13_educ_01==-999] <- NA
el$alter_ed_02 <- el$r13_educ_02; el$alter_ed_02[el$r13_educ_02==-999] <- NA
el$alter_ed_03 <- el$r13_educ_03; el$alter_ed_03[el$r13_educ_03==-999] <- NA
el$alter_ed_04 <- el$r13_educ_04; el$alter_ed_04[el$r13_educ_04==-999] <- NA
el$alter_ed_05 <- el$r13_educ_05; el$alter_ed_05[el$r13_educ_05==-999] <- NA
el$alter_ed_01 <- factor(el$alter_ed_01, labels=levels(el$educaR_w02))
el$alter_ed_02 <- factor(el$alter_ed_02, labels=levels(el$educaR_w02))
el$alter_ed_03 <- factor(el$alter_ed_03, labels=levels(el$educaR_w02))
el$alter_ed_04 <- factor(el$alter_ed_04, labels=levels(el$educaR_w02))
el$alter_ed_05 <- factor(el$alter_ed_05, labels=levels(el$educaR_w02))
table(el$alter_ed_01, exclude = NULL) + table(el$alter_ed_02, exclude = NULL) + 
  table(el$alter_ed_03, exclude = NULL) + table(el$alter_ed_04, exclude = NULL) + 
  table(el$alter_ed_05,  exclude = NULL)

#Alters religion
rel_str <- "1='Católico'; 2='Evangélico'; 3:4='Ninguna / Ateo'; 5='Otra religión'; -999:-888=NA"
el$alter_relig_01 = car::recode(el$r13_relig_01, rel_str, as.factor=T)
el$alter_relig_02 = car::recode(el$r13_relig_02, rel_str, as.factor=T)
el$alter_relig_03 = car::recode(el$r13_relig_03, rel_str, as.factor=T)
el$alter_relig_04 = car::recode(el$r13_relig_04, rel_str, as.factor=T)
el$alter_relig_05 = car::recode(el$r13_relig_05, rel_str, as.factor=T)
table(el$alter_relig_01) + table(el$alter_relig_02) + 
  table(el$alter_relig_03) + table(el$alter_relig_04) + 
  table(el$alter_relig_05)

# Emparejar nombres de variables como base de dato de smith

el %>% dplyr::select(gender=hombre_w02, 
                     gender1=alter_hombre_01,gender2=alter_hombre_02,gender3=alter_hombre_03,gender4=alter_hombre_04,gender5=alter_hombre_05,
                     age=edad_w02R,
                     age1=alter_age_01,age2=alter_age_02,age3=alter_age_03,age4=alter_age_04,age5=alter_age_05,
                     educ=educaR_w02,
                     educ1=alter_ed_01,educ2=alter_ed_02,educ3=alter_ed_03,educ4=alter_ed_04,educ5=alter_ed_05,
                     rel=religid_w02,
                     rel1=alter_relig_01,rel2=alter_relig_02,rel3=alter_relig_03,rel4=alter_relig_04,rel5=alter_relig_05) ->ego

table(ego$age)

table(ego$age1)
table(el$m0)

#cargar base de datos de Vicente
load("ego_network_elsoc2017.Rdata")

# Replicación codigo de Smith ---------------------------------------------
library(biglm)
library(ergm)
library(doParallel)


#name of degree column on data frame:
var.name.degree="deg"

#names of key ego attribute columns in data frame:
var.name.characs=c("educ", "relig", "sex") 

var.name.characs.alter=list()
for (p in 1:length(var.name.characs)){
  var.name.characs.alter[[p]]=paste(var.name.characs[[p]],1:5,sep="")
}

#Let's take a look:
var.name.characs.alter


formula_ego_mod <- as.formula( ~ nodematch("race") + nodemix("educ_cat") + 
                                 nodematch("gender") + nodefactor("gender"))


formula_ego_elsoc<- as.formula(~nodematch("sex") + nodefactor("sex")+
                                 nodematch("educ")+nodematch("relig"))




#Note that it is possible to include a list of formulas, and the algorithm
#will run over each formula, estimating different models on the same constructed
#case control data frame. 

#We are now in a position to run the case control logistic regression to estimate the strength of homophily. 
#The function is egonet_case_control_model. The main inputs are the formula,
#the ego network data frame, the vectors defining the variables on the data frame and 
#a series of specifications determining how to run the case control model. 
#Here we list commonly used inputs: 

#formula=formula to use in logistic regression (we specified this above)

#ego_data=input ego network data (read in above)

#var.name.degree=name of variable in data that has the degree of the respondents (set above)

#var.name.characs= vector of names of attributes for ego (set above)

#var.name.characs.alter=list of column names for alter attributes, same order as var.name.charac (set above)

#case.control.type=type of null hypothesis used to construct the control portion
#of the data frame (the 0s in the regression), one of:
#"weighted.random.matching","one_one_matching","one_one_pair_matching", "case.case.matching",
#"weighted.random.matching" if you want to randomly pair respondents together based on probability weights
#"case.case.matching" if want to simply match all respondents with all other respondents to form controls
#"one_one_matching" if you want to match each case with only one other case for the control part of the dataset
#"one_one_pair_matching" if only want each respondent in one pair in control part-either
#as sender or reciever but not both
#"weighted.random.matching" is the default choice,randomly pairing the respondents 
#together to form the control (non-tie) portion of the data frame. 

#max.control.data.N=max size used when constructing control portion of data frame
#when set to NULL (as in this example), this will be set to the number of possible
#dyads, based on the number of respondents: N*(N-1)/2
#when the input data frame has large N and thus the number of dyads is quite large
#it can be useful to set this to limit the size of the control part of the data frame;
#for example set at 100000 (or something similarly large but less than the number of
#possible dyads). 

#max.alter=max number of alters the respondents were allowed to name (here set to 5)

#remove.isolates.control=should isolates be removed from control part of dataset? T/F
#(here we set this to T)

#weight.var.name=name of variable specifying vector of weights to create representative population  (or NULL)
#this variable will be used when doing bootstrap sampling, so that the bootstrap 
#samples are selected based on the input weights. If NULL, assumes equal 
#probability of selection for each respondent. Here we set these to NULL, so equal weight for every respondent

#weight.var.name.control=name of variable for weights of respondents (if not NULL)
#this variable will be used when pairing people together to form the control portion 
#of the data frame (the 0s), when using weighted.random.matching.
#Higher weights mean they will have higher chance of being
#selected in the pairing process. If NULL, default is to use weight.var.name. 
#Here we set this to NULL.

#num.iterations=number of different times the algorithm
#should reconstruct the control portion and reestimate the model 
#(in this example we this to 2, but in an actual analysis this would be much higher, say 100). 
#Note that we would only need 1 iteration if we set case.control.type
#to case.case.matching.

#bootstrap.sample=T/F, should the algorithm run the model multiple times 
#using different samples each time? (here we set to T)
#bootstrap sampling is necessary in order to estimate the SEs in the estimates. 

#num.bootstrap.samples= how many bootstrap samples to take? 
#only relevant if bootstrap.sample=T
#(here we do 10 bootstrap samples; in an actual analysis this would be much higher)

#useparallel=T/F should employ multiple CPUs when running analyses?
#(here we set to T)

#num.cores= how many cores to utilize? only relevant if useparallel=T
#(here we use parallel processing and 5 cores)

#maxit=number of maximum iterations to use in bigglm function (here set to 20)

#nodemix.reference=vector specifying the reference category if using nodemix terms
#(here we set "C.Some College.C.Some College" to the reference for the educational node mixing terms

#adjust.intercept=T/F should attempt to adjust intercept to map onto known, actual size of full population?
#(here set to F; if T would also need to set true.pop.size)

egonet_case_control_output=egonet_case_control_model(formula=formula_ego_elsoc,
                                                     ego_data=ego_network_elsoc2017,
                                                     var.name.degree=var.name.degree, 
                                                     var.name.characs=var.name.characs
                                                     ,var.name.characs.alter=var.name.characs.alter, 
                                                     case.control.type="weighted.random.matching", max.alter=5,
                                                     max.control.data.N=NULL,
                                                     remove.isolates.control=T, 
                                                     weight.var.name=NULL, weight.var.name.control=NULL, 
                                                     num.iterations=2,
                                                     bootstrap.sample=T, num.bootstrap.samples=10,
                                                     useparallel=T, num.cores=5,
                                                     nodemix.reference=c("C.Some College.C.Some College"),
                                                     maxit=20,
                                                     adjust.intercept=F)

names(egonet_case_control_output)

#The output is a list with two parts. The first contains the coefficients 
#for each iteration and/or bootstrap sample. A researcher could take those coefficients
#and summarize over the samples, calculate standard errors, and so on. The second part
#contains fit statistics, like AIC and BIC. Note that these fit statistics are 
#calculated based on the number of dyads (not the number of cases) in the data frame. 

#Let's grab the coefficients: 
coefs=egonet_case_control_output$coefs

head(coefs)

#We can see that we have estimated logistic regression coefficients predicting 
#a tie as a function of matching on race and gender, and nodemix terms for education.
#We also have a nodefactor term for gender. 

#We can take this data and summarize over all the samples (or iterations).
#Here we take the mean over the iterations for each sample.

coefs_sample=aggregate(.~sample, data=coefs, mean)

#Let's take a look at the results: 
coefs_sample

#We see there is a 1 row per sample now. 

#Now, we will take out the columns for sample and iteration
coefs_sample=coefs_sample[,-which(colnames(coefs_sample) %in% c("sample","iteration"))]

#At this point we can calculate means, SEs, and so on to summarize 
#the coefficients across the different samples:
apply(coefs_sample, 2, mean)
apply(coefs_sample, 2, sd)
apply(coefs_sample, 2, quantile, c(.025, .975))

#The results suggest, for example, that two actors that match on race
#are more likely to have a tie than two that do not, as there is a positive, significant
#coefficient for racial matching. 







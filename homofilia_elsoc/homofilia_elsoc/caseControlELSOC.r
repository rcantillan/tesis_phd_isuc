## Jeffrey Smith's code

## ## data preparation
## load("~/Dropbox/fondecyt2017/borradores/homofilia/case_control_logistic/example_ego_network_data/egonet_data.Rdata") #example data
summary(ego_network_data)
##
head(ego_network_data)
## nrow(ego_network_data) #300 sampled egos

## Jeffrey Smith's code

## load("~/Dropbox/ELSOC_2016_2017_2018_Merge_V1.00_R/ELSOC_Wide_2016_2018_v1.00_R.RData")
## names(elsoc_wide_2016_2018) #sin etiquetas

## tipo de atricion
## 1. 2016-2018 # en olas 2016, 2017 y 2018
## 2. 2016 y 2017 # sólo en olas 2016 y 2017
## 3. 2016 y 2018 # sólo en olas 2016 y 2018
## 4. Solo 2016
## 5. Solo 2018 (M. Original)
## 6. Solo 2018 (M. Refresco)

## ## solamente ola 2 (2017)
## elsoc2017<-subset(elsoc_wide_2016_2018, elsoc_wide_2016_2018$tipo_atricion==1 | elsoc_wide_2016_2018$tipo_atricion==2)
## save(elsoc2017, file='Elsoc2017.RData')
## names(elsoc2017)
load('Elsoc2017.RData')
elsoc2017$barrio.ego_02 <-rep('same', length(elsoc2017[,1]))
ego_network_elsoc2017<-elsoc2017[,c(814, 530,539,548,557,566, 817, 531,540,549,558,567, 1054, 534,543,552,561,570,  820, 535,544,553,562,571, 918, 536,545,554,563,572, 144, 537,546,555,564,573, 529)]
save(ego_network_data_elsoc, file='ego_network_elsoc2017.RData')

load('ego_network_elsoc2017.RData')
names(ego_network_elsoc2017)

## sexo egos
table(ego_network_elsoc2017$m0_sexo_w02)
ego_network_elsoc2017$m0_sexo_w02<-ifelse(elsoc2017$m0_sexo_w02==1 & !is.na(elsoc2017$m0_sexo_w02), 'male', 'female')

## sexo alter
for (j in 1:5){
    eval(parse(text=paste('ego_network_elsoc2017$r13_sexo_0',j,'_w02 <- ifelse(elsoc2017$r13_sexo_0',j,'_w02==1  & !is.na(elsoc2017$r13_sexo_0', j, '_w02), \'male\', \'female\')', sep='')))
    }

table(ego_network_elsoc2017$r13_sexo_01_w02)

## educación ego m01_w02 823
## 1, 2, 3|4|5|6,7|8,9,10

table(ego_network_elsoc2017$m01_w02) #educ

ego_network_elsoc2017$m01_w02<-ifelse(!is.na(elsoc2017$m01_w02) & (elsoc2017$m01_w02==1 | elsoc2017$m01_w02==2 | elsoc2017$m01_w02==3), 'lt.secondary', ifelse(!is.na(elsoc2017$m01_w02) & ego_network_elsoc2017$m01_w02==4, 'some.secondary', ifelse(!is.na(elsoc2017$m01_w02) & elsoc2017$m01_w02==5, 'secondary',
ifelse(!is.na(elsoc2017$m01_w02) & (elsoc2017$m01_w02==6 | elsoc2017$m01_w02==7), 'technical.ed',  ifelse(!is.na(elsoc2017$m01_w02) & (elsoc2017$m01_w02==8 | elsoc2017$m01_w02==9 | elsoc2017$m01_w02==10), 'college.ed', elsoc2017$m01_w02)))))


## educ alter
table(ego_network_elsoc2017$r13_educ_03_w02)

for (j in 1:5){
    eval(parse(text=(paste('ego_network_elsoc2017$r13_educ_0',j, '_w02 <-ifelse(!is.na(elsoc2017$r13_educ_0',j, '_w02) & elsoc2017$r13_educ_0',j,'_w02==1, \'lt.secondary\', ifelse(!is.na(elsoc2017$r13_educ_0',j,'_w02) & elsoc2017$r13_educ_0',j,'_w02==2, \'some.secondary\', ifelse(!is.na(elsoc2017$r13_educ_0',j,'_w02) & elsoc2017$r13_educ_0',j,'_w02==3, \'secondary\', ifelse(!is.na(elsoc2017$r13_educ_0',j,'_w02) & elsoc2017$r13_educ_0',j,'_w02==4, \'technical.ed\', ifelse(!is.na(elsoc2017$r13_educ_0',j,'_w02) & elsoc2017$r13_educ_0',j,'_w02==5, \'college.ed\', elsoc2017$r13_educ_0',j,'_w02)))))', sep=''))))
}
                                                                                                        
## religión
## relig ego: m38_w02 918
## 1. cat| 2. evan, 3. protes|9.ninguna|5 no adherente|7. ateo, 8. agnóstico|4.judío 6.otra
table(ego_network_elsoc2017$m38_w02) #relig
ego_network_elsoc2017$m38_w02<- ifelse(!is.na(elsoc2017$m38_w02) & elsoc2017$m38_w02==1, 'catholic', ifelse(!is.na(elsoc2017$m38_w02) & (elsoc2017$m38_w02==2 | ego_network_elsoc2017$m38_w02==3), 'evangelical', ifelse(!is.na(elsoc2017$m38_w02) & (elsoc2017$m38_w02==5 | elsoc2017$m38_w02==9), 'none', ifelse(!is.na(elsoc2017$m38_w02) & (elsoc2017$m38_w02==7 | elsoc2017$m38_w02==8), 'atheist', ifelse(!is.na(elsoc2017$m38_w02) & (elsoc2017$m38_w02==4 | elsoc2017$m38_w02==6), 'other', elsoc2017$m38_w02)))))

## Alter: relig, católico, evangélico, ninguno, ateo/agn, otro
table(ego_network_elsoc2017$r13_relig_03_w02)

for (i in 1:5){
eval(parse(text=(paste('ego_network_elsoc2017$r13_relig_0',i,'_w02[elsoc2017$r13_relig_0',i,'_w02==1 & !is.na(elsoc2017$r13_relig_0',i,'_w02)]<-\'catholic\' ;', 
'ego_network_elsoc2017$r13_relig_0',i,'_w02[elsoc2017$r13_relig_0',i,'_w02==2 & !is.na(elsoc2017$r13_relig_0',i,'_w02)]<-\'evangelical\' ;',
'ego_network_elsoc2017$r13_relig_0',i,'_w02[elsoc2017$r13_relig_0',i,'_w02==3 & !is.na(elsoc2017$r13_relig_0',i,'_w02)]<-\'none\' ;',
'ego_network_elsoc2017$r13_relig_0',i,'_w02[elsoc2017$r13_relig_0',i,'_w02==4 & !is.na(elsoc2017$r13_relig_0',i,'_w02)]<-\'atheist\' ;',
'ego_network_elsoc2017$r13_relig_0',i,'_w02[elsoc2017$r13_relig_0',i,'_w02==5 & !is.na(elsoc2017$r13_relig_0',i,'_w02)]<-\'other\' ', sep=''))))
}

## ideología
## ideol ego: c15_w02 144
## 0,1,2. izq,3,4 centro izq, 5 centro, 6,7 centro der, 8,9,10 derecha, ninguno(12)

table(ego_network_elsoc2017$c15_w02) #ideol

ego_network_elsoc2017$c15_w02[elsoc2017$c15_w02 == 0 | elsoc2017$c15_w02 == 1 | elsoc2017$c15_w02 == 2]<- 'rightwinger'
ego_network_elsoc2017$c15_w02[elsoc2017$c15_w02 == 3 | elsoc2017$c15_w02 == 4] <- 'center-right' 
ego_network_elsoc2017$c15_w02[elsoc2017$c15_w02 == 5] <- 'center'
ego_network_elsoc2017$c15_w02[elsoc2017$c15_w02 == 11] <- 'independent'
ego_network_elsoc2017$c15_w02[elsoc2017$c15_w02 == 12] <- 'none'
ego_network_elsoc2017$c15_w02[elsoc2017$c15_w02 == 6 | elsoc2017$c15_w02 == 7]<- 'center-left'
ego_network_elsoc2017$c15_w02[elsoc2017$c15_w02 == 8 | elsoc2017$c15_w02 == 9 | elsoc2017$c15_w02 == 10] <- 'leftwinger'

## ideol alter, derecha, centro derecha, centro, centro izquierda, izquierda, ninguno (6)
table(ego_network_elsoc2017$r13_ideol_03_w02) #

for (i in 1:5){
eval(parse(text=paste('ego_network_elsoc2017$r13_ideol_0',i,'_w02[elsoc2017$r13_ideol_0',i,'_w02==1 & !is.na(elsoc2017$r13_ideol_0',i,'_w02)]<-\'rightwinger\' ;',
'ego_network_elsoc2017$r13_ideol_0',i,'_w02[elsoc2017$r13_ideol_0',i,'_w02==2 & !is.na(elsoc2017$r13_ideol_0',i,'_w02)]<-\'center-right\' ;',
'ego_network_elsoc2017$r13_ideol_0',i,'_w02[elsoc2017$r13_ideol_0',i,'_w02==3 & !is.na(elsoc2017$r13_ideol_0',i,'_w02)]<-\'center\' ;',
'ego_network_elsoc2017$r13_ideol_0',i,'_w02[elsoc2017$r13_ideol_0',i,'_w02==6 & !is.na(elsoc2017$r13_ideol_0',i,'_w02)]<-\'none\' ;',
'ego_network_elsoc2017$r13_ideol_0',i,'_w02[elsoc2017$r13_ideol_0',i,'_w02==4 & !is.na(elsoc2017$r13_ideol_0',i,'_w02)]<-\'center-left\' ;',
'ego_network_elsoc2017$r13_ideol_0',i,'_w02[elsoc2017$r13_ideol_0',i,'_w02==5 & !is.na(elsoc2017$r13_ideol_0',i,'_w02)]<-\'leftwinger\' ', sep='')))
}

## barrio
## ego_network_elsoc2017$barrio.ego_02<-'same'
table(ego_network_elsoc2017$barrio.ego_02)

## barrio alter
for (i in 1:5){
eval(parse(text=paste('ego_network_elsoc2017$r13_barrio_0',i,'_w02[elsoc2017$r13_barrio_0',i,'_w02==1]<-\'same\' ;',
'ego_network_elsoc2017$r13_barrio_0',i,'_w02[elsoc2017$r13_barrio_0',i,'_w02==2]<-\'another\' ', sep='')))
}

table(ego_network_elsoc2017$r13_barrio_03_w02)

names(ego_network_elsoc2017)


n<-paste(paste('sex', 1:5, collapse=',', sep=''), ',', 
paste('age', 1:5, collapse=',', sep=''), ',',
paste('barrio', 1:5, collapse=',', sep=''), ',',
paste('educ', 1:5, collapse=',', sep=''), ',',
paste('relig', 1:5, collapse=',', sep=''), ',',
paste('ideol', 1:5, collapse=',', sep=''), collapse=',', sep='')
gsub(',', '\',\'' ,n  )
names(ego_network_elsoc2017)<- c('sex', 'sex1','sex2','sex3','sex4','sex5', 'age', 'age1','age2','age3','age4','age5', 'barrio', 'barrio1','barrio2','barrio3','barrio4','barrio5', 'educ', 'educ1','educ2','educ3','educ4','educ5', 'relig', 'relig1','relig2','relig3','relig4','relig5', 'ideol', 'ideol1','ideol2','ideol3','ideol4','ideol5','deg')
save(ego_network_elsoc2017, file='ego_network_elsoc2017.RData')

load('ego_network_elsoc2017.RData')
head(ego_network_elsoc2017)


## el archivo se organiza por ego y 5 alter
## en orden sexo, edad, barrio, educación, religión, ideología
## última variable: grado de la red

## probablemente rinda más agrupar las edades o calcular similaridad con un rango +/- 3, p.ej.

u <- 530:574
names(elsoc2017)
    paste(u[seq(8, length(u), 9)], collapse=',', sep='')
1,2, 5 (barrio), 6 educ, 7 relig, 8 ideol


## sexo,
## edad,
## relación (cónyuge, hijx, pariente, amigo, otro),
## tiempo (en años: <1, 1-2, 3-5, 5-10, 10+),
## barrio, mismo = 1, otro =2
## educ, bás o menos, EM incompl, EM completa, tec.sup, u.
## contacto, presencial, telefónico, redes, mail

## datos ego
## sexo: m0_sexo_w02 814
## edad: m0_edad_w02 817

## escolaridad entrevistado 
## escentr<- rep(NA, length(elsoc2018[,1])) #inicializa variable escolaridad entrevistado
## escentr[elsoc2018$m01_w03==1]<-0 # sin educación
## escentr[elsoc2018$m01_w03==2]<-c(floor(runif(length(which(elsoc2018$m01_w03==2)), min=1, max=7))) # básica incompleta
## escentr[elsoc2018$m01_w03==3]<-8 #básica completa
## escentr[elsoc2018$m01_w03==4]<-c(floor(runif(length(which(elsoc2018$m01_w03==4)), min=9, max=11))) #media incompleta
## escentr[elsoc2018$m01_w03==5]<-12 # media completa
## escentr[elsoc2018$m01_w03==6]<-13 # t.s. incompleta
## escentr[elsoc2018$m01_w03==7]<-14 # t.s. completa
## escentr[elsoc2018$m01_w03==8]<-15 # u. incompleta
## escentr[elsoc2018$m01_w03==9]<-17 # u. completa
## escentr[elsoc2018$m01_w03==10]<-20 # postgado
## table(escentr)

## cat(paste(letters, 100* 1:26), fill = TRUE, labels = paste0("{", 1:10, "}:"))
names(elsoc2017)


## load("/home/vicente/Dropbox/fondecyt2017/borradores/homofilia/case_control_logistic/example_ego_network_data/egonet_data.Rdata")

head(ego_network_data)
nrow(ego_network_data) #300 sampled egos

## name of degree column on data frame:
var.name.degree="deg"

## names of key ego attribute columns in data frame:
var.name.characs=c("race", "educ_cat", "gender") 

## list to describe alter's characteristics
var.name.characs.alter=list()
for (p in 1:length(var.name.characs)){
var.name.characs.alter[[p]]=paste(var.name.characs[[p]],1:5,sep="")

var.name.characs.alter #list of 3 elements with ego + five alteri in each

## Reading in functions
source("~/Dropbox/fondecyt2017/borradores/homofilia/case_control_logistic/functions/ego_net_case_control_model.R")
(egonet_case_control_model)
source("~/Dropbox/fondecyt2017/borradores/homofilia/case_control_logistic/functions/output_glm_formula.R")

source("~/Dropbox/fondecyt2017/borradores/homofilia/case_control_logistic/functions/prepare_case_control_logistic_data_function_create_vars.R")

source("~/Dropbox/fondecyt2017/borradores/homofilia/case_control_logistic/functions/prepare_case_control_logistic_data_function.R")
objects()
## load required packages
library(biglm)
library(ergm)
library(doParallel)

## set formula for the model
## Here we estimate a model based on racial, education and gender homophily.
## In this example, we will include nodematch terms for race and gender and 
## nodemix terms for education (including terms for all pairs of educational categories).
## We also include nodefactor terms for gender, capturing if males or females having higher average degree (or more named alters).

formula_ego_mod <- as.formula( ~ nodematch("race") + nodemix("educ_cat") + 
nodematch("gender") + nodefactor("gender"))

## run the case control logistic regression to estimate the strength of homophily.
egonet_case_control_output=egonet_case_control_model(formula=formula_ego_mod,
ego_data=ego_network_data
,var.name.degree=var.name.degree, 
var.name.characs=var.name.characs
,var.name.characs.alter=var.name.characs.alter, 
case.control.type="weighted.random.matching", max.alter=5,
max.control.data.N=NULL,
remove.isolates.control=T, 
weight.var.name=NULL, weight.var.name.control=NULL, 
num.iterations=2,
bootstrap.sample=T, num.bootstrap.samples=10,
useparallel=T, num.cores=5,
nodemix.reference=c("C.Some College.C.Some College"), maxit=20,
adjust.intercept=F)

names(egonet_case_control_output) # two parts

coefs=egonet_case_control_output$coefs # logistic regression coefficients

head(coefs)

## mean over the iterations for each sample.

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



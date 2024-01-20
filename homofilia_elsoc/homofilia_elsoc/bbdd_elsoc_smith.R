
library(haven)
elsoc_wide_2016_2018 <- sjlabelled::read_stata("ELSOC_Wide_2016_2018_v1.00_Stata14.dta")
#View(elsoc_wide_2016_2018)

## solamente ola 2 (2017)
elsoc2017<-subset(elsoc_wide_2016_2018, elsoc_wide_2016_2018$tipo_atricion==1 | elsoc_wide_2016_2018$tipo_atricion==2)
save(elsoc2017, file='Elsoc2017.RData')

#names(elsoc2017)
#load('Elsoc2017.RData')

# selecionar variables (por n?columna) para data
## dar con el n?mero de la comuna del ponderador 2017
#which(colnames(elsoc2017)=="ponderador02_w02" )


elsoc2017$barrio.ego_02 <-rep('same', length(elsoc2017[,1]))
ego_network_elsoc2017<-elsoc2017[,c(814,530,539,548,557,566,817,531,540,549,558,
                                    567,1054,534,543,552,561,570,820,535,544,553,
                                    562,571,918,536,545,554,563,572,144,537,546,
                                    555,564,573,532,541,550,559,568,529,1043)]

save(ego_network_elsoc2017, file='ego_network_elsoc2017.RData')

load('ego_network_elsoc2017.RData')
names(ego_network_elsoc2017)

table(ego_network_elsoc2017$barrio.ego_02)

## sexo egos
table(ego_network_elsoc2017$m0_sexo_w02)
ego_network_elsoc2017$m0_sexo_w02<-ifelse(elsoc2017$m0_sexo_w02==1 & !is.na(elsoc2017$m0_sexo_w02), 'male', 'female')

## sexo alter
for (j in 1:5){
  eval(parse(text=paste('ego_network_elsoc2017$r13_sexo_0',j,'_w02 <- ifelse(elsoc2017$r13_sexo_0',j,'_w02==1, \'male\', ifelse(elsoc2017$r13_sexo_0',j,'_w02==2, \'female\', elsoc2017$r13_sexo_0',j,'_w02))', sep='')))
}

table(ego_network_elsoc2017$r13_sexo_02_w02)

## educaci?n ego m01_w02 823
## 1, 2, 3|4|5|6,7|8,9,10

table(ego_network_elsoc2017$m01_w02) #educ

ego_network_elsoc2017$m01_w02<-ifelse(!is.na(elsoc2017$m01_w02) & (elsoc2017$m01_w02==1 | elsoc2017$m01_w02==2 | elsoc2017$m01_w02==3), 'lt.secondary', ifelse(!is.na(elsoc2017$m01_w02) & ego_network_elsoc2017$m01_w02==4, 'some.secondary', ifelse(!is.na(elsoc2017$m01_w02) & elsoc2017$m01_w02==5, 'secondary',
                                                                                                                                                                                                                                                      ifelse(!is.na(elsoc2017$m01_w02) & (elsoc2017$m01_w02==6 | elsoc2017$m01_w02==7), 'technical.ed',  ifelse(!is.na(elsoc2017$m01_w02) & (elsoc2017$m01_w02==8 | elsoc2017$m01_w02==9 | elsoc2017$m01_w02==10), 'college.ed', elsoc2017$m01_w02)))))
## educ alter
table(ego_network_elsoc2017$r13_educ_03_w02)

for (j in 1:5){
  eval(parse(text=(paste('ego_network_elsoc2017$r13_educ_0',j, '_w02 <-ifelse(!is.na(elsoc2017$r13_educ_0',j, '_w02) & elsoc2017$r13_educ_0',j,'_w02==1, \'lt.secondary\', ifelse(!is.na(elsoc2017$r13_educ_0',j,'_w02) & elsoc2017$r13_educ_0',j,'_w02==2, \'some.secondary\', ifelse(!is.na(elsoc2017$r13_educ_0',j,'_w02) & elsoc2017$r13_educ_0',j,'_w02==3, \'secondary\', ifelse(!is.na(elsoc2017$r13_educ_0',j,'_w02) & elsoc2017$r13_educ_0',j,'_w02==4, \'technical.ed\', ifelse(!is.na(elsoc2017$r13_educ_0',j,'_w02) & elsoc2017$r13_educ_0',j,'_w02==5, \'college.ed\', elsoc2017$r13_educ_0',j,'_w02)))))', sep=''))))
}



## religi?n
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


## ideolog?a
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

## ego_network_elsoc2017$barrio.ego_02<-'same'
table(ego_network_elsoc2017$barrio.ego_02)

## barrio alter
for (i in 1:5){
  eval(parse(text=paste('ego_network_elsoc2017$r13_barrio_0',i,'_w02[elsoc2017$r13_barrio_0',i,'_w02==1]<-\'same\' ;',
                        'ego_network_elsoc2017$r13_barrio_0',i,'_w02[elsoc2017$r13_barrio_0',i,'_w02==2]<-\'another\' ', sep='')))
}

table(ego_network_elsoc2017$r13_barrio_03_w02)
names(ego_network_elsoc2017)

## relaci?n alter
for (i in 1:5){
  eval(parse(text=paste('ego_network_elsoc2017$r13_relacion_0',i,'_w02[elsoc2017$r13_relacion_0',i,'_w02==1 & !is.na(elsoc2017$r13_relacion_0',i,'_w02)]<-\'fam\' ;',
                        'ego_network_elsoc2017$r13_relacion_0',i,'_w02[elsoc2017$r13_relacion_0',i,'_w02==2 & !is.na(elsoc2017$r13_relacion_0',i,'_w02)]<-\'fam\' ;',
                        'ego_network_elsoc2017$r13_relacion_0',i,'_w02[elsoc2017$r13_relacion_0',i,'_w02==3 & !is.na(elsoc2017$r13_relacion_0',i,'_w02)]<-\'fam\' ;',
                        'ego_network_elsoc2017$r13_relacion_0',i,'_w02[elsoc2017$r13_relacion_0',i,'_w02==4 & !is.na(elsoc2017$r13_relacion_0',i,'_w02)]<-\'nofam\' ;',
                        'ego_network_elsoc2017$r13_relacion_0',i,'_w02[elsoc2017$r13_relacion_0',i,'_w02==5 & !is.na(elsoc2017$r13_relacion_0',i,'_w02)]<-\'nofam\' ', sep='')))
}


n<-paste(paste('sex', 1:5, collapse=',', sep=''), ',', 
         paste('age', 1:5, collapse=',', sep=''), ',',
         paste('barrio', 1:5, collapse=',', sep=''), ',',
         paste('educ', 1:5, collapse=',', sep=''), ',',
         paste('relig', 1:5, collapse=',', sep=''), ',',
         paste('ideol', 1:5, collapse=',', sep=''), ',',
         paste('relacion', 1:5, collapse=',', sep=''), collapse=',', sep='')
gsub(',', '\',\'' ,n  )
names(ego_network_elsoc2017)<- c('sex', 'sex1','sex2','sex3','sex4','sex5', 
                                 'age', 'age1','age2','age3','age4','age5', 
                                 'barrio', 'barrio1','barrio2','barrio3','barrio4','barrio5', 
                                 'educ', 'educ1','educ2','educ3','educ4','educ5', 
                                 'relig', 'relig1','relig2','relig3','relig4','relig5', 
                                 'ideol', 'ideol1','ideol2','ideol3','ideol4','ideol5',
                                 'relacion1','relacion2','relacion3','relacion4','relacion5',
                                 'deg',
                                 'pondera')

save(ego_network_elsoc2017, file='ego_network_elsoc2017.RData')

load('ego_network_elsoc2017.RData')
head(ego_network_elsoc2017)

## NA
ego_network_elsoc2017[ego_network_elsoc2017=="-999"] <- NA
ego_network_elsoc2017[ego_network_elsoc2017=="-888"] <- NA




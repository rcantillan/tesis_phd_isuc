

library(tidyverse)
library(haven)
library(questionr)
library(summaryTools)
library(foreign)
library(kableExtra)

iss <- read_dta("~/Desktop/ISS_2017/ZA6980_v2-0-0.dta")
glimpse(iss)

#devtools::install_github("DiogoFerrari/occupar", dependencies=F)
#library(occupar)
#isco88.labels = occupar::isco88labels(isco88)

# a bus/lorry driver = 36
# a senior executive of a large company = 70
# a home or office cleaner = 17
# a hairdresser/barber = 32
# a human resource manager/personnel manager = 68
# a lawyer = 85
# a car mechanic = 38
# a nurse = 42
# a police officer = 53
# a school teacher = 71 

t1<-prop.table(table(iss$v1))
t2<-prop.table(table(iss$v2))
t3<-prop.table(table(iss$v3))
t4<-prop.table(table(iss$v4))
t5<-prop.table(table(iss$v5))
t6<-prop.table(table(iss$v6))
t7<-prop.table(table(iss$v7))
t8<-prop.table(table(iss$v8))
t9<-prop.table(table(iss$v9))
t10<-prop.table(table(iss$v10))
t<-rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10);t<-as.data.frame(t)
ocup<-c("driver", "executive", "cleaner", "barber", "personnel manager", "lawyer",
       "car mechanic", "nurse", "police officer", "school teacher"); 
ocup<-as.tibble(ocup);
t$ocup<-cbind(ocup)
isei<-c(36,70,17,32,68,85,38,42,53,71);isei<-as.tibble(isei)
t$isei<-cbind(isei)
colnames(t)<-cbind("familia","amigo","otro conocido","no puedo elegir", NA, NA, "ocupacion", "ISEI")
t<-t%>%select("ocupacion","familia","amigo","otro conocido","no puedo elegir","ISEI")

#tabla
t %>%
  kbl(caption = "Generador de posiciones ISS 2017 (no hay datos para Chile)") %>%
  kable_classic(full_width = F, html_font = "Cambria")
















  
  
  



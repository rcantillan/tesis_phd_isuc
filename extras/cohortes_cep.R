
# library
library(tidyverse)

# cargar data
cep <- readRDS("~/Documents/doctorado-UC/tesis/data/base_consolidada_1990_2022_v2-1.Rds_/base_consolidada_1990_2022_v2 (1).Rds")

# Explorar cohortes CEP
cep_sub<-cep%>%select(edad, esc_nivel_1, info_hogar_7, info_enc_20, iden_pol_1, 
                      religion_1)




# edad =	Edad exacta
# esc_nivel_1 = Escolaridad por nivel 1	
# info_hogar_7 = ¿Con cuánto ingreso se dispone mensualmente en esta casa en total aproximadamente? Por favor, indíquenos el rango de ingreso	
# info_enc_20	= De los siguientes tramos de ingresos mensuales que se presentan en esta tarjeta. ¿Podría Ud. indicarme en cuál de los siguientes tramos se encuentra UD., considerando SU ingreso líquido por sueldo, jubilación si la tuviera, pensión, aportes de parientes o amigos, mesadas, arriendos y otros? ¿o no tiene ningún tipo ingresos?	


# iden_pol_1 = Posición polpitica del encuestado 
# iden_pol_3_T = 
# religion_1 = ¿Podría Ud. decirme la religión o iglesia a la que pertenece o se siente más cercano? [8 categorías]	
# info_enc_1_A = En relación a su actividad y ocupación, ¿cuál es o era su actividad principal? (v1)	
# info_enc_1_D = En relación a su actividad y ocupación, ¿cuál es o era su actividad principal? (código CIUO 88)	
# info_enc_19_D = En relación a su actividad y ocupación, ¿cuál es o era su actividad principal? (código ciuo 88)	 





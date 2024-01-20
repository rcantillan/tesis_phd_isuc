

load(url("https://raw.githubusercontent.com/JeffreyAlanSmith/case_control_logistic/master/example_ego_network_data/egonet_data.Rdata"))
head(ego_network_data)

var.name.degree="deg"
var.name.characs=c("race", "educ_cat", "gender") 

var.name.characs.alter=list()
for (p in 1:length(var.name.characs)){
  var.name.characs.alter[[p]]=paste(var.name.characs[[p]],1:5,sep="")
}

var.name.characs.alter


source("https://raw.githubusercontent.com/JeffreyAlanSmith/case_control_logistic/master/functions/ego_net_case_control_model.R")
source("https://raw.githubusercontent.com/JeffreyAlanSmith/case_control_logistic/master/functions/output_glm_formula.R")
source("https://raw.githubusercontent.com/JeffreyAlanSmith/case_control_logistic/master/functions/prepare_case_control_logistic_data_function_create_vars.R")
source("https://raw.githubusercontent.com/JeffreyAlanSmith/case_control_logistic/master/functions/prepare_case_control_logistic_data_function.R")

library(biglm)
library(ergm)
library(doParallel)


formula_ego_mod <- as.formula( ~ nodematch("race") + nodemix("educ_cat") + 
                                 nodematch("gender") + nodefactor("gender"))



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


names(egonet_case_control_output)

coefs=egonet_case_control_output$coefs
head(coefs)
coefs_sample=aggregate(.~sample, data=coefs, mean)
coefs_sample



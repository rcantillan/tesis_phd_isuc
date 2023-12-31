#codigos de replicación de Jefrey Smith

#This is example R code, showing how to take input ego network data 
#and run the case control logistic regression model introduced in:
#Smith Jeffrey A., McPherson, Miller, and Lynn Smith-Lovin.  2014.
#“Social Distance in the United States: Sex, Race, Religion, 
#Age and Education Homophily among Confidants, 1985-2004.” American Sociological Review 79:432-456.

#The model takes basic ego network data, including number of partners,
#ego attributes and alter attributes and estimates the strength 
#of homophily (relative to chance) along the demographic dimensions of interest.
#For example, we might want to know how much more likely a tie is to exist
#between two people who match on race compared to two people who do not match on race.
#The model takes the form of a logistic regression, where y=1 if there is a 
#an observed tie and 0 if there is no tie. The '1s' are based on the observed ties 
#between ego-alter pairs in the input ego network data. The '0s', or non-ties,
#are based on pairing the sampled respondents (or egos) together, capturing
#what random mixing in the population might look like. 

#In this case we will demonstrate the basic approach on one ego network data set
#(i.e., one ego network sample coming from one network). The example ego network
#data is stored as an .Rdata file. You can find all necessary data frames and functions
#on the github site, https://github.com/JeffreyAlanSmith/case_control_logistic.

#We begin by reading in our example ego network data. The ego network data is housed in the ego_network_data folder. 
#Let's read this ego network data in:

load(url("https://raw.githubusercontent.com/JeffreyAlanSmith/case_control_logistic/master/example_ego_network_data/egonet_data.Rdata"))

#Let's take a look at the ego network data we will be working with. 
head(ego_network_data)

#We see demographic attributes for ego (race, educ_cat, gender)
#as well for the named alters (race1, race2...).
#For example, for the first respondent, we see that they are white and name 1 alter,
#who is also white. There is a also column called deg, which captures the degree of the node.
#Note that here we assume that we only have information on up to 5 named alters. 

#Let's also check the number of rows:
nrow(ego_network_data)

#So, we have 300 sampled egos, with ego and alter attributes for 
#race, gender and education. We also have degree information for ego. 

#Our goal is to take the information on the 300 cases (degree, ego/alter attributes)
#and estimate a model showing the strength of homophily (does ego and alter
#match on the attribute?) for race, education and gender. 

#Here we set some useful inputs that we will use below: 

#name of degree column on data frame:
var.name.degree="deg"

#names of key ego attribute columns in data frame:
var.name.characs=c("race", "educ_cat", "gender") 

#Here we are going to create a list describing the columns names of the alter
#attributes on the ego network data frame. 
#The alter attribute columns have the form of race1 race2...
#We will create a list, where each slice is a different attribute, 
#first race then education, then gender. This is the same order as in var.name.characs.

var.name.characs.alter=list()
for (p in 1:length(var.name.characs)){
  var.name.characs.alter[[p]]=paste(var.name.characs[[p]],1:5,sep="")
}

#Let's take a look:
var.name.characs.alter

##################################################
#Reading in functions

#Now we will read in the core functions to estimate the model. We will read in 
#four main functions to run the model. They are housed in the functions folder.

source("https://raw.githubusercontent.com/JeffreyAlanSmith/case_control_logistic/master/functions/ego_net_case_control_model.R")
source("https://raw.githubusercontent.com/JeffreyAlanSmith/case_control_logistic/master/functions/output_glm_formula.R")
source("https://raw.githubusercontent.com/JeffreyAlanSmith/case_control_logistic/master/functions/prepare_case_control_logistic_data_function_create_vars.R")
source("https://raw.githubusercontent.com/JeffreyAlanSmith/case_control_logistic/master/functions/prepare_case_control_logistic_data_function.R")

#Now we load some useful packages. 
library(biglm)
library(ergm)
library(doParallel)

##################################################

#Now we will set the formula for the model of interest.
#The possible model terms are: 
#nodematch=a simple match/no match term for the attribute of interest (does ego 
#and alter match on the attribute?)
#nodemix=includes terms for all pairs of categories in the attribute of interest
#absdiff=includes a term to show absolute difference between ego and alter (appropriate
#for continuous variables).
#nodefactor=controls for the degree of different groups (adjusting the homophily estimates 
#for fact that some groups, e.g., males, have more ties than other groups, like females.
#nodecov=similar to nodefactor but appropriate for continuous variables. 

#Here we estimate a model based on racial, education and gender homophily.
#In this example, we will include nodematch terms for race and gender and 
#nodemix terms for education (including terms for all pairs of educational categories).
#We also include nodefactor terms for gender, capturing if males or females having higher average degree (or more named alters).

formula_ego_mod <- as.formula( ~ nodematch("race") + nodemix("educ_cat") + 
                                 nodematch("gender") + nodefactor("gender"))

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

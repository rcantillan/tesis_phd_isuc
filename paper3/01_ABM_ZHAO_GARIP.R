
library(matrixStats) 
library(parallel)
library(plot3D)

# PARAMETER MEANINGS
# H: Number of social positions within each dimension of social life (heterogeneity)
# D: Complexity is the total number of dimensions in the society
# G: Group size is the number of people located in each social position. 
# N = H times G = population size 
# example: say there are 20 jobs each with 10 people. Population is 200. Say there are also 20 ages each with 10 people. This is complexity 

# PARAMETER VALUES
# Keep size of population constant. N = 3200 is what Centola chooses. For our replication we can get the same main result with N = 1600
# Density of ties constant Z = 10
# For Centola, main results are for H = 32, D = 10, Z = 10, G = 100 (averaged over 100 realizations)
# WHAT WE DO IN OUR REPLICATION H = 32, D = 10, Z = 10, G = 50 (averaged over 50 realizations)

# SOME NOTES ON ROBUSTNESS/ VALIDITY OF REPLICATION
# Centola investigates the robustness of these results to parameters values
# Dimensions as in complexity: 2 <= D <= 20
# Size of groups: 50<=G<=400
# Note that thus heterogeneity changes as it is N/G
# When low number of dimensions (complexity), integration succeeds even with no consolidation
#  We keep it the same. The larger the group size, the more dependent diffusion is on social consolidation (larger range of consolidation for which diffusion is possible) so since we choose a smaller group size, results are conservative 

# For homogeneity H (how many possible groups), returns the social distance between individual in group i and group j in a given (OR DIFFERENT) dimension
distance <- function(H, i, j){ 
  numcuts = log(H, base = 2)+1
  dst = 0
  if(numcuts ==1) {
    dst = 1
  }
  else{
    if(i <= H/2 && j > H/2){
      dst = numcuts
    }
    else if(j <= H/2 && i > H/2){
      dst = numcuts
    }
    else{
      nh = H/2
      dst = distance(nh, ((i-1) %% nh) + 1, ((j-1) %% nh) + 1 )
    }
  }
  return(dst)
}

# For homogeneity H (how many possible groups), returns matrix of social distances where mat[i,j] is social distance between two individuals one in group i and the other in group j
distmatrix <- function(H){  
  mat = matrix(0, H, H)
  for(i in 1:H){
    for(j in 1:H){
      mat[i,j] = distance(H, i, j)
    }
  }
  return(mat)
}

# For homogeneity H, group size G, and complexity D, set the reference social distance matrix 
# This creates a matrix with H*G (population size) as the rows and social dimensions as the columns
# This matrix represents which group in each dimension each individual has in society 
socposmatrix <- function(H, G, D, beta){  
  dstref = distmatrix(H)
  # Assign social positions according to consolidation 
  # Choose an individual i at random, and at a distance x  with probability p(x) = ce^-alpha
  # Consolidation: Assign an individual's social positions at H2-HH at distance y from her position in h1 with probability p(y) = ce^(-ebeta*y)
  positions <- matrix(0, nrow = H*G, ncol = D)
  # Each row of social position matrix represents a person. Each column represents a social dimension
  # so for column one (say it represents the dimension occupation) we should have H different values with each of the values being assigned to G people
  # Assign starting social positions to first social dimension
  positions[,1] <- sample(1:H, replace = TRUE, H*G)
  # sum(c*exp(-options*beta))  check that it works, yes it works
  options <- (1:(log(H, base = 2)+ 1))
  c = 1/sum(exp(-options*beta)) # normalizing constant
  dism <- matrix(0, nrow = H*G, ncol = D-1)
  for(i in 1:(H*G)){
    for(j in 2:D){
      dis = sample(options, 1, replace = TRUE, prob = c*exp(-options*beta)) # consolidation distribution, select distance "dis"
      dism[i,j-1] = dis
      optj = which(dstref[positions[i,1],]<=dis) # assign positions in following dimensions within distance "dis" from position in dimension 1
      if(length(optj)==1){
        positions[i,j] = optj
      }
      else{
        positions[i,j] = sample(optj,1)
      }
    } 
  }
  return(positions)
}

# Create social distance matrix between two people where social distance[i,j] is the social distance between person i and j
socialdismatrix <- function(H, G, D, beta){
  dstref = distmatrix(H)
  positions = socposmatrix(H, G, D, beta)
  socialdistance = matrix(0, H*G, H*G)
  for(i in 1:(H*G)){
    for(j in 1:(H*G)){
      socialdistance[i,j] = mean(sort(diag(dstref[positions[i,], positions[j,]]))[1]) # minimum social distance over the dimensions
    }
  }
  return(list(socdis = socialdistance,
              socpos = positions))
}

# by plotting say the social distance of the first element with respect to the others 
# for different beta you can see how consolidation (beta) increases social distance 
# socialdistancetest = socialdismatrix(H, G, D)
# hist(socialdistancetest[1,]) 

# Make Ties
makenetwork <- function(H, G, D, Z, alpha, beta){
  print("setting social distances")
  x = socialdismatrix(H, G, D, beta)
  socialdistance = x$socdis
  positions = x$socpos
  distopt = 1:(log(H, base = 2)+1) 
  c_alpha = 1/sum(exp(-distopt*alpha)) # normalizing constant
  sum(c_alpha*exp(-distopt*alpha)) # homophily distribution
  tie = matrix(0, H*G, H*G) # matrix of ties 
  complete = (rowSums(tie)>=Z) # number of people with complete networks
  counter = 0 # counter for number of times no eligible person (network incomplete and satisfies consolidation requirement) was found
  prevTie = tie
  max_counter = 2000 # conservative 
  print("making ties")
  while(sum(complete)<H*G){ # while not everyone has a complete network
    if(length(which(complete == FALSE))>1){
      i = sample(which(complete == FALSE), 1) # pick a random person 
    }
    else if(length(which(complete == FALSE))==1){
      i = which(complete == FALSE)
    }
    distconstraint = sample(distopt, 1, prob = c_alpha*exp(-distopt*alpha)) # pick a social distance according to cons. distribution
    optj = which((socialdistance[i,]<=distconstraint) & complete == FALSE) # which people are at this social distance and still need friends?
    if(length(which(optj == i))!=0) {
      optj = optj[-which(optj == i)] # you can't be friends with yourself
    }
    if(length(optj)>1){ # if there were multiple friend options
      newtiej = sample(optj, 1) # pick a random person among these people
      tie[i, newtiej] = 1 # make friends
      tie[newtiej, i] = 1 # friendship is transitive
    }
    else if(length(optj)==1){ # this case is because the sample function gets confused if optj is of length 1 
      newtiej = optj # if only one friend satisfies this criteria, make friends
      tie[i, newtiej] = 1
      tie[newtiej, i] = 1
    }
    complete = (rowSums(tie)>=Z) # update how many people have a complete network
    if(identical(prevTie, tie)) { # if the network did not change, update this as well
      counter = counter + 1
      #print(counter)
    }
    prevTie = tie # update the most recent tie matrix
    if(counter > max_counter){ # if not much has changed many times, BREAK LOOP
      return(list(ties = tie, 
                  socpos = positions))
      break
    }	  
    # cat(100*sum(tie)/(H*G*Z), "% of ties formed")
  }
  print("TESTING")
  return(list(ties = tie, 
              socpos = positions))	
}

# SIMULATE ADOPTION FUNCTION
simulate.adoption.ineq = function(tie, max_time, H, status){
  # tie         - A tie matrix defining a network
  # max_time    - Number of time iterations in the simulation 
  #             --- note that final state seems to be reached very quickly within 20 steps for sure
  #  cont_level  - Number of adoptees among ties necessary for adoption
  max_obs = nrow(tie)
  highstatus = which(status > 14)
  avgstatus = which(status > 2 & status <= 14)
  lowstatus = which(status <=2 )
  init = sample(highstatus,1)    # Initial adopters
  init = c(init, which(tie[init,]==1))
  init2 = sample(avgstatus,1)    # Initial adopters
  init2 = c(init2, which(tie[init2,]==1))
  init3 = sample(lowstatus, 1)
  init3 = c(init3, which(tie[init3,]==1))
  init = c(init, init2, init3)
  adopt_ind <- matrix(c(0),nrow=max_obs,ncol=max_time) # Adoption indicator matrix - Rows: Nodes 1-N - Cols: Time 1-T 
  adopt_ind[init,1] = 1
  adopt_all <- rep(0,max_time)	 	# Overall number of adopters at each time period
  adopt_all[1] = sum(length(init))
  rate_all <- adopt_all[]/max_obs	   	# Overall adoption rate at each time period  
  rate_high <- rep(0, max_time)
  rate_avg <- rep(0, max_time)
  rate_low <- rep(0, max_time)
  cont <- rep(0, length(status))
  cont[highstatus] = 2
  cont[avgstatus] = 3
  cont[lowstatus] = 4
  # Compute the overall adoption rates
  for (t in 2:max_time) {
    adopt_ind[,t] = adopt_ind[,t-1]
    for(i in 1:max_obs){
      num.adopt.network = sum(adopt_ind[,t-1]==1 & tie[i,]==1) # or sum(adopt_ind[ties[i,]==1,t-1]==1)      
      #cont_level = round(((17-status[i])/H+1)*1.33333)
      cont_level = cont[i]
      if(num.adopt.network >= cont_level){      
        adopt_ind[i,t] = 1	# Update the adopters    
      }   
    }
    adopt_all[t] = sum(adopt_ind[,t]);          # Count overall number of adopters
    rate_all[t] = adopt_all[t]/max_obs          # Compute overall rate of adopters
    rate_high[t] = sum(adopt_ind[highstatus,t])/length(highstatus)
    rate_avg[t] = sum(adopt_ind[avgstatus,t])/length(avgstatus)
    rate_low[t] = sum(adopt_ind[lowstatus,t])/length(lowstatus)
  }
  return(list(rate_all = rate_all, rate_high = rate_high, rate_avg = rate_avg, rate_low = rate_low))
}


save_networks <- function(D = 10, 
                          H, G, Z, numsim, time, alpha, beta) {
  # PUT IT HERE TO BE FAST AND ONLY DO ONE NETWORK PER SET OF PARAMETERS
  #x = makenetwork(H, G, D, Z, alpha, beta)
  #ties = x$ties
  #positions = x$socpos
  #status = positions[, 1]
  obsdiffusion = matrix(0, numsim, time)
  obsdiffusion_h = matrix(0, numsim, time)
  obsdiffusion_a = matrix(0, numsim, time)
  obsdiffusion_l = matrix(0, numsim, time)
  obsdiffusion_lor = matrix(0, numsim, time)
  for(i in 1:numsim){
    # PUT IT HERE TO MAKE IT DO NUMSIM NETWORKS PER SET OF PARAMETERS
    x = makenetwork(H, G, D, Z, alpha, beta)
    ties = x$ties
    positions = x$socpos
    status = positions[, 1]
    update = simulate.adoption.ineq(ties, time, H, status)    
    obsdiffusion[i,] = update$rate_all
    obsdiffusion_h[i,] = update$rate_high
    obsdiffusion_a[i,] = update$rate_avg
    obsdiffusion_l[i,] = update$rate_low
    obsdiffusion_lor[i,] = log(update$rate_high/update$rate_low)
  }
  print(obsdiffusion)
  print(obsdiffusion_lor)
  return(list(obs = colMeans(obsdiffusion), obsv = colSds(obsdiffusion), 
              obs_lor = colMeans(obsdiffusion_lor), obs_lorv = colSds(obsdiffusion_lor)))
}



vary_params <- function(beta_input){
  params_alpha = -2:6
  params_alpha = params_alpha/2
  list_cons <- vector("list", length(params_alpha))
  for(i in 1:length(params_alpha)){
    list_cons[[i]] = list(H = 16, G = 50, Z = 10, numsim = 100, time = 50, beta = beta_input)
    list_cons[[i]]$alpha = params_alpha[i]
  }
  networks <- mclapply(list_cons, function(x) 
    save_networks(H = x$H, G = x$G, Z = x$Z, numsim = x$numsim,
                  time = x$time, alpha = x$alpha, beta = x$beta), mc.cores = 9)
  return(networks)
}


params_beta = -2:6
params_beta = params_beta/2

networksoverall = mclapply(params_beta, function(x) vary_params(beta_input = x), mc.cores = 9)
networksoverall3 = networksoverall

# first number (i) is consolidation
inequalitymatrix = matrix(0, 9, 9)
diffusionmatrix = matrix(0, 9, 9)
diffusionmatrix_v = matrix(0, 9, 9)
inequalitymatrix_v = matrix(0, 9, 9)
for(i in 1:9){
  for(j in 1:9){
    print("i")
    print(i)
    print("j")
    print(j)
    diffusionmatrix[i,j] = networksoverall3[[i]][[j]]$obs[[50]]
    diffusionmatrix_v[i,j] = networksoverall3[[i]][[j]]$obsv[[50]]
    inequalitymatrix[i,j] = networksoverall3[[i]][[j]]$obs_lor[[50]]
    inequalitymatrix_v[i,j] = (networksoverall3[[i]][[j]]$obs_lorv[[50]])
    #inequalitymatrix[i,j] = (networksoverall3[[i]][[j]]$obsh[[50]])/(0.000001 + networksoverall3[[i]][[j]]$obso[[50]])  
  }
}

##################################################################################################
###########################################  FIGURE 2  ###########################################
##################################################################################################

persp(params_beta , params_beta, diffusionmatrix, cex.lab = 1.1, cex.axis = 1.1,
      zlim = range(-0.5, 1), xlab ="consolidation", ylab = "homophily", zlab = "diffusion", 
      theta = 40, phi = 55, d = 2,  main = "Diffusion: averaged over 100 realizations")

image2D(x = params_beta, y = params_beta, diffusionmatrix, cex.lab = 1.6, cex.axis = 1.1,
        zlim = range(-0.5, 1), xlab ="consolidation", ylab = "homophily", zlab = "diffusion", 
        main = "Diffusion: averaged over 100 realizations", col = ramp.col(c("white", "black")))

##################################################################################################
##############################  FIGURE 3A Diffusion Low Consolidation  ###########################
##################################################################################################

numsim = 200

df.p1 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == -1)]]$obs, 
                             networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == -1)]]$obsv))
df.p1$time <- 1:50
df.p1$llim = df.p1$V1 - 1.96*sqrt(df.p1$V2)/sqrt(numsim)
df.p1$ulim  = df.p1$V1 + 1.96*sqrt(df.p1$V2)/sqrt(numsim)

df.p2 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == 0)]]$obs, 
                             networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == 0)]]$obsv))
df.p2$time <- 1:50
df.p2$llim = df.p2$V1 - 1.96*sqrt(df.p2$V2)/sqrt(numsim)
df.p2$ulim  = df.p2$V1 + 1.96*sqrt(df.p2$V2)/sqrt(numsim)


df.p3 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == 1)]]$obs, 
                             networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == 1)]]$obsv))
df.p3$time <- 1:50
df.p3$llim = df.p3$V1 - 1.96*sqrt(df.p3$V2)/sqrt(numsim)
df.p3$ulim  = df.p3$V1 + 1.96*sqrt(df.p3$V2)/sqrt(numsim)


df.p4 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == 2)]]$obs, 
                             networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == 2)]]$obsv))
df.p4$time <- 1:50
df.p4$llim = df.p4$V1 - 1.96*sqrt(df.p4$V2)/sqrt(numsim)
df.p4$ulim  = df.p4$V1 + 1.96*sqrt(df.p4$V2)/sqrt(numsim)


(p1 <- ggplot(df.p1, aes(x = time))+
    geom_point(data=df.p1, aes(time, V1, shape = "h = -1"), cex = 5.5)+
    geom_line(data=df.p1, aes(time, V1))+
    geom_ribbon(data=df.p1, aes(ymin=llim,ymax=ulim),alpha=0.2) +
    geom_point(data=df.p2, aes(time, V1, shape = "h = 0"), cex = 5.5)+
    geom_line(data=df.p2, aes(time, V1))+
    geom_ribbon(data=df.p2, aes(ymin=llim,ymax=ulim),alpha=0.2) +
    geom_point(data=df.p3, aes(time, V1, shape = "h = 1"), cex = 5.5)+
    geom_line(data=df.p3, aes(time, V1))+
    geom_ribbon(data=df.p3, aes(ymin=llim,ymax=ulim),alpha=0.2)+
    geom_point(data=df.p4, aes(time, V1, shape = "h = 2"), cex = 5.5)+
    geom_line(data=df.p4, aes(time, V1))+
    geom_ribbon(data=df.p4, aes(ymin=llim,ymax=ulim),alpha=0.2)+
    xlim(2,50) +
    ylim(0,1.1) + 
    labs(x = "Time", y = "Diffusion") + 
    scale_shape_manual(name="Homophily", 
                       values=c("h = -1" = 1, "h = 0"= 2, "h = 1" = 3, "h = 2" = 15),
                       labels = list(expression(paste(alpha, " = -1")), expression(paste(alpha, " = 0")), 
                                     expression(paste(alpha, " = 1")), expression(paste(alpha, " = 2")))) + 
    theme(plot.title = element_text(size = 17), 
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 17),
          legend.title = element_text(size = 20)))


##################################################################################################
##############################  FIGURE 3B Diffusion High Consolidation  ##########################
##################################################################################################

df.p1 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 2)]][[which(params_beta == -1)]]$obs, 
                             networksoverall3[[which(params_beta == 2)]][[which(params_beta == -1)]]$obsv))
df.p1$time <- 1:50
df.p1$llim = df.p1$V1 - 1.96*sqrt(df.p1$V2)/sqrt(numsim)
df.p1$ulim  = df.p1$V1 + 1.96*sqrt(df.p1$V2)/sqrt(numsim)

df.p2 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 2)]][[which(params_beta == 0)]]$obs, 
                             networksoverall3[[which(params_beta == 2)]][[which(params_beta == 0)]]$obsv))
df.p2$time <- 1:50
df.p2$llim = df.p2$V1 - 1.96*sqrt(df.p2$V2)/sqrt(numsim)
df.p2$ulim  = df.p2$V1 + 1.96*sqrt(df.p2$V2)/sqrt(numsim)


df.p3 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 2)]][[which(params_beta == 1)]]$obs, 
                             networksoverall3[[which(params_beta == 2)]][[which(params_beta == 1)]]$obsv))
df.p3$time <- 1:50
df.p3$llim = df.p3$V1 - 1.96*sqrt(df.p3$V2)/sqrt(numsim)
df.p3$ulim  = df.p3$V1 + 1.96*sqrt(df.p3$V2)/sqrt(numsim)


df.p4 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 2)]][[which(params_beta == 2)]]$obs, 
                             networksoverall3[[which(params_beta == 2)]][[which(params_beta == 2)]]$obsv))
df.p4$time <- 1:50
df.p4$llim = df.p4$V1 - 1.96*sqrt(df.p4$V2)/sqrt(numsim)
df.p4$ulim  = df.p4$V1 + 1.96*sqrt(df.p4$V2)/sqrt(numsim)


(p1 <- ggplot(df.p1, aes(x = time))+
    geom_point(data=df.p1, aes(time, V1, shape = "h = -1"), cex = 5.5)+
    geom_line(data=df.p1, aes(time, V1))+
    geom_ribbon(data=df.p1, aes(ymin=llim,ymax=ulim),alpha=0.2) +
    geom_point(data=df.p2, aes(time, V1, shape = "h = 0"), cex = 5.5)+
    geom_line(data=df.p2, aes(time, V1))+
    geom_ribbon(data=df.p2, aes(ymin=llim,ymax=ulim),alpha=0.2) +
    geom_point(data=df.p3, aes(time, V1, shape = "h = 1"), cex = 5.5)+
    geom_line(data=df.p3, aes(time, V1))+
    geom_ribbon(data=df.p3, aes(ymin=llim,ymax=ulim),alpha=0.2)+
    geom_point(data=df.p4, aes(time, V1, shape = "h = 2"), cex = 5.5)+
    geom_line(data=df.p4, aes(time, V1))+
    geom_ribbon(data=df.p4, aes(ymin=llim,ymax=ulim),alpha=0.2)+
    xlim(2,50) +
    ylim(0,1.1) + 
    labs(x = "Time", y = "Diffusion") + 
    scale_shape_manual(name="Homophily", 
                       values=c("h = -1" = 1, "h = 0"= 2, "h = 1" = 3, "h = 2" = 15),
                       labels = list(expression(paste(alpha, " = -1")), expression(paste(alpha, " = 0")), 
                                     expression(paste(alpha, " = 1")), expression(paste(alpha, " = 2")))) + 
    theme(plot.title = element_text(size = 17), 
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 17),
          legend.title = element_text(size = 20)))


##################################################################################################
#############################  FIGURE 4A INEQUALITY Low Consolidation  ###########################
##################################################################################################

df.p1 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == -1)]]$obs_lor, 
                             networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == -1)]]$obs_lorv))
df.p1$time <- 1:50
df.p1$llim = df.p1$V1 - 1.96*sqrt(df.p1$V2)/sqrt(numsim)
df.p1$ulim  = df.p1$V1 + 1.96*sqrt(df.p1$V2)/sqrt(numsim)

df.p2 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == 0)]]$obs_lor, 
                             networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == 0)]]$obs_lorv))
df.p2$time <- 1:50
df.p2$llim = df.p2$V1 - 1.96*sqrt(df.p2$V2)/sqrt(numsim)
df.p2$ulim  = df.p2$V1 + 1.96*sqrt(df.p2$V2)/sqrt(numsim)


df.p3 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == 1)]]$obs_lor, 
                             networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == 1)]]$obs_lorv))
df.p3$time <- 1:50
df.p3$llim = df.p3$V1 - 1.96*sqrt(df.p3$V2)/sqrt(numsim)
df.p3$ulim  = df.p3$V1 + 1.96*sqrt(df.p3$V2)/sqrt(numsim)


df.p4 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == 2)]]$obs_lor, 
                             networksoverall3[[which(params_beta == 0.5)]][[which(params_beta == 2)]]$obs_lorv))
df.p4$time <- 1:50
df.p4$llim = df.p4$V1 - 1.96*sqrt(df.p4$V2)/sqrt(numsim)
df.p4$ulim  = df.p4$V1 + 1.96*sqrt(df.p4$V2)/sqrt(numsim)


(p1 <- ggplot(df.p1, aes(x = time))+
    geom_point(data=df.p1, aes(time, V1, shape = "h = -1"), cex = 5.5)+
    geom_line(data=df.p1, aes(time, V1))+
    geom_ribbon(data=df.p1, aes(ymin=llim,ymax=ulim),alpha=0.2) +
    geom_point(data=df.p2, aes(time, V1, shape = "h = 0"), cex = 5.5)+
    geom_line(data=df.p2, aes(time, V1))+
    geom_ribbon(data=df.p2, aes(ymin=llim,ymax=ulim),alpha=0.2) +
    geom_point(data=df.p3, aes(time, V1, shape = "h = 1"), cex = 5.5)+
    geom_line(data=df.p3, aes(time, V1))+
    geom_ribbon(data=df.p3, aes(ymin=llim,ymax=ulim),alpha=0.2)+
    geom_point(data=df.p4, aes(time, V1, shape = "h = 2"), cex = 5.5)+
    geom_line(data=df.p4, aes(time, V1))+
    geom_ribbon(data=df.p4, aes(ymin=llim,ymax=ulim),alpha=0.2)+
    xlim(2,50) +
    ylim(-0.1,2.5) + 
    labs(x = "Time", y = "Odds Ratio of Adoption") + 
    scale_shape_manual(name="Homophily", 
                       values=c("h = -1" = 1, "h = 0"= 2, "h = 1" = 3, "h = 2" = 15),
                       labels = list(expression(paste(alpha, " = -1")), expression(paste(alpha, " = 0")), 
                                     expression(paste(alpha, " = 1")), expression(paste(alpha, " = 2")))) + 
    theme(plot.title = element_text(size = 17), 
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 17),
          legend.title = element_text(size = 20)))


##################################################################################################
#############################  FIGURE 4B Inequality High Consolidation  ##########################
##################################################################################################

df.p1 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 2)]][[which(params_beta == -1)]]$obs_lor, 
                             networksoverall3[[which(params_beta == 2)]][[which(params_beta == -1)]]$obs_lorv))
df.p1$time <- 1:50
df.p1$llim = df.p1$V1 - 1.96*sqrt(df.p1$V2)/sqrt(numsim)
df.p1$ulim  = df.p1$V1 + 1.96*sqrt(df.p1$V2)/sqrt(numsim)

df.p2 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 2)]][[which(params_beta == 0)]]$obs_lor, 
                             networksoverall3[[which(params_beta == 2)]][[which(params_beta == 0)]]$obs_lorv))
df.p2$time <- 1:50
df.p2$llim = df.p2$V1 - 1.96*sqrt(df.p2$V2)/sqrt(numsim)
df.p2$ulim  = df.p2$V1 + 1.96*sqrt(df.p2$V2)/sqrt(numsim)


df.p3 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 2)]][[which(params_beta == 1)]]$obs_lor, 
                             networksoverall3[[which(params_beta == 2)]][[which(params_beta == 1)]]$obs_lorv))
df.p3$time <- 1:50
df.p3$llim = df.p3$V1 - 1.96*sqrt(df.p3$V2)/sqrt(numsim)
df.p3$ulim  = df.p3$V1 + 1.96*sqrt(df.p3$V2)/sqrt(numsim)


df.p4 <- as.data.frame(cbind(networksoverall3[[which(params_beta == 2)]][[which(params_beta == 2)]]$obs_lor, 
                             networksoverall3[[which(params_beta == 2)]][[which(params_beta == 2)]]$obs_lorv))
df.p4$time <- 1:50
df.p4$llim = df.p4$V1 - 1.96*sqrt(df.p4$V2)/sqrt(numsim)
df.p4$ulim  = df.p4$V1 + 1.96*sqrt(df.p4$V2)/sqrt(numsim)


(p1 <- ggplot(df.p1, aes(x = time))+
    geom_point(data=df.p1, aes(time, V1, shape = "h = -1"), cex = 5.5)+
    geom_line(data=df.p1, aes(time, V1))+
    geom_ribbon(data=df.p1, aes(ymin=llim,ymax=ulim),alpha=0.2) +
    geom_point(data=df.p2, aes(time, V1, shape = "h = 0"), cex = 5.5)+
    geom_line(data=df.p2, aes(time, V1))+
    geom_ribbon(data=df.p2, aes(ymin=llim,ymax=ulim),alpha=0.2) +
    geom_point(data=df.p3, aes(time, V1, shape = "h = 1"), cex = 5.5)+
    geom_line(data=df.p3, aes(time, V1))+
    geom_ribbon(data=df.p3, aes(ymin=llim,ymax=ulim),alpha=0.2)+
    geom_point(data=df.p4, aes(time, V1, shape = "h = 2"), cex = 5.5)+
    geom_line(data=df.p4, aes(time, V1))+
    geom_ribbon(data=df.p4, aes(ymin=llim,ymax=ulim),alpha=0.2)+
    xlim(2,50) +
    ylim(-0.1,2.4) + 
    labs(x = "Time", y = "Odds Ratio of Adoption") + 
    scale_shape_manual(name="Homophily", 
                       values=c("h = -1" = 1, "h = 0"= 2, "h = 1" = 3, "h = 2" = 15),
                       labels = list(expression(paste(alpha, " = -1")), expression(paste(alpha, " = 0")), 
                                     expression(paste(alpha, " = 1")), expression(paste(alpha, " = 2")))) + 
    theme(plot.title = element_text(size = 17), 
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 17),
          legend.title = element_text(size = 20)))
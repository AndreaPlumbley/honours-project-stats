## Andrea Plumbley - Honours Project
## Code for the package simA
## Likelihoods functions given by Emeritus Professor Linda Haines


##Time-to-Detection likelihoods
# Single visit
gM2sT=function(param, tvec, Tmax)
{lam=exp(param[1])
h=exp(param[2])
# organize data
ti=as.matrix(subset(tvec,tvec < Tmax))
Rt=nrow(ti)
sumt=sum(ti)
# evaluate likelihood
loglik=-lam*sum(1-exp(-h*tvec))+
  Rt*log(h*lam)-h*sumt
return(-loglik)
}

# Multiple visits
gM2mT=function(param, tmat, Tmax, R, J)
{lam=exp(param[1])
h=exp(param[2])
# organize data
Jt=matrix(0,R,1)
sumtT=matrix(0,R,1)
for(i in 1:R)
{Jt[i]=length(subset(tmat[i,],
                     tmat[i,] < Tmax[i,]))
sumtT[i]=sum(tmat[i,])}
# calculate likelihood
mu=lam*exp(-h*sumtT)
fin=matrix(0,R,1)
for(i in 1:R)
{
  if(Jt[i] > 0)
  {term=0
  for(k in 1:Jt[i])
  {term=term+(mu[i]^k)*Stirling2(Jt[i],k)}
  fin[i]=fin[i]+log(term)
  }
}
loglik=-R*lam+sum(mu)+sum(Jt)*log(h)+
  sum(log(choose(J,Jt)))+sum(fin)
return(-loglik)
}


##Detection/Non-detection Likelihoods
# Explicit Likelihood
gM1mT=function(param, Tmax, R, J, wvec)
{
  lam=exp(param[1])
  h=exp(param[2])
  Tmax1=Tmax[1,1]
  rmat=1-exp(-h*Tmax1)
  s=1-rmat
  # cycle over sites
  loglik=matrix(0,R,1)
  for(i in 1:R)
  {
    w=wvec[i]
    term=0
    for(j in 0:w)
    {term=term+choose(w,j)*(-1)^j*exp(lam*s^(J-w+j))}
    loglik[i]=log(choose(J,w))-lam +log(term)}
  return(-sum(loglik))
}

# Approximated Likelihood
gM1msum=function(param, Tmax, R, J, wvec, K)
{ lam=exp(param[1])
h=exp(param[2])
Tmax1=Tmax[1,1]
rmat=1-exp(-h*Tmax1)
s=1-rmat
# cycle over sites
loglik=matrix(0,R,1)
for(i in 1:R)
{ w=wvec[i]
term=0
for(n in  0:K)
{term1=choose(J,w)*(1-s^n)^w*s^(n*(J-w))
term2=lam^n*exp(-lam)/factorial(n)
term=term+term1*term2}
loglik[i]=log(term)
}
return(-sum(loglik))
}


## Binomial N-mixture model Likelihood
gM3BmT=function(param, J, R, Tmax, h, ymat)
{
  lam=exp(param[1])
  h=exp(param[2])
  loglik=0
  for(i in 1:R)
  {
    # set up yvec, p, lograt
    yvec=ymat[i,]
    p=1-exp(-h*Tmax[i,])
    lograt=sum(yvec*log(p/(1-p)))
    # now sort yvec
    yvec=sort(yvec)
    yij=yvec[-J]
    yiJ=yvec[J]
    term=sum(log(choose(yiJ,yij)))
    theta=lam*prod(1-p)
    hgfun=genhypergeo(rep(yiJ,(J-1)) + 1,yiJ - yij + 1,theta)
    term=sum(log(choose(yiJ,yij)))
    loglik=loglik-lam+lograt+yiJ*log(theta)+
      log(hgfun)+term-log(factorial(yiJ))
  }
  return(-loglik)
}

## Simulate the data
#  create_data is a function that simulate the three data types given a set of parameters
create_data = function(maxR, maxJ, lambda, h, numSim, tmax){
  #Create some storage for the data
  data = list()
  Tmax = matrix(tmax, nrow=maxR, ncol=maxJ)
  #Loop through the number of simulations required
  for(s in 1:numSim){
    #n_i is Poisson distributed
    ni = rpois(maxR, lambda = lambda)

    #Create matrix to store first arrival and count for each site and visit
    arrivals = matrix(NA, nrow=maxR, ncol=maxJ)
    counts = matrix(NA, nrow=maxR, ncol=maxJ)

    #Loop through sites
    for(i in 1:maxR){
      #if no individual set arrival to Tmax
      if(ni[i]==0){
        t_given_ni = rep(tmax,times = maxJ)
        counts[i,] = rep(0, times=maxJ)
      }else{
        t_given_ni = rep(NA,times = maxJ)
        for(j in 1:maxJ){
          # simulate n_i arrival times from an exponential with rate h
          visit_arrivals = rexp(ni[i], h)
          # Obtain the counts
          counts[i, j] = sum(visit_arrivals<tmax)
          # Sort list and select smallest value for time-to-first-detection
          t_given_ni[j] = sort(visit_arrivals)[1]
        }
        t_given_ni = replace(t_given_ni, t_given_ni>tmax, tmax)
      }
      #Set i-th row of arrival matrix to the above selected values
      arrivals[i,] = t_given_ni
    }
    # Obtain DND data from arrivals and convert to numeric
    detect_non_detect = arrivals<Tmax
    for(i in 1:maxR){
      detect_non_detect[i,] = as.numeric(detect_non_detect[i,])
    }

    #Store the arrival matrix, count matrix and detect/non-detect matrix
    this_sim = list(arrivals, detect_non_detect, counts)
    data[[s]] = this_sim
  }
  return (data)
}

## TTD_estimates takes in a data set made by create_data function and returns estimates
## of abundance and detection rate using the time-to-detection model
TTD_estimates = function(lambda, h, numSim, R, J, tmax, data){
  Tmax = matrix(tmax, nrow=R, ncol=J)
  # Create storage for the estimates
  lambda_est = rep(NA, numSim)
  h_est = rep(NA, numSim)
  # Choose data for specific simulation iteration
  this_data = data[1:numSim]
  for(s in 1:numSim){
    #Select the time-to-detection matrix
    arrivals = this_data[[s]][[1]]
    # If visits = 1 use gM2sT function
    if(J==1){
      tvec = arrivals[1:R,1]
      optim_ttd = optim(c(log(lambda),log(h)), gM2sT, method = "BFGS",
                        tvec=tvec, Tmax=Tmax)
    # If visits is greater that 1 use gM2mT fuction
    }else{
      tmat = arrivals[1:R,1:J]
      optim_ttd = optim(c(log(lambda),log(h)), gM2mT, method = "BFGS",
                        tmat=tmat, Tmax=Tmax, R=R, J=J)
    }
    lambda_est[s] = exp(optim_ttd$par[1])
    h_est[s] = exp(optim_ttd$par[2])
  }
  return(list(Lambda_Estimaties =lambda_est, h_estimates = h_est))
}

## DND_estimates takes in a data set made by create_data function and returns estimates
## of abundance and detection rate using the detection/non-detection model
DND_estimates = function(lambda, h, numSim, R, J, tmax, data, DND_func = "sum"){
  Tmax = matrix(tmax, nrow=R, ncol=J)
  #Create Storage
  lambda_est = rep(NA, numSim)
  h_est = rep(NA, numSim)
  # Choose data for specific simulation iteration
  this_data = data[1:numSim]
  for(s in 1:numSim){
    #Select the corrent number of rows and columns
    detect_non_detect = this_data[[s]][[2]][1:R,1:J]
    wvec = rowSums(detect_non_detect)
    # Use approximation to function
    if(DND_func=="sum"){
      optim_DND = optim(c(log(lambda),log(h)), gM1msum, method = "BFGS",
                        Tmax=Tmax, R=R, J=J, wvec=wvec, K=100)
    }
    # Use explicit function if specified
    if(DND_func=="explicit"){
      optim_DND = optim(c(log(lambda),log(h)), gM1mT, method = "BFGS",
                        Tmax=Tmax, R=R, J=J, wvec = wvec)
    }
    lambda_est[s] = exp(optim_DND$par[1])
    h_est[s] = exp(optim_DND$par[2])
  }
  return(list(Lambda_Estimaties =lambda_est, h_estimates = h_est))
}


## C_estimates takes in a data set made by create_data function and returns estimates
## of abundance and detection rate using the binomial N-mixture model (count model)
C_estimates = function(lambda, h, numSim, R, J, tmax, data){
  Tmax = matrix(tmax, nrow=R, ncol=J)
  # Create Storage
  lambda_est = rep(NA, numSim)
  h_est = rep(NA, numSim)
  # Select correct data
  this_data = data[1:numSim]
  for(s in 1:numSim){
    # Choose Count data and specific rows and columns
    ymat= this_data[[s]][[3]][1:R, 1:J]
    optim_counts = optim(c(log(lambda),log(h)), gM3BmT, method = "BFGS",
                         J=J, R=R, Tmax=Tmax, h=h, ymat=ymat)

    lambda_est[s] = exp(optim_counts$par[1])
    h_est[s] = exp(optim_counts$par[2])
  }
  return(list(Lambda_Estimaties =lambda_est, h_estimates = h_est))
}


##Simulation Function
# This was the initial function used to simulate data and estimates in one before the
# separate functions were created.
simulate = function(lambda, h, tmax, R, J, dataType, numSim, DND_func = "explicit"){
  lambda = lambda  #Abundance
  h = h              #Detection Rate
  R = R              #Number of Sites
  J = J            #Number of Visits
  tmax = tmax      #Length of visit

  #Create an R x J matrix with value tmax
  Tmax = matrix(tmax, nrow=R, ncol=J)

  #Create vectors to store abundance and detection prob estimates
  lambda_est = rep(NA, numSim)
  h_est = rep(NA, numSim)

  tic()

  for(s in 1:numSim){
    #print(s)
    #Ni is Poisson distributed
    ni = rpois(R, lambda = lambda)
    #Create matrix to store first arrival for each site and visit
    arrivals = matrix(NA, nrow=R, ncol=J)
    counts = matrix(NA, nrow=R, ncol=J)

    #Loop through site
    for(i in 1:R){
      #if no individual set arrival to Tmax
      if(ni[i]==0){
        t_given_ni = rep(tmax,times = J)
        counts[i,] = rep(0, times=J)
      }else{
        t_given_ni = rep(NA,times = J)
        for(j in 1:J){
          visit_arrivals = rexp(ni[i], h)
          counts[i, j] = sum(visit_arrivals<tmax)
          t_given_ni[j] = sort(visit_arrivals)[1]
        }
        t_given_ni = replace(t_given_ni, t_given_ni>tmax, tmax)
      }
      #Set i-th row of arrival matrix to the above selected values
      arrivals[i,] = t_given_ni
    }


    #If the data type used is TTD do the following
    if(dataType=="TTD"){
      #If J = 1 use tvec and gM2sT else use tmat and gM2mT
      if(J==1){
        tvec = arrivals[,1]
        optim_ttd = optim(c(log(lambda),log(h)), gM2sT, method = "BFGS",
                          tvec=tvec, Tmax=Tmax)
      }else{
        tmat = arrivals
        optim_ttd = optim(c(log(lambda),log(h)), gM2mT, method = "BFGS",
                          tmat=tmat, Tmax=Tmax, R=R, J=J)
      }
      lambda_est[s] = exp(optim_ttd$par[1])
      h_est[s] = exp(optim_ttd$par[2])
    }

    #If the data type used is DND do the following
    if(dataType=="DND"){
      detect_non_detect = arrivals<Tmax

      for(i in 1:R){
        detect_non_detect[i,] = as.numeric(detect_non_detect[i,])
      }
      #print(detect_non_detect)
      #set wvec and K
      wvec = rowSums(detect_non_detect)
      if(DND_func=="sum"){
        optim_DND = optim(c(log(lambda),log(h)), gM1msum, method = "BFGS",
                          Tmax=Tmax, R=R, J=J, wvec=wvec, K=100)
      }
      if(DND_func=="explicit"){
        optim_DND = optim(c(log(lambda),log(h)), gM1mT, method = "BFGS",
                          Tmax=Tmax, R=R, J=J, wvec = wvec)
      }

      lambda_est[s] = exp(optim_DND$par[1])
      h_est[s] = exp(optim_DND$par[2])
    }

    #If dataType is counts
    if(dataType=="count"){

      ymat = counts
      optim_counts = optim(c(log(lambda),log(h)), gM3BmT, method = "BFGS",
                           J=J, R=R, Tmax=Tmax, h=h, ymat=ymat)

      lambda_est[s] = exp(optim_counts$par[1])
      h_est[s] = exp(optim_counts$par[2])
    }
  }

  length_to_run_sim = toc()

  return(list(Lambda_Estimates = lambda_est,
              h_Estimates = h_est))
}

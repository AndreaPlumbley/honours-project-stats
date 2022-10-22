## Andrea Plumbley - Honours Project
## This R script runs code to check what the standard errors are of certain estimates

#Function has been modified from the TTD function in simA so that standard errors are also returned
TTD_estimates_wError = function(lambda, h, numSim, R, J, tmax, data){  
  Tmax = matrix(tmax, nrow=R, ncol=J)
  #Create Storage
  lambda_est = rep(NA, numSim)
  h_est = rep(NA, numSim)
  l_std_error = rep(NA, numSim)
  h_std_error = rep(NA, numSim)
  this_data = data[1:numSim]
  for(s in 1:numSim){
    arrivals = this_data[[s]][[1]]
    if(J==1){
      tvec = arrivals[1:R,1]
      optim_ttd = optim(c(log(lambda),log(h)), gM2sT, method = "BFGS",
                        tvec=tvec, Tmax=Tmax, hessian = TRUE)
    }else{
      tmat = arrivals[1:R,1:J]
      optim_ttd = optim(c(log(lambda),log(h)), gM2mT, method = "BFGS",
                        tmat=tmat, Tmax=Tmax, R=R, J=J, hessian = TRUE)
    }
    lambda_est[s] = exp(optim_ttd$par[1])
    h_est[s] = exp(optim_ttd$par[2])
    # Get that standard error values from the hessian matrix
    l_std_error[s] = sqrt(solve(optim_ttd$hessian))[1,1]
    h_std_error[s] = sqrt(solve(optim_ttd$hessian))[1,1]
  }
  return(list(Lambda_Estimaties =lambda_est, h_estimates = h_est, std_errors_l = l_std_error))
}

## Run and plot this for two cases
TTD2_stderr_eg1 = TTD_estimates_wError(lambda = 2, h =0.22, numSim = 1000, R =50, J=1,tmax=1, data = A2_h0.22)
TTD2_stderr_eg2 = TTD_estimates_wError(lambda = 5, h =2.3, numSim = 1000, R =100, J=3,tmax=1, data = A5_h2.3)
plot(TTD2_stderr_eg1$Lambda_Estimaties~TTD2_stderr_eg1$std_errors_l, 
     xlab = "Standard Error of Estimate", ylab = "Estimated Abundance", col="blue", ylim = c(0,6500))
plot(TTD2_stderr_eg2$Lambda_Estimaties~TTD2_stderr_eg2$std_errors_l, 
     xlab = "Standard Error of Estimate", ylab = "Estimated Abundance", col="blue", ylim=c(0,6500))

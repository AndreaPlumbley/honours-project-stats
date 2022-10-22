## Andrea Plumbley - Honours Project
## This R script is used to create the contour plot of probability of non-detection
## for a range of lambda and hT values. 

library(plotly)
library(simA)

#function for probability of non-detection
pr_non_detect = function(lambda, W)
{
  return(exp(-lambda*(1-exp(-W))))
}

lambda_seq = seq(0, 5, length = 20)
h_seq = seq(0, 3, length=20) 
pr_non_detects = outer(lambda_seq,h_seq, FUN = "pr_non_detect")

fig <- plot_ly(
  x = lambda_seq, 
  y = h_seq, 
  z = pr_non_detects, 
  type = "contour" ,
  contours = list(showlabels = TRUE)
)

fig <- fig %>% colorbar(title = "Probability of \nnon-detection")
fig <- fig %>% layout(xaxis = list(title="Lambda"), yaxis =list(title = "hT"))
fig

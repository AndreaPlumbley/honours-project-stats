## Andrea Plumbley - Honours Project
## This R script can be run to create the simulated data for the project and obtain
## estimates of abundance and detection rate using each of the different models. 
## It calls on the functions within the simA package
## Warning: Functions to obtain estimates can take quite long to run. 

#Call the required libraries
library(tictoc)
library(copula)
library(simA)
library(ggplot2)
library(scales)
library(ggpubr)
library(hypergeo)
library(grid)
set.seed(2022)

## Create the data for different cases using create_data() function
A2_h0.22 = create_data(maxR = 100, maxJ = 5, tmax = 1, lambda = 2, h=0.22, 
                       numSim = 1000)
A2_h0.69 = create_data(maxR = 100, maxJ = 5, tmax = 1, lambda = 2, h=0.69, 
                       numSim = 1000)
A2_h2.3 = create_data(maxR = 100, maxJ = 5, tmax = 1, lambda = 2, h=2.3, 
                      numSim = 1000)

A5_h0.22 = create_data(maxR = 100, maxJ = 5, tmax = 1, lambda = 5, h=0.22, 
                       numSim = 1000)
A5_h0.69 = create_data(maxR = 100, maxJ = 5, tmax = 1, lambda = 5, h=0.22, 
                       numSim = 1000)
A5_h2.3 = create_data(maxR = 100, maxJ = 5, tmax = 1, lambda = 5, h=0.22, 
                      numSim = 1000)

## Obtain estimates using simA functions
## Time-to-detection model estimates
##J = 1, R = 100, TTD
TTD2_1_100_0.22 = TTD_estimates(lambda = 2, h =0.22, numSim = 1000, R = 100, J=1,tmax=1, data = A2_h0.22)
TTD2_1_100_0.69 = TTD_estimates(lambda = 2, h =0.69, numSim = 1000, R = 100, J=1,tmax=1, data = A2_h0.69)
TTD2_1_100_2.3 = TTD_estimates(lambda = 2, h =2.3, numSim = 1000, R = 100, J=1,tmax=1, data = A2_h2.3)
TTD5_1_100_0.22 = TTD_estimates(lambda = 5, h =0.22, numSim = 1000, R = 100, J=1,tmax=1, data = A5_h0.22)
TTD5_1_100_0.69 = TTD_estimates(lambda = 5, h =0.69, numSim = 1000, R = 100, J=1,tmax=1, data = A5_h0.69)
TTD5_1_100_2.3 = TTD_estimates(lambda = 5, h =2.3, numSim = 1000, R = 100, J=1,tmax=1, data = A5_h2.3)

##J = 1, R = 50, TTD
TTD2_1_50_0.22 = TTD_estimates(lambda = 2, h =0.22, numSim = 1000, R = 50, J=1,tmax=1, data = A2_h0.22)
TTD2_1_50_0.69 = TTD_estimates(lambda = 2, h =0.69, numSim = 1000, R = 50, J=1,tmax=1, data = A2_h0.69)
TTD2_1_50_2.3 = TTD_estimates(lambda = 2, h =2.3, numSim = 1000, R = 50, J=1,tmax=1, data = A2_h2.3)
TTD5_1_50_0.22 = TTD_estimates(lambda = 5, h =0.22, numSim = 1000, R = 50, J=1,tmax=1, data = A5_h0.22)
TTD5_1_50_0.69 = TTD_estimates(lambda = 5, h =0.69, numSim = 1000, R = 50, J=1,tmax=1, data = A5_h0.69)
TTD5_1_50_2.3 = TTD_estimates(lambda = 5, h =2.3, numSim = 1000, R = 50, J=1,tmax=1, data = A5_h2.3)

##J = 3, R = 100, TTD
TTD2_3_100_0.22 = TTD_estimates(lambda = 2, h =0.22, numSim = 1000, R = 100, J=3,tmax=1, data = A2_h0.22)
TTD2_3_100_0.69 = TTD_estimates(lambda = 2, h =0.69, numSim = 1000, R = 100, J=3,tmax=1, data = A2_h0.69)
TTD2_3_100_2.3 = TTD_estimates(lambda = 2, h =2.3, numSim = 1000, R = 100, J=3,tmax=1, data = A2_h2.3)
TTD5_3_100_0.22 = TTD_estimates(lambda = 5, h =0.22, numSim = 1000, R = 100, J=3,tmax=1, data = A5_h0.22)
TTD5_3_100_0.69 = TTD_estimates(lambda = 5, h =0.69, numSim = 1000, R = 100, J=3,tmax=1, data = A5_h0.69)
TTD5_3_100_2.3 = TTD_estimates(lambda = 5, h =2.3, numSim = 1000, R = 100, J=3,tmax=1, data = A5_h2.3)

##J = 3, R = 50, TTD
TTD2_3_50_0.22 = TTD_estimates(lambda = 2, h =0.22, numSim = 1000, R = 50, J=3,tmax=1, data = A2_h0.22)
TTD2_3_50_0.69 = TTD_estimates(lambda = 2, h =0.69, numSim = 1000, R = 50, J=3,tmax=1, data = A2_h0.69)
TTD2_3_50_2.3 = TTD_estimates(lambda = 2, h =2.3, numSim = 1000, R = 50, J=3,tmax=1, data = A2_h2.3)
TTD5_3_50_0.22 = TTD_estimates(lambda = 5, h =0.22, numSim = 1000, R = 50, J=3,tmax=1, data = A5_h0.22)
TTD5_3_50_0.69 = TTD_estimates(lambda = 5, h =0.69, numSim = 1000, R = 50, J=3,tmax=1, data = A5_h0.69)
TTD5_3_50_2.3 = TTD_estimates(lambda = 5, h =2.3, numSim = 1000, R = 50, J=3,tmax=1, data = A5_h2.3)

##J = 5, R = 100, TTD
TTD2_5_100_0.22 = TTD_estimates(lambda = 2, h =0.22, numSim = 1000, R = 100, J=5,tmax=1, data = A2_h0.22)
TTD2_5_100_0.69 = TTD_estimates(lambda = 2, h =0.69, numSim = 1000, R = 100, J=5,tmax=1, data = A2_h0.69)
TTD2_5_100_2.3 = TTD_estimates(lambda = 2, h =2.3, numSim = 1000, R = 100, J=5,tmax=1, data = A2_h2.3)
TTD5_5_100_0.22 = TTD_estimates(lambda = 5, h =0.22, numSim = 1000, R = 100, J=5,tmax=1, data = A5_h0.22)
TTD5_5_100_0.69 = TTD_estimates(lambda = 5, h =0.69, numSim = 1000, R = 100, J=5,tmax=1, data = A5_h0.69)
TTD5_5_100_2.3 = TTD_estimates(lambda = 5, h =2.3, numSim = 1000, R = 100, J=5,tmax=1, data = A5_h2.3)

##J = 5, R = 50, TTD
TTD2_5_50_0.22 = TTD_estimates(lambda = 2, h =0.22, numSim = 1000, R = 50, J=5,tmax=1, data = A2_h0.22)
TTD2_5_50_0.69 = TTD_estimates(lambda = 2, h =0.69, numSim = 1000, R = 50, J=5,tmax=1, data = A2_h0.69)
TTD2_5_50_2.3 = TTD_estimates(lambda = 2, h =2.3, numSim = 1000, R = 50, J=5,tmax=1, data = A2_h2.3)
TTD5_5_50_0.22 = TTD_estimates(lambda = 5, h =0.22, numSim = 1000, R = 50, J=5,tmax=1, data = A5_h0.22)
TTD5_5_50_0.69 = TTD_estimates(lambda = 5, h =0.69, numSim = 1000, R = 50, J=5,tmax=1, data = A5_h0.69)
TTD5_5_50_2.3 = TTD_estimates(lambda = 5, h =2.3, numSim = 1000, R = 50, J=5,tmax=1, data = A5_h2.3)


##Detection/Non-detection model estimates
##J = 3, R = 100, DND
DND2_3_100_0.22 = DND_estimates(lambda = 2, h =0.22, numSim = 1000, R = 100, J=3,tmax=1, data = A2_h0.22, DND_func = "sum")
DND2_3_100_0.69 = DND_estimates(lambda = 2, h =0.69, numSim = 1000, R = 100, J=3,tmax=1, data = A2_h0.69, DND_func = "sum")
DND2_3_100_2.3 = DND_estimates(lambda = 2, h =2.3, numSim = 1000, R = 100, J=3,tmax=1, data = A2_h2.3, DND_func = "sum")
DND5_3_100_0.22 = DND_estimates(lambda = 5, h =0.22, numSim = 1000, R = 100, J=3,tmax=1, data = A5_h0.22, DND_func = "sum")
DND5_3_100_0.69 = DND_estimates(lambda = 5, h =0.69, numSim = 1000, R = 100, J=3,tmax=1, data = A5_h0.69, DND_func = "sum")
DND5_3_100_2.3 = DND_estimates(lambda = 5, h =2.3, numSim = 1000, R = 100, J=3,tmax=1, data = A5_h2.3, DND_func = "sum")

##J = 3, R = 50, DND
DND2_3_50_0.22 = DND_estimates(lambda = 2, h =0.22, numSim = 1000, R = 50, J=3,tmax=1, data = A2_h0.22, DND_func = "sum")
DND2_3_50_0.69 = DND_estimates(lambda = 2, h =0.69, numSim = 1000, R = 50, J=3,tmax=1, data = A2_h0.69, DND_func = "sum")
DND2_3_50_2.3 = DND_estimates(lambda = 2, h =2.3, numSim = 1000, R = 50, J=3,tmax=1, data = A2_h2.3, DND_func = "sum")
DND5_3_50_0.22 = DND_estimates(lambda = 5, h =0.22, numSim = 1000, R = 50, J=3,tmax=1, data = A5_h0.22, DND_func = "sum")
DND5_3_50_0.69 = DND_estimates(lambda = 5, h =0.69, numSim = 1000, R = 50, J=3,tmax=1, data = A5_h0.69, DND_func = "sum")
DND5_3_50_2.3 = DND_estimates(lambda = 5, h =2.3, numSim = 1000, R = 50, J=3,tmax=1, data = A5_h2.3, DND_func = "sum")

##J = 5, R = 100, DND
DND2_5_100_0.22 = DND_estimates(lambda = 2, h =0.22, numSim = 1000, R = 100, J=5,tmax=1, data = A2_h0.22, DND_func = "sum")
DND2_5_100_0.69 = DND_estimates(lambda = 2, h =0.69, numSim = 1000, R = 100, J=5,tmax=1, data = A2_h0.69, DND_func = "sum")
DND2_5_100_2.3 = DND_estimates(lambda = 2, h =2.3, numSim = 1000, R = 100, J=5,tmax=1, data = A2_h2.3, DND_func = "sum")
DND5_5_100_0.22 = DND_estimates(lambda = 5, h =0.22, numSim = 1000, R = 100, J=5,tmax=1, data = A5_h0.22, DND_func = "sum")
DND5_5_100_0.69 = DND_estimates(lambda = 5, h =0.69, numSim = 1000, R = 100, J=5,tmax=1, data = A5_h0.69, DND_func = "sum")
DND5_5_100_2.3 = DND_estimates(lambda = 5, h =2.3, numSim = 1000, R = 100, J=5,tmax=1, data = A5_h2.3, DND_func = "sum")

##J = 5, R = 50, DND
DND2_5_50_0.22 = DND_estimates(lambda = 2, h =0.22, numSim = 1000, R = 50, J=5,tmax=1, data = A2_h0.22, DND_func = "sum")
DND2_5_50_0.69 = DND_estimates(lambda = 2, h =0.69, numSim = 1000, R = 50, J=5,tmax=1, data = A2_h0.69, DND_func = "sum")
DND2_5_50_2.3 = DND_estimates(lambda = 2, h =2.3, numSim = 1000, R = 50, J=5,tmax=1, data = A2_h2.3, DND_func = "sum")
DND5_5_50_0.22 = DND_estimates(lambda = 5, h =0.22, numSim = 1000, R = 50, J=5,tmax=1, data = A5_h0.22, DND_func = "sum")
DND5_5_50_0.69 = DND_estimates(lambda = 5, h =0.69, numSim = 1000, R = 50, J=5,tmax=1, data = A5_h0.69, DND_func = "sum")
DND5_5_50_2.3 = DND_estimates(lambda = 5, h =2.3, numSim = 1000, R = 50, J=5,tmax=1, data = A5_h2.3, DND_func = "sum")

## Counts
## Some of these can sometimes result in errors occuring.
##J = 3, R = 100, C
C2_3_100_0.22 = C_estimates(lambda = 2, h =0.22, numSim = 1000, R = 100, J=3,tmax=1, data = A2_h0.22)
C2_3_100_0.69 = C_estimates(lambda = 2, h =0.69, numSim = 1000, R = 100, J=3,tmax=1, data = A2_h0.69)
C2_3_100_2.3 = C_estimates(lambda = 2, h =2.3, numSim = 1000, R = 100, J=3,tmax=1, data = A2_h2.3)
C5_3_100_0.22 = C_estimates(lambda = 5, h =0.22, numSim = 1000, R = 100, J=3,tmax=1, data = A5_h0.22)
C5_3_100_0.69 = C_estimates(lambda = 5, h =0.69, numSim = 1000, R = 100, J=3,tmax=1, data = A5_h0.69)
C5_3_100_2.3 = C_estimates(lambda = 5, h =2.3, numSim = 1000, R = 100, J=3,tmax=1, data = A5_h2.3)

##J = 3, R = 50, C
C2_3_50_0.22 = C_estimates(lambda = 2, h =0.22, numSim = 1000, R = 50, J=3,tmax=1, data = A2_h0.22)
C2_3_50_0.69 = C_estimates(lambda = 2, h =0.69, numSim = 1000, R = 50, J=3,tmax=1, data = A2_h0.69)
C2_3_50_2.3 = C_estimates(lambda = 2, h =2.3, numSim = 1000, R = 50, J=3,tmax=1, data = A2_h2.3)
C5_3_50_0.22 = C_estimates(lambda = 5, h =0.22, numSim = 1000, R = 50, J=3,tmax=1, data = A5_h0.22)
C5_3_50_0.69 = C_estimates(lambda = 5, h =0.69, numSim = 1000, R = 50, J=3,tmax=1, data = A5_h0.69)
C_3_50_2.3 = C_estimates(lambda = 5, h =2.3, numSim = 1000, R = 50, J=3,tmax=1, data = A5_h2.3)

##J = 5, R = 100, C
C2_5_100_0.22 = C_estimates(lambda = 2, h =0.22, numSim = 1000, R = 100, J=5,tmax=1, data = A2_h0.22)
C2_5_100_0.69 = C_estimates(lambda = 2, h =0.69, numSim = 1000, R = 100, J=5,tmax=1, data = A2_h0.69)
C2_5_100_2.3 = C_estimates(lambda = 2, h =2.3, numSim = 1000, R = 100, J=5,tmax=1, data = A2_h2.3)
C5_5_100_0.22 = C_estimates(lambda = 5, h =0.22, numSim = 1000, R = 100, J=5,tmax=1, data = A5_h0.22)
C5_5_100_0.69 = C_estimates(lambda = 5, h =0.69, numSim = 1000, R = 100, J=5,tmax=1, data = A5_h0.69)
C5_5_100_2.3 = C_estimates(lambda = 5, h =2.3, numSim = 1000, R = 100, J=5,tmax=1, data = A5_h2.3)

##J = 5, R = 50, C
C2_5_50_0.22 = C_estimates(lambda = 2, h =0.22, numSim = 1000, R = 50, J=5,tmax=1, data = A2_h0.22)
C2_5_50_0.69 = C_estimates(lambda = 2, h =0.69, numSim = 1000, R = 50, J=5,tmax=1, data = A2_h0.69)
C2_5_50_2.3 = C_estimates(lambda = 2, h =2.3, numSim = 1000, R = 50, J=5,tmax=1, data = A2_h2.3)
C5_5_50_0.22 = C_estimates(lambda = 5, h =0.22, numSim = 1000, R = 50, J=5,tmax=1, data = A5_h0.22)
C5_5_50_0.69 = C_estimates(lambda = 5, h =0.69, numSim = 1000, R = 50, J=5,tmax=1, data = A5_h0.69)
C5_5_50_2.3 = C_estimates(lambda = 5, h =2.3, numSim = 1000, R = 50, J=5,tmax=1, data = A5_h2.3)

#save.image("D:/Estimates.RData")
#load("D:/Estimates.RData")

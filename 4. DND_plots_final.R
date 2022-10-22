## Andrea Plumbley - Honours Project
## DND Model Plots.
## This code produces plots of the results that were created in the Create Data and Estimates R file. 
## This file should be run after the results have been loaded into the global environment.

library(ggplot2)
library(ggpubr)
library(grid)

## Create dataframe for boxplots
## Lambda = 2
DND2_R50_h0.22 = data.frame(Visits = c(rep(3,1000), rep(5,1000)),
                            Estimates = c(DND2_3_50_0.22$Lambda_Estimaties,
                                          DND2_5_50_0.22$Lambda_Estimaties))

DND2_R100_h0.22 = data.frame(Visits = c(rep(3,1000), rep(5,1000)),
                            Estimates = c(DND2_3_100_0.22$Lambda_Estimaties,
                                          DND2_5_100_0.22$Lambda_Estimaties))

DND2_R50_h0.69 = data.frame(Visits = c(rep(3,1000), rep(5,1000)),
                            Estimates = c(DND2_3_50_0.69$Lambda_Estimaties,
                                          DND2_5_50_0.69$Lambda_Estimaties))

DND2_R100_h0.69 = data.frame(Visits = c(rep(3,1000), rep(5,1000)),
                             Estimates = c(DND2_3_100_0.69$Lambda_Estimaties,
                                           DND2_5_100_0.69$Lambda_Estimaties))

DND2_R50_h2.3 = data.frame(Visits = c(rep(3,1000), rep(5,1000)),
                            Estimates = c(DND2_3_50_2.3$Lambda_Estimaties,
                                          DND2_5_50_2.3$Lambda_Estimaties))

DND2_R100_h2.3 = data.frame(Visits = c(rep(3,1000), rep(5,1000)),
                             Estimates = c(DND2_3_100_2.3$Lambda_Estimaties,
                                           DND2_5_100_2.3$Lambda_Estimaties))

## Lambda = 5
DND5_R50_h0.22 = data.frame(Visits = c(rep(3,1000), rep(5,1000)),
                            Estimates = c(DND5_3_50_0.22$Lambda_Estimaties,
                                          DND5_5_50_0.22$Lambda_Estimaties))

DND5_R100_h0.22 = data.frame(Visits = c(rep(3,1000), rep(5,1000)),
                             Estimates = c(DND5_3_100_0.22$Lambda_Estimaties,
                                           DND5_5_100_0.22$Lambda_Estimaties))

DND5_R50_h0.69 = data.frame(Visits = c(rep(3,1000), rep(5,1000)),
                            Estimates = c(DND5_3_50_0.69$Lambda_Estimaties,
                                          DND5_5_50_0.69$Lambda_Estimaties))

DND5_R100_h0.69 = data.frame(Visits = c(rep(3,1000), rep(5,1000)),
                             Estimates = c(DND5_3_100_0.69$Lambda_Estimaties,
                                           DND5_5_100_0.69$Lambda_Estimaties))

DND5_R50_h2.3 = data.frame(Visits = c(rep(3,1000), rep(5,1000)),
                           Estimates = c(DND5_3_50_2.3$Lambda_Estimaties,
                                         DND5_5_50_2.3$Lambda_Estimaties))

DND5_R100_h2.3 = data.frame(Visits = c(rep(3,1000), rep(5,1000)),
                            Estimates = c(DND5_3_100_2.3$Lambda_Estimaties,
                                          DND5_5_100_2.3$Lambda_Estimaties))


## Plots

# Function to calculate percentage of outlying points
outliers_funcD = function(data_frame, cutoff){
  visit_3_above_cutoff = length(data_frame[1:1000,2][data_frame[1:1000,2]>cutoff])
  visit_5_above_cutoff = length(data_frame[1001:2000,2][data_frame[1001:2000,2]>cutoff])
  return(c(visit_3_above_cutoff/10, visit_5_above_cutoff/10))
}

## Lambda = 2
Plot1D = ggplot(DND2_R50_h0.22, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous( limits = c(0,20))
P1_outliersD = outliers_funcD(DND2_R50_h0.22, 20)
P1_labelledD = Plot1D + annotate("text", x=0.8, y=20, label=paste(P1_outliersD[1],"%"),color="red", size=5) + 
  annotate("text", x=1.8, y=20, label=paste(P1_outliersD[2],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
P1_labelledD

Plot2D = ggplot(DND2_R50_h0.69, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,20))
P2_outliersD = outliers_funcD(DND2_R50_h0.69, 20)
P2_labelledD = Plot2D +
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
P2_labelledD

Plot3D = ggplot(DND2_R50_h2.3, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,20))
P3_outliersD = outliers_funcD(DND2_R50_h2.3, 20)
P3_labelledD = Plot3D + annotate("text", x=0.8, y=20, label=paste(P3_outliersD[1],"%"),color="red", size=5) + 
  annotate("text", x=1.8, y=20, label=paste(P3_outliersD[2],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
P3_labelledD

Plot4D = ggplot(DND2_R100_h0.22, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,20))
P4_outliersD = outliers_funcD(DND2_R100_h0.22, 20)
P4_labelledD = Plot4D + annotate("text", x=0.8, y=20, label=paste(P4_outliersD[1],"%"),color="red", size=5) +
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
P4_labelledD

Plot5D = ggplot(DND2_R100_h0.69, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,20))
P5_outliersD = outliers_funcD(DND2_R100_h0.69, 20)
P5_labelledD = Plot5D +
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
P5_labelledD

Plot6D = ggplot(DND2_R100_h2.3, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,20))
P6_outliersD = outliers_funcD(DND2_R100_h2.3, 20)
P6_labelledD = Plot6D  +
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
P6_labelledD

## Lambda = 5
Plot1D = ggplot(DND5_R50_h0.22, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous( limits = c(0,20))
P1_outliersD = outliers_funcD(DND5_R50_h0.22, 20)
P1_labelledD = Plot1D + annotate("text", x=0.8, y=20, label=paste(P1_outliersD[1],"%"),color="red", size=5) + 
  annotate("text", x=1.8, y=20, label=paste(P1_outliersD[2],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 5, col ="red")
P1_labelledD

Plot2D = ggplot(DND5_R50_h0.69, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,20))
P2_outliersD = outliers_funcD(DND5_R50_h0.69, 20)
P2_labelledD = Plot2D + annotate("text", x=0.8, y=20, label=paste(P2_outliersD[1],"%"),color="red", size=5) + 
  annotate("text", x=1.8, y=20, label=paste(P2_outliersD[2],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 5, col ="red")
P2_labelledD

Plot3D = ggplot(DND5_R50_h2.3, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,20))
P3_outliersD = outliers_funcD(DND5_R50_h2.3, 20)
P3_labelledD = Plot3D + annotate("text", x=0.8, y=20, label=paste(P3_outliersD[1],"%"),color="red", size=5) + 
  annotate("text", x=1.8, y=20, label=paste(P3_outliersD[2],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 5, col ="red")
P3_labelledD

Plot4D = ggplot(DND5_R100_h0.22, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,20))
P4_outliersD = outliers_funcD(DND5_R100_h0.22, 20)
P4_labelledD = Plot4D + annotate("text", x=0.8, y=20, label=paste(P4_outliersD[1],"%"),color="red", size=5) +
  annotate("text", x=1.8, y=20, label=paste(P4_outliersD[2],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 5, col ="red")
P4_labelledD

Plot5D = ggplot(DND5_R100_h0.69, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,20))
P5_outliersD = outliers_funcD(DND5_R100_h0.69, 20)
P5_labelledD = Plot5D + annotate("text", x=0.8, y=20, label=paste(P5_outliersD[1],"%"),color="red", size=5) +
  annotate("text", x=1.8, y=20, label=paste(P5_outliersD[2],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 5, col ="red")
P5_labelledD

Plot6D = ggplot(DND5_R100_h2.3, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,20))
P6_outliersD = outliers_funcD(DND5_R100_h2.3, 20)
P6_labelledD = Plot6D  + annotate("text", x=0.8, y=20, label=paste(P6_outliersD[1],"%"),color="red", size=5) +
  annotate("text", x=1.8, y=20, label=paste(P6_outliersD[2],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 5, col ="red")
P6_labelledD

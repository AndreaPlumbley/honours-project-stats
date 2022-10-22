## Andrea Plumbley - Honours Project
## TTD Model Plots.
## This code produces plots of the results that were created in the Create Data and Estimates R file. 
## This file should be run after the results have been loaded into the global environment.

library(ggplot2)
library(ggpubr)
library(grid)

#Create Data Frames for boxplots to be made
#Lambda=2
TTD2_R50_h0.22 = data.frame(Visits = c(rep(1,1000), rep(3,1000), rep(5,1000)),
                            Estimates = c(TTD2_1_50_0.22$Lambda_Estimaties, 
                                          TTD2_3_50_0.22$Lambda_Estimaties,
                                          TTD2_5_50_0.22$Lambda_Estimaties))

TTD2_R100_h0.22 = data.frame(Visits = c(rep(1,1000), rep(3,1000), rep(5,1000)),
                             Estimates = c(TTD2_1_100_0.22$Lambda_Estimaties, 
                                           TTD2_3_100_0.22$Lambda_Estimaties,
                                           TTD2_5_100_0.22$Lambda_Estimaties))

TTD2_R50_h0.69 = data.frame(Visits = c(rep(1,1000), rep(3,1000), rep(5,1000)),
                            Estimates = c(TTD2_1_50_0.69$Lambda_Estimaties, 
                                          TTD2_3_50_0.69$Lambda_Estimaties,
                                          TTD2_5_50_0.69$Lambda_Estimaties))

TTD2_R100_h0.69 = data.frame(Visits = c(rep(1,1000), rep(3,1000), rep(5,1000)),
                             Estimates = c(TTD2_1_100_0.69$Lambda_Estimaties, 
                                           TTD2_3_100_0.69$Lambda_Estimaties,
                                           TTD2_5_100_0.69$Lambda_Estimaties))

TTD2_R50_h2.3 = data.frame(Visits = c(rep(1,1000), rep(3,1000), rep(5,1000)),
                            Estimates = c(TTD2_1_50_2.3$Lambda_Estimaties, 
                                          TTD2_3_50_2.3$Lambda_Estimaties,
                                          TTD2_5_50_2.3$Lambda_Estimaties))

TTD2_R100_h2.3 = data.frame(Visits = c(rep(1,1000), rep(3,1000), rep(5,1000)),
                             Estimates = c(TTD2_1_100_2.3$Lambda_Estimaties, 
                                           TTD2_3_100_2.3$Lambda_Estimaties,
                                           TTD2_5_100_2.3$Lambda_Estimaties))

## Lambda = 5
TTD5_R50_h0.22 = data.frame(Visits = c(rep(1,1000), rep(3,1000), rep(5,1000)),
                            Estimates = c(TTD5_1_50_0.22$Lambda_Estimaties, 
                                          TTD5_3_50_0.22$Lambda_Estimaties,
                                          TTD5_5_50_0.22$Lambda_Estimaties))

TTD5_R100_h0.22 = data.frame(Visits = c(rep(1,1000), rep(3,1000), rep(5,1000)),
                             Estimates = c(TTD5_1_100_0.22$Lambda_Estimaties, 
                                           TTD5_3_100_0.22$Lambda_Estimaties,
                                           TTD5_5_100_0.22$Lambda_Estimaties))

TTD5_R50_h0.69 = data.frame(Visits = c(rep(1,1000), rep(3,1000), rep(5,1000)),
                            Estimates = c(TTD5_1_50_0.69$Lambda_Estimaties, 
                                          TTD5_3_50_0.69$Lambda_Estimaties,
                                          TTD5_5_50_0.69$Lambda_Estimaties))

TTD5_R100_h0.69 = data.frame(Visits = c(rep(1,1000), rep(3,1000), rep(5,1000)),
                             Estimates = c(TTD5_1_100_0.69$Lambda_Estimaties, 
                                           TTD5_3_100_0.69$Lambda_Estimaties,
                                           TTD5_5_100_0.69$Lambda_Estimaties))

TTD5_R50_h2.3 = data.frame(Visits = c(rep(1,1000), rep(3,1000), rep(5,1000)),
                           Estimates = c(TTD5_1_50_2.3$Lambda_Estimaties, 
                                         TTD5_3_50_2.3$Lambda_Estimaties,
                                         TTD5_5_50_2.3$Lambda_Estimaties))

TTD5_R100_h2.3 = data.frame(Visits = c(rep(1,1000), rep(3,1000), rep(5,1000)),
                            Estimates = c(TTD5_1_100_2.3$Lambda_Estimaties, 
                                          TTD5_3_100_2.3$Lambda_Estimaties,
                                          TTD5_5_100_2.3$Lambda_Estimaties))


## Plots

#Function to calculate number of outliers outside of plot
outliers_func = function(data_frame, cutoff){
  visit_1_above_cutoff = length(data_frame[1:1000,2][data_frame[1:1000,2]>cutoff])
  visit_3_above_cutoff = length(data_frame[1001:2000,2][data_frame[1001:2000,2]>cutoff])
  visit_5_above_cutoff = length(data_frame[2001:3000,2][data_frame[2001:3000,2]>cutoff])
  return(c(visit_1_above_cutoff/10, visit_3_above_cutoff/10, visit_5_above_cutoff/10))
}

#Lambda = 2
Plot1 = ggplot(TTD2_R50_h0.22, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,100))
P1_outliers = outliers_func(TTD2_R50_h0.22, 100)
P1_labelled = Plot1 + annotate("text", x=0.8, y=100, label=paste(P1_outliers[1],"%"),color="red", size=5) + 
  annotate("text", x=1.8, y=100, label=paste(P1_outliers[2],"%"),color="red", size=5) +
  annotate("text", x=2.8, y=100, label=paste(P1_outliers[3],"%"), color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
P1_labelled

Plot2 = ggplot(TTD2_R50_h0.69, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,100))+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
P2_outliers = outliers_func(TTD2_R50_h0.69, 100)
P2_labelled = Plot2 + annotate("text", x=0.8, y=100, label=paste(P2_outliers[1],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
P2_labelled

Plot3 = ggplot(TTD2_R50_h2.3, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,100))
P3_outliers = outliers_func(TTD2_R50_h2.3, 100)
P3_labelled = Plot3 + annotate("text", x=0.8, y=100, label=paste(P3_outliers[1],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))
P3_labelled

Plot4 = ggplot(TTD2_R100_h0.22, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,100))
P4_outliers = outliers_func(TTD2_R100_h0.22, 100)
P4_labelled = Plot4 + annotate("text", x=0.8, y=100, label=paste(P4_outliers[1],"%"),color="red", size=5) + 
  annotate("text", x=1.8, y=100, label=paste(P4_outliers[2],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
P4_labelled

Plot5 = ggplot(TTD2_R100_h0.69, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,100))+
  geom_hline(yintercept = 2, col ="red")
P5_outliers = outliers_func(TTD2_R100_h0.69, 100)
P5_labelled = Plot5 + annotate("text", x=0.8, y=100, label=paste(P5_outliers[1],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))
P5_labelled

Plot6 = ggplot(TTD2_R100_h2.3, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,100))+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))
P6_outliers = outliers_func(TTD2_R100_h2.3, 100)
P6_labelled = Plot6
P6_labelled

#Lambda =5

Plot1 = ggplot(TTD5_R50_h0.22, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous( limits = c(0,100))
P1_outliers = outliers_func(TTD5_R50_h0.22, 100)
P1_labelled = Plot1 + annotate("text", x=0.8, y=100, label=paste(P1_outliers[1],"%"),color="red", size=5) + 
  annotate("text", x=1.8, y=100, label=paste(P1_outliers[2],"%"),color="red", size=5) +
  annotate("text", x=2.8, y=100, label=paste(P1_outliers[3],"%"), color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 5, col ="red")
P1_labelled

Plot2 = ggplot(TTD5_R50_h0.69, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,100))
P2_outliers = outliers_func(TTD5_R50_h0.69, 100)
P2_labelled = Plot2 + annotate("text", x=0.8, y=100, label=paste(P2_outliers[1],"%"),color="red", size=5) + 
  annotate("text", x=1.8, y=100, label=paste(P2_outliers[2],"%"),color="red", size=5) +
  annotate("text", x=2.8, y=100, label=paste(P2_outliers[3],"%"), color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 5, col ="red")
P2_labelled

Plot3 = ggplot(TTD5_R50_h2.3, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,100))
P3_outliers = outliers_func(TTD5_R50_h2.3, 100)
P3_labelled = Plot3 + annotate("text", x=0.8, y=100, label=paste(P3_outliers[1],"%"),color="red", size=5) + 
  annotate("text", x=1.8, y=100, label=paste(P3_outliers[2],"%"),color="red", size=5) +
  annotate("text", x=2.8, y=100, label=paste(P3_outliers[3],"%"), color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 5, col ="red")
P3_labelled

Plot4 = ggplot(TTD5_R100_h0.22, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,100))
P4_outliers = outliers_func(TTD5_R100_h0.22, 100)
P4_labelled = Plot4 + annotate("text", x=0.8, y=100, label=paste(P4_outliers[1],"%"),color="red", size=5) + 
  annotate("text", x=1.8, y=100, label=paste(P4_outliers[2],"%"),color="red", size=5) +
  annotate("text", x=2.8, y=100, label=paste(P4_outliers[3],"%"), color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 5, col ="red")
P4_labelled

Plot5 = ggplot(TTD5_R100_h0.69, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,100))
P5_outliers = outliers_func(TTD5_R100_h0.69, 100)
P5_labelled = Plot5 + annotate("text", x=0.8, y=100, label=paste(P5_outliers[1],"%"),color="red", size=5) + 
  annotate("text", x=1.8, y=100, label=paste(P5_outliers[2],"%"),color="red", size=5) +
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 5, col ="red")
P5_labelled

Plot6 = ggplot(TTD5_R100_h2.3, aes(x=as.factor(Visits), y=Estimates)) + geom_boxplot() + 
  xlab("Number of Visits") +ylab(expression(lambda)) + scale_y_continuous( limits = c(0,100))
P6_outliers = outliers_func(TTD5_R100_h2.3, 100)
P6_labelled = Plot6 + annotate("text", x=0.8, y=100, label=paste(P6_outliers[1],"%"),color="red", size=5) + 
  annotate("text", x=1.8, y=100, label=paste(P6_outliers[2],"%"),color="red", size=5) +
  annotate("text", x=2.8, y=100, label=paste(P6_outliers[3],"%"), color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 5, col ="red")
P6_labelled

## Insets created by changing axis limits

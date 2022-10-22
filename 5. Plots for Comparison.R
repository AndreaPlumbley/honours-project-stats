## Andrea Plumbley - Honours Project
## Comparison of the three models plots.
## This code produces plots of the results that were created in the Create Data and Estimates R file. 
## This file should be run after the results have been loaded into the global environment.

library(ggplot2)

## Identify percentage of outliers above the top of plot
outliers_func_spec = function(data_frame, cutoff){
  visit_DND_above_cutoff = length(data_frame[1:1000,2][data_frame[1:1000,2]>cutoff])
  visit_TTD_above_cutoff = length(data_frame[1001:2000,2][data_frame[1001:2000,2]>cutoff])
  return(c(visit_DND_above_cutoff/10, visit_TTD_above_cutoff/10))
}

## Identify percentage of outliers above the top of plot
outliers_func_comp = function(data_frame, cutoff){
  visit_DND_above_cutoff = length(data_frame[1:1000,2][data_frame[1:1000,2]>cutoff])
  visit_TTD_above_cutoff = length(data_frame[1001:2000,2][data_frame[1001:2000,2]>cutoff])
  visit_C_above_cutoff = length(data_frame[2001:3000,2][data_frame[2001:3000,2]>cutoff])
  return(c(visit_DND_above_cutoff/10, visit_TTD_above_cutoff/10, visit_C_above_cutoff/10))
}

## Create Comparison data frames which contain the data types and estimates 
Comparison1 = data.frame(Type = c(rep("DND", 1000), rep("TTD", 1000), rep("COUNT", 1000)),
                         Estimates = c(DND2_3_50_0.22$Lambda_Estimaties,
                                       TTD2_3_50_0.22$Lambda_Estimaties,
                                       rep(NA, 1000)))

Comparison2 = data.frame(Type = c(rep("DND", 1000), rep("TTD", 1000), rep("COUNT", 1000)),
                         Estimates = c(DND2_3_50_0.69$Lambda_Estimaties,
                                       TTD2_3_50_0.69$Lambda_Estimaties,
                                       C2_3_50_0.69$Lambda_Estimaties))

Comparison3 = data.frame(Type = c(rep("DND", 1000), rep("TTD", 1000), rep("COUNT", 1000)),
                         Estimates = c(DND2_3_50_2.3$Lambda_Estimaties,
                                       TTD2_3_50_2.3$Lambda_Estimaties,
                                       C2_3_50_2.3$Lambda_Estimaties))

Comparison4 = data.frame(Type = c(rep("DND", 1000), rep("TTD", 1000), rep("COUNT", 1000)),
                         Estimates = c(DND2_3_100_0.22$Lambda_Estimaties,
                                       TTD2_3_100_0.22$Lambda_Estimaties,
                                       C2_3_100_0.22$Lambda_Estimaties))

Comparison5 = data.frame(Type = c(rep("DND", 1000), rep("TTD", 1000), rep("COUNT", 1000)),
                         Estimates = c(DND2_3_100_0.69$Lambda_Estimaties,
                                       TTD2_3_100_0.69$Lambda_Estimaties,
                                       C2_3_100_0.69$Lambda_Estimaties))

Comparison6 = data.frame(Type = c(rep("DND", 1000), rep("TTD", 1000), rep("COUNT", 1000)),
                         Estimates = c(DND2_3_100_2.3$Lambda_Estimaties,
                                       TTD2_3_100_2.3$Lambda_Estimaties,
                                       C2_3_100_2.3$Lambda_Estimaties))

## Create plots using ggplot
ComPlot1 = ggplot(Comparison1, aes(x=as.factor(Type), y=Estimates)) + geom_boxplot() + 
  xlab("Data Type") +ylab(expression(lambda)) + scale_y_continuous(limits=c(0,10))
ComPlot1_outliers = outliers_func_spec(Comparison1, 10)
ComPlot1_labelled = ComPlot1 + annotate("text", x=1.8, y=10, label=paste(ComPlot1_outliers[1],"%"),color="red", size=5) + 
  annotate("text", x=2.8, y=10, label=paste(ComPlot1_outliers[2],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
ComPlot1_labelled


ComPlot2 = ggplot(Comparison2, aes(x=as.factor(Type), y=Estimates)) + geom_boxplot() + 
  xlab("Data Type") +ylab(expression(lambda)) + scale_y_continuous(limits = c(0,10)) +
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
ComPlot2

ComPlot3 = ggplot(Comparison3, aes(x=as.factor(Type), y=Estimates)) + geom_boxplot() + 
  xlab("Data Type") +ylab(expression(lambda)) + scale_y_continuous(limits=c(0,10))+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
ComPlot3_outliers = outliers_func_comp(Comparison3, 10)
ComPlot3_labelled = ComPlot3 + 
  annotate("text", x=1.8, y=10, label=paste(ComPlot3_outliers[1],"%"),color="red", size=5)
ComPlot3_labelled


ComPlot4 = ggplot(Comparison4, aes(x=as.factor(Type), y=Estimates)) + geom_boxplot() + 
  xlab("Data Type") +ylab(expression(lambda)) + scale_y_continuous( limits=c(0,10))
ComPlot4_outliers = outliers_func_comp(Comparison4, 10)
ComPlot4_labelled = ComPlot4 + 
  annotate("text", x=0.8, y=10, label=paste(ComPlot4_outliers[3],"%"),color="red", size=5)+
  annotate("text", x=1.8, y=10, label=paste(ComPlot4_outliers[1],"%"),color="red", size=5)+
  annotate("text", x=2.8, y=10, label=paste(ComPlot4_outliers[2],"%"),color="red", size=5)+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
ComPlot4_labelled


ComPlot5 = ggplot(Comparison5, aes(x=as.factor(Type), y=Estimates)) + geom_boxplot() + 
  xlab("Data Type") +ylab(expression(lambda)) + scale_y_continuous(limits=c(0,10))+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
ComPlot5

ComPlot6 = ggplot(Comparison6, aes(x=as.factor(Type), y=Estimates)) + geom_boxplot() + 
  xlab("Data Type") +ylab(expression(lambda)) + scale_y_continuous(limits=c(0,10))+
  theme(axis.text =element_text(size=14),
        axis.title.y = element_text(size = 22),
        axis.title.x =element_text(size=14))+
  geom_hline(yintercept = 2, col ="red")
ComPlot6



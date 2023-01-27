library(ggplot2)
library(ggpubr)
library(tidyverse)
#In case we need to read an RDS or bunch them
#Resultscheck<- readRDS("C:/Users/pourfarajv/Desktop/Nosatlambda0.1dstep5.rds")


#-------Reading in all RDS file and combining them

densityimapctdf <- list.files( path =
'C:/Users/pourfarajv/Desktop/MLS70/', pattern = "*.rds", full.names = TRUE ) %>%
  map_dfr(readRDS)

### Plotting
ggplot(densityimapctdf, aes(x=factor(densitylambda), y=maxcatchno))+
  geom_boxplot() +
  stat_summary(fun = "mean",aes(colour="mean")) +
  ylab("Number of lobster caught per trap")+
  xlab("Lobsters density") +
  theme(panel.border = element_blank()) +
  ggtitle("Relationship between lobster density and the mean catch when no trap saturation")


ggplot(densityimapctdf, aes(x=factor(densitylambda), y=legalcatchwt))+
  geom_boxplot()+
  stat_summary(fun = "mean",aes(colour="mean")) +
  ylab("Weight of legal catch") +
  xlab("Lobsters density") +
  theme(panel.border = element_blank()) +
  ggtitle("Relationship between lobster density and the mean weight of legal catch when no trap saturation")


# to get summary stats
summarydf<- densityimapctdf %>%
  group_by(densitylambda) %>%
  summarise(mean = mean(maxcatchno))



#---- density and movement
densityANDdstep <- list.files( path =
'C:/Users/pourfarajv/Desktop/MLS70/') %>% 
  map_dfr(readRDS)

densityANDdstep$dstepmov <- as.factor(densityANDdstep$dstepmov)
densityANDdstep$densitylambda <- as.factor(densityANDdstep$densitylambda)

summarydf<- densityANDdstep %>%
  group_by( dstepmov) %>%
  summarise(mean = mean(maxcatchno))

ggplot(summarydf, aes(x = densitylambda, y = mean, group = dstepmov)) +
  geom_line(aes(linetype = dstepmov, color = dstepmov))+
  geom_point(aes(shape = dstepmov))+ ylab("Mean number of lobster caught per trap")+
  xlab("Lobsters density") +
  theme(panel.border = element_blank()) +
  ggtitle("Relationship between lobster density and lobster movement
          on the mean catch - No trap saturation")

ggplot(densityANDdstep, aes(x = dstepmov, y = maxcatchno, group = dstepmov)) +
  geom_boxplot()+
  stat_summary(fun = "mean",aes(colour="mean")) +
  ylab("Number of lobster in trap") +
  xlab("Lobsters movement") +
  theme(panel.border = element_blank()) +
  ggtitle("Relationship between lobster movement and number of sublegal lobster caught")

#### Adam's CPUE results
cpue<- readRDS ("tempCatchability.rds")

a<- ggplot(cpue, aes(x=temp, y=pred)) +
  geom_point(size=1, alpha=0.05) + xlab("Temperature") + ylab ("Catch rate") + ylim(0,1.5)

b<- ggplot(cpue, aes(x=temp, y=res)) +
  geom_point(size=1, alpha=0.05) + xlab("Temperature") + ylab ("Corrected catch rate") + ylim(0,1.5)

CPUE_plots<- ggarrange(a, b,
          labels = c("A", "B"), 
          ncol = 2, nrow = 1)

ggsave(filename = "desktop/CPUE.png", plot = CPUE_plots, width = 12, height = 10, dpi = 300, units = "cm")









library(ggplot2)
library(ggpubr)
library(tidyverse)
#In case we need to read an RDS or bunch them
#Resultscheck<- readRDS("C:/Users/pourfarajv/Desktop/Nosatlambda0.1dstep5.rds")


#-------Reading in all RDS file and combining them

NOSat_densityimapctdf <- list.files( path =
'C:/Users/pourfarajv/Desktop/Kumu_R_Visulization/AgentbasedModeling/lobsterCatch/resultsVP/densityimpact/NoSaturation/', pattern = "*.rds", full.names = TRUE ) %>%
  map_dfr(readRDS) %>%
  mutate(Trap_saturation= "No trap saturation")

Sat5_densityimapctdf <- list.files( path ='C:/Users/pourfarajv/Desktop/Kumu_R_Visulization/AgentbasedModeling/lobsterCatch/resultsVP/densityimpact/', pattern = "*.rds", full.names = TRUE ) %>%
  map_dfr(readRDS) %>%
  mutate(Trap_saturation= "5")

SatimpactDF<- bind_rows(Sat5_densityimapctdf,NOSat_densityimapctdf)

### Plotting


selecteddensity<- filter(SatimpactDF, densitylambda == 0.1 | densitylambda== 0.5 | densitylambda== 1 | densitylambda== 1.6)

ggplot(selecteddensity, aes(x=factor(densitylambda), y=maxcatchno)) +
  stat_summary(fun.y=mean,  geom="line", aes(group = Trap_saturation, color=Trap_saturation)) +
  stat_summary(fun.y=mean,  geom="point", aes(group = Trap_saturation, color=Trap_saturation)) +
  xlab("Lobster density") +
  theme(panel.border = element_blank()) +
  scale_y_continuous(name = "Mean cath (no lobster per trap)", limits = c(0,25))  +
  ggtitle("Fig 4 of Addison & Bell paper replicated using 3 traps")


stat_summary(fun = "mean", geom = "point") +

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









################################################################################
## Pot versus longline survey comparison 2023
## This script examines and compares the side-by-side fishing of pot
## and longline gear for use when then the survey is switched from a longline 
## survey to a pot survey, keeping in line with the change in the fishery
##
## Author: Phil Joy
##
################################################################################

{library(mosaic)
library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggridges)
library(ggthemes)
library(GGally)
library(mgcViz)
library(mgcv)
library(lme4)}

ll_bio<-read.csv("data/survey/2023 SSEI LL Survey Bio Data.csv")
ll_set<-read.csv("data/survey/2023 SSEI LL Survey Set Data.csv")
pot<-read.csv("data/survey/SSEI Pot Survey Data 2023.csv")

ll_bio<-left_join(ll_bio %>% 
                select(Trip.No,Adfg.No,Effort.No,Station.No,G.Stat.Area,Species,Specimen.No,
                       Sex, Maturity, Length = Length.Millimeters, Weight = Weight.Kilograms),
              unique(ll_set %>% group_by(Set.No) %>% 
                       mutate(Sable.set.cpue = sum(Sablefish)) %>% 
                       #group_by(Subset.No) %>%
                       #mutate(Sable.skate.cpue = sum(Sablefish)) %>% 
                       ungroup() %>%
                select(Trip.No, Adfg.No, Effort.No = Set.No, G.Stat.Area, Station.No, 
                       Start.lat = Start.Latitude.Decimal.Degrees,
                       Start.Lon = Start.Longitude.Decimal.Degree,
                       Soak = Set.Soak.Time, Depth = Avg.Depth.Fathoms,
                       Substrate = Substrate.Type, Sable.set.cpue) ),
              by = c("Trip.No", "Adfg.No", "Effort.No", "Station.No", "G.Stat.Area")) %>%
  mutate(Gear = "ll")

head(ll_bio,30)
ll_bio[c(30:60),]
ll_bio %>% filter(Effort.No == 4)
plot(ll_bio$Sablefish.No ~ ll_bio$Effort.No)

ll_cpue<-ll_set %>% group_by(Set.No) %>% 
  mutate(Sable.set.cpue = sum(Sablefish),
         Sleeper.Shark.set.cpue = sum(Sleeper.Shark), 
         Skate.Longnose.set.cpue = sum(Skate.Longnose), 
         Skate.General.set.cpue = sum(Skate.General),
         Skate.Big.set.cpue = sum(Skate.Big), 
         Shortraker.set.cpue = sum(Shortraker), 
         Rougheye.set.cpue = sum(Rougheye), 
         Rockfish.Other.set.cpue = sum(Rockfish.Other), 
         Rex.Sole.set.cpue = sum(Rex.Sole),
         Redbanded.set.cpue = sum(Redbanded), 
         Pacific.Cod.set.cpue = sum(Pacific.Cod), 
         Other.set.cpue = sum(Other), 
         Idiot.set.cpue = sum(Idiot), 
         Grenadier.set.cpue = sum(Grenadier), 
         English.Sole.set.cpue = sum(English.Sole), 
         Dover.Sole.set.cpue = sum(Dover.Sole), 
         Dogfish.set.cpue = sum(Dogfish), 
         Coral.set.cpue = sum(Coral), 
         Brown.King.Crab.set.cpue = sum(Brown.King.Crab), 
         Arrowtooth.set.cpue = sum(Arrowtooth), 
         Yelloweye.set.cpue = sum(Yelloweye), 
         Halibut.set.cpue = sum(Halibut)) %>% ungroup() %>% 
  #summarize()
  select(Trip.No, Adfg.No, Effort.No = Set.No, Subset.No, G.Stat.Area, Station.No, 
         Start.lat = Start.Latitude.Decimal.Degrees,
         Start.Lon = Start.Longitude.Decimal.Degree,
         Soak = Set.Soak.Time, Depth = Avg.Depth.Fathoms,
         Substrate = Substrate.Type,
         Sablefish.skate.cpue = Sablefish, Sable.set.cpue,
         Sleeper.Shark.skate.cpue = Sleeper.Shark, Sleeper.Shark.set.cpue,
         Skate.Longnose.skate.cpue = Skate.Longnose, Skate.Longnose.set.cpue,
         Skate.General.skate.cpue = Skate.General, Skate.General.set.cpue,
         Skate.Big.skate.cpue = Skate.Big, Skate.Big.set.cpue,
         Shortraker.skate.cpue = Shortraker, Shortraker.set.cpue,
         Rougheye.skate.cpue = Rougheye, Rougheye.set.cpue, 
         Rockfish.Other.skate.cpue = Rockfish.Other, Rockfish.Other.set.cpue,
         Rex.Sole.skate.cpue = Rex.Sole, Rex.Sole.set.cpue,
         Redbanded.skate.cpue = Redbanded, Redbanded.set.cpue, 
         Pacific.Cod.skate.cpue = Pacific.Cod, Pacific.Cod.set.cpue,
         Other.skate.cpue = Other, Other.set.cpue,
         Idiot.skate.cpue = Idiot, Idiot.set.cpue, 
         Grenadier.skate.cpue = Grenadier, Grenadier.set.cpue, 
         English.Sole.skate.cpue = English.Sole, English.Sole.set.cpue, 
         Dover.Sole.skate.cpue = Dover.Sole, Dover.Sole.set.cpue, 
         Dogfish.skate.cpue = Dogfish, Dogfish.set.cpue, 
         Coral.skate.cpue = Coral, Coral.set.cpue, 
         Brown.King.Crab.skate.cpue = Brown.King.Crab, Brown.King.Crab.set.cpue, 
         Arrowtooth.skate.cpue = Arrowtooth, Arrowtooth.set.cpue, 
         Yelloweye.skate.cpue = Yelloweye, Yelloweye.set.cpue, 
         Halibut.skate.cpue = Halibut, Halibut.set.cpue)


str(pot)
unique(pot$Species)
pot<-pot %>% select(Trip.No = Trip.Number, Adfg.No = ADFG.Number,
                    Effort.No = Effort.Number,
                    Subset.No = Pot.Number.in.Order.Set,
                    G.Stat.Area = Groundfish.Stat.Area,
                    Station.No = Station.Number,
                    Start.lat = Start.Latitude.Decimal.Degrees,
                    Start.Lon = Start.Longitude.Decimal.Degrees,
                    Soak = Soak.Time, Depth = Average.Depth.Fathoms,
                    No.Pots = Number.of.Pots.Retrieved,
                    Pot.Type, Pot.Type.1, Substrate = Substrate.Type,
                    Specimen.No = Specimen.Number, Species, 
                    Length = Length.Millimeters) %>% 
  mutate(Gear = "pot")

str(ll_bio)
str(pot)

#-------------------------------------------------------------------------------

bios<-rbind(ll_bio %>% select(-c(Sable.set.cpue, Weight, Sex, Maturity)) %>%
                                mutate(Pot.Type = "Longline",
                                       Pot.Type.1 = "Longline") %>%
                                filter(Species == "Sablefish"),
            pot %>% select(-c(No.Pots, Subset.No)) %>% filter(Species == "Sablefish"))  
  

cpue<-rbind(ll_cpue %>% select(Trip.No, Adfg.No, Effort.No, Subset.No, G.Stat.Area,
                               Station.No, Start.lat, Start.Lon, Soak, Depth, Substrate,
                               set.cpue = Sable.set.cpue, sub.cpue = Sablefish.skate.cpue) %>%
              mutate(Gear = "ll", Pot.Type = "ll", Pot.Type.1 = "Longline", No.subs = 25, Gear = "ll") %>%
              data.frame(),
            unique(pot %>% select(-c(Specimen.No,Length)) %>%
                     group_by(Effort.No) %>% mutate(set.cpue = n()) %>%
                     group_by(Effort.No, Subset.No) %>% mutate(sub.cpue = n(),
                                                               No.subs = No.Pots) %>% 
                     select(-c(No.Pots, Species)) %>% data.frame())) %>%
  mutate(soak.time = hm(Soak)@hour+hm(Soak)@minute/60)

str(cpue)

eg<-cpue %>% filter(Station.No == 12)
View(eg)
#-------------------------------------------------------------------------------
# cpue by skate/pot
#cpue_comp %>% group_by(Pot.Type.1) %>%
#  dplyr::summarize(sub_samples = n(),
#                   mean_cpue = mean(sub.cpue))
cpue_comp<-cpue %>% filter(Station.No %in% c(12,37,44,103,107,109,120,122,125))

cpue_comp %>% group_by(Station.No,Pot.Type.1) %>% 
  dplyr::summarize(sub_samples = n(),
                   mean_cpue = mean(sub.cpue),
                   std = sd(sub.cpue)) -> pot_skate_cpue

pot_skate_cpue %>% group_by(Pot.Type.1) %>%
  dplyr::summarize(sets = n(),
                   cpue = mean(mean_cpue),
                   mean_std = sqrt(sum(std^2)/sets),
                   cpue_lo95 = cpue - 1.96*mean_std,
                   cpue_hi95 = cpue + 1.96*mean_std) -> mean_potskate_cpue

cpue_comp %>% group_by(Station.No,Gear) %>% 
  dplyr::summarize(sub_samples = n(),
                   cpue = mean(set.cpue)) -> set_cpue

cpue_comp %>% group_by(Station.No,Pot.Type.1) %>% 
  dplyr::summarize(sub_samples = n(),
                   cpue = mean(set.cpue)) -> set_cpue_det

#---------------------------------------------------------------------------


colnames(bios)

cols <- c("#F76D5E", "#FFFFBF", "#72D8FF")

# Basic density plot in ggplot2
ggplot(bios, aes(x = Length, fill = Gear)) +
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = cols) + 
  geom_vline(xintercept=mean(bios$Length[bios$Gear == "ll"]), col = cols[1]) +
  geom_vline(xintercept=mean(bios$Length[bios$Gear == "pot"], na.rm=T), col = cols[2]) 

getwd()
ggsave(paste0("figures/2023/length_distr_pot_vs_ll.png"), dpi = 300, height = 5, width = 5, units = "in")

ggplot(bios, aes(x = Length, fill = Pot.Type.1)) +
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = cols) +
  geom_vline(xintercept=mean(bios$Length[bios$Pot.Type.1 == "Lg. Slinky"], na.rm=T), col = cols[1]) +
  geom_vline(xintercept=mean(bios$Length[bios$Pot.Type.1 == "Longline"], na.rm=T), col = cols[2]) +
  geom_vline(xintercept=mean(bios$Length[bios$Pot.Type.1 == "Sm. Slinky"], na.rm=T), col = cols[3])

ggsave(paste0("figures/2023/length_distr_pot_vs_ll_2.png"), dpi = 300, height = 5, width = 5, units = "in")

colnames(cpue)

ggplot(cpue,aes(x=set.cpue, col = Gear, fill = Gear)) + 
  geom_density(alpha=0.5) + #, aes(y=..count../sum(..count..))) +
  xlab("set cpue")
ggplot(cpue,aes(x=sub.cpue, col = Gear, fill = Gear)) + 
  geom_density(alpha=0.5) + #, aes(y = stat(count / sum(count)))) +
  xlab("Skate/Pot cpue")
ggplot(cpue,aes(x=sub.cpue, col = Pot.Type.1, fill = Pot.Type.1)) + 
  #geom_histogram(alpha=0.5) +
  geom_density(alpha=0.5) +
  xlab("Skate/Pot cpue")

ggplot(cpue_comp,aes(x=sub.cpue, col = Gear, fill = Gear)) + geom_histogram(alpha=0.5) +
  xlab("Skate/Pot cpue")
ggplot(cpue_comp,aes(x=sub.cpue, col = Pot.Type.1, fill = Pot.Type.1)) + geom_histogram(alpha=0.5) +
  xlab("Skate/Pot cpue")


ggplot(data=cpue_comp, aes(x=Gear, y=set.cpue, color = Gear, fill = Gear)) + 
  geom_bar(stat = "summary", fun = "mean") +
  ylab("Fish per set") +
  facet_wrap(~Station.No)

ggsave(paste0("figures/2023/cpue_pot_vs_ll_by_station.png"), dpi = 300, height = 5, width = 5, units = "in")

ggplot(data=cpue_comp, mapping=aes(x=Pot.Type.1 , y=sub.cpue, color = Pot.Type.1, fill = Pot.Type.1))+
  geom_bar(stat = "summary", fun = "mean") +
  xlab("Fish per skate/pot") +
  facet_wrap(~Station.No)

ggsave(paste0("figures/2023/cpue_pot_vs_ll_by_station2.png"), dpi = 300, height = 5, width = 5, units = "in")

ggplot(data=set_cpue, mapping=aes(x=Gear, y=cpue, fill = Gear),alpha=0.7)+
  geom_boxplot() +
  #geom_violin() +
  geom_jitter(width=0.1, height=0, col="black", size=2, alpha=0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per set")

ggsave(paste0("figures/2023/cpue_byset_pot_vs_ll.png"), dpi = 300, height = 5, width = 5, units = "in")


#ggplot(data=cpue_comp, mapping=aes(x=set.cpue, y=Pot.Type.1, fill = Pot.Type.1))+geom_boxplot() +
#  stat_summary(fun = "mean", geom = "point", shape = 8,
#               size = 2, color = "black")  +
#  coord_flip() + xlab("Fish per set")

#ggplot(data=cpue_comp, mapping=aes(x=sub.cpue, y=Gear, fill = Gear))+geom_boxplot() +
#  stat_summary(fun = "mean", geom = "point", shape = 8,
#               size = 2, color = "black")  +
#  coord_flip() + xlab("Fish per skate or pot")

ggplot(data=pot_skate_cpue, mapping=aes(x=Pot.Type.1, y=mean_cpue, fill = Pot.Type.1))+
  geom_boxplot(outlier.shape=NA) +
  #geom_violin()+
  geom_jitter(width=0.1, height=0, col="black", size=2, alpha=0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per skate or pot")           
ggsave(paste0("figures/2023/cpue_byset_2pot_vs_ll.png"), dpi = 300, height = 5, width = 5, units = "in") 

ggplot(data=cpue_comp, mapping=aes(x=Gear, y=set.cpue, fill = Gear))+
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per set") +
  facet_wrap(~Substrate)

ggplot(data=cpue_comp, mapping=aes(x=Gear, y=set.cpue, fill = Gear))+geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  xlab("Fish per set") +
  facet_wrap(~G.Stat.Area)

ggplot(data=cpue_comp, mapping=aes(x=sub.cpue, y=Pot.Type.1, fill = Pot.Type.1))+geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black")  +
  coord_flip() + xlab("Fish per skate/pot") +
  facet_wrap(~G.Stat.Area)

#ggplot(cpue_comp, aes(soak.time, sub.cpue, col=Pot.Type.1, fill = Pot.Type.1)) + geom_point(shape = 2) + 
#  geom_smooth(size = 2, se = TRUE, method = "glm")  #"lm", "glm", "gam", "loess"

#ggplot(cpue_comp, aes(Depth, sub.cpue, col=Pot.Type.1, fill = Pot.Type.1)) + geom_point(shape = 2) + 
#  geom_smooth(size = 2, se = TRUE, method = "loess")

ggplot(cpue_comp, aes(soak.time, set.cpue, col=Gear, fill = Gear)) + geom_point(shape = 2) + 
  geom_smooth(size = 2, se = TRUE, method = "glm")  #"lm", "glm", "gam", "loess"

ggplot(cpue_comp, aes(Depth, set.cpue, col=Gear, fill = Gear)) + geom_point(shape = 2) + 
  geom_smooth(size = 2, se = TRUE, method = "glm")

plot(data=cpue_comp, soak.time ~ Depth); abline(lm(data=cpue_comp, soak.time ~ Depth))

mod<-bam(data = cpue_comp, set.cpue ~ s(G.Stat.Area, bs='re') + s(Station.No, bs = 're') +
           s(soak.time, k=3, m=1) + s(Depth, k=3) + Pot.Type.1,
         gamma=1.4)
summary(mod) 
plot(mod)


#mod<-lmer(data = cpue_comp, sub.cpue ~ s(G.Stat.Area, bs='re') +
#           s(soak.time, k=3, m=1) + s(Depth, k=3) + Pot.Type.1)

lmer <-lmer(data = cpue_comp, set.cpue ~ soak.time*Depth*Pot.Type.1*(1|G.Stat.Area))
help('isSingular')
summary(lmer)
anova(lmer)

{library(arm)
library(plyr)
library(MuMIn)   #r.squaredGLMM(object)

library(glmulti)
library(AICcmodavg)}


lmer.glmulti <- function (formula, data, random = "", ...) {
  lmer(paste(deparse(formula), random), data = data, REML=F, ...)
}

setMethod('getfit', 'merMod', function(object, ...) {
  summ<-summary(object)$coef
  summ1<-summ[,1:2]
  if (length(dimnames(summ)[[1]])==1) {
    summ1<-matrix(summ1, nr=1, dimnames=list(c("(Intercept)"),c("Estimate","Std.Error")))
  }
  cbind(summ1, df=rep(10000,length(fixef(object))))
})

runmodels<-glmulti(sub.cpue ~ soak.time*Depth*Pot.Type.1,#s(set_depth, k=4), 
                       data=cpue_comp,
                       method="h",imm=0.5, sexrate=0.1, crit=aicc , deltaM=0.01, 
                       conseq=20, marginality=T, maxsize=5, confsetsize=17, plotty=T,
                       level=2, fitfunc=lmer.glmulti, random="+(1|G.Stat.Area)+(1|Station.No)")
coef.glmulti(runmodels)
plot(runmodels,type="s")

summary(runmodels)
summary(runmodels)$bestmodel

runmodels@objects


simp<-glmulti(sub.cpue ~ soak.time*Depth*Pot.Type.1,#s(set_depth, k=4), 
                   data=cpue_comp,
                   method="h",imm=0.5, sexrate=0.1, crit=aicc , deltaM=0.01, 
                   conseq=20, marginality=T, maxsize=5, confsetsize=17, plotty=T,
                   level=1, fitfunc=lmer.glmulti, random="+(1|G.Stat.Area)+(1|Station.No)")
coef.glmulti(simp)
plot(simp,type="s")

summary(simp)
summary(simp)$bestmodel

simp@objects
  


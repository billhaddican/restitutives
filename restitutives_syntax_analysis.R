library(ggplot2)
library(lme4)
library(lmerTest)
library(Rmisc)
library(RColorBrewer)

#1. LOAD DATA ##################################################################

d<-read.csv("restitutives_syntax_data.csv")

#2. MODEL ######################################################################

dat<-subset(d, Order!="Filler") #separate out experimental items
dat$Order<-factor(dat$Order)
dat$Order<-relevel(dat$Order, ref="VPO")
m<-lmer(Value ~ Bias + Order +  (1 + Bias | Participant) + (1 + Bias | Lexicalization), data=dat)

#3. ERROR BAR PLOT #############################################################

dat<-subset(d, Order!="Filler") #separate out experimental items
dat$Order<-as.character(dat$Order)
dat$Bias<-as.character(dat$Bias)
dat$Bias<-ifelse(dat$Bias=="Rep", "Repetitive", "Restitutive")
dat$Order[dat$Order=="U"]<-"Ungrammatical"

dfX = summarySE(dat, measurevar = "Value", groupvars = c("Bias", "Order"))
dfX$min = dfX$Value-dfX$ci
dfX$max = dfX$Value+dfX$ci
ggplot(dfX, aes(x=Bias, y=Value, colour=Order)) +
  geom_point(position=position_dodge(.5))+
  ylab("Rating") + 
  xlab("Bias")+
  geom_errorbar(aes(ymin=Value-ci, ymax=Value+ci), width=0, size=1, position=position_dodge(.5))+
  theme_bw()+
  ylim(0,100)+
  scale_colour_manual(values=c("#999999", "#E69F00", "#0072B2"))


#4. FILLER DISTRIBUTION PLOT  ##################################################

f<-subset(d, Order=="Filler") #separate out filler items
f<-droplevels(f)
f$Value<-as.numeric(f$Value)
f$Bias<-as.character(f$Bias)
ff = summarySE(f, measurevar = "Value", groupvars = c("Bias"))
ff$min = ff$Value-ff$ci
ff$max = ff$Value+ff$ci
ggplot(ff, aes(x=Bias, y=Value)) +
  geom_point()+
  ylab("Rating") + 
  xlab("Bias")+
  geom_errorbar(aes(ymin=Value-ci, ymax=Value+ci), width=0, size=1)+
  theme_bw()+
  ylim(0,100)


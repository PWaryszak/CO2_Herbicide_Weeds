#For Loop to produce binary data frame===========
#install.packages(c("survival", "survminer","tidyverse"))
library("tidyverse")
library("survival")
library("survminer")

#Week1 survival
#SurvivalData <- read.csv("survivalFIXED.csv")
#str(SurvivalData)#data.frame':	96 obs. of  15 variables:
#head(SurvivalData)
#DATA CLEANING of survival2.csv (SurvivalFIXED is a clean version of survival2 & should be good to go):
SurvivalData$Week1Survived<- SurvivalData$Week1-SurvivalData$Week2
range(SurvivalData$Week1Survived)#-1  6
#Commnent Week 1 reported less seedlings than week 2 in two cases (9 instead of 10), so For loop does not work.
#1= ambient.recom.glyphosate.Bromus.catharticus
#2= elevated.Control.Zero.Tradescantia.fluminensis (controls = all must be 8 but were 9&7 instances - double checked with raw data in Tanja & Pawel Herbicide Excel file)
#Because Tanja thought they were not sprayed. I changed back to 10

SurvivalData$Week2Survived<- SurvivalData$Week1-SurvivalData$Week3
range(SurvivalData$Week2Survived)# 0 10 = All good!

SurvivalData$Week3Survived<- SurvivalData$Week1-SurvivalData$Week4
range(SurvivalData$Week3Survived)#NA NA fixing data:
#ambient.double.glyphosate.Ageratina.adenophora = week5 & week6 should be = 0 not NA or 8.
#elevated.double.glyphosate.Ehrharta.erecta = week4 & week5 & week6 = 4

SurvivalData$Week4Survived<- SurvivalData$Week1-SurvivalData$Week5
range(SurvivalData$Week4Survived)#NA NA fixing data:
#ambient.double.glyphosate.Verbena.bonariensis = NA = 0, Week6 = 0 too fixed

SurvivalData$Week5Survived<- SurvivalData$Week1-SurvivalData$Week6
range(SurvivalData$Week5Survived)#  0 10 = ALL GOOD!
View(SurvivalData)

#remove controls!!!!!!!!!!!!!!!!
#All controls survived (28 rows of controls removed)
#NEW FILE = WeedSurvival.csv

#LOAD DATA=========
SurvivalData <- read.csv("WeedSurvival.csv")
str(SurvivalData)#data.frame':68 obs. of  15 variables:

#Survival = Status in Kaplan-Meier way of understanding
#where 0 = no death (right-censored), 1 = death

#Week1 to Week2==========
#First Data Frame on total input
in.1<-SurvivalData[,1:10]#first frame (number of herbicide-treated)
str(in.1)#68 obs. of  11 variables:
in.1$count<-in.1$Week1 #this count of plants treated in week 1
head(in.1)

#Second Data Frame on total survived after week 1.
out.2<-SurvivalData[,c(1:9,11)]#second frame (number of survived till week2)
str(out.2)#	68 obs. of  11 variables:
out.2$count<-out.2$Week2

Species2 <- as.character(unique(in.1$Species))# n of species x treatments
Species2 # length = 96
Output <- NULL
for (X in 1:length(Species2)) {
  ind <- which(in.1$Species == Species2[X])
  N1 <- in.1$count[ind]
  N2 <- out.2$count[ind]
  Died <- N1 - N2
  Surv <- N1 - Died
  NSpecies <- rep( Species2[X], N1)
  NSurv <- rep(0, Surv)
  NDied <- rep(1, Died)
  saveoutput <- data.frame(Species2=NSpecies, time= "1Week", Status=c(NSurv, NDied))
  Output <- rbind(Output, saveoutput)
}

Output
str(Output)
head(Output)
Out1weeks<-Output
Out1weeks$Week<-1
head(Out1weeks)


#Week1 to Week3==============
#First Data Frame on total input
in.1<-SurvivalData[,1:10]#first frame (total number of herbicide-treated plants, always the same)
str(in.1)#68 obs. of  11 variables:
in.1$count<-in.1$Week1 #this count of plants treated in week 1
head(in.1)

#Second Data Frame on total survived after week 1.
out.2<-SurvivalData[,c(1:9,12)]#second frame (number of survived till week3)
str(out.2)#	68 obs. of  11 variables:
out.2$count<-out.2$Week3

Species2 <- as.character(unique(in.1$Species))# n of species x treatments
Species2 # length = 96
Output <- NULL
for (X in 1:length(Species2)) {
  ind <- which(in.1$Species == Species2[X])
  N1 <- in.1$count[ind]
  N2 <- out.2$count[ind]
  Died <- N1 - N2
  Surv <- N1 - Died
  NSpecies <- rep( Species2[X], N1)
  NSurv <- rep(0, Surv)
  NDied <- rep(1, Died)
  saveoutput <- data.frame(Species2=NSpecies, time= "2Weeks", Status=c(NSurv, NDied))
  Output <- rbind(Output, saveoutput)
}

Output
str(Output)
head(Output)
Out2weeks<-Output
Out2weeks$Week<-2
head(Out2weeks)

#Week1 to Week4==============
#First Data Frame on total input
in.1<-SurvivalData[,1:10]#first frame (total number of herbicide-treated plants, always the same)
str(in.1)#68 obs. of  11 variables:
in.1$count<-in.1$Week1 #this count of plants treated in week 1
head(in.1)

#Second Data Frame on total survived after week 1.
out.2<-SurvivalData[,c(1:9,13)]#second frame (number of survived till week3)
str(out.2)#68 obs. of  11 variables:
out.2$count<-out.2$Week4

Species2 <- as.character(unique(in.1$Species))# n of species x treatments
Species2 # length = 96
Output <- NULL
for (X in 1:length(Species2)) {
  ind <- which(in.1$Species == Species2[X])
  N1 <- in.1$count[ind]
  N2 <- out.2$count[ind]
  Died <- N1 - N2
  Surv <- N1 - Died
  NSpecies <- rep( Species2[X], N1)
  NSurv <- rep(0, Surv)
  NDied <- rep(1, Died)
  saveoutput <- data.frame(Species2=NSpecies, time= "3Weeks", Status=c(NSurv, NDied))
  Output <- rbind(Output, saveoutput)
}

Output
str(Output)#
head(Output)
Out3weeks<-Output
Out3weeks$Week<-3
head(Out3weeks)

#Week1 to Week5==============
#First Data Frame on total input
in.1<-SurvivalData[,1:10]#first frame (total number of herbicide-treated plants, always the same)
str(in.1)#68 obs. of  11 variables:
in.1$count<-in.1$Week1 #this count of plants treated in week 1
head(in.1)

#Second Data Frame on total survived after week 1.
out.2<-SurvivalData[,c(1:9,14)]#second frame (number of survived till week3)
str(out.2)#	68 obs. of  11 variables:
out.2$count<-out.2$Week5

Species2 <- as.character(unique(in.1$Species))# n of species x treatments
Species2 # length = 96
Output <- NULL
for (X in 1:length(Species2)) {
  ind <- which(in.1$Species == Species2[X])
  N1 <- in.1$count[ind]
  N2 <- out.2$count[ind]
  Died <- N1 - N2
  Surv <- N1 - Died
  NSpecies <- rep( Species2[X], N1)
  NSurv <- rep(0, Surv)
  NDied <- rep(1, Died)
  saveoutput <- data.frame(Species2=NSpecies, time= "4Weeks", Status=c(NSurv, NDied))
  Output <- rbind(Output, saveoutput)
}

Output
str(Output)
head(Output)
Out4weeks<-Output
Out4weeks$Week<-4
head(Out4weeks)

#Week1 to Week6==============
#First Data Frame on total input
in.1<-SurvivalData[,1:10]#first frame (total number of herbicide-treated plants, always the same)
str(in.1)#68 obs. of  11 variables:
in.1$count<-in.1$Week1 #this count of plants treated in week 1
head(in.1)

#Second Data Frame on total survived after week 1.
out.2<-SurvivalData[,c(1:9,15)]#second frame (number of survived till week3)
str(out.2)#	68 obs. of  11 variables:
out.2$count<-out.2$Week6

Species2 <- as.character(unique(in.1$Species))# n of species x treatments
Species2 # length = 68
Output <- NULL
for (X in 1:length(Species2)) {
  ind <- which(in.1$Species == Species2[X])
  N1 <- in.1$count[ind]
  N2 <- out.2$count[ind]
  Died <- N1 - N2
  Surv <- N1 - Died
  NSpecies <- rep( Species2[X], N1)
  NSurv <- rep(0, Surv)
  NDied <- rep(1, Died)
  saveoutput <- data.frame(Species2=NSpecies, time= "5Weeks", Status=c(NSurv, NDied))
  Output <- rbind(Output, saveoutput)
}

Output
str(Output)#738 obs. of  3 variables:
head(Output)
Out5weeks<-Output
Out5weeks$Week<-5
head(Out5weeks)

#All WEEKS Together===========
AllWeeks<-rbind(Out1weeks,Out2weeks,Out3weeks,Out4weeks,Out5weeks)
str(AllWeeks)#data.frame':	3690 obs. of  4 variables:
df =separate(data = AllWeeks, col = Species2, into = c("CO2","Dose","herbicide","genus","species"), sep = "\\.")
head(df)
df$genus.species<-interaction(df$genus,df$species)
str(df)#3690 obs. of  9 variables:
#write.csv(df, file = "WeedsBinary.csv", row.names = FALSE)

#START FROM HERE:
df<-read.csv("WeedsBinary.csv")
fit2 <- survfit(Surv(Week, Status) ~ Dose + CO2 + genus, data = df)
print(fit2)# Summary of survival curves
summary(fit2)# Access to the sort summary table
summary(fit2)$table  

#PLOT Mine Kapler-Meier========:
#Glyphosate only:

glyph<-df[df$herbicide=="glyphosate",]
glyph$genus<-factor(glyph$genus)
fit3 <- survfit(Surv(Week, Status) ~ Dose + CO2+ genus, data = glyph)
summary(fit3)
summary(fit3)$table  
str(glyph)#2930 obs. of  9 variables:

levels(droplevels(glyph$genus.species))
[1] Ageratina.adenophora     
[2] Anredera.cordifolia      
[3] Asparagus.aethiopicus    
[4] Avena.barbata            
[5] Bromus.catharticus       
[6] Chloris.gayana           
[7] Cotoneaster.glaucophyllus
[8] Ehrharta.erecta          
[9] Ipomoea.indica           
[10] Lantana.camara           
[11] Pennisetum.clandestinum  
[12] Senna.pendula            
[13] Tradescantia.fluminensis 
[14] Verbena.bonariensis 

#Ageratina.adenophora TINKERING =======
Ageratina<-glyph[glyph$genus=="Ageratina", ]
dim(Ageratina)#200   9
fit.Ageratina <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Ageratina)
summary(fit.Ageratina)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Ageratina, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Ageratina adenophora") + theme_classic() +xlab("") + ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = c(0.2,.3),
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=20),
                 legend.text = element_text(size = 10),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=22, hjust = 0.5))

g2 

plot.Ageratina <- g2 + guides(shape=FALSE)
plot.Ageratina


#Ageratina.adenophora=======
Ageratina<-glyph[glyph$genus=="Ageratina", ]
fit.Ageratina <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Ageratina)
summary(fit.Ageratina)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Ageratina, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Ageratina adenophora") + theme_classic() +xlab("")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=20),
                 legend.text = element_text(size = 10),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=22, hjust = 0.5))

plot.Ageratina <- g2
plot.Ageratina
##ggsave(filename = "plotAgeratina.png", width = 280, height = 280, units = "mm" )  

#Anredera.cordifolia============ 
Anredera<-glyph[glyph$genus=="Anredera", ]
fit.Anredera <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Anredera)
summary(fit.Anredera)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Anredera, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Anredera cordifolia") + theme_classic() +xlab("")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=20),
                 legend.text = element_text(size = 10),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=22, hjust = 0.5))

plot.Anredera  <- g2
plot.Anredera
##ggsave(filename = "plotAnredera3.png", width = 280, height = 280, units = "mm" )  


#Asparagus.aethiopicus============ 
Asparagus<-glyph[glyph$genus=="Asparagus", ]
fit.Asparagus <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Asparagus)
summary(fit.Asparagus)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Asparagus, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Asparagus aethiopicus ") + theme_classic() +xlab("")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=26, hjust = 0.5))

plot.Asparagus  <- g2
plot.Asparagus
##ggsave(filename = "PLOT_Asparagus.png", width = 280, height = 280, units = "mm" )  

#Avena.barbata============ 
Avena<-glyph[glyph$genus=="Avena", ]
fit.Avena <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Avena)
summary(fit.Avena)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Avena, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Avena barbata") + theme_classic() +xlab("")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=26, hjust = 0.5))

plot.Avena  <- g2
plot.Avena
##ggsave(filename = "PLOT_Avena.png", width = 280, height = 280, units = "mm" )  


#Bromus.catharticus============ 
Bromus<-glyph[glyph$genus=="Bromus", ]
fit.Bromus <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Bromus)
summary(fit.Bromus)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Bromus, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Bromus catharticus") + theme_classic() +xlab("")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=26, hjust = 0.5))

plot.Bromus  <- g2
plot.Bromus
##ggsave(filename = "PLOT_Bromus.png", width = 280, height = 280, units = "mm" )  

#Chloris.gayana============ 
Chloris<-glyph[glyph$genus=="Chloris", ]
fit.Chloris <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Chloris)
summary(fit.Chloris)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Chloris, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Chloris gayana") + theme_classic() +xlab("")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=26, hjust = 0.5))

plot.Chloris  <- g2
plot.Chloris
##ggsave(filename = "PLOT_Chloris.png", width = 280, height = 280, units = "mm" )  

#Cotoneaster.glaucophyllus============ 
Cotoneaster<-glyph[glyph$genus=="Cotoneaster", ]
fit.Cotoneaster <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Cotoneaster)
summary(fit.Cotoneaster)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Cotoneaster, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Cotoneaster glaucophyllus") + theme_classic() +xlab("")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=26, hjust = 0.5))

plot.Cotoneaster  <- g2
plot.Cotoneaster
##ggsave(filename = "PLOT_Cotoneaster.png", width = 280, height = 280, units = "mm" )  

#Ehrharta.erecta ============ 
Ehrharta<-glyph[glyph$genus=="Ehrharta", ]
fit.Ehrharta <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Ehrharta)
summary(fit.Ehrharta)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Ehrharta, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Ehrharta erecta") + theme_classic() +xlab("")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=26, hjust = 0.5))

plot.Ehrharta  <- g2
plot.Ehrharta
##ggsave(filename = "PLOT_Ehrharta.png", width = 280, height = 280, units = "mm" )  

#Ipomoea.indica  ============ 
Ipomoea<-glyph[glyph$genus=="Ipomoea", ]
fit.Ipomoea <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Ipomoea)
summary(fit.Ipomoea)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Ipomoea, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Ipomoea indica ") + theme_classic() +xlab("")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=26, hjust = 0.5))

plot.Ipomoea  <- g2
plot.Ipomoea
#ggsave(filename = "PLOT_Ipomoea.png", width = 280, height = 280, units = "mm" )  


#Lantana.camara  ============ 
Lantana<-glyph[glyph$genus=="Lantana", ]
fit.Lantana <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Lantana)
summary(fit.Lantana)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Lantana, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Lantana camara ") + theme_classic() +xlab("")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=26, hjust = 0.5))

plot.Lantana  <- g2
plot.Lantana
#ggsave(filename = "PLOT_Lantana.png", width = 280, height = 280, units = "mm" )  

#Pennisetum.clandestinum  ============ 
Pennisetum<-glyph[glyph$genus=="Pennisetum", ]
fit.Pennisetum <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Pennisetum)
summary(fit.Pennisetum)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Pennisetum, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Cenchrus clandestinum") + theme_classic() +xlab("")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=26, hjust = 0.5))

plot.Pennisetum  <- g2
plot.Pennisetum
#ggsave(filename = "PLOT_Pennisetum.png", width = 280, height = 280, units = "mm" )  

#Senna.pendula ============ 
Senna<-glyph[glyph$genus=="Senna", ]
fit.Senna <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Senna)
summary(fit.Senna)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Senna, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Senna pendula") + theme_classic() +xlab("")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=26, hjust = 0.5))

plot.Senna  <- g2
plot.Senna
#ggsave(filename = "PLOT_Senna.png", width = 280, height = 280, units = "mm" )  

#Tradescantia.fluminensis  ============ 
Tradescantia<-glyph[glyph$genus=="Tradescantia", ]
fit.Tradescantia <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Tradescantia)
summary(fit.Tradescantia)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Tradescantia, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Tradescantia fluminensis") + theme_classic() +xlab("Week")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "bottom",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=20),
                 legend.title = element_blank(),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=22, hjust = 0.5))

plot.Tradescantia  <- g2
plot.Tradescantia
#ggsave(filename = "PLOT_Tradescantia2.png", width = 280, height = 280, units = "mm" )  


#Verbena.bonariensis =================
Verbena<-glyph[glyph$genus=="Verbena", ]
fit.Verbena <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Verbena)
summary(fit.Verbena)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Verbena, pval = TRUE,linetype = c(1,2,3,4), size=4, censor = FALSE)
g1<- g$plot + ggtitle("Verbena bonariensis") + theme_classic() +xlab("Week")+ylab("Fraction Survival")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=20),
                 legend.text = element_text(size = 10),
                 plot.title = element_text(lineheight=1.2, face="bold.italic",size=22, hjust = 0.5))

plot.Verbena  <- g2
plot.Verbena
#ggsave(filename = "PLOT_Verbena2.png", width = 280, height = 280, units = "mm" )  

#Multiplot=========
multiplot(plot.Ageratina, plot.Tradescantia,plot.Anredera,  cols=2)

#Practice:===========
#http://www.mas.ncl.ac.uk/~nmf16/teaching/mas3311/practical1.html

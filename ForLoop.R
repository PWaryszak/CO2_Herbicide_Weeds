#For Loop to produce binary data frame===========
#Week1 survival
SurvivalData <- read.csv("survivalFIXED.csv")
str(SurvivalData)#data.frame':	96 obs. of  15 variables:
head(SurvivalData)
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


#Week1 to Week2==========
#First Data Frame on total input
in.1<-SurvivalData[,1:10]#first frame (number of herbicide-treated)
str(in.1)#data.frame':	96 obs. of  9 variables:
in.1$count<-in.1$Week1 #this count of plants treated in week 1
head(in.1)

#Second Data Frame on total survived after week 1.
out.2<-SurvivalData[,c(1:9,11)]#second frame (number of survived till week2)
str(out.2)#	96 obs. of  10 variables:
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
  NSurv <- rep(1, Surv)
  NDied <- rep(0, Died)
  saveoutput <- data.frame(Species2=NSpecies, time= "1Week", Survival=c(NSurv, NDied))
  Output <- rbind(Output, saveoutput)
}

Output
str(Output)#1002 obs. of  3 variables:
head(Output)
Out1weeks<-Output
Out1weeks$Week<-1
head(Out1weeks)


#Week1 to Week3==============
#First Data Frame on total input
in.1<-SurvivalData[,1:10]#first frame (total number of herbicide-treated plants, always the same)
str(in.1)#data.frame':	96 obs. of  9 variables:
in.1$count<-in.1$Week1 #this count of plants treated in week 1
head(in.1)

#Second Data Frame on total survived after week 1.
out.2<-SurvivalData[,c(1:9,12)]#second frame (number of survived till week3)
str(out.2)#	96 obs. of  10 variables:
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
  NSurv <- rep(1, Surv)
  NDied <- rep(0, Died)
  saveoutput <- data.frame(Species2=NSpecies, time= "2Weeks", Survival=c(NSurv, NDied))
  Output <- rbind(Output, saveoutput)
}

Output
str(Output)#1002 obs. of  3 variables:
head(Output)
Out2weeks<-Output
Out2weeks$Week<-2
head(Out2weeks)

#Week1 to Week4==============
#First Data Frame on total input
in.1<-SurvivalData[,1:10]#first frame (total number of herbicide-treated plants, always the same)
str(in.1)#data.frame':	96 obs. of  9 variables:
in.1$count<-in.1$Week1 #this count of plants treated in week 1
head(in.1)

#Second Data Frame on total survived after week 1.
out.2<-SurvivalData[,c(1:9,13)]#second frame (number of survived till week3)
str(out.2)#	96 obs. of  10 variables:
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
  NSurv <- rep(1, Surv)
  NDied <- rep(0, Died)
  saveoutput <- data.frame(Species2=NSpecies, time= "3Weeks", Survival=c(NSurv, NDied))
  Output <- rbind(Output, saveoutput)
}

Output
str(Output)#1002 obs. of  3 variables:
head(Output)
Out3weeks<-Output
Out3weeks$Week<-3
head(Out3weeks)

#Week1 to Week5==============
#First Data Frame on total input
in.1<-SurvivalData[,1:10]#first frame (total number of herbicide-treated plants, always the same)
str(in.1)#data.frame':	96 obs. of  9 variables:
in.1$count<-in.1$Week1 #this count of plants treated in week 1
head(in.1)

#Second Data Frame on total survived after week 1.
out.2<-SurvivalData[,c(1:9,14)]#second frame (number of survived till week3)
str(out.2)#	96 obs. of  10 variables:
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
  NSurv <- rep(1, Surv)
  NDied <- rep(0, Died)
  saveoutput <- data.frame(Species2=NSpecies, time= "4Weeks", Survival=c(NSurv, NDied))
  Output <- rbind(Output, saveoutput)
}

Output
str(Output)#1002 obs. of  3 variables:
head(Output)
Out4weeks<-Output
Out4weeks$Week<-4
head(Out4weeks)

#Week1 to Week6==============
#First Data Frame on total input
in.1<-SurvivalData[,1:10]#first frame (total number of herbicide-treated plants, always the same)
str(in.1)#data.frame':	96 obs. of  9 variables:
in.1$count<-in.1$Week1 #this count of plants treated in week 1
head(in.1)

#Second Data Frame on total survived after week 1.
out.2<-SurvivalData[,c(1:9,15)]#second frame (number of survived till week3)
str(out.2)#	96 obs. of  10 variables:
out.2$count<-out.2$Week6

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
  NSurv <- rep(1, Surv)
  NDied <- rep(0, Died)
  saveoutput <- data.frame(Species2=NSpecies, time= "5Weeks", Survival=c(NSurv, NDied))
  Output <- rbind(Output, saveoutput)
}

Output
str(Output)#1002 obs. of  3 variables:
head(Output)
Out5weeks<-Output
Out5weeks$Week<-5
head(Out5weeks)

#All WEEKS Together===========
AllWeeks<-rbind(Out1weeks,Out2weeks,Out3weeks,Out4weeks,Out5weeks)
str(AllWeeks)#data.frame':	5010 obs. of  4 variables:
df = transform(AllWeeks, Species2 = colsplit(Species2, split = "\\.", names = c('a', 'b','c','d','e')))
df =separate(data = AllWeeks, col = Species2, into = c("CO2","Dose","herbicide","genus","species"), sep = "\\.")
head(df)
df$genus.species<-interaction(df$genus,df$species)



fit2 <- survfit(Surv(Week, Survival) ~ Dose + CO2 + herbicide + genus, data = df)
print(fit2)# Summary of survival curves
summary(fit2)# Access to the sort summary table
summary(fit2)$table  

#PLOT Mine Kapler-Meier========:
ggsurvplot(fit2, data = df)
ggsurvplot(fit2, pval = TRUE, 
           break.time.by = 6,
           risk.table = TRUE,
           risk.table.height = 0.5#Useful when you have multiple groups
)
ggsurvplot(fit, data = lung)

fit2 <- survfit( Surv(time, status) ~ rx, data = colon )
fit2 <- survfit(Surv(Week, Survival) ~  genus, data = ss1)

ggsurv <- ggsurvplot(fit2, conf.int = TRUE)
ggsurv
# Facet
ggsurv$plot + theme_bw()+facet_wrap(~Dose)

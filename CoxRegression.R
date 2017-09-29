#Cox on Glyphosate-treated plants ========
#install.packages(c("survival", "survminer","tidyverse"))
library("tidyverse")
library("survival")
library("survminer")

df<-read.csv("WeedsBinary.csv")#data will be available once published
glyph<-df[df$herbicide=="glyphosate",] #subset species that were treated with gluphosate
glyph$genus<-factor(glyph$genus)#makes sure these species are factors to ease running Cox model
str(glyph)#data.frame':	2930 obs. of  9 variables:
levels(glyph$Dose)#"double" "recom"  - LEt us relevel the Dose factor to show double in stats table below:
glyph$Dose<- factor(glyph$Dose, levels = c( "recom","double"))

#Looping Cox regresstion over all species:
#Remove Tradescantia as it all survived and cox regression produces errors on Trad.
glyph <- glyph[ glyph$genus != "Tradescantia",]
glyph$genus.species <- factor(glyph$genus.species)
levels(glyph$genus.species)#13 now

#For Loop to run Cox's regression on all species seperately:
Cox.Output <- NULL #we need to set an empty shelf for data called Output
for ( i in unique(glyph$genus.species) ){
  #create a subset data
  Species_Subset <- subset(glyph, genus.species == i)
  
  datalm <- assign(paste0(i), coef(summary(coxph(Surv(Week, Status) ~ Dose * CO2, data = Species_Subset)), digits = 2))
  
  namelm <- paste0(i)
  
  saveoutput <- data.frame(datalm, GenusName=namelm)
  Cox.Output <- rbind(Cox.Output, saveoutput)
}
dim(Cox.Output)#39  6 and should be 42 (14sp X 3 terms = 42 rows of output)
#write.csv(Cox.Output, file= "Cox.STATS.csv")

#"Tradescantia.fluminensis" is missing as COX goes crazy when all survived.
Trad = subset(glyph, glyph$genus=="Tradescantia")
#Asparagus, Tradescantia, Cotonaster showed no response to treatments henc Cox goes crazy a little:
#"When one of the coefficients goes to infinity in a Cox model, the Wald rest of significance beta/se(beta) breaks down, and is no longer reliable.
#https://stat.ethz.ch/pipermail/r-help/2008-September/174201.html

Tradescantia<-glyph[glyph$genus=="Tradescantia", ] 
dim(Tradescantia)#200   9
Tradescantia.Cox <- coxph(Surv(Week, Status) ~ Dose * CO2, data = Tradescantia)
Tradescantia.Cox
#All plants survived, hence error/warning:#Error in fitter(X, Y, strats, offset, init, control, weights = weights,  : 
Asparagus<-glyph[glyph$genus=="Asparagus", ]
dim(Asparagus)#200   9
Asparagus.Cox <- coxph(Surv(Week, Status) ~ Dose * CO2, data = Asparagus)
summary(Asparagus.Cox)
coef(summary(Asparagus.Cox))##Results in accordance with what KM survival curves plot
Cox.Asparagus.Only <-as.data.frame(coef(summary(Asparagus.Cox)))


#COX on STARANE-treated plant species:===========
#install.packages(c("survival", "survminer","tidyverse"))
library("tidyverse")
library("survival")
library("survminer")

df<-read.csv("WeedsBinary.csv")#data will be available once published
star<-df[df$herbicide=="starane",] #subset species that were treated with starane
star$genus<-factor(star$genus)#makes sure these species are factors to ease running Cox model
str(star)#data.frame':760 obs. of  9 variables:
levels(star$Dose)#"double" "recom"  - LEt us relevel the Dose factor to show double in stats table below:
star$Dose<- factor(star$Dose, levels = c( "recom","double"))

Cox.Output.Starane <- NULL #we need to set an empty shelf for data called Output
for ( i in unique(star$genus.species) ){
  #create a subset data
  Species_Subset <- subset(star, genus.species == i)
  
  datalm <- assign(paste0(i), coef(summary(coxph(Surv(Week, Status) ~ Dose * CO2, data = Species_Subset)), digits = 2))
  
  namelm <- paste0(i)
  
  saveoutput <- data.frame(datalm, GenusName=namelm)
  Cox.Output.Starane <- rbind(Cox.Output.Starane, saveoutput)
}
dim(Cox.Output.Starane)#9  6 = OK!
Cox.Output.Starane
#write.csv(Cox.Output.Starane, file= "Cox.STATS_STARANE.csv")

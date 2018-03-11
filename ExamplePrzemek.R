#Load Example Data and libraries:
weeds<- read.csv("https://raw.githubusercontent.com/PWaryszak/CO2_Herbicide_Weeds/master/ExamplePrzemek.csv")
str(weeds)#data.frame':	950 obs. of  4 variables:

library("tidyverse")
library("survival")
library("survminer")

#Plot 4 plots for 4 weed types in response to
#CO.Herbicide treatment (4 levels factor):
Ageratina <- weeds[weeds$genus=="Ageratina", ]
fit.Ageratina <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Ageratina)
#Plot:
plot.Ageratina <- ggforest(fit.Ageratina, main = "Ageratina adenophora (glyphosate)", fontsize = 0.9)

Anredera <- weeds[weeds$genus=="Anredera", ]
fit.Anredera <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Anredera)
#Plot:
plot.Anredera <- ggforest(fit.Anredera,fontsize = 0.9,main = "Anredera cordifolia (glyphosate)")

Avena<-weeds[weeds$genus=="Avena", ]
fit.Avena <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Avena)
#Plot:
plot.Avena  <- ggforest(fit.Avena, main = "Avena barbata (glyphosate)",fontsize=0.9)

Bromus<-weeds[weeds$genus=="Bromus", ]
fit.Bromus <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Bromus)
#Plot:
plot.Bromus  <- ggforest(fit.Bromus, main = "Bromus catharticus (glyphosate)",fontsize=0.9)

#Arrange Plots into Multiplot does not work:
ggarrange(plot.Ageratina,plot.Avena,plot.Anredera, plot.Bromus,
          ncol = 2, nrow = 2, align = "h")
#ERROR MSG:
# Argument needs to be of class "ggplot", "gtable", "recordedplot",
#or a function that plots to an 
#R graphics device when called, but is a textgrobgDesc
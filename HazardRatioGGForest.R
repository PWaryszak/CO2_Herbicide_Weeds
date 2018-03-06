#Kaplan Meier (KM) Curves:
#Multiplot function:
library("tidyverse")
library("survival")
library("survminer")
citation("survival")
citation("survminer")

#Data for Glyphosate Treatment==========
#START FROM HERE:
df<-read.csv("WeedsBinary.csv")
#Glyphosate only:
glyph<-df[df$herbicide=="glyphosate",]
glyph$genus<-factor(glyph$genus)
str(glyph)#2930 obs. of  9 variables:
glyph$CO2.Herbicide <- glyph$CO2_Dose_
glyph$CO2.Herbicide<- factor(glyph$CO2.Herbicide, levels = c("ambient.recom","ambient.double","elevated.recom","elevated.double"))


#These plots to be arranged by species as in Biomass Figure:
#Avena ============ 
Avena<-glyph[glyph$genus=="Avena", ]
fit.Avena <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Avena)
summary(fit.Avena)# Access to the sort summary table
#Plot:
plot.Avena  <- ggforest(fit.Avena, main = "Avena barbata (glyphosate)")
plot.Avena

#Bromus =================
Bromus<-glyph[glyph$genus=="Bromus", ]
fit.Bromus <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Bromus)
summary(fit.Bromus)# Access to the sort summary table
#Plot:
plot.Bromus  <- ggforest(fit.Bromus, main = "Bromus catharticus (glyphosate)")
plot.Bromus

#Ehrharta  ============ 
Ehrharta<- glyph[glyph$genus=="Ehrharta", ]
fit.Ehrharta<- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Ehrharta)
summary(fit.Ehrharta) # Access to the sort summary table
#Plot:
plot.Ehrharta <- ggforest(fit.Ehrharta, main ="Ehrharta erecta (glyphosate)") 
plot.Ehrharta


#Cenchrus  ============ 
Pennisetum <- glyph[glyph$genus=="Pennisetum", ]
fit.Pennisetum <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Pennisetum)
summary(fit.Pennisetum) # Access to the sort summary table
#Plot:
plot.Pennisetum  <- ggforest(fit.Pennisetum, main = "Cenchrus clandestinum (glyphosate)")
plot.Pennisetum

#Chloris  ============ 
Chloris <- glyph[glyph$genus=="Chloris", ]
fit.Chloris <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Chloris)
summary(fit.Chloris) # Access to the sort summary table
#Plot:
plot.Chloris <- ggforest(fit.Chloris, main = "Chloris gayana (glyphosate)")
plot.Chloris

#MIDDLE PANEL:======
#Ageratina ============ 
Ageratina <- glyph[glyph$genus=="Ageratina", ]
fit.Ageratina <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Ageratina)
summary(fit.Ageratina) # Access to the sort summary table
#Plot:
plot.Ageratina <- ggforest(fit.Ageratina, main = "Ageratina adenophora (glyphosate)")
plot.Ageratina

#Asparagus ============ 
Asparagus<-glyph[glyph$genus=="Asparagus", ]
fit.Asparagus <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Asparagus)
summary(fit.Asparagus)# Access to the sort summary table
#Plot:
plot.Asparagus <- ggforest(fit.Asparagus,main = "Asparagus aethiopicus (glyphosate)")
plot.Asparagus

#Cotoneaster =================
Cotoneaster<-glyph[glyph$genus=="Cotoneaster", ]
fit.Cotoneaster <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Cotoneaster)
summary(fit.Cotoneaster)# Access to the sort summary table
#Plot:
plot.Cotoneaster <- ggforest(fit.Cotoneaster, main = "Cotoneaster glaucophyllus (glyphosate)")
plot.Cotoneaster

#Lantana ============ 
Lantana<- glyph[glyph$genus=="Lantana", ]
fit.Lantana<- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Lantana)
summary(fit.Lantana) # Access to the sort summary table
#Plot:
plot.Lantana <- ggforest(fit.Lantana, main = "Lantana camara (glyphosate)")
plot.Lantana

#Senna ============ 
Senna <- glyph[glyph$genus=="Senna", ]
fit.Senna <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Senna)
summary(fit.Senna) # Access to the sort summary table
#Plot:
plot.Senna <- ggforest(fit.Senna,main = "Senna pendula (glyphosate)") 
plot.Senna

#Anredera  ============ 
Anredera <- glyph[glyph$genus=="Anredera", ]
fit.Anredera <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Anredera)
summary(fit.Anredera) # Access to the sort summary table
#Plot:
plot.Anredera <- ggforest(fit.Anredera, main = "Anredera cordifolia (glyphosate)")
plot.Anredera

#LOWER PANEL:=========
#Ipomea ALL SURVIVED ============ 
Ipomea <- glyph[glyph$genus=="Ipomea", ]
fit.Ipomea <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Ipomea)
summary(fit.Ipomea) # Access to the sort summary table
#Plot:
plot.Ipomea <- ggforest(fit.Ipomea, main = "Ipomoea indica (glyphosate)")
plot.Ipomea

#Verbena ============ 
Verbena <- glyph[glyph$genus=="Verbena", ]
fit.Verbena <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Verbena)
summary(fit.Verbena) # Access to the sort summary table
#Plot:
plot.Verbena <- ggforest(fit.Verbena, main = "Verbena bonariensis (glyphosate)")
plot.Verbena


#COMBINE ALL PLOTS========
figLongUpper <- grid.arrange(plot.Avena, plot.Bromus,plot.Ehrharta, plot.Pennisetum,
                             plot.Chloris, plot.Tradescantia,  ncol=6)
#ggsave(figLongUpper, filename = "PLOT_BIOMASS_6sp_UP_PANEL.jpeg", width = 840, height = 140, units = "mm" )  

figLongMiddle <- grid.arrange(plot.Ageratina,plot.Asparagus, plot.Cotoneaster,plot.Lantana,
                             plot.Senna,plot.Anredera,  ncol=6)
#ggsave(figLongMiddle, filename = "PLOT_BIOMASS_6sp_MIDDLE_PANEL.jpeg", width = 840, height = 140, units = "mm" )  

figLongLower <- grid.arrange(plot.Ipomea, plot.Verbena, plot.Anredera.starane,
                             plot.Lantana.starane, plot.Tradescantia.starane, ncol=6)
#ggsave(figLongLower, filename = "PLOT_BIOMASS_5sp_Lower_PANEL.jpeg", width = 840, height = 140, units = "mm" )  


fig12species <- grid.arrange(plot.Avena, plot.Bromus,plot.Ehrharta,plot.Chloris,
                             plot.Pennisetum, plot.Ageratina,plot.Anredera, plot.Senna,
                             plot.Verbena,plot.Anredera.starane,plot.Tradescantia.starane,
                             plot.Lantana.starane, 
                             ncol=3)
fig12species
ggsave(fig12species, filename = "PLOT_SURVIVAL_12sp_PANEL.jpeg", width = 840, height = 1120, units = "mm" )  

multiplot(plot.Avena, plot.Bromus,plot.Ehrharta,plot.Chloris,
                             cols=3)


#STARANE Treatment==========
library("tidyverse")
library("survival")
library("survminer")

df<-read.csv("WeedsBinary.csv")#data will be available once published
star<-df[df$herbicide=="starane",] #subset species that were treated with starane
star$genus<-factor(star$genus)#makes sure these species are factors to ease running Cox model
str(star)#data.frame':760 obs. of  9 variables:
levels(star$Dose)#"double" "recom"  - LEt us relevel the Dose factor to show double in stats table below:
star$Dose<- factor(star$Dose, levels = c("recom","double"))
levels(star$genus)#3 only: "Anredera","Lantana", "Tradescantia"
star$CO2.Herbicide <- star$CO2_Dose_
star$CO2.Herbicide<- factor(star$CO2.Herbicide, levels = c("ambient.recom","ambient.double","elevated.recom","elevated.double"))

#Anredera under Starane ============ 
Anredera <- star[star$genus=="Anredera", ]
fit.Anredera <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Anredera)
summary(fit.Anredera) # Access to the sort summary table
#Plot:
plot.Anredera.starane <- ggforest(fit.Anredera, main = "Anredera cordifolia (starane)")
plot.Anredera.starane

#Lantana under Starane============ 
Lantana<- star[star$genus=="Lantana", ]
fit.Lantana<- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Lantana)
summary(fit.Lantana) # Access to the sort summary table
#Plot:
plot.Lantana.starane <- ggforest(fit.Lantana, main = "Lantana camara (starane)")
plot.Lantana.starane


#Tradescantia under Starane  ============ 
Tradescantia <- star[star$genus=="Tradescantia", ]
fit.Tradescantia <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Tradescantia)
summary(fit.Tradescantia) # Access to the sort summary table
#Plot:
plot.Tradescantia.starane <- ggforest(fit.Tradescantia, main = "Tradescantia fluminensis (starane)") 
plot.Tradescantia.starane





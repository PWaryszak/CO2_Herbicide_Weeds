#Kaplan Meier (KM) Curves:
#Multiplot function:
library("tidyverse")
library("glyph")
library("survminer")
library("survival")
#Assumption TEST: http://www.sthda.com/english/wiki/cox-model-assumptions

#Data for 2 Herbicide Treatments (Glyphosate & Starane)==========
#START FROM HERE:
df<-read.csv("WeedsBinary2.csv")
str(df)#6221 obs. of  16 variables:
#Glyphosate only subset:
glyph<-df[df$herbicide=="glyphosate",]
glyph$genus<-factor(glyph$genus)
str(glyph)#3456 obs. of  16 variables:
glyph$CO2.Herbicide<- factor(glyph$CO2.Herbicide, levels = c("ambient.recom","ambient.double","elevated.recom","elevated.double"))

#STARANE only subset:
star<-df[df$herbicide=="starane",] #subset species that were treated with starane
star$genus<-factor(star$genus)#makes sure these species are factors to ease running Cox model
str(star)#845 obs. of  16 variables:
levels(star$Dose)#"double" "recom"  - LEt us relevel the Dose factor to show double in stats table below:
star$Dose<- factor(star$Dose, levels = c("recom","double"))
levels(star$genus)#3 only: "Anredera","Lantana", "Tradescantia"
star$CO2.Herbicide<- factor(star$CO2.Herbicide, levels = c("ambient.recom","ambient.double","elevated.recom","elevated.double"))


#These plots to be arranged by species as in KM plot:
#1Ageratina ============ 
Ageratina <- glyph[glyph$genus=="Ageratina", ]
fit.Ageratina <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Ageratina)
summary(fit.Ageratina) # Access to the sort summary table
#Plot:
plot.Ageratina <- ggforest(fit.Ageratina, main = "Ageratina adenophora (glyphosate)",
                           fontsize = 1.1)+  theme(plot.title = element_text(face = "italic", size=20, hjust =0)) 


#2Anredera under glyphosate============ 
Anredera <- glyph[glyph$genus=="Anredera", ]
fit.Anredera <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Anredera)
summary(fit.Anredera) # Access to the sort summary table
#Plot:
plot.Anredera <- ggforest(fit.Anredera,fontsize = 1.1,main = "Anredera cordifolia (glyphosate)")+ 
  theme(plot.title = element_text(face = "italic", size=20, hjust =0))

#3Anredera under Starane ============ 
Anredera <- star[star$genus=="Anredera", ]
fit.Anredera <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Anredera)
summary(fit.Anredera) # Access to the sort summary table
#Plot:
plot.Anredera.starane <- ggforest(fit.Anredera, main = "Anredera cordifolia (starane)",fontsize = 1.1)+ 
  theme(plot.title = element_text(face = "italic", size=20, hjust =0))

#4Avena ============ 
Avena<-glyph[glyph$genus=="Avena", ]
fit.Avena <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Avena)
summary(fit.Avena)# Access to the sort summary table
#Plot:
plot.Avena  <- ggforest(fit.Avena, main = "Avena barbata (glyphosate)",fontsize=1.1)+  theme(plot.title = element_text(face = "italic", size=20, hjust =0))
#plot.Avena
#5Bromus =================
Bromus<-glyph[glyph$genus=="Bromus", ]
fit.Bromus <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Bromus)
summary(fit.Bromus)# Access to the sort summary table
#Plot:
plot.Bromus  <- ggforest(fit.Bromus, main = "Bromus catharticus (glyphosate)",fontsize=1.1)+  theme(plot.title = element_text(face = "italic", size=20, hjust =0))

#6Cenchrus  ============ 
Pennisetum <- glyph[glyph$genus=="Pennisetum", ]
fit.Pennisetum <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Pennisetum)
summary(fit.Pennisetum) # Access to the sort summary table
#Plot:
plot.Pennisetum  <- ggforest(fit.Pennisetum, main = "Cenchrus clandestinum (glyphosate)",fontsize=1.1)+  theme(plot.title = element_text(face = "italic", size=20, hjust =0))

#7Chloris  ============ 
Chloris <- glyph[glyph$genus=="Chloris", ]
fit.Chloris <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Chloris)
summary(fit.Chloris) # Access to the sort summary table
#Plot:
plot.Chloris <- ggforest(fit.Chloris, main = "Chloris gayana (glyphosate)",fontsize = 1.1)+  theme(plot.title = element_text(face = "italic", size=20, hjust =0))

#8Ehrharta  ============ 
Ehrharta<- glyph[glyph$genus=="Ehrharta", ]
fit.Ehrharta<- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Ehrharta)
#Plot:
plot.Ehrharta <- ggforest(fit.Ehrharta, main ="Ehrharta erecta (glyphosate)",fontsize=1.1) +  theme(plot.title = element_text(face = "italic", size=20, hjust =0))


#9Lantana under Starane============ 
Lantana<- star[star$genus=="Lantana", ]
fit.Lantana<- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Lantana)
summary(fit.Lantana) # Access to the sort summary table
#Plot:
plot.Lantana.starane <- ggforest(fit.Lantana, main = "Lantana camara (starane)", fontsize = 1.1)+
  theme(plot.title = element_text(face = "italic", size=20, hjust =0))
#plot.Lantana.starane

#10Senna ============ 
Senna <- glyph[glyph$genus=="Senna", ]
fit.Senna <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Senna)
plot.Senna <- ggforest(fit.Senna,main = "Senna pendula (glyphosate)",fontsize = 1.1) + 
  theme(plot.title = element_text(face = "italic", size=20, hjust =0))

#11Tradescantia under Starane  ============ 
Tradescantia <- star[star$genus=="Tradescantia", ]
fit.Tradescantia <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Tradescantia)
summary(fit.Tradescantia) # Access to the sort summary table
#Plot:
plot.Tradescantia.starane <- ggforest(fit.Tradescantia,
                                      main = "Tradescantia fluminensis (starane)",
                                      fontsize=1.1) +
  theme(plot.title = element_text(face = "italic", size=20, hjust =0))

#12Verbena ============ 
Verbena <- glyph[glyph$genus=="Verbena", ]
fit.Verbena <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Verbena)
summary(fit.Verbena) # Access to the sort summary table
#Plot:
plot.Verbena <- ggforest(fit.Verbena, main = "Verbena bonariensis (glyphosate)", fontsize = 1.1)+
  theme(plot.title = element_text(face = "italic", size=20, hjust =0))

#GRID-Arrange ALL PLOTS========
#Landscape view, bigger font:
grassesLandscape <- ggarrange(plot.Avena, plot.Bromus , plot.Pennisetum ,
                              plot.Chloris , plot.Ehrharta, plot.Verbena, ncol = 3, nrow = 2)
ggsave(grassesLandscape, filename = "10.GrassesPlusVerbena_Landscape.jpg", width =26, height = 12 )  

weedsRestLandscape <- ggarrange( plot.Anredera,plot.Ageratina,plot.Senna,
                                 plot.Anredera.starane,plot.Lantana.starane,plot.Tradescantia.starane, 
                                 ncol = 3, nrow = 2)
ggsave(weedsRestLandscape, filename = "01.HR_WeedsRest_Landscape.jpg", width = 26, height = 12 )  

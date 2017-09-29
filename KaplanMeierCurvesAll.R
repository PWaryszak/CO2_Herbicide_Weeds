#Kaplan Meier (KM) Curves:
#Multiplot function:
library("tidyverse")
library("survival")
library("survminer")

#Data for Glyphosate Treatment==========
#START FROM HERE:
df<-read.csv("WeedsBinary.csv")
#Glyphosate only:
glyph<-df[df$herbicide=="glyphosate",]
glyph$genus<-factor(glyph$genus)
str(glyph)#2930 obs. of  9 variables:
levels(droplevels(glyph$genus.species))#It should be 14 weed species listed

#NEW TWO_PANELS WAY, UPPER PANEL==========
#These plots to be arranged by species as in Biomass Figure:
#Avena ============ 
Avena<-glyph[glyph$genus=="Avena", ]
fit.Avena <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Avena)
summary(fit.Avena)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Avena,pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Avena barbata (glyphosate)") + theme_classic() + xlab("Week") + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Avena  <- g1
plot.Avena

#Bromus =================
Bromus<-glyph[glyph$genus=="Bromus", ]
fit.Bromus <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Bromus)
summary(fit.Bromus)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Bromus, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Bromus catharticus (glyphosate)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 axis.text.x=element_text(size=20),
                 axis.title.y = element_blank(), #no Survival title
                 legend.text = element_text(size = 10),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Bromus  <- g2
plot.Bromus

#Ehrharta  ============ 
Ehrharta<- glyph[glyph$genus=="Ehrharta", ]
fit.Ehrharta<- survfit( Surv(Week, Status) ~  Dose + CO2, data = Ehrharta)
summary(fit.Ehrharta) # Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Ehrharta, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Ehrharta erecta (glyphosate)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.y = element_blank(), #no Survival title
                 axis.text.x=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Ehrharta  <- g2
plot.Ehrharta


#Cenchrus  ============ 
Pennisetum <- glyph[glyph$genus=="Pennisetum", ]
fit.Pennisetum <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Pennisetum)
summary(fit.Pennisetum) # Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Pennisetum, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Cenchrus clandestinum (glyphosate)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.y = element_blank(), #no Survival title
                 axis.text.x=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Pennisetum  <- g2
plot.Pennisetum

#Chloris  ============ 
Chloris <- glyph[glyph$genus=="Chloris", ]
fit.Chloris <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Chloris)
summary(fit.Chloris) # Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Chloris, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Chloris gayana (glyphosate)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.y = element_blank(), #no Survival title
                 axis.text.x=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Chloris  <- g2
plot.Chloris

#Tradescantia.fluminensis  ============ 
Tradescantia <- glyph[glyph$genus=="Tradescantia", ]
fit.Tradescantia <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Tradescantia)
summary(fit.Tradescantia) # Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Tradescantia, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Tradescantia fluminensis (glyphosate)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = c(.6,.5),
                 axis.text.y=element_text(size=20),
                 axis.title.y = element_blank(), #no Survival title
                 axis.text.x=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Tradescantia  <- g2
plot.Tradescantia

library(gridExtra)
library(grid)

figLongUpper <- grid.arrange(plot.Avena, plot.Bromus,plot.Ehrharta, plot.Pennisetum,plot.Chloris,
                   plot.Tradescantia,  ncol=6)
ggsave(figLongUpper, filename = "PLOT_BIOMASS_6sp_UPPER_PANEL.jpeg", width = 840, height = 140, units = "mm" )  

#MIDDLE PANEL:======
#Ageratina ============ 
Ageratina <- glyph[glyph$genus=="Ageratina", ]
fit.Ageratina <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Ageratina)
summary(fit.Ageratina) # Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Ageratina, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Ageratina adenophora (glyphosate)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Ageratina  <- g2
plot.Ageratina

#Asparagus ============ 
Asparagus<-glyph[glyph$genus=="Asparagus", ]
fit.Asparagus <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Asparagus)
summary(fit.Asparagus)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Asparagus,pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Asparagus aethiopicus (glyphosate)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Asparagus  <- g2
plot.Asparagus

#Cotoneaster =================
Cotoneaster<-glyph[glyph$genus=="Cotoneaster", ]
fit.Cotoneaster <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Cotoneaster)
summary(fit.Cotoneaster)# Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Cotoneaster, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Cotoneaster glaucophyllus (glyphosate)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 axis.text.x=element_text(size=20),
                 axis.title.y = element_blank(), #no Survival title
                 legend.text = element_text(size = 10),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Cotoneaster  <- g2
plot.Cotoneaster

#Lantana ============ 
Lantana<- glyph[glyph$genus=="Lantana", ]
fit.Lantana<- survfit( Surv(Week, Status) ~  Dose + CO2, data = Lantana)
summary(fit.Lantana) # Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Lantana, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Lantana camara (glyphosate)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.y = element_blank(), #no Survival title
                 axis.text.x=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Lantana <- g2
plot.Lantana

#Senna ============ 
Senna <- glyph[glyph$genus=="Senna", ]
fit.Senna <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Senna)
summary(fit.Senna) # Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Senna, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Senna pendula (glyphosate)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.y = element_blank(), #no Survival title
                 axis.text.x=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Senna  <- g2
plot.Senna

#Anredera  ============ 
Anredera <- glyph[glyph$genus=="Anredera", ]
fit.Anredera <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Anredera)
summary(fit.Anredera) # Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Anredera, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Anredera cordifolia (glyphosate)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.y = element_blank(), #no Survival title
                 axis.text.x=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Anredera  <- g2
plot.Anredera

#LOWER PANEL:=========
#Ipomea ============ 
Ipomea <- glyph[glyph$genus=="Ipomea", ]
fit.Ipomea <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Ipomea)
summary(fit.Ipomea) # Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Ipomea, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Ipomoea indica (glyphosate)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=20),
                 axis.title.y=element_text(size=24),
                 legend.text = element_text(size = 16),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Ipomea  <- g2
plot.Ipomea

#Verbena ============ 
Verbena <- glyph[glyph$genus=="Verbena", ]
fit.Verbena <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Verbena)
summary(fit.Verbena) # Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Verbena, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Verbena bonariensis (glyphosate)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.y = element_blank(), #no Survival title
                 axis.text.x=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 plot.title = element_text(lineheight=1.2, face="italic", hjust = 0.5))

plot.Verbena  <- g2
plot.Verbena

library(gridExtra)
library(grid)
figLongUpper <- grid.arrange(plot.Avena, plot.Bromus,plot.Ehrharta, plot.Pennisetum,
                             plot.Chloris, plot.Tradescantia,  ncol=6)
ggsave(figLongUpper, filename = "PLOT_BIOMASS_6sp_UP_PANEL.jpeg", width = 840, height = 140, units = "mm" )  

figLongMiddle <- grid.arrange(plot.Ageratina,plot.Asparagus, plot.Cotoneaster,plot.Lantana,
                             plot.Senna,plot.Anredera,  ncol=6)
ggsave(figLongMiddle, filename = "PLOT_BIOMASS_6sp_MIDDLE_PANEL.jpeg", width = 840, height = 140, units = "mm" )  

figLongLower <- grid.arrange(plot.Ipomea, plot.Verbena, plot.Anredera.starane,
                             plot.Lantana.starane, plot.Tradescantia.starane, ncol=6)
ggsave(figLongLower, filename = "PLOT_BIOMASS_5sp_Lower_PANEL.jpeg", width = 840, height = 140, units = "mm" )  



#Data for Starane Treatment==========
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

#Anredera under Starane ============ 
Anredera <- star[star$genus=="Anredera", ]
fit.Anredera <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Anredera)
summary(fit.Anredera) # Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Anredera, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Anredera cordifolia (starane)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.y = element_blank(), #no Survival title
                 axis.text.x=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 plot.title = element_text(lineheight=1.2, face="bold.italic", hjust = 0.5))

plot.Anredera.starane  <- g2
plot.Anredera.starane

#Lantana under Starane============ 
Lantana<- star[star$genus=="Lantana", ]
fit.Lantana<- survfit( Surv(Week, Status) ~  Dose + CO2, data = Lantana)
summary(fit.Lantana) # Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Lantana, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Lantana camara (starane)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.y = element_blank(), #no Survival title
                 axis.text.x=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 plot.title = element_text(lineheight=1.2, face="bold.italic", hjust = 0.5))

plot.Lantana.starane <- g2
plot.Lantana.starane


#Tradescantia under Starane  ============ 
Tradescantia <- star[star$genus=="Tradescantia", ]
fit.Tradescantia <- survfit( Surv(Week, Status) ~  Dose + CO2, data = Tradescantia)
summary(fit.Tradescantia) # Access to the sort summary table
#Plot:
g<- ggsurvplot(fit.Tradescantia, pval = TRUE, size=2, censor.shape = "strata", color = "strata", linetype = "strata")
g1<- g$plot + ggtitle("Tradescantia fluminensis (starane)") + theme_classic() +xlab("Week")
g2 <- g1 + theme(legend.position = "none",
                 axis.text.y=element_text(size=20),
                 axis.title.y = element_blank(), #no Survival title
                 axis.text.x=element_text(size=20),
                 axis.title.x=element_text(size=20),
                 plot.title = element_text(lineheight=1.2, face="bold.italic", hjust = 0.5))

plot.Tradescantia.starane  <- g2
plot.Tradescantia.starane





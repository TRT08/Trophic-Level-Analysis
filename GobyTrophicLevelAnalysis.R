#######################################
###ROUND GOBY TROPHIC LEVEL ANALYSIS###
#######################################


library(ggplot2)
library(scales)
library(compositions)
library(adehabitat)
library(lme4)
library(paleoMAS)
library(doBy)
library(reshape)
library(reshape2)
library(scales)
library(lubridate)
library(extrafont)
library(plyr)
library(outliers)
library(forams)
library(vegan)
library(FSA)
library(compare)
library(nlme)

#  RUN THESE FIRST TO GET THE NECESSARY FILES FOR THIS SCRIPT TO RUN ########
source("F:/DATA/SLBE/R scripts/Diet biomass and frequency/dietbiomassfreqscript.R",echo=TRUE, max.deparse.length=10000)
NA.omit.biomass.diets<- NA.omit.biomass
rm(NA.omit.biomass)
source("F:/DATA/SLBE/R scripts/Benthos biomass and frequency/biomassfreqscript.R",echo=TRUE, max.deparse.length=10000)
NA.omit.biomass.benthos<- NA.omit.biomass
rm(NA.omit.biomass)
setwd("F:/DATA/SLBE/R scripts/Trophic Level Analysis/")#set directory 

logfile <- file("output.log") #creates a backlog of the script activity (look in folder)
sink(logfile, append=TRUE) 
sink(logfile, append=TRUE, type="message")
source("dietbiomassfreqscript.R",echo=TRUE, max.deparse.length=10000) #Load the data and run calculations
sink()  # Restore output to console
sink(type="message")
rm(fishdiet2,NA.omit.biomass)

#font_import(pattern='arial') #Get arial
#loadfonts(device = "win")

Goby_theme <- theme_bw()+theme(
  text = element_text(family="Arial", colour="black", size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.key = element_blank())+
  theme(axis.title.y = element_text(vjust=1))+ 
  theme(strip.background = element_rect(fill = 'gray97'))

Goby_pres_theme <- theme_bw()+theme(
  text = element_text(colour="black", size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.key = element_blank())+
  theme(axis.title.y = element_text(vjust=1))+ 
  theme(strip.background = element_rect(fill = 'gray97'))

cbPalette <- c("#000000","#999999", "#c0c0c0", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Month_scale <-scale_x_discrete("Month", labels = c("5"= "May","6" = "Jun","7" = "Jul","8" = "Aug","9" = "Sep","10" = "Oct", "11"="Nov"))



###README####
###ALL FISH AND FISH PARTS HAVE BEEN EXCLUDED FROM ANALYSIS
###ALL MUSSEL SHELL FRAGMENTS HAVE BEEN EXCLUDED FROM ANALYSIS
###ALL EMPTY FISH STOMACHS HAVE BEEN EXCLUDED FROM ANALYSIS
###the d dataframe and the measured.biomass frame come from CompiledFishLogandDiets
###and CompiledBenthosLogandFreq, respectively.  I had to go trhough and change several family and order names
###Gammaridae --> Amphipoda, Canthocamptidae --> Harpacticoida, Cyclopidae --> Cyclopoida are a couple
###so that they were the same between the two datasets and to facilitate everything i just saved
###them as csvs so you can just read them into R.  
###IF ANYTHING CHANGES IN THE DATABASE, you gotta go through and do that again.
###6/10/2015  there were a couple of crayfish, generic hemiptera and a parasite that dont have biomass estimates yet
###fill in odonate biomass. havent found any in benthos yet sooooo yeah.  May be a good idea to find a generic
###odonate biomass to put in here...although there was only one found in all fish.
#d$biomass.mg.mean.general[d$Order =="Odonata"] <- d$est.final.biomass.mg[d$Order =="Odonata"]
#names(CompiledFishLogandDiets)
#d <- CompiledFishLogandDiets
#d <- subset(CompiledFishLogandDiets, Cheese.=="N") #remove cheesiness
# <-d[!(d$Diet.Item %in% c("Fish", "Fish Egg", "Empty", "Fish Spp (whole or mostly)", "Fish scales" ,"???")),]
# <-d[!(d$Order %in% c("Fish", "???", "Empty")),]
#d$Month <- month(d$Date)

#c <- CompiledBenthosLogandFreq
#levels(c$Status)
#selected<-c("Processed")
#c <- c[c$Status %in% selected,]
#c$Month <- month(c$Date)
#c$DayNum <- yday(c$Date)
#c<-subset(c, Year >=2011)
#c$Year <- as.factor(c$Year)
######
##fill in holes (heh) with average biomass from benthos data.
##unhash if anything changes in the database.  if thats the case:
##make a df of all the animals that have been measured
#measured.benthos<-subset(c, Num.critters.averaged.4.biomass>="1")
##make a df of all the animals that havent been measured yet
#unmeasured.benthos <-subset(c, is.na(c$ Num.critters.averaged.4.biomass))
#write.csv(measured.benthos, "MeasuredBenthos.csv")
#######



d<-read.csv(file="F:\\DATA\\SLBE\\R scripts\\Diet biomass and frequency\\d.csv", header=TRUE, sep=",")
measured.benthos<-read.csv(file="F:\\DATA\\SLBE\\R scripts\\Trophic Level Analysis\\MeasuredBenthos.csv", header=TRUE, sep=",")

avg.biomass.order<- summaryBy(Av.biomass.mg ~ Order,data=measured.benthos, FUN=c(mean))
names(avg.biomass.order)[names(avg.biomass.order)=="Av.biomass.mg.mean"]<-"biomass.mg.mean.general"


#calculate total number of bugs per stomach
perc <- summaryBy(Total.Organisms.sum ~ YearSERUnique,data=d, FUN=c(sum))
names(perc)[names(perc)=="Total.Organisms.sum.sum"] <-"Gut.total"
d<-join(d,perc, by=c("YearSERUnique"))
#join the average biomasses for each order
d<-join(d,avg.biomass.order, by=c("Order"), match="all")
d$est.final.biomass.mg<-ifelse(is.na(d$Final.biomass.mg), d$biomass.mg.mean.general, d$Final.biomass.mg)
#the est.final.biomass.mg column is then one you will use from now on for anything biomass related
#double check to see what doesnt have biomass estimates yet
unmeasured.diet.items <-subset(d, is.na(d$est.final.biomass.mg))
d <- d[!is.na(d$est.final.biomass.mg),] #remove all animals w/o a biomass estimate
d<-d[!is.na(d$TrophicPosition),] #removes anything w/o a TrophicPosition (nematodes)
unmeasured.diet.items <-subset(d, is.na(d$est.final.biomass.mg))

#d is ready to go



##make df that has each fish with biomasses and N of each diet item
volume.est <-subset(d[,c("YearSERUnique", "Diet.Item", "Order" , "Family" ,"TrophicPosition", 
                         "Total.Organisms.sum", "Gut.total","est.final.biomass.mg")])

#calculate the percent of each organism (N in the equation).  Column called "Gut.total" for now
volume.est$percent<-volume.est$Total.Organisms.sum/volume.est$Gut.total

##Now make a column that has the total biomass in each fish stomach
volume.est$tot.biomass.org<- (volume.est$Gut.total*volume.est$est.final.biomass.mg)
biomass.sum<-summaryBy(tot.biomass.org~YearSERUnique, data=volume.est, FUN=c(sum))
names(biomass.sum)[names(biomass.sum)=="tot.biomass.org.sum"]<-"tot.biomass.per.fish"
volume.est<-join(volume.est,biomass.sum,by=c("YearSERUnique"))

#change the names of the columns so that they fit in the equation
colnames(volume.est)
names(volume.est)[names(volume.est)=="percent"] <-"N"
names(volume.est)[names(volume.est)=="tot.biomass.per.fish"]<- "mass.a"
names(volume.est)[names(volume.est)=="est.final.biomass.mg"] <-"mass.i"
colnames(volume.est)

#OK...NOW  we can FINALLY caluculate percent volume.
volume.est$V<-((volume.est$Total.Organisms.sum*volume.est$mass.i)/volume.est$mass.a)

#now calculate trophic position:
volume.est$TrophEst<-(volume.est$V*volume.est$TrophicPosition)

#Trophic positon for each fish
troph.per.fish<-summaryBy(volume.est$TrophEst~YearSERUnique, data=volume.est, FUN=c(sum))
names(troph.per.fish)[names(troph.per.fish)=="volume.est$TrophEst.sum"] <-"TrophicLevelEst"

###now what you can do is join this to the d dataframe and do some calculations with time
###sizeclass, etc.......

##join trophic level estimates to things like size class etc....
poop<- d[ , c("YearSERUnique","Month","Year","SizeClass","Lengthmm","GeneralLoc","Depth.m.standard","DepNonDep","Site.Condition.For.Event" )]
poop.smoothie<- summaryBy(Month ~ YearSERUnique+Month+Year+SizeClass+Lengthmm+GeneralLoc+ Depth.m.standard + 
                            DepNonDep+Site.Condition.For.Event, data=poop, FUN= (mean))

troph.join <-join(troph.per.fish, poop.smoothie, by=c("YearSERUnique"))

##Final usable df= troph.join
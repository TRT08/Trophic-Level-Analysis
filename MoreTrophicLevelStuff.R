####Misc Goby Trophic Level Analysis

###RUN THESE FIRST TO GET THE NECESSARY FILES FOR THIS SCRIPT TO RUN ########
source("F:/DATA/SLBE/R scripts/Diet biomass and frequency/dietbiomassfreqscript.R",echo=TRUE, max.deparse.length=10000)
NA.omit.biomass.diets<- NA.omit.biomass
rm(NA.omit.biomass)
source("F:/DATA/SLBE/R scripts/Benthos biomass and frequency/biomassfreqscript.R",echo=TRUE, max.deparse.length=10000)
NA.omit.biomass.benthos<- NA.omit.biomass
rm(NA.omit.biomass)
source("F:/DATA/SLBE/R scripts/Trophic Level Analysis/GobyTrophicLevelAnalysis.R",echo=TRUE, max.deparse.length=10000)
NA.omit.biomass.diets<-NA.omit.biomass
rm(NA.omit.biomass)
setwd("F:/DATA/SLBE/R scripts/Trophic Level Analysis/")#set directory 
#############################################

#make a version of troph.join that is only good harbor gobies
GHgobies<-troph.join[!(troph.join$GeneralLoc %in% c("SleepingBearShoals","SouthManitou")),]

##make a dataframe that is a summary of the trophic level estimates and other factors
#average trophic position of each size class
SC.mean<-summaryBy(TrophicLevelEst~SizeClass +Year+Month+GeneralLoc+Site.Condition.For.Event, data=troph.join, FUN=c(length,mean, se, min, max))
SC.mean <-SC.mean[!(SC.mean$GeneralLoc %in% "SleepingBearShoals"),]
SC.mean<-na.omit(SC.mean)
#do this same summary for GH
SC.mean.GH<-subset(SC.mean, GeneralLoc=='GoodHarbor')
SC.mean.GH2012<-subset(SC.mean.GH, Year=="2012")


#scatter plot of trophic level by TL
plot(TrophicLevelEst~Lengthmm,data=troph.join, main="trophic position by Length", ylab="Est. trophic position")
attach(troph.join)
abline(lm(TrophicLevelEst~Lengthmm), col="red") # regression line (y~x)


#boxplots of trophic level by SizeClass and Month
boxplot(TrophicLevelEst~SizeClass,data=troph.join, main="trophic position by SizeClass", ylab="Est. trophic position")
boxplot(TrophicLevelEst~Month,data=troph.join, main="trophic position by Month", ylab="Est. trophic position")

#boxplot of GHgobiees per size class and month
ggplot(GHgobies, aes(factor(Month), TrophicLevelEst))+
  geom_boxplot()+Goby_theme +facet_grid(.~SizeClass)

#barplot of average troph level of GH gobies per habitat & SizeClass
ggplot(SC.mean.GH, aes(x=Month, y=TrophicLevelEst.mean))+
  geom_bar(stat="identity")+Goby_theme +facet_grid(SizeClass~Site.Condition.For.Event)+
  ggtitle('GH goby trophic level estimates per month & SizeClass')


#Avg Goby trophic levels by SizeClass and Site.Condition.For (ugly)
ggplot(SC.mean.GH, aes(x=Month, y=TrophicLevelEst.mean, fill=factor(Site.Condition.For.Event)))+
  geom_bar(stat="identity", position="dodge")+Goby_theme +facet_grid(SizeClass~Year)+
  ggtitle('GH goby trophic level estimates per month & Site condition')+scale_x_discrete()

#Avg Goby trophic levels by SizeClass and Site.Condition.For and GeneralLoc (ugly)
ggplot(SC.mean, aes(x=Month, y=TrophicLevelEst.mean, fill=factor(Site.Condition.For.Event)))+
  geom_bar(stat="identity", position="dodge")+Goby_theme +facet_grid(SizeClass~GeneralLoc)+
  ggtitle('GH goby trophic level estimates per month & Site condition')+scale_x_discrete()


###look at relationship of average GH troph level for each month with all
###SizeClasses pooled in 2012
month<-summaryBy(TrophicLevelEst~ Year+Month+GeneralLoc, data=troph.join, FUN=c(mean, se))
month <-month[!(month$GeneralLoc %in% c("SleepingBearShoals", "SouthManitou")),]
month<-na.omit(month)

bymonth2012<-subset(month, Year=="2012")

plot(TrophicLevelEst.mean~Month,data=bymonth2012, main="GH2012:trophic position by month", ylab="Est. trophic position")
attach(bymonth2012)
abline(lm(TrophicLevelEst.mean~Month), col="red") # regression line (y~x)
detach(bymonth2012)

#faceted plot of average trophic scores at GH 2012 for each size class & hab
ggplot(SC.mean.GH2012, aes(x=Month, y=TrophicLevelEst.mean))+
  geom_point(stat="identity")+Goby_theme +facet_grid(SizeClass~Site.Condition.For.Event)+
  ggtitle('GH goby trophic level estimates per month & SizeClass')+ylim(0,2.5)+stat_smooth(method="lm", se=FALSE)



#lets see if theres any sig differences between size
#classes between habitats over time

#use this to change GHgobies month number to year to test assumptions
GHgobies<-na.omit(GHgobies)
GHgobies$Month[GHgobies$Month %in% "5"] <- "May"
GHgobies$Month[GHgobies$Month %in% "6"] <- "Jun"
GHgobies$Month[GHgobies$Month %in% "7"] <- "Jul"
GHgobies$Month[GHgobies$Month %in% "8"] <- "Aug"
GHgobies$Month[GHgobies$Month %in% "9"] <- "Sep"
GHgobies$Month[GHgobies$Month %in% "10"] <- "Oct"
GHgobies$Month[GHgobies$Month %in% "11"] <- "Nov"

GHlme<-lme(TrophicLevelEst~Site.Condition.For.Event+Month, data=GHgobies)
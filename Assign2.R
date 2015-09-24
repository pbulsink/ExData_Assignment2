#Assignment 2.
#Plots generated stepwise, but this will do it all at once.

NEI<-readRDS("summarySCC_PM25.rds")
SCC<-readRDS("Source_Classification_Code.rds")

#PLOT ONE
years<-unique(NEI$year)
annualEmissions<-sapply(years, function(x) sum(subset(NEI, year==x)$Emissions))

png(file="plot1.png")
plot(years, annualEmissions, col="blue", main="Total Emission by Year", xlab="Year", ylab="Total Emissions")
abline(lm(annualEmissions~years))
dev.off()

#PLOT TWO
baltimoreEmissions<-sapply(years, function(x) sum(subset(NEI, year==x & fips=="24510")$Emissions))

png(file="plot2.png")
plot(years, baltimoreEmissions, col="blue", main="Total Emission by Year", xlab="Year", ylab="Total Emissions")
abline(lm(baltimoreEmissions~years))
dev.off()

#PLOT THREE
sourceTypes<-unique(NEI$type)

#prep the data. melt & Cast
library(reshape2)
baltimoreEmissions<-subset(NEI, fips=="24510")
baltimoreEmissions<-baltimoreEmissions[,!names(baltimoreEmissions) %in% c("fips","SCC","Pollutant")]
baltimoreEmissions<-melt(baltimoreEmissions, measure.vars = "Emissions")
baltimoreEmissions<-dcast(baltimoreEmissions, year~type, sum)
#remelt to make plotting easy
baltimoreEmissions<-melt(baltimoreEmissions, id.vars = "year")

png(file="plot3.png")
ggplot(baltimoreEmissions, aes(year, value))+
    facet_grid(.~variable)+
    geom_point()+
    geom_smooth(method="lm")+
    xlab("Year")+
    ylab("Emission")+
    ggtitle("Emission by Year For Each Source")
dev.off()

#PLOT FOUR
#Find 'Coal' in the unique SCC$EI.Sectors
coalNames<-grep("Coal", unlist(unique(SCC$EI.Sector)))
#Get the text of that:
coalNames<-sapply(coalNames, function(x) as.character(unlist(unique(SCC$EI.Sector))[x]))

#Get the associated SCC:
coalSCC<-as.character(subset(SCC, EI.Sector %in% coalNames)$SCC)

coalEmissions<-subset(NEI, SCC %in% coalSCC)
coalEmissions<-coalEmissions[,!names(coalEmissions) %in% c("fips","SCC","Pollutant", "type")]
coalEmissions<-melt(coalEmissions, measure.vars = "Emissions")
coalEmissions<-dcast(coalEmissions, year~variable, sum)

png(file="plot4.png")
ggplot(coalEmissions, aes(year, Emissions))+
    geom_point()+
    geom_smooth(method="lm")+
    xlab("Year")+
    ylab("Emission")+
    ggtitle("Emission by Year For Coal Emissions")
dev.off()

#PLOT FIVE
#The codebook says 'Mobile - On-Road' is the right EI.Sector to use
#Find 'all of them' in the unique SCC$EI.Sectors
roadNames<-grep("Mobile - On-Road", unlist(unique(SCC$EI.Sector)))
#Get the text of that:
roadNames<-sapply(roadNames, function(x) as.character(unlist(unique(SCC$EI.Sector))[x]))

#Get the associated SCC:
roadSCC<-as.character(subset(SCC, EI.Sector %in% roadNames)$SCC)

roadEmissions<-subset(NEI, SCC %in% roadSCC)
roadEmissions<-roadEmissions[,!names(roadEmissions) %in% c("fips","SCC","Pollutant", "type")]
roadEmissions<-melt(roadEmissions, measure.vars = "Emissions")
roadEmissions<-dcast(roadEmissions, year~variable, sum)

png(file="plot5.png")
ggplot(roadEmissions, aes(year, Emissions))+
    geom_point()+
    geom_smooth(method="lm")+
    xlab("Year")+
    ylab("Emission")+
    ggtitle("Emission by Year For all Motor Vehicles")
dev.off()

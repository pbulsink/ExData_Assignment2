#Assignment 2.
#Plots generated stepwise, but this will do it all at once.

NEI<-readRDS("summarySCC_PM25.rds", colClasses=)
SCC<-readRDS("Source_Classification_Code.rds")

#PLOT ONE
years<-unique(NEI$year)
annualEmissions<-sapply(years, function(x) sum(subset(NEI, year==x)$Emissions))

png(file="plot1.png")
plot(years, annualEmissions, col="blue", main="Total Emission by Year", xlab="Year", ylab="Total Emissions")
abline(lm(annualEmissions~years))
dev.off()


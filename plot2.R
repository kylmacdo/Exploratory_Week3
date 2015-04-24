plot2<- function(){
        #
        #
        

        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        NEI$year<-as.factor(NEI$year)
        
        
        NEI <- NEI[NEI$fips==24510,]
        NEI.data<-data.frame(year=levels(NEI$year))
        
        NEI.data$emission<-tapply(NEI$Emissions,NEI$year,sum)
        png(filename="Plot2.png")
        plot(NEI.data$year,NEI.data$emission,type="l",xlab="Year",ylab="Total PM2.5 emissions per year")
        lines(lowess(NEI.data$year,NEI.data$emission,f=2))

        dev.off()
}
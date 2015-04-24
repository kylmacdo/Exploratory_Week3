plot1<- function(){
        #
        #
        
        
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        NEI$year<-as.factor(NEI$year)
        
        NEI.data<-data.frame(row.names=levels(NEI$year))
        
        NEI.data<-tapply(NEI$Emissions,NEI$year,sum)
        png(filename="Plot1.png")
        plot(names(NEI.data),NEI.data,type="l",xlab="Year",ylab="Total PM2.5 emissions per year")
        dev.off()
}
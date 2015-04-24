plot3<- function(){
        #
        #
        library(ggplot2)        

        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        NEI <- NEI[NEI$fips==24510,]
        NEI$year<-as.factor(NEI$year)
        NEI$type <- as.factor(NEI$type)
                
        NEI.data<-data.frame(year=rep(levels(NEI$year),times=4),type=rep(levels(NEI$type),each=4))

        NEI.data$emission[NEI.data$type=="POINT"]<-tapply(NEI$Emissions[NEI$type=="POINT"],
                                                          NEI$year[NEI$type=="POINT"],
                                                          sum)
        NEI.data$emission[NEI.data$type=="ON-ROAD"]<-tapply(NEI$Emissions[NEI$type=="ON-ROAD"],
                                                          NEI$year[NEI$type=="ON-ROAD"],
                                                          sum)
        NEI.data$emission[NEI.data$type=="NONPOINT"]<-tapply(NEI$Emissions[NEI$type=="NONPOINT"],
                                                          NEI$year[NEI$type=="NONPOINT"],
                                                          sum)
        NEI.data$emission[NEI.data$type=="NON-ROAD"]<-tapply(NEI$Emissions[NEI$type=="NON-ROAD"],
                                                          NEI$year[NEI$type=="NON-ROAD"],
                                                          sum)
        
        
        png(filename="Plot3.png")
        plot(NEI.data$year,NEI.data$emission,type="l",xlab="Year",ylab="Total PM2.5 emissions per year")
        lines(lowess(NEI.data$year,NEI.data$emission,f=2))

        dev.off()
}
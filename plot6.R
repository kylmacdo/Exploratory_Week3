plot6<- function(){
        #
        #
        library(ggplot2)        

        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        
        #Find SCC codes for motor or vehicle related sources
        motor.sources<-SCC$SCC[grepl("motor",SCC$Short.Name, ignore.case=TRUE)|
                                 grepl("vehicle",SCC$Short.Name,ignore.case=TRUE)]
        
        #Subset all NEI data based on The city of Baltimore or Los Angeles County
        NEI <- NEI[NEI$fips=="24510"|NEI$fips=="06037",]
        
        #Subset all NEI data based on whether the NEI SCC code is in our list of motor vehicle based sources
        NEI <- NEI[NEI$SCC %in% motor.sources,]
        NEI$year<-as.factor(NEI$year)
        NEI$type <- as.factor(NEI$type)
        NEI$fips <- as.factor(NEI$fips)
        
        levels(NEI$fips)[levels(NEI$fips)=="06037"] <- "Los Angeles County"
        levels(NEI$fips)[levels(NEI$fips)=="24510"] <- "Baltimore City"
                
        #NEI.data<-data.frame(year=levels(NEI$year))
                
        #NEI.data$emissions<-tapply(NEI$Emissions,NEI$year,sum)
       
       
        NEI.data<-data.frame(year=rep(levels(NEI$year),times=2),fips=rep(levels(NEI$fips),each=4))
        
        NEI.data$emission[NEI.data$fips=="Los Angeles County"]<-tapply(NEI$Emissions[NEI$fips=="Los Angeles County"],
                                                          NEI$year[NEI$fips=="Los Angeles County"],
                                                          sum)
        NEI.data$emission[NEI.data$fips=="Baltimore City"]<-tapply(NEI$Emissions[NEI$fips=="Baltimore City"],
                                                            NEI$year[NEI$fips=="Baltimore City"],
                                                            sum)
       
        
        plot <- ggplot(NEI.data,aes(year,emission)) + geom_point(shape=1, size=1) + facet_grid(. ~ fips) + theme_bw(base_size = 3)
        ggsave(plot,filename="Plot6.png",width=3,height=2)
        
        
        #plot <- ggplot(NEI.data,aes(year,emissions)) + 
         # geom_point(shape=1, size=1) +
         # theme_bw(base_family = "Avenir", base_size = 2.5) +
         # labs(title="Emissions totals for motor vehicle based sources in Baltimore")
        #ggsave(plot,filename="Plot6.png",width=3,height=2)

}
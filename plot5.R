plot5<- function(){
        #
        #
        library(ggplot2)        

        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        
        #Find SCC codes for motor or vehicle related sources
        motor.sources<-SCC$SCC[grepl("motor",SCC$Short.Name, ignore.case=TRUE)|
                                 grepl("vehicle",SCC$Short.Name,ignore.case=TRUE)]
        
        #Subset all NEI data based on The city of Baltimore
        NEI <- NEI[NEI$fips==24510,]
        
        #Subset all NEI data based on whether the NEI SCC code is in our list of motor vehicle based sources
        NEI <- NEI[NEI$SCC %in% motor.sources,]
        NEI$year<-as.factor(NEI$year)
        NEI$type <- as.factor(NEI$type)
                
        NEI.data<-data.frame(year=levels(NEI$year))
        
        
        NEI.data$emissions<-tapply(NEI$Emissions,NEI$year,sum)
       
        plot <- ggplot(NEI.data,aes(year,emissions)) + 
          geom_point(shape=1, size=1) +
          theme_bw(base_family = "Avenir", base_size = 2.5) +
          labs(title="Emissions totals for motor vehicle based sources in Baltimore")
        ggsave(plot,filename="Plot5.png",width=3,height=2)

}
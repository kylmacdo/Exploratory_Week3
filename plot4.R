plot4<- function(){
        #
        #
        library(ggplot2)        

        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        
        #Find SCC codes for coal related sources
        coal.sources<-SCC$SCC[grepl("coal",SCC$Short.Name, ignore.case=TRUE)]
        
        #Subset all NEI data based on whether the NEI SCC code is in our list of coal based sources
        NEI <- NEI[NEI$SCC %in% coal.sources,]
        NEI$year<-as.factor(NEI$year)
        NEI$type <- as.factor(NEI$type)
                
        NEI.data<-data.frame(year=levels(NEI$year))
        
        
        NEI.data$emissions<-tapply(NEI$Emissions,NEI$year,sum)
       
        plot <- ggplot(NEI.data,aes(year,emissions)) + 
          geom_point(shape=1, size=1) +
          theme_bw(base_family = "Avenir", base_size = 2.5) +
          labs(title="Emissions totals for coal based sources across the US")
        ggsave(plot,filename="Plot4.png",width=3,height=2)

}
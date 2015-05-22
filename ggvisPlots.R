##Gathers data and computes Share Price for a stuck, outputs a plot
SPvis<- function(Ticker,months=6){
        library(ggplot2)
        library(ggvis)
        library(dplyr)
        library(grid)
        library(quantmod)
        library(TTR)
        library(lubridate)
        library(scales)
        histdata <- getSymbols.yahoo(paste(Ticker,".AX",sep = ""),env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
        histdata[,7] <- row.names(histdata)
        histdata$V7 <- as.POSIXlt(histdata$V7,format = "%Y-%m-%d")
        histdata$V7 <- as.Date(histdata$V7)
        rownames(histdata) <- NULL
        histdata <- histdata[,c(7,4)]
        colnames(histdata) <- c("Date","SharePrice")
        histdata$Date <- as.Date(histdata$Date)
        data <- histdata
        rownames(data) <- NULL
        data$MA50 <- SMA(x = data$SharePrice,n=50)
        data$EMA50 <- EMA(x = data$SharePrice,n = 50)
        data <- data[data$Date >  Sys.Date() - months(months),]
        data <- na.exclude(data)
        data$min <- min(data$SharePrice)*0.8
        
        
        ##Share Price Chart
        b <- data %>%
                ggvis(~Date,~SharePrice)%>%
                layer_lines(stroke := "darkorange", strokeWidth := 1)%>%
                layer_lines(x = ~Date,y = ~MA50,stroke := "red",strokeWidth := 1)%>%
                layer_lines(x = ~Date,y = ~EMA50,stroke := "black",strokeWidth := 1)%>%
                layer_ribbons(x = ~Date, y = ~SharePrice, fill := "orange",fillOpacity := 0.5, y2 = 0)
              
                
                
                
                #layer_ribbons(aes(x = Date,ymin=min, ymax=SharePrice), fill="orange",alpha=0.5)+
                #geom_line(aes(x = Date,y = SharePrice),color = "darkorange",lwd=0.5) +
                #geom_line(aes(x = Date,y = MA50,color="MA50"),lwd=0.5) +
                #geom_line(aes(x = Date,y = EMA50,color = "EMA50"),lwd=0.5) +
                #scale_color_manual(values=c("MA50" = "red","EMA50"="black"))+
                #theme(legend.title=element_blank(),
                      #plot.title = element_text(lineheight=1,face="bold",vjust = 0.25,hjust = 0.0),
                      #legend.justification=c(0,0), 
                     # legend.position=c(0,0),
                     # legend.background = element_rect(colour = 'lightgrey', fill = 'lightgrey'),
                     # plot.margin=unit(c(0,10,1,3),"mm"))+ 
               # scale_x_date(expand=c(0,0)) + 
                #scale_y_continuous(labels = dollar_format(largest_with_cents = 5),
                                  # limits = c(min(data$SharePrice)*0.8,max(data$SharePrice)*1.1),expand = c(0,0))+
                #labs(title = paste(Ticker," Share Price",sep = ""),x = NULL,y = NULL)+
                #geom_text(data = subset(data[nrow(data),]),aes(x = Date,y = SharePrice, label = SharePrice),hjust=1, vjust=0,size=4,colour = "darkgreen") +
                #geom_text(data = subset(data[nrow(data),]),aes(x = Date,y = SharePrice, label = Date),hjust=1, vjust=-1.5,size=4,colour = "darkgreen")
        b
        
}
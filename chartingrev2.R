##Reads ASX Data for a particular stockv
##random change by jmoore!!!
readASX <- function(Ticker,Days){
  library(ggplot2)
  library(dplyr)
  library(grid)
  library(quantmod)
  histdata <- getSymbols.yahoo(paste(Ticker,".AX",sep = ""),env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
  histdata[,7] <- row.names(histdata)
  histdata$V7 <- as.POSIXlt(histdata$V7,format = "%Y-%m-%d")
  histdata$V7 <- as.Date(histdata$V7)
  stockdata<-read.csv(paste("http://chartapi.finance.yahoo.com/instrument/1.0/",Ticker,".AX/chartdata;type=quote;range=",Days,"d/csv",sep = ""),skip=19,header = FALSE)
  stockdata[,1] <- as.POSIXct(stockdata$V1,origin = "1970-01-01")
  colnames(stockdata) <- c("Timestamp","Close","High","Low","Open","Volume")
  stockdata <- mutate(stockdata,CumulativeVolume = cumsum(stockdata$Volume))
  
  a <- qplot(data = stockdata,x = Timestamp,y = CumulativeVolume,geom = "line",xlab = "Time",ylab = "Cumulative Volume",main = paste(Ticker," Volume Traded",sep = ""),colour = "blue")+ theme(legend.position = "none")
  
  b <- qplot(data = stockdata,x = Timestamp,y = Close,geom = "line",xlab = "Time",ylab = "Stock Price",main = paste(Ticker," Share Price",sep = ""),colour = "blue")+ theme(legend.position = "none")
 
  c <- qplot(data = stockdata,x = Timestamp,y = Volume,geom = "line",xlab = "Time",ylab = "Volume Traded",main = paste(Ticker," Volume by Quantile",sep = ""),colour = "blue")+ theme(legend.position = "none")
  
  d <- qplot(data = histdata,x = V7,y = histdata[,5],geom = "line",xlab = "Date",ylab = "Volume Traded",xlim = c(as.Date("2015-01-1"),Sys.Date()),main = paste(Ticker," Volume by Day",sep = ""),colour = "blue")+ theme(legend.position = "none")
 
  e <- qplot(data = histdata,x = V7,y = histdata[,4],geom = "line",xlab = "Date",ylab = "Share Price",xlim = c(as.Date("2015-01-1"),Sys.Date()),main = paste(Ticker," Share Price by Day",sep = ""),colour = "blue")+ theme(legend.position = "none")
  
  multiplot(a,b,c,d,e,cols=1)
  
}
  
##Function for printing multiple plts
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }

##Outputs a list of stocks shorted today by volume and %
shortlist <- function(){
  library(dplyr)
  short <- read.fwf("http://www.asx.com.au/data/shortsell.txt",skip = 8,widths = c(4,38,15,20,15,10))
  short <- as.data.frame(apply(short,2,function(x)gsub('\\s+', '',x)))
  colnames(short) <- c("ASXCode","CompanyName","Product","ShortSales","IssuedCapital","Shorted")
  short <- arrange(short,desc(Shorted))
  short
}

##Gathers short history data for a stock from the ASX and outputs the data and plot to a list with % of stock traded as the data
shorthistory <- function(Ticker){
  library(reshape2)
  shorthistory <- read.csv("http://asic.gov.au/Reports/YTD/2015/RR20150511-001-SSDailyYTD.csv",skip=1,fileEncoding = "UTF-16",sep = "\t")
  shorthistory <- shorthistory[-(1:2),]
  shorthistory <- cbind(Row.Names = rownames(shorthistory), shorthistory)
  rownames(shorthistory) <- NULL
  colnames(shorthistory) <- substr(colnames(shorthistory),2,11)
  colnames(shorthistory)[1] <- "Company"
  colnames(shorthistory)[2] <- "Ticker"
  shorthist1 <- shorthistory[,1:2]
  i=3 ##start at first volume column with short data
  while(i<=length(colnames(shorthistory))){
    if(i%%2 == 0){
      shorthist1 <- cbind(shorthist1,shorthistory[i])
      i <- i+1
      }
    else{
      i <- i+1
    }
  }
  melted <- melt(data = shorthist1,id = c("Ticker","Company"))
  melted$variable <- as.POSIXlt(x = melted$variable,format = "%Y.%m.%d")
  melted$value[melted$value==""] <- 0.00
  melted$value <- as.numeric(melted$value)
  a <- melted[grep(pattern = Ticker,x = melted$Ticker),]
  b <- qplot(data = melted[grep(pattern = Ticker,x = melted$Ticker),],x = variable,y = value,main = paste("Shorting % of Total Stocks on Issue for ",Ticker,sep = ""),xlab = "2015 Date",ylab = "Percentage Shorted",geom = "line",width = 1)
  list(data = a,plot = b)
}

##Gathers short history data for a stock from the ASX and outputs the data and plot to a list with volume as the data
shorthistoryvolume <- function(Ticker){
  library(reshape2)
  shorthistory <- read.csv("http://asic.gov.au/Reports/YTD/2015/RR20150511-001-SSDailyYTD.csv",skip=1,fileEncoding = "UTF-16",sep = "\t")
  shorthistory <- shorthistory[-(1:2),]
  shorthistory <- cbind(Row.Names = rownames(shorthistory), shorthistory)
  rownames(shorthistory) <- NULL
  colnames(shorthistory) <- substr(colnames(shorthistory),2,11)
  colnames(shorthistory)[1] <- "Company"
  colnames(shorthistory)[2] <- "Ticker"
  shorthist1 <- shorthistory[,1:2]
  ##Add date columns to volume headers
  i=3
  while(i <= length(colnames(shorthistory))){
    if(i%%2 != 0){
      colnames(shorthistory)[i] <- colnames(shorthistory)[i+1]
      i <- i+1
    }
    else{
      i <- i+1
    }
  }
  ##Remove percentage column to just retain volume
  i=3 ##start at first volume column with short data
    while(i<=length(colnames(shorthistory))){
    if(i%%2 != 0){
      shorthist1 <- cbind(shorthist1,shorthistory[i])
      i <- i+1
    }
    else{
      i <- i+1
    }
  }
  melted <- melt(data = shorthist1,id = c("Ticker","Company"))
  melted$variable <- as.POSIXlt(x = melted$variable,format = "%Y.%m.%d")
  melted$value[melted$value==""] <- 0.00
  melted$value <- as.numeric(melted$value)
  a <- melted[grep(pattern = Ticker,x = melted$Ticker),]
  b <- qplot(data = melted[grep(pattern = Ticker,x = melted$Ticker),],x = variable,y = value,main = paste("Shorting % of Total Stocks on Issue for ",Ticker,sep = ""),xlab = "2015 Date",ylab = "Percentage Shorted",geom = "line",width = 1)
  list(data = a,plot = b)
}

##Plots multiple Volume charts (Volume, volume Shorted, Short Ratio, and Share Price)
volumeplot <- function(Ticker){
  library(ggplot2)
  library(dplyr)
  library(grid)
  library(quantmod)
  histdata <- getSymbols.yahoo(paste(Ticker,".AX",sep = ""),env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
  histdata[,7] <- row.names(histdata)
  histdata$V7 <- as.POSIXlt(histdata$V7,format = "%Y-%m-%d")
  histdata$V7 <- as.Date(histdata$V7)
  shortdata <- shorthistoryvolume(Ticker)$data
  rownames(shortdata) <- NULL
  rownames(histdata) <- NULL
  shortdata <- shortdata[,c(3,4)]
  colnames(shortdata) <- c("Date","ShortVolume")
  histdata <- histdata[,c(7,5,4)]
  colnames(histdata) <- c("Date","BuyVolume","SharePrice")
  histdata$Date <- as.Date(histdata$Date)
  shortdata$Date <- as.Date(shortdata$Date)
  combined <- merge(x = histdata,y = shortdata,by = "Date")
  melted <- melt(combined,id.vars = "Date")
  ggplot( data = melted, aes( x = Date,y = value,colour = variable)) + 
    geom_line() + 
    labs(title = paste("Comparison of Open Volume Shorted to Daily Trade Volume for ",Ticker,sep = ""),x = "2015 Date",y = "Volume")
  
  a <- ggplot(data = combined,aes(x = Date,y = BuyVolume)) + 
    geom_line(color = "green",lwd=1) +
    labs(title = paste(Ticker," Volume Traded",sep = ""),x = "2015 Date",y = "Volume")

  b <- ggplot(data = combined,aes(x = Date,y = ShortVolume)) + 
    geom_line(color = "red",lwd=1) +
    labs(title = paste(Ticker," Volume Shorted",sep = ""),x = "2015 Date",y = "Volume")
  
  c <- ggplot(data = combined,aes(x = Date,y = SharePrice)) + 
    geom_line(color = "lightpink4",lwd=1) +
    labs(title = paste(Ticker," Share Price",sep = ""),x = "2015 Date",y = "Share Price")
  
  combined$ShortRatio <- combined$ShortVolume/combined$BuyVolume
  
  d <- ggplot(data = combined,aes(x = Date,y = ShortRatio)) + 
    geom_line(color = "black",lwd=1) +
    geom_hline(yintercept = 8,lwd =1,color = "red") +
    labs(title = paste(Ticker," Short Ratio",sep = ""),x = "2015 Date",y = "Short Ratio")
  
  list(BuyVolume = a,ShortVolume = b,SharePrice = c, ShortRatio = d)
  
}

##Moving Average Convergence Divergence - outputs multiple charts
MACD <- function(Ticker,months=6,ShortVolume=TRUE,ShortRatio=TRUE){
  library(ggplot2)
  library(dplyr)
  library(grid)
  library(quantmod)
  library(TTR)
  library(lubridate)
  histdata <- getSymbols.yahoo(paste(Ticker,".AX",sep = ""),env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
  histdata[,7] <- row.names(histdata)
  histdata$V7 <- as.POSIXlt(histdata$V7,format = "%Y-%m-%d")
  histdata$V7 <- as.Date(histdata$V7)
  rownames(histdata) <- NULL
  histdata <- histdata[,c(7,4)]
  colnames(histdata) <- c("Date","SharePrice")
  histdata$Date <- as.Date(histdata$Date)
  data <- histdata
  data$EMA12 <- EMA(x = data$SharePrice,n = 12)
  data$EMA26 <- EMA(x = data$SharePrice,n = 26)
  data <- data[-(1:26),]
  data$MACD <- data$EMA12 - data$EMA26
  data$SIGNAL <- EMA(x = data$MACD,n = 9)
  data <- data[-(1:8),] 
  data$difference <- data$MACD-data$SIGNAL
  rownames(data) <- NULL
  data <- data[data$Date >  Sys.Date() - months(months),]
  ##MACD Chart
  a <- ggplot(data = data) + 
    geom_line(aes(x = Date,y = MACD,color = "MACD"),lwd=0.5) +
    geom_line(aes(x = Date,y = SIGNAL,color = "Signal Line"),lwd=0.5) +
    geom_bar(aes(x=Date,y=difference),color="blue",position = "identity",stat = "identity",alpha=I(.2),fill=ifelse(data$difference > 0,"green","red"),linetype=0)+
    scale_color_manual(values=c("MACD" = "black","Signal Line" = "red","MACD-Signal" = "blue"))+
    theme(legend.title=element_blank(),plot.title = element_text(lineheight=1, face="bold"))+
    labs(title = paste(Ticker," MA Convergence Divergence Chart",sep = ""),x = "2015 Date",y = "Ratio")
  ##Share Price Chart
  b <- ggplot(data = data) + 
    geom_line(aes(x = Date,y = SharePrice,color = "Share Price"),lwd=0.5) +
    scale_color_manual(values=c("Share Price" = "black"))+
    theme(legend.title=element_blank(),plot.title = element_text(lineheight=1, face="bold"))+ 
    labs(title = paste(Ticker," Share Price",sep = ""),x = "2015 Date",y = "Share Price")
    
  e <- tradevolume(Ticker,months)
  if(ShortRatio==TRUE) {c <- shortratio(Ticker,months)}
  if(ShortVolume==TRUE){d <- shortvolume(Ticker,months)}
  if(ShortRatio==TRUE && ShortVolume==TRUE){multiplot(b,a,c,d,e,cols=1)}
  if(ShortRatio!=TRUE && ShortVolume==TRUE){multiplot(b,a,d,e,cols=1)}
  if(ShortRatio==TRUE && ShortVolume!=TRUE){multiplot(b,a,c,e,cols=1)}
  if(ShortRatio!=TRUE && ShortVolume!=TRUE){multiplot(b,a,e,cols=1)}
}

##Gathers data and computes short data for a stock, outputs a plot
shortratio <- function(Ticker,months){
    library(ggplot2)
    library(dplyr)
    library(grid)
    library(quantmod)
    library(lubridate)
    histdata <- getSymbols.yahoo(paste(Ticker,".AX",sep = ""),env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
    histdata[,7] <- row.names(histdata)
    histdata$V7 <- as.POSIXlt(histdata$V7,format = "%Y-%m-%d")
    histdata$V7 <- as.Date(histdata$V7)
    shortdata <- shorthistoryvolume(Ticker)$data
    rownames(shortdata) <- NULL
    rownames(histdata) <- NULL
    shortdata <- shortdata[,c(3,4)]
    colnames(shortdata) <- c("Date","ShortVolume")
    histdata <- histdata[,c(7,5,4)]
    colnames(histdata) <- c("Date","BuyVolume","SharePrice")
    histdata$Date <- as.Date(histdata$Date)
    shortdata$Date <- as.Date(shortdata$Date)
    combined <- merge(x = histdata,y = shortdata,by = "Date")
    melted <- melt(combined,id.vars = "Date")
    combined$ShortRatio <- combined$ShortVolume/combined$BuyVolume
    combined <- combined[combined$Date >  Sys.Date() - months(months),]
    a <- ggplot(data = combined,aes(x = Date)) + 
        geom_bar(aes(x=Date,y = ShortRatio,color="ShortRatio"),bin=5,position = "identity",stat = "identity",alpha=I(.5),linetype=0,fill="red")+
        theme(legend.title=element_blank(),
              plot.title = element_text(lineheight=0.25,face="bold",vjust = -1.5,hjust = 0.0),
              axis.text.x=element_blank(),
              legend.justification=c(0,1), 
              legend.position=c(0,1),
              panel.grid.major = element_line(colour="grey"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour="grey",fill = NA),
              panel.background = element_blank(),
              axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank(),
              plot.margin=unit(c(0,5,0,3),"mm"))+
        scale_color_manual(values=c("ShortRatio" = "red"))+
        scale_x_date(expand=c(0,0),limits=c(Sys.Date() - months(months),Sys.Date())) + 
        scale_y_continuous(limits = c(0,max(combined$ShortRatio)*1.1),expand = c(0,0))+
        labs(title = NULL,x = NULL,y = NULL)
    a
    
        
        ##geom_line(aes(color="Short Ratio"),lwd=0.5) +
        ##geom_hline(yintercept = 8,lwd =1,color = "red") +
        ##scale_color_manual(values=c("Short Ratio" = "black"))+
        ##theme(legend.title=element_blank(),plot.title = element_text(lineheight=1, face="bold"))+
        ##labs(title = paste(Ticker," Short Ratio",sep = ""),x = "2015 Date",y = "Short Ratio")
        
}

##Gathers data and computes short volume for a stock, outputs a plot
shortvolume <- function(Ticker,months){
    library(ggplot2)
    library(dplyr)
    library(grid)
    library(quantmod)
    library(lubridate)
    histdata <- getSymbols.yahoo(paste(Ticker,".AX",sep = ""),env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
    histdata[,7] <- row.names(histdata)
    histdata$V7 <- as.POSIXlt(histdata$V7,format = "%Y-%m-%d")
    histdata$V7 <- as.Date(histdata$V7)
    shortdata <- shorthistoryvolume(Ticker)$data
    rownames(shortdata) <- NULL
    rownames(histdata) <- NULL
    shortdata <- shortdata[,c(3,4)]
    colnames(shortdata) <- c("Date","ShortVolume")
    histdata <- histdata[,c(7,5,4)]
    colnames(histdata) <- c("Date","BuyVolume","SharePrice")
    histdata$Date <- as.Date(histdata$Date)
    shortdata$Date <- as.Date(shortdata$Date)
    combined <- merge(x = histdata,y = shortdata,by = "Date")
    melted <- melt(combined,id.vars = "Date")
    combined$ShortRatio <- combined$ShortVolume/combined$BuyVolume
    data <- combined[combined$Date >  Sys.Date() - months(months),]

    b <- ggplot(data = combined,aes(x = Date,y = ShortVolume)) + 
        geom_line(aes(color="Short Volume"),lwd=0.5) +
        scale_color_manual(values=c("Short Volume" = "black"))+
        theme(legend.title=element_blank(),plot.title = element_text(lineheight=1, face="bold"))+
        labs(title = paste(Ticker," Short Volume",sep = ""),x = "2015 Date",y = "Short Volume")
    b
}

##Gathers data and computes short volume for a stock, outputs a plot
tradevolume <- function(Ticker,months=6){
    library(ggplot2)
    library(dplyr)
    library(grid)
    library(quantmod)
    library(lubridate)
    library(scales)
    histdata <- getSymbols.yahoo(paste(Ticker,".AX",sep = ""),env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
    histdata[,7] <- row.names(histdata)
    histdata$V7 <- as.POSIXlt(histdata$V7,format = "%Y-%m-%d")
    histdata$V7 <- as.Date(histdata$V7)
    rownames(histdata) <- NULL
    histdata <- histdata[,c(7,5,4)]
    colnames(histdata) <- c("Date","BuyVolume","SharePrice")
    histdata$Date <- as.Date(histdata$Date)
    data <- histdata[histdata$Date >  Sys.Date() - months(months),]
    rownames(data) <- NULL
    a <- ggplot(data = data,x=Date) + 
        geom_bar(aes(x=Date,y = BuyVolume,color = "Volume"),bin=5,position = "identity",stat = "identity",alpha=I(.4),linetype=0,fill="blue")+
        theme(legend.title=element_blank(),
              plot.title = element_text(lineheight=0.25,face="bold",vjust = -1.5,hjust = 0.0),
              axis.text.x=element_blank(),
              legend.justification=c(0,1), 
              legend.position=c(0,1),
              panel.grid.major = element_line(colour="grey"),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour="grey",fill = NA),
              panel.background = element_blank(),
              axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank(),
              plot.margin=unit(c(0,5,0,3),"mm"))+
        scale_color_manual(values=c("Volume" = "blue"))+
        scale_x_date(expand=c(0,0)) + 
        scale_y_continuous(limits = c(0,max(data$BuyVolume)*1.1),expand = c(0,0))+
        labs(title = NULL,x = NULL,y = NULL)
    a

}

##Gathers data and computes MACD for a stock, outputs a plot
MACDsolo <- function(Ticker,months=6){
    library(ggplot2)
    library(dplyr)
    library(grid)
    library(quantmod)
    library(TTR)
    library(lubridate)
    histdata <- getSymbols.yahoo(paste(Ticker,".AX",sep = ""),env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
    histdata[,7] <- row.names(histdata)
    histdata$V7 <- as.POSIXlt(histdata$V7,format = "%Y-%m-%d")
    histdata$V7 <- as.Date(histdata$V7)
    rownames(histdata) <- NULL
    histdata <- histdata[,c(7,4)]
    colnames(histdata) <- c("Date","SharePrice")
    histdata$Date <- as.Date(histdata$Date)
    data <- histdata
    data$EMA12 <- EMA(x = data$SharePrice,n = 12)
    data$EMA26 <- EMA(x = data$SharePrice,n = 26)
    data <- data[-(1:26),]
    data$MACD <- data$EMA12 - data$EMA26
    data$SIGNAL <- EMA(x = data$MACD,n = 9)
    data <- data[-(1:8),] 
    data$difference <- data$MACD-data$SIGNAL
    rownames(data) <- NULL
    data <- data[data$Date >  Sys.Date() - months(months),]
    ##MACD Chart
    a <- ggplot(data = data) + 
        geom_line(aes(x = Date,y = MACD,color = "MACD"),lwd=0.5) +
        geom_ribbon(data = data,aes(x =Date, ymax=MACD, ymin=SIGNAL),colour = NA, fill="orange", alpha=.1)+
        geom_line(aes(x = Date,y = SIGNAL,color = "Signal Line"),lwd=0.5) +
        geom_bar(aes(x=Date,y=difference),color="blue",position = "identity",stat = "identity",alpha=I(.2),fill=ifelse(data$difference > 0,"green","red"),linetype=0)+
        scale_color_manual(values=c("MACD" = "black","Signal Line" = "red","MACD-Signal" = "blue"))+
        theme(legend.title=element_blank(),
              axis.text.x=element_blank(),
              legend.justification=c(0,1), 
              legend.position=c(0,1),
              axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank(),
              legend.background = element_rect(colour = 'lightgrey', fill = 'lightgrey'),
              plot.margin=unit(c(0,5,0,3),"mm"))+
        scale_x_date(expand=c(0,0)) +
        labs(title = NULL,x = NULL,y = NULL)
    a
}

##Gathers data and computes Share Price for a stuck, outputs a plot
SPsolo <- function(Ticker,months=6){
    library(ggplot2)
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
    b <- ggplot(data = data,x = Date) + 
        geom_ribbon(aes(x = Date,ymin=min, ymax=SharePrice), fill="orange",alpha=0.5)+
        geom_line(aes(x = Date,y = SharePrice),color = "darkorange",lwd=0.5) +
        geom_line(aes(x = Date,y = MA50,color="MA50"),lwd=0.5) +
        geom_line(aes(x = Date,y = EMA50,color = "EMA50"),lwd=0.5) +
        scale_color_manual(values=c("MA50" = "red","EMA50"="black"))+
        theme(legend.title=element_blank(),
              plot.title = element_text(lineheight=1,face="bold",vjust = 0.25,hjust = 0.0),
              legend.justification=c(0,0), 
              legend.position=c(0,0),
              legend.background = element_rect(colour = 'lightgrey', fill = 'lightgrey'),
              plot.margin=unit(c(0,10,1,3),"mm"))+ 
        scale_x_date(expand=c(0,0)) + 
        scale_y_continuous(labels = dollar_format(largest_with_cents = 5),
                           limits = c(min(data$SharePrice)*0.8,max(data$SharePrice)*1.1),expand = c(0,0))+
        labs(title = paste(Ticker," Share Price",sep = ""),x = NULL,y = NULL)+
        geom_text(data = subset(data[nrow(data),]),aes(x = Date,y = SharePrice, label = SharePrice),hjust=1, vjust=0,size=4,colour = "darkgreen") +
        geom_text(data = subset(data[nrow(data),]),aes(x = Date,y = SharePrice, label = Date),hjust=1, vjust=-1.5,size=4,colour = "darkgreen")
    b

}

##multiplot for many plots...need to research
{
    rbind_gtable_max <- function(...){
        
        gtl <- list(...)
        stopifnot(all(sapply(gtl, is.gtable)))
        bind2 <- function (x, y) 
        {
            stopifnot(ncol(x) == ncol(y))
            if (nrow(x) == 0) 
                return(y)
            if (nrow(y) == 0) 
                return(x)
            y$layout$t <- y$layout$t + nrow(x)
            y$layout$b <- y$layout$b + nrow(x)
            x$layout <- rbind(x$layout, y$layout)
            x$heights <- gtable:::insert.unit(x$heights, y$heights)
            x$rownames <- c(x$rownames, y$rownames)
            x$widths <- grid::unit.pmax(x$widths, y$widths)
            x$grobs <- append(x$grobs, y$grobs)
            x
        }
        
        Reduce(bind2, gtl)
    }
    
    cbind_gtable_max <- function(...){
        
        gtl <- list(...)
        stopifnot(all(sapply(gtl, is.gtable)))
        bind2 <- function (x, y) 
        {
            stopifnot(nrow(x) == nrow(y))
            if (ncol(x) == 0) 
                return(y)
            if (ncol(y) == 0) 
                return(x)
            y$layout$l <- y$layout$l + ncol(x)
            y$layout$r <- y$layout$r + ncol(x)
            x$layout <- rbind(x$layout, y$layout)
            x$widths <- gtable:::insert.unit(x$widths, y$widths)
            x$colnames <- c(x$colnames, y$colnames)
            x$heights <- grid::unit.pmax(x$heights, y$heights)
            x$grobs <- append(x$grobs, y$grobs)
            x
        }
        Reduce(bind2, gtl)
    }
}

dualplot<- function(A,B){
    library(gridExtra)
    library(ggplot2)
    gA <- ggplotGrob(A)
    gB <- ggplotGrob(B)
    maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
    gA$widths[2:5] <- as.list(maxWidth)
    gB$widths[2:5] <- as.list(maxWidth)
    grid.arrange(gA, gB, ncol=1,heights=c(2, 1))
}

triplot<- function(A,B,C){
    library(gridExtra)
    library(ggplot2)
    gA <- ggplotGrob(A)
    gB <- ggplotGrob(B)
    gC <- ggplotGrob(C)
    maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5],gC$widths[2:5])
    gA$widths[2:5] <- as.list(maxWidth)
    gB$widths[2:5] <- as.list(maxWidth)
    gC$widths[2:5] <- as.list(maxWidth)
    grid.arrange(gA, gB, gC, ncol=1,heights=c(2, 1,1))
}

MACDplot<- function(Ticker,months=6){
    library(gridExtra)
    library(ggplot2)
    gA <- ggplotGrob(SPsolo(Ticker,months))
    gB <- ggplotGrob(tradevolume(Ticker,months))
    gC <- ggplotGrob(MACDsolo(Ticker,months))
    gD <- ggplotGrob(shortratio(Ticker,months))
    maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5],gC$widths[2:5],gD$widths[2:5])
    gA$widths[2:5] <- as.list(maxWidth)
    gB$widths[2:5] <- as.list(maxWidth)
    gC$widths[2:5] <- as.list(maxWidth)
    gD$widths[2:5] <- as.list(maxWidth)
    png(filename = paste(Ticker," MACD for ",Sys.Date(),".png"),width = 400,height = 210,res = 1200,units = "mm")
    grid.arrange(gA, gC, gB, gD, ncol=1, heights=c(3,3,1,1))
    dev.off()  
    grid.arrange(gA, gC, gB, gD, ncol=1, heights=c(3,3,1,1))
}

##Chaikan Oscillator 
Chaikan <- function(Ticker,months=6){
#Gathers data and computes Share Price for a stuck, outputs a plot
##For info http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:chaikin_oscillator
##or http://www.investopedia.com/articles/active-trading/031914/understanding-chaikin-oscillator.asp
##or http://www.investopedia.com/articles/technical/03/021903.asp
##http://www.investopedia.com/ask/answers/071414/whats-difference-between-chaikin-money-flow-cmf-and-money-flow-index-mfi.asp
    library(ggplot2)
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
    data <- histdata[,c(7,1:5)]
    colnames(data) <- c("Date","Open","High","Low","Close","Volume")
    data$Date <- as.Date(data$Date)
    ##1. Money Flow Multiplier = [(Close  -  Low) - (High - Close)] /(High - Low) 
    data$MFM <- ((data$Close-data$Low)-(data$High-data$Close))/(data$High-data$Low)
    data <- na.exclude(data)
    ##2. Money Flow Volume = Money Flow Multiplier x Volume for the Period
    data$MFV <- data$MFM *data$Volume
    ##3. ADL = Previous ADL + Current Period's Money Flow Volume
    data$ADL <- NA
    data$ADL[1] <- data$MFV[1]
    i <- 2
    while(i <= nrow(data)){
        data$ADL[i] <- data$ADL[i-1] + data$MFV[i]
        i <- i+1
    }
    ##4. Chaikin Oscillator = (3-day EMA of ADL)  -  (10-day EMA of ADL)    
    data$EMA3 <- EMA(x = data$ADL,n=3)
    data$EMA10 <- EMA(x = data$ADL,n = 10)
    data$Chai <-data$EMA3-data$EMA10
    data$SMA10 <- SMA(x = data$Chai,10)
    data <- na.exclude(data)
    data <- data[data$Date >  Sys.Date() - months(months),]
    rownames(data) <- NULL
    ##Plot ADL EMA 3 and 10
    b <- ggplot(data = data,x = Date) + 
        geom_line(aes(x = Date,y = EMA3,color="EMA3"),lwd=0.5) +
        geom_line(aes(x = Date,y = EMA10,color = "EMA10"),lwd=0.5) +
        geom_ribbon(aes(x =Date, ymax=EMA10, ymin=EMA3),colour = NA, fill="maroon", alpha=.1)+
        scale_color_manual(values=c("EMA3" = "red","EMA10"="blue"))+
        theme(legend.title=element_blank(),
              plot.title = element_text(lineheight=1,face="bold",vjust = 0.25,hjust = 0.0),
              legend.justification=c(0,0), 
              legend.position=c(0,0),
              legend.background = element_rect(colour = 'lightgrey', fill = 'lightgrey'),
              plot.margin=unit(c(0,10,1,3),"mm"))+ 
        scale_x_date(expand=c(0,0)) + 
        ##scale_y_continuous(limits = c(0,max(data$EMA3,data$EMA10)*1.3),expand = c(1,1))+
        labs(title = paste(Ticker," Chaikan Oscillator",sep = ""),x = NULL,y = NULL)
        ##geom_text(data = subset(data[nrow(data),]),aes(x = Date,y = Close, label = Close),hjust=1, vjust=0,size=4,colour = "darkgreen")
    ##Plot Chaikan
    a <- ggplot(data = data) + 
        geom_line(aes(x = Date,y = Chai,color = "Chai Osc"),lwd=0.5) +
        geom_line(aes(x = Date,y = SMA10,color = "SMA10"),lwd=0.5) +
        geom_bar(aes(x=Date,y=Chai),color="blue",position = "identity",stat = "identity",alpha=I(.2),fill=ifelse(data$Chai > 0,"green","red"),linetype=0)+
        theme(legend.title=element_blank(),
              axis.text.x=element_blank(),
              legend.justification=c(0,1), 
              legend.position=c(0,1),
              axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank(),
              legend.background = element_rect(colour = 'lightgrey', fill = 'lightgrey'),
              plot.margin=unit(c(0,5,0,3),"mm"))+
        scale_x_date(expand=c(0,0)) +
        scale_color_manual(values=c("Chai Osc" = "darkred","SMA10" = "black"))+
        labs(title = NULL,x = NULL,y = NULL)
    
    png(filename = paste(Ticker," Chaikan for ",Sys.Date(),".png"),width = 400,height = 210,res = 1200,units = "mm")
    triplot(b,a,tradevolume(Ticker,months))
    dev.off()  
    triplot(b,a,tradevolume(Ticker,months))
    }

chaikanplot <- function(Ticker,months=6){
    library(ggplot2)
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
    data <- histdata[,c(7,1:5)]
    colnames(data) <- c("Date","Open","High","Low","Close","Volume")
    data$Date <- as.Date(data$Date)
    ##1. Money Flow Multiplier = [(Close  -  Low) - (High - Close)] /(High - Low) 
    data$MFM <- ((data$Close-data$Low)-(data$High-data$Close))/(data$High-data$Low)
    data <- na.exclude(data)
    ##2. Money Flow Volume = Money Flow Multiplier x Volume for the Period
    data$MFV <- data$MFM *data$Volume
    ##3. ADL = Previous ADL + Current Period's Money Flow Volume
    data$ADL <- NA
    data$ADL[1] <- data$MFV[1]
    i <- 2
    while(i <= nrow(data)){
        data$ADL[i] <- data$ADL[i-1] + data$MFV[i]
        i <- i+1
    }
    ##4. Chaikin Oscillator = (3-day EMA of ADL)  -  (10-day EMA of ADL)    
    data$EMA3 <- EMA(x = data$ADL,n=3)
    data$EMA10 <- EMA(x = data$ADL,n = 10)
    data$Chai <-data$EMA3-data$EMA10
    data$SMA10 <- SMA(x = data$Chai,10)
    data <- na.exclude(data)
    data <- data[data$Date >  Sys.Date() - months(months),]
    rownames(data) <- NULL
    ##geom_text(data = subset(data[nrow(data),]),aes(x = Date,y = Close, label = Close),hjust=1, vjust=0,size=4,colour = "darkgreen")
    ##Plot Chaikan
    a <- ggplot(data = data) + 
        geom_line(aes(x = Date,y = Chai,color = "Chai Osc"),lwd=0.5) +
        geom_line(aes(x = Date,y = SMA10,color = "SMA10"),lwd=0.5) +
        geom_bar(aes(x=Date,y=Chai),color="blue",position = "identity",stat = "identity",alpha=I(.2),fill=ifelse(data$Chai > 0,"green","red"),linetype=0)+
        theme(legend.title=element_blank(),
              axis.text.x=element_blank(),
              legend.justification=c(0,1), 
              legend.position=c(0,1),
              axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank(),
              legend.background = element_rect(colour = 'lightgrey', fill = 'lightgrey'),
              plot.margin=unit(c(0,5,0,3),"mm"))+
        scale_x_date(expand=c(0,0)) +
        scale_color_manual(values=c("Chai Osc" = "red","SMA10" = "black"))+
        labs(title = NULL,x = NULL,y = NULL)
    
a
}

##Money Flow Index 
##http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:money_flow_index_mfi
##http://www.investopedia.com/ask/answers/071414/whats-difference-between-chaikin-money-flow-cmf-and-money-flow-index-mfi.asp


##Aroon http://www.investopedia.com/articles/trading/06/aroon.asp
#http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:aroon
aroonplot <- function(Ticker,months=6){
#The Aroon indicator is used best by traders and investors interested in whether or not a trend is 
#still intact. It can help traders avoid inefficient use of capital by allowing them to seek other 
#opportunities during sideways markets and only hold positions during strong trends. However, it is important to watch carefully and analyze stocks using other studies in conjunction with Aroon to avoid the primary weakness in this system - sharp price movements.
#Read more: http://www.investopedia.com/articles/trading/06/aroon.asp#ixzz3aWvEUQvv 
  
    library(quantmod)
    library(ggplot2)
    library(dplyr)
    library(grid)
    library(TTR)
    library(scales)
    library(lubridate)
    x <- getSymbols.yahoo(paste(Ticker,".AX",sep = ""),env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
    start <- Sys.Date()- months(2*months)
    end <- Sys.Date()
    # the below is done redundantly for ease of maintenance later on
    #First, strip OHLC data (need to vectorize)
    date <- as.Date(rownames(x))
    open <- as.vector(Op(x))
    high <- as.vector(Hi(x))
    low <- as.vector(Lo(x))
    close <- as.vector(Cl(x))
    
    #Then build the data frame
    data <-data.frame('Date'=date,'Open'=open,'High'= high,'Low'=low,'Close'=close)
    aroontrend <- aroon(data[,c("High","Low")],n=20)
    data <- cbind(data,aroontrend)
    data <- na.exclude(data)
    data <- data[data$Date >  Sys.Date() - months(months),]
    
    a <- ggplot(data = data) + 
        geom_line(aes(x = Date,y = aroonUp,color = "AroonUp(Bull)"),lwd=0.5) +
        geom_line(aes(x = Date,y = aroonDn,color = "AroonDn(Bear)"),lwd=0.5) +
        geom_bar(aes(x=Date,y=oscillator,fill = "Oscillator"),position = "identity",stat = "identity",alpha=I(.2),linetype=0)+
        theme(legend.title=element_blank(),
              axis.text.x=element_blank(),
              legend.justification=c(0,1), 
              legend.position=c(0,1),
              axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank(),
              legend.background = element_rect(colour = 'lightgrey', fill = 'lightgrey'),
              plot.margin=unit(c(0,5,0,3),"mm"))+
        scale_x_date(expand=c(0,0)) +
        scale_color_manual(values=c("AroonUp(Bull)" = "Black","AroonDn(Bear)" = "Red"))+
        scale_fill_manual(values = c("Oscillator" = "blue"))+
        labs(title = NULL,x = NULL,y = NULL)
    
    a
}


##Disparity Index



##Bollinger Bands&reg 
#http://www.investopedia.com/articles/trading/05/022205.asp



##pure price momentum oscillator or pattern analysis.



##Accumulation Distribution Line
#http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:accumulation_distribution_line



#Stochastic Oscillator
#http://www.investopedia.com/terms/s/stochasticoscillator.asp
stochastic <- function(Ticker,months=6){
    ##%K = 100[(C - L14)/(H14 - L14)] 
    ##C = the most recent closing price 
    ##L14 = the low of the 14 previous trading sessions 
    ##H14 = the highest price traded during the same 14-day period.
    ##%D = 3-period moving average of %K 

   
}



##Fibonacci http://www.investopedia.com/articles/trading/06/fibonacci.asp

##Money Flow Index
##http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:money_flow_index_mfi
MFIplot <- function(Ticker,months=6){
    library(quantmod)
    library(ggplot2)
    library(dplyr)
    library(grid)
    library(TTR)
    library(scales)
    library(lubridate)
    x <- getSymbols.yahoo(paste(Ticker,".AX",sep = ""),env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
    start <- Sys.Date()- months(2*months)
    end <- Sys.Date()
    # the below is done redundantly for ease of maintenance later on
    #First, strip OHLC data (need to vectorize)
    date <- as.Date(rownames(x))
    open <- as.vector(Op(x))
    high <- as.vector(Hi(x))
    low <- as.vector(Lo(x))
    close <- as.vector(Cl(x))
    volume <- as.vector(Vo(x))
    #Then build the data frame
    data <-data.frame('Date'=date,'Open'=open,'High'= high,'Low'=low,'Close'=close,"Volume" = volume)
    #* 1. Typical Price = (High + Low + Close)/3        
    #* 2. Raw Money Flow = Typical Price x Volume
    #* 3. Money Flow Ratio = (14-period Positive Money Flow)/(14-period Negative Money Flow)
    #* 4. Money Flow Index = 100 - 100/(1 + Money Flow Ratio)  
    mfi <- MFI(HLC = data[,c("High","Low","Close")],data[,"Volume"],n=14)
    data <- cbind(data,mfi)    
    data <- na.exclude(data)
    data <- data[data$Date >  Sys.Date() - months(months),]
    
    a <- ggplot(data = data) + 
        geom_line(aes(x = Date,y = mfi,color = "MFI"),lwd=0.6) +
        geom_hline(aes(yintercept = 80,color = "Overbought(Sell)"),lwd=0.75)+
        geom_hline(aes(yintercept = 20,color = "Oversold(Buy)"),lwd=0.75)+
        theme(legend.title=element_blank(),
              axis.text.x=element_blank(),
              legend.justification=c(0,1), 
              legend.position=c(0,1),
              axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank(),
              legend.background = element_rect(colour = 'lightgrey', fill = 'lightgrey'),
              plot.margin=unit(c(0,5,0,3),"mm"))+
        scale_x_date(expand=c(0,0)) +
        scale_color_manual(values=c("MFI" = "blue","Overbought(Sell)" = "red","Oversold(Buy)"="darkgreen"))+
        labs(title = NULL,x = NULL,y = NULL)
    a
    
    
}

##http://en.wikipedia.org/wiki/Beta_(finance)

#Create Candlestick Chart
candlestick <- function(Ticker, months = 6){
    library(quantmod)
    library(ggplot2)
    library(dplyr)
    library(grid)
    library(TTR)
    library(scales)
    library(lubridate)
        x <- getSymbols.yahoo(paste(Ticker,".AX",sep = ""),env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
        start <- Sys.Date()- months(months)
        end <- Sys.Date()
        # the below is done redundantly for ease of maintenance later on
        #First, strip OHLC data (need to vectorize)
        date <- as.Date(rownames(x))
        open <- as.vector(Op(x))
        high <- as.vector(Hi(x))
        low <- as.vector(Lo(x))
        close <- as.vector(Cl(x))
        
        #Then build the data frame
        xSubset <-data.frame('date'=date,'open'=open,'high'= high,'low'=low,'close'=close)
        
        #We want to construct our candlesticks  
        xSubset$candleLower <- pmin(xSubset$open, xSubset$close)
        xSubset$candleMiddle <- NA
        xSubset$candleUpper <- pmax(xSubset$open, xSubset$close)
        xSubset$fill <- ''
        xSubset$fill[xSubset$open < xSubset$close] = 'white'
        xSubset$fill[xSubset$fill ==''] = 'red'
        
        #Add Moving Averages
        xSubset$ma200 <- SMA(xSubset$close, 200)
        xSubset$ma50 <- SMA(xSubset$close, 50)
        
        #Trim Data
        xSubset <-subset(xSubset, xSubset$date > start & xSubset$date < end)
        
        #Graphing Step
        g <- ggplot(xSubset, aes(x=date, lower=candleLower, middle=candleMiddle, upper=candleUpper, ymin=low, ymax=high))+ 
             geom_boxplot(stat='identity', aes(group=date, fill=fill),alpha = 0.5)+
             scale_fill_manual(values = c("red", "green"))+
             #geom_line(aes(x=date, y=ma50))+ 
             #geom_line(aes(x=date, y=ma200))+
             theme(legend.title=element_blank(),
                  plot.title = element_text(lineheight= 1 ,face="bold",vjust = 0.25,hjust = 0.0),
                  legend.justification=c(0,0), 
                  legend.position="none",
                  legend.background = element_rect(colour = 'lightgrey', fill = 'lightgrey'),
                  plot.margin=unit(c(0,10,1,3),"mm"))+ 
             scale_x_date(expand=c(0,0)) + 
             scale_y_continuous(labels = dollar_format(largest_with_cents = 5),
                           limits = c(min(xSubset$candleLower*0.8),max(xSubset$candleUpper)*1.2),expand = c(0,0))+
             labs(title = paste(Ticker," Share Price",sep = ""),x = NULL,y = NULL)
        g 
    
}


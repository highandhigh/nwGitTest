
sectorpull <- function(sector){
      ##Gather company names, ticker, and market cap for a metalk
      library(plyr)
      data <- read.csv(file = "http://www.asx.com.au/asx/research/ASXListedCompanies.csv",header = TRUE, skip = 2)
      colnames(data) <- c("Company","Ticker","Industry")
      tickers <- data[grepl(pattern = sector,x = data$Industry,ignore.case = TRUE),2]
      tickers <- as.character(tickers)
      stats <- ldply(tickers,getKeyStats_xpath)
      companies <- stats[,c(59,1:9,25,27)]
      
      #while loops to remove B and M and re-insert dollar figures      
      i=1
      while(i <= nrow(companies)){
            if(grepl(pattern = "B",x = companies[i,2])){
                  companies[i,2] <- as.numeric(gsub(pattern = "B",x = companies[i,2] ,replacement = "",ignore.case = TRUE))*1000000
                  i=i+1
                  next
            }
            if(grepl(pattern = "M",x = companies[i,2])){
                  companies[i,2] <- as.numeric(gsub(pattern = "M",x = companies[i,2] ,replacement = "",ignore.case = TRUE))*1000
                  i=i+1
                  next
            }
            if(grepl(pattern = "k",x = companies[i,2])){
                  companies[i,2] <- as.numeric(gsub(pattern = "K",x = companies[i,2] ,replacement = "",ignore.case = TRUE))
                  i=i+1
                  next
            }
            i=i+1
            next
      }
      i=1
      while(i <= nrow(companies)){
            if(grepl(pattern = "B",x = companies[i,3])){
                  companies[i,3] <- as.numeric(gsub(pattern = "B",x = companies[i,3] ,replacement = "",ignore.case = TRUE))*1000000
                  i=i+1
                  next
            }
            if(grepl(pattern = "M",x = companies[i,3])){
                  companies[i,3] <- as.numeric(gsub(pattern = "M",x = companies[i,3] ,replacement = "",ignore.case = TRUE))*1000
                  i=i+1
                  next
            }
            if(grepl(pattern = "K",x = companies[i,3])){
                  companies[i,3] <- as.numeric(gsub(pattern = "k",x = companies[i,3] ,replacement = "",ignore.case = TRUE))
                  i=i+1
                  next
            }
            i=i+1
            next   
            }  
      
      ##Remove thousand seperators
      companies[,-(1)] <- lapply(companies[,-(1)], function(x) as.numeric(gsub(pattern = ",",replacement = "", as.character(x))))
      companies
}


getKeyStats_xpath <- function(symbol) {
      ##To call use:
      ##tickers <- c("ZIP.AX","KAR.AX")
      ##stats <- ldply(tickers,getKeyStats_xpath)      
      require(xml2)
      require(plyr)
      yahoo.URL <- "http://finance.yahoo.com/q/ks?s="
      html_text <- read_html(paste(yahoo.URL, symbol,".AX", sep = ""), encoding="UTF-8")
      
      #search for <td> nodes anywhere that have class 'yfnc_tablehead1'
      nodes <- xml_find_all(html_text, "/*//td[@class='yfnc_tablehead1']")
      
      if(length(nodes) > 0 ) {
            measures <- sapply(nodes, xml2::xml_text)
            
            #Clean up the column name
            measures <- gsub(" *[0-9]*:", "", gsub(" \\(.*?\\)[0-9]*:","", measures))   
            
            #Remove dups
            dups <- which(duplicated(measures))
            #print(dups) 
            for(i in 1:length(dups)) 
                  measures[dups[i]] = paste(measures[dups[i]], i, sep=" ")
            
            #use siblings function to get value
            values <- sapply(nodes, function(x)  xml2::xml_text(xml_siblings(x)))
            
            df <- data.frame(t(values),stringsAsFactors = FALSE)
            colnames(df) <- measures
            df$Ticker <- symbol
            return(df)
      } else {
            return()
      }
}

analyzesector <- function(sector){
      library(ggplot2)
      library(reshape2)
      library(dplyr)
      library(ggthemes)
      library(Hmisc)
      data <- sectorpull(sector)
      data <- arrange(data,desc(data[,6]))
      #data[,2] <- gsub(x = data[,2],pattern = ".AX",replacement = "",ignore.case = TRUE)
      data <- data[1:15,c(-3)]
      data <- melt(data,id.vars = "Ticker")
      data <- data[data$value <75,]
      data <- data[data$value >(-20),]
      rownames(data) <- NULL
      data <- na.exclude(data)
      
      c <-  ggplot(data = data,aes(x = Ticker,y = value)) +
            geom_bar(stat="identity")+
            facet_wrap(facets = ~variable,ncol = 3,scales = "free")+
            theme_economist()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            ggtitle(paste(capitalize(sector)," Market Analysis for ",Sys.Date()))
      
      c
      
}


theme_gray <- function (base_size = 12, base_family = "") 
{
      theme(
            line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"), 
            rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1), 
            text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9), 
            
            axis.text = element_text(size = rel(0.8), colour = "grey50"), 
            strip.text = element_text(size = rel(0.8)), 
            axis.line = element_blank(), 
            axis.text.x = element_text(vjust = 1), 
            axis.text.y = element_text(hjust = 1), 
            axis.ticks = element_line(colour = "grey50"), 
            axis.title.x = element_text(), 
            axis.title.y = element_text(angle = 90), 
            axis.ticks.length = unit(0.15, "cm"), 
            axis.ticks.margin = unit(0.1, "cm"), 
            
            legend.background = element_rect(colour = NA), 
            legend.margin = unit(0.2, "cm"), 
            legend.key = element_rect(fill = "grey95", colour = "white"), 
            legend.key.size = unit(1.2, "lines"), 
            legend.key.height = NULL, 
            legend.key.width = NULL, 
            legend.text = element_text(size = rel(0.8)), 
            legend.text.align = NULL, 
            legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0), 
            legend.title.align = NULL, 
            legend.position = "right", 
            legend.direction = NULL, 
            legend.justification = "center", 
            legend.box = NULL, 
            
            panel.background = element_rect(fill = "grey90", colour = NA), 
            panel.border = element_blank(), 
            panel.grid.major = element_line(colour = "white"), 
            panel.grid.minor = element_line(colour = "grey95", size = 0.25), 
            panel.margin = unit(0.25, "lines"), 
            panel.margin.x = NULL, 
            panel.margin.y = NULL, 
            
            strip.background = element_rect(fill = "grey80", colour = NA), 
            strip.text.x = element_text(), 
            strip.text.y = element_text(angle = -90), 
            
            plot.background = element_rect(colour = "white"), 
            plot.title = element_text(size = rel(1.2)), 
            plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"), complete = TRUE)
}



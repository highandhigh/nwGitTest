
metalsector <- function(metal){
      ##Gather company names, ticker, and market cap for a metal
      library(rvest)
      library(plyr)
      url <- paste(sep = "","http://www.miningfeeds.com/",metal,"-mining-report-australia")
      html <- rvest::html(url)
      htmlnodes <- html_nodes(x = html, css = "td")
      data <- html_text(htmlnodes, trim = TRUE)
      data1 <- as.data.frame(matrix(data = data,ncol = 12,byrow = TRUE),stringsAsFactors = FALSE)
      columns <- htmlnodes <- html_nodes(x = html, css = "th")
      columns <- html_text(columns, trim = TRUE)
      columns <- as.data.frame(matrix(data = columns,ncol = 12,byrow = TRUE))
      colnames(data1) <- unlist(columns[1,])
      data1 <- data1[,-1]
      #data1[,11] <- gsub(pattern = ",",replacement = "",x = data1[,11])
      #data1[,11] <- as.numeric(data1[,11])
      colnames(data1)[11] <- "Market Cap"
      companies <- data1[,c(1:2)]
      tickers <- companies[,2]
      stats <- ldply(tickers,getKeyStats_xpath)
      companies <- cbind(companies,stats)
      companies <- companies[,c(1:11,27,28,29)]
      
      #while loops to remove B and M and re-insert dollar figures      
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
            if(grepl(pattern = "k",x = companies[i,3])){
                  companies[i,3] <- as.numeric(gsub(pattern = "K",x = companies[i,3] ,replacement = "",ignore.case = TRUE))
                  i=i+1
                  next
            }
            i=i+1
            next
      }
      i=1
      while(i <= nrow(companies)){
            if(grepl(pattern = "B",x = companies[i,4])){
                  companies[i,4] <- as.numeric(gsub(pattern = "B",x = companies[i,4] ,replacement = "",ignore.case = TRUE))*1000000
                  i=i+1
                  next
            }
            if(grepl(pattern = "M",x = companies[i,4])){
                  companies[i,4] <- as.numeric(gsub(pattern = "M",x = companies[i,4] ,replacement = "",ignore.case = TRUE))*1000
                  i=i+1
                  next
            }
            if(grepl(pattern = "K",x = companies[i,4])){
                  companies[i,4] <- as.numeric(gsub(pattern = "k",x = companies[i,4] ,replacement = "",ignore.case = TRUE))
                  i=i+1
                  next
            }
            i=i+1
            next   
      }
      i=1
      while(i <= nrow(companies)){
            if(grepl(pattern = "B",x = companies[i,13])){
                  companies[i,13] <- as.numeric(gsub(pattern = "B",x = companies[i,13] ,replacement = "",ignore.case = TRUE))*1000000
                  i=i+1
                  next
            }
            if(grepl(pattern = "M",x = companies[i,13])){
                  companies[i,13] <- as.numeric(gsub(pattern = "M",x = companies[i,13] ,replacement = "",ignore.case = TRUE))*1000
                  i=i+1
                  next
            }
            if(grepl(pattern = "K",x = companies[i,13])){
                  companies[i,13] <- as.numeric(gsub(pattern = "k",x = companies[i,13] ,replacement = "",ignore.case = TRUE))
                  i=i+1
                  next
            }
            i=i+1
            next   
      }
      ##Remove thousand seperators
      companies[,-(1:2)] <- lapply(companies[,-(1:2)], function(x) as.numeric(gsub(pattern = ",",replacement = "", as.character(x))))
      companies
      
}

getKeyStats_xpath <- function(symbol) {
##To call use:
##tickers <- c("ZIP.AX","KAR.AX")
##stats <- ldply(tickers,getKeyStats_xpath)      
      
      
      require(xml2)
      require(plyr)
            yahoo.URL <- "http://finance.yahoo.com/q/ks?s="
            html_text <- read_html(paste(yahoo.URL, symbol, sep = ""), encoding="UTF-8")
            
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
                  return(df)
            } else {
                  break
      }
}
      
analyzeindustry <- function(metal){
      library(ggplot2)
      library(reshape2)
      library(dplyr)
      library(ggthemes)
      library(Hmisc)
      data <- metalsector(metal)
      data <- arrange(data,desc(data[,6]))
      data[,2] <- gsub(x = data[,2],pattern = ".AX",replacement = "",ignore.case = TRUE)
      data <- data[1:15,c(-1,-3,-4,-13)]
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
            ggtitle(paste(capitalize(metal)," Market Analysis for ",Sys.Date()))
            
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



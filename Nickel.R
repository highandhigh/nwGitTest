


##Gather company names, ticker, and market cap for a metal
metalsector <- function(metal){
      library(rvest)
      url <- paste(sep = "","http://www.miningfeeds.com/",metal,"-mining-report-australia")
      html <- html(url)
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
      companies <- companies[,c(1:11,14:36,38,40:48)]
      
      #Convert Numbers to Numerics
      x=1
      converts <- c()
      while
      
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
                  companies[i,3] <- as.numeric(gsub(pattern = "K",x = companies[i,3] ,replacement = "",ignore.case = TRUE))
                  i=i+1
                  next
            }
            
      }
      companies
}


getKeyStats_xpath <- function(symbol) {
##To call use:
##tickers <- c("ZIP.AX","KAR.AX")
##stats <- ldply(tickers,getKeyStats_xpath)      
      
      
      require(XML)
      require(plyr)
            yahoo.URL <- "http://finance.yahoo.com/q/ks?s="
            html_text <- htmlParse(paste(yahoo.URL, symbol, sep = ""), encoding="UTF-8")
            
            #search for <td> nodes anywhere that have class 'yfnc_tablehead1'
            nodes <- getNodeSet(html_text, "/*//td[@class='yfnc_tablehead1']")
            
            if(length(nodes) > 0 ) {
                  measures <- sapply(nodes, xmlValue)
                  
                  #Clean up the column name
                  measures <- gsub(" *[0-9]*:", "", gsub(" \\(.*?\\)[0-9]*:","", measures))   
                  
                  #Remove dups
                  dups <- which(duplicated(measures))
                  #print(dups) 
                  for(i in 1:length(dups)) 
                        measures[dups[i]] = paste(measures[dups[i]], i, sep=" ")
                  
                  #use siblings function to get value
                  values <- sapply(nodes, function(x)  xmlValue(getSibling(x)))
                  
                  df <- data.frame(t(values),stringsAsFactors = FALSE)
                  colnames(df) <- measures
                  return(df)
            } else {
                  break
      }
}
      
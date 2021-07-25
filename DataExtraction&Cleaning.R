#Introduction to Web Mining For Social Scientists
#Noah Julian Angara
#June 2021

# load all required packages
library(httr)       
library(rvest)      
library(dplyr)      
library(stringr)   
library(tidytext) 
library(vader)      
library(RSelenium)
library(readr)
library(fBasics)
library(ggthemes)
library(ggplot2)
library(data.table)
library(stargazer)
library(RCurl)
library(RJSONIO)

#set working directory
setwd("/Users/noahangara/Documents/Master's/8th Semester/Web Mining/Assignment")

# Download Approval Rating Time Series ------------------------------------
approval_rating <- read_csv("https://projects.fivethirtyeight.com/biden-approval-data/approval_topline.csv")
write_csv(approval_rating, file = "approval_rating.csv")

#plot approval rating of Joe Biden
approval_rating <- approval_rating[approval_rating$subgroup == "All polls", 
                                        c("modeldate", "approve_estimate", "disapprove_estimate")]
approval_plot = ggplot(data = melt(approval_rating, "modeldate")) + 
  geom_line(aes(y = value, x = as.Date(modeldate, format = "%m/%d/%Y"), color = variable), size = 1.5) +
  labs(x = "", y = "%", color = "") + 
  theme_fivethirtyeight() + theme(legend.position = "top") + 
  scale_color_manual(values = c("#009f29", "#ff7400"), labels =  c("Approval", "Disapproval"))
ggsave(approval_plot, filename = "approval.png", width = 14, height = 10, units = "cm")

#summary statistics of approval rating
descriptive_stats <- cbind(basicStats(approval_rating$approve_estimate), basicStats(approval_rating$disapprove_estimate))
stargazer(t(descriptive_stats))

# Extracting Headlines from NYT Page -----------------------------------------

#urls to navigate to (NYTimes page already sorted and filtered)
urlmarch22 <- "https://www.nytimes.com/search?dropmab=false&endDate=20210602&query=joe%20biden&sort=newest&startDate=20210123"
urljan23 <- "https://www.nytimes.com/search?dropmab=false&endDate=20210322&query=joe%20biden&sort=newest&startDate=20210123"

#Function to Extract Headlines and Dates
extract_headlines <- function(url, name){
  #setup Selenium server
  rD <- rsDriver(browser = "firefox", port = 6768L, 
                 check = F, #note that first time running RSelenium need to have check = T
                 verbose = F)
  
  #assign server and client to remDr
  remDr <- rD[["client"]]
  remDr$navigate(url) #navigate to page
  webElem <- remDr$findElement(using = "css selector", "body")
  for(i in 1:150){#There are 1'921 results and 10 articles per scroll, since code is executed in 2 parts 150 is conservative estimate
    if (i == 1){#on first iteration need to click accept to make popup go away
    message(paste("Iteration",i))
    popup <- remDr$findElement(using = "css selector", 
                    value = ".css-aovwtd")
    remDr$mouseMoveToLocation(webElement = popup)
    popup$click() #click away cookie/privacy popup
    Sys.sleep(2)
    webElem$sendKeysToElement(list(key = "end"))#scroll to end of page
    Sys.sleep(1)
    elem_click <- remDr$findElement(using = "css selector", #find show more button
                                  value = ".css-vsuiox > button:nth-child(1)")
    Sys.sleep(2) #need to let things load otherwise we get an error
    webElem$sendKeysToElement(list(key = "end")) 
    Sys.sleep(2)
    remDr$mouseMoveToLocation(webElement = elem_click)#click show more button
    elem_click$click()
    }
  else {
    #After the first iteration of the loop we only need to click the show more button
    message(paste("Iteration",i))
    webElem$sendKeysToElement(list(key = "end")) #scroll to end
    Sys.sleep(2) #let things load
    success <- tryCatch({
      #need to error handle the case where the show more button doesn't exist i.e. once at the end of the page or doesn't work
      remDr$findElement("css selector", ".css-vsuiox > button:nth-child(1)")
      TRUE
    }, 
    warning = function(w) { FALSE },
    error = function(e) { FALSE },
    finally = { })
    if (!success) next
    elem_click <- remDr$findElement(using = "css selector", #find show more button
                                    value = ".css-vsuiox > button:nth-child(1)")
    webElem$sendKeysToElement(list(key = "end"))
    remDr$mouseMoveToLocation(webElement = elem_click) #move mouse to show more button
    elem_click$click() #click show more button
    }
  #Sleep to Let Things Load
  Sys.sleep(4)}
  pagesource <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html() #get HTML source
  assign(paste0("html_obj_", name), pagesource, envir = .GlobalEnv) #assign HTML source to global environment
  # Shut Down Client and Server
  remDr$close()
  rD$server$stop()
  pingr::ping_port("localhost", "6768") #doublecheck that port has been closed (else need to sudo lsof -i:6768 and sudo kill -9 PID)
  }

# Apply Extract Headline Function to Get HTML from webpage in two parts
# RUNTIME: ~27 MINUTES
start.time <- Sys.time() #measure how long it takes to run (with end.time at bottom)
extract_headlines(urlmarch22, "march22")
extract_headlines(urljan23, "jan23")
end.time  <- Sys.time()

# Extract News Headlines and Date from HTML For Both Samples
headline_march22 <- html_obj_march22 %>% html_elements(".css-2fgx4k") %>% html_text()
date_march22 <- html_obj_march22 %>% html_elements(".css-17ubb9w") %>% html_text()

headline_jan23 <- html_obj_jan23 %>% html_elements(".css-2fgx4k") %>% html_text()
date_jan23 <- html_obj_jan23 %>% html_elements(".css-17ubb9w") %>% html_text()

#Combine both data frames into one
df_nyt_march22 <- data.frame("Date" = date_march22, "Headline" = headline_march22)
df_nyt_jan22 <- data.frame("Date" = date_jan23, "Headline" = headline_jan23)
nyt_df_clean <- rbind(df_nyt_march22, df_nyt_jan22)


# Extracting Headlines from The Guardian Page --------------------------------------
guardian_url <- "https://content.guardianapis.com/search?from-date=2021-01-23&to-date=2021-06-02&show-fields=headline&"
guardian_url2 <- "&page-size=200&q=Joe%20Biden&api-key=541393a9-a58f-461b-92d3-8359ba3ad1cd"
pages <- seq(1,12,1)

#As the Guardian Limits Articles Per Page to 200 We Need to Loop Through Individual Pages
news_data <- list()
for (i in pages) {
  scrape_url <- paste0(guardian_url, paste0("page=",i), guardian_url2)
  raw <- getURL(scrape_url)
  df <- fromJSON(raw, nullValue = NA)#convert JSON format
  news_data[[i]] <- df
}


# Data Cleaning ------------------------------------------------------

# NYT articles
# Format Date
nyt_df_clean <- nyt_df_clean %>%
  mutate(Month = word(nyt_df_clean$Date, 1)) %>%
  mutate(Day = word(nyt_df_clean$Date, 2)) %>%
  mutate(Year = 2021)

#remove "." after Feb and Jan
nyt_df_clean[which(nyt_df_clean$Month == "Feb."), "Month"] = "February"
nyt_df_clean[which(nyt_df_clean$Month == "Jan."), "Month"] = "January"
  
#Replace date column with now properly formatted date
nyt_df_clean$Month <- match(nyt_df_clean$Month, month.name)
nyt_df_clean$Date <- as.Date(paste0(nyt_df_clean$Year,"-",nyt_df_clean$Month,"-", nyt_df_clean$Day))
nyt_df_clean <- nyt_df_clean %>%
  select(c(Date, Headline))

# Guardian Articles
# Convert articles from list format to data frame, pulling date and headline only
news_data_clean <- data.frame()
for(i in 1:(length(pages)-1)){
  for(j in 1:news_data[[1]][["response"]][["pageSize"]])
  {
    if (length(news_data[[i]][["response"]][["results"]][[j]])<12){
      
    }
    else {
      new_row <- as.data.frame(news_data[[i]][["response"]][["results"]][[j]])
      news_data_clean <- rbind(news_data_clean, new_row)
    }
  }
}

#clean up df with dplyr
news_data_clean$webPublicationDate <- as.Date(substr(news_data_clean$webPublicationDate, 1, 10), format = "%Y-%m-%d")

#select columns of interest and sort by date
news_data_clean <- news_data_clean %>%
  select(c("webPublicationDate", "webTitle")) %>%
  arrange(webPublicationDate) 

#format column names to match NYT data frame
colnames(news_data_clean) <- c("Date", "Headline")

#reset row index
row.names(news_data_clean) <- NULL

#save both files as .csv for analysis
write_csv(news_data_clean, file = "guardian_articles.csv")
write_csv(nyt_df_clean, file = "nyt_articles.csv")





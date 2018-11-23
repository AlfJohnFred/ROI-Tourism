library(textcat)
library(tidyverse)
library(reshape)
library(ggplot2)
library(RMySQL)
library("cld2")
library("cld3")
library("tidytext")
con <- dbConnect(MySQL(),user="twitter",password="password",dbname="twitter",host="52.209.227.60")
on.exit(dbDisconnect(con))
rs <- dbSendQuery(con, "select tweet_text, created_at from tweets LIMIT 100000 OFFSET 500000;")
data <- fetch(rs, n=Inf) 
summary(data)
data <- data %>% mutate(textcat = textcat(x = tweet_text),
                        cld2 = cld2::detect_language(text = tweet_text, plain_text = FALSE),
                        cld3 = cld3::detect_language(text = tweet_text)) %>%select(tweet_text, textcat, cld2, cld3, created_at) %>%filter(cld2 == "en" & cld3 == "en")
data$RT <- startsWith(data$tweet_text, "RT") 
data <- data[!data$RT, ]
summary(data)


df <- data.frame(read.csv("data0.csv"))
df1 <- data.frame(read.csv("data1.csv"))
df2 <- data.frame(read.csv("data2.csv"))
df3 <- data.frame(read.csv("data3.csv"))
df4 <- data.frame(read.csv("data4.csv"))
df5 <- data.frame(read.csv("data5.csv"))

tweets <- do.call("rbind",list(df,df1,df2,df3,df4,df5))
View(head(tweets))
countries <- c("GBR","FRA","DEU","ITA","ESP","CZE","TUR","AUT","NLD","SWE","PRT")
uk <- filter(tweets,grepl("England|London|UK|uk|london|england|united", tweet_text))
france <- filter(tweets,grepl("france|french|paris|lyon|nice|bordeaux|Marseille|Dijon|amiens", tweet_text))
germany <- filter(tweets,grepl("german|germany|munich|berlin|hamburg|frankfurt|cologne|stuttgart|nuremberg", tweet_text))
italy <- filter(tweets,grepl("italy|italian|rome|venice|milan|amalfi|turin|vatican|sienna|florence|naples", tweet_text))
spain <- filter(tweets,grepl("spain|spanish|espanol|viva|barcelona|madrid|seville|toledo|granada|santiago|cadiz|salamanca|cordoba|canary|valencia", tweet_text))
czech  <- filter(tweets,grepl("prague|brno|switzerland|ostrava|liberec|valtice", tweet_text))
turkey <- filter(tweets,grepl("turkey|turkish|istanbul|antalya|ankara|izmir|bodrum|trabzon|bursa|mardin|edirne|konya", tweet_text))
austria <- filter(tweets,grepl("austria|vienna|salzburg|innsbruck|graz|hallstatt|klagenfurt|linz|tyril|bad gastein|worthersee|arlberg|wachau", tweet_text))
netherland <- filter(tweets,grepl("netherland|amsterdam|hague|rotterdam|utrecht|maastricht|tilburg|holland", tweet_text))
sweden <- filter(tweets,grepl("sweden|swede|stockholm|gothenburg|copenhagen|greater copenhagen", tweet_text))
portugal <- filter(tweets,grepl("portugal|portugese|lisbon|porto|sintra|lagos|coimbra|braga", tweet_text))
countriesFull <- c("UK","France","Germany","Italy","Spain","Czech","Turkey","Austria","Netherland","Sweden","Portugal")

sentimentTweets <- data.frame()
interim <- data_frame(text = portugal$tweet_text)
interim$text <- as.character(interim$text)
interim <- interim %>% unnest_tokens(word,text)
nrc <- get_sentiments("nrc") 
nrcSent <- interim %>% inner_join(nrc) %>% count( sentiment) %>% spread(sentiment, n, fill = 0) %>% select(positive,negative,joy,anticipation,trust)
str(nrcSent)

sentimentTweets <- rbind(sentimentTweets,nrcSent)

View(sentimentTweets)

sentimentTweets <- cbind(sentimentTweets,countries)
sentimentTweets <- cbind(sentimentTweets,countriesFull)
sentimentTweets <- mutate(sentimentTweets,sentiment = positive - negative)
sentimentTweets <- sentimentTweets[c(6,7,8,1,2,3,4,5)]
names(sentimentTweets) <- c("CountryThree","Country","Sentiment","positive","negative","joy","anticipation","trust") 


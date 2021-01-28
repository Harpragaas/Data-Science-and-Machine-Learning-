library("rvest")
library("dplyr")


movies_url <- read_html("https://www.imdb.com/list/ls004221468/")

#1. Movies Rank 
rank_html <- html_nodes(movies_url,'.text-primary')
rank_text <- html_text(rank_html)
rank_text<- as.numeric(rank_text)

#2. Movie Title
title_html <- html_nodes(movies_url,'.lister-item-header a')
title_text <- html_text(title_html)

#3. Movie genre
genre_html <- html_nodes(movies_url,'.genre')
genre_text <- html_text(genre_html)
genre_text <- gsub("\n","",genre_text)
genre_text <- gsub(" ","",genre_text)
genre_text <- gsub(",.*","",genre_text)
genre_text <- as.factor(genre_text)

#4. IMDB Rating 
rating_html <- html_nodes(movies_url,'.ipl-rating-star.small .ipl-rating-star__rating')
rating_text <- html_text(rating_html)
rating_text <- as.numeric(rating_text)

#5. Votes
votes_html <- html_nodes(movies_url,'.text-muted+ span:nth-child(2)')
votes_text <- html_text(votes_html)
votes_text<-gsub(",","",votes_text)
votes_text<-as.numeric(votes_text)
votes_text<-na.omit(votes_text)

#6.OTT
ott_html <- html_nodes(movies_url,'.wtw-option-standalone')
ott_text <- html_text(ott_html)
length(ott_text)
ott_text

for (i in c(2,6,12,14,20,22,25,28,29,30,33,40,42,44,47,49,50,57,62,63,65,66,69,71,73,81,85,87,89,91,93)){
  
  a<-ott_text[1:(i-1)]
  
  
  b<-ott_text[i:length(gross_text)]
  
  ott_text<-append(a,list("Prime Video"))
  
  ott_text<-append(ott_text,b)}

ott_text<-as.character(ott_text)



#Making a dataframe :=

movies_dataframe1 <-data.frame(Rank = rank_text,Name = title_text, 
                               Genre = genre_text, Rating = rating_text, Votes = votes_text
                               ,OTT = ott_text)
movies_dataframe1

# Dataframe 2

movies_url2 <- read_html("https://www.imdb.com/list/ls004221468/?sort=list_order,asc&st_dt=&mode=detail&page=2")

#1. Movies Rank 
rank_html2 <- html_nodes(movies_url2,'.text-primary')
rank_text2 <- html_text(rank_html2)
rank_text2<- as.numeric(rank_text2)

#2. Movie Title
title_html2 <- html_nodes(movies_url2,'.lister-item-header a')
title_text2 <- html_text(title_html2)

#3. Movie genre
genre_html2 <- html_nodes(movies_url2,'.genre')
genre_text2 <- html_text(genre_html2)
genre_text2 <- gsub("\n","",genre_text2)
genre_text2 <- gsub(" ","",genre_text2)
genre_text2 <- gsub(",.*","",genre_text2)
genre_text2 <- as.factor(genre_text2)

#4. IMDB Rating 
rating_html2 <- html_nodes(movies_url2,'.ipl-rating-star.small .ipl-rating-star__rating')
rating_text2 <- html_text(rating_html2)
rating_text2 <- as.numeric(rating_text2)

#5. Votes
votes_html2 <- html_nodes(movies_url2,'.text-muted+ span:nth-child(2)')
votes_text2 <- html_text(votes_html2)
votes_text2<-gsub(",","",votes_text2)
votes_text2<-as.numeric(votes_text2)
votes_text2<-na.omit(votes_text2)

# Making a dataframe 
movies_dataframe2 <-data.frame(Rank = rank_text2,Name = title_text2,#Runtime = runtime_text2, 
                               Genre = genre_text2, Rating = rating_text2, Votes = votes_text2
)



# Dataframe 3

movies_url3 <- read_html("https://www.imdb.com/list/ls004221468/?st_dt=&mode=detail&page=3&sort=list_order,asc")

#1. Movies Rank 
rank_html3 <- html_nodes(movies_url3,'.text-primary')
rank_text3 <- html_text(rank_html3)
rank_text3<- as.numeric(rank_text3)

#2. Movie Title
title_html3 <- html_nodes(movies_url3,'.lister-item-header a')
title_text3 <- html_text(title_html3)

#3. Movie genre
genre_html3 <- html_nodes(movies_url3,'.genre')
genre_text3 <- html_text(genre_html3)
genre_text3 <- gsub("\n","",genre_text3)
genre_text3 <- gsub(" ","",genre_text3)
genre_text3 <- gsub(",.*","",genre_text3)
genre_text3 <- as.factor(genre_text3)

#4. IMDB Rating 
rating_html3 <- html_nodes(movies_url3,'.ipl-rating-star.small .ipl-rating-star__rating')
rating_text3 <- html_text(rating_html3)
rating_text3 <- as.numeric(rating_text3)

#5. Votes
votes_html3 <- html_nodes(movies_url3,'.text-muted+ span:nth-child(2)')
votes_text3 <- html_text(votes_html3)
votes_text3<-gsub(",","",votes_text3)
votes_text3<-as.numeric(votes_text3)
votes_text3<-na.omit(votes_text3)

# Making a dataframe
movies_dataframe3 <-data.frame(Rank = rank_text3,Name = title_text3,#Runtime = runtime_text2, 
                               Genre = genre_text3, Rating = rating_text3, Votes = votes_text3
)

str(movies_dataframe3)

#Combine all dataframes 
movies_dataframe3
movies_dataframe <- do.call("rbind", list(movies_dataframe1, movies_dataframe2, movies_dataframe3))
str(movies_dataframe)

movies_dataframe


#Save as csv
write.csv(movies_dataframe,"C:\\Users\\Harpragaas Singh\\Desktop\\Kings Digital\\Web Scraping\\movies_dataframe.csv", row.names = FALSE)

























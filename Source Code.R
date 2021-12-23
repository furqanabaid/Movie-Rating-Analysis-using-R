library(md.table)
library(plotly)
library(ggplot2)
library(ggthemes)
library(lambda.r)
library(lubridate)
library(stringr)
library(reticulate)
library(dplyr)
library(Hmisc)
library(scales)
library(lessR)
#Reading file - movies.dat
movies = do.call(rbind,strsplit(readLines('movies.dat'),'::'))
movies <- as.md.frame(movies)
View(head(movies,10)) 
#changing column names
colnames(movies) <- c("ID","Title","Genre")
View(head(movies,10)) 
#converting id of each row to numeric
movies$ID <- as.numeric(movies$ID)
View(head(movies,10)) 
#Reading file - ratings.dat
ratings = read.delim("ratings.dat", header= FALSE ,sep = ':', colClasses = c(NA, "NULL"))
head(ratings)
colnames(ratings) <- c("User","ID","Ratings","Timestamp")
View(head(ratings,10))
#merging both movies and ratings to one table
md = merge(movies,ratings,by = "ID")
md = md[with(md, order(ID)),]
View(head(md,10))
ratings<- dplyr::count(md, Ratings, sort = TRUE) %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))
ggplot(ratings, aes(x = "", y = perc, fill = factor(Ratings)),alpha = 0.5) +
  geom_col() + geom_text(aes(label = labels), position = position_stack(vjust = 0.1)) +
  coord_polar(theta = "y")


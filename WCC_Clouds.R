
#Load libraries
library(rvest) #Web Scraping
library(dplyr)
library(tm)
library(wordcloud)
library(ggplot2)

##The url to scrape the data from
target_url <- read_html("http://stateoftheunion.onetwothree.net/texts/")

#Collect the text content on the site
president <- target_url %>% html_nodes("#text li a") %>% html_text() %>% as.matrix()     
links <- target_url %>% html_nodes("#text li a") %>% html_attr("href") %>% as.matrix()
dates <- cbind(president, links) %>% data.frame()
colnames(dates) <- c("President", "link")

#Create a dataframe with the names of the presidents and link to their speech
dat <- data.frame(presidents = dates$President, link = links)  
dat$link <- as.character(dat$link)
dat$link <- paste("http://stateoftheunion.onetwothree.net/texts/", dat$link, sep= "")
dat$link  <- as.character(dat$link)

#Create the dataframe and collect the speeches of all the presidents in US history
cont <- data.frame()
for(i in 1:nrow(dat)){
  speech <- read_html(dat$link[i]) %>% html_nodes("p") %>% html_text()
  speech <- gsub("\n", " ", speech)
  x <- paste(unlist(speech), collapse = " ") %>% data.frame()
  cont <- rbind(cont, x)

}

#store the contents of the speech alongside the corresponding presidents name
df <- cbind(dat, cont)
colnames(df) <- c("presidents", "speechtext")


custom_stopwords <- c("get", "can", "still", "bit", "now", "come", "later", "got", "tell"
                      , "said", "just", "else")


#Subset the data based on two presidents to generate comparison cloud
sotu <- df[c(223, 231),]
con <- Corpus(VectorSource(sotu$Speechtext)) %>%
  tm_map(content_transformer(tolower)) %>%  # Make all text lower case
  tm_map(removeNumbers) %>% # Remove numbers
  tm_map(removeWords, stopwords("english")) %>%  # Remove english common stopwords
  tm_map(removePunctuation) %>%  # Remove punctuations
  tm_map(stripWhitespace) #%>% # Eliminate extra white spaces
  tm_map(removeWords, custom_stopwords)

tdm <- TermDocumentMatrix(con) %>%
       as.matrix()
colnames(tdm) <- c("Obama", "Trump")

#Generate the comparison cloud
par(mfrow=c(1,1))
comparison.cloud(tdm, random.order = FALSE, colors = c("lightsteelblue3","indianred3"), title.size = 2.5,
                 max.words = 450)

#Generate the commonality cloud
commonality.cloud(tdm, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=400)

#subset the data based on a single president
sotu <- df[c(223),]

#Create a corpus for the word cloud
speech <- Corpus(VectorSource(sotu$speech)) %>%
  tm_map(content_transformer(tolower)) %>%  # Make all text lower case
  tm_map(removeNumbers) %>% # Remove numbers
  tm_map(removeWords, stopwords("english")) %>%  # Remove english common stopwords
  tm_map(removePunctuation) %>%  # Remove punctuations
  tm_map(stripWhitespace) %>% # Eliminate extra white spaces
  tm_map(removeWords, custom_stopwords)


#Build a term document matrix
dtm <- TermDocumentMatrix(speech)  %>% as.matrix() 

desc <- sort(rowSums(dtm), decreasing = TRUE)
dat <- data.frame(word = names(desc), freq = desc)


#generate wordcloud
set.seed(123)
n <- nrow(dat)
wordcloud(words = dat$word, freq = dat$freq, min.freq = 1, max.words = n, random.order = FALSE, rot.per = 0.2,
          colors=brewer.pal(8, "Dark2"))

#subset the data based on top 20 frequently used words
dt <- dat[1:20,]


#Create a barplot for the top 20 most frequently used words
ggplot(dt, aes(x = reorder(word, -freq), y = freq)) + geom_bar(stat = "identity", fill="darkred") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 



library(RCurl) #to download files from Web servers, post forms, getURL()
library(XML) #to read xml documents and create XML documents xpathSApply()
library(stringr) #for string operations and data cleaning str_split 

################### SCRAPE SONG NAMES ###################
#### source: wikipedia.org #source to get the song names

bill_music <- data.frame()  #creating a dataframe to store song info
for (i in 1965:2017) { 
     # create the URL for each year
     URL <- paste("http://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_",i,sep="")
     # parse the HTML
     results <- htmlTreeParse(getURL(URL, followlocation=TRUE), useInternal=TRUE) #parse html file and generate a R structure
     billboard_text <- xpathSApply(results, "//table[@class='wikitable sortable']//tr",xmlValue) #scraping xml in R
     split_billboard_text <- str_split_fixed(billboard_text,"\n",3)  #splitting the text
     billboard <- as.data.frame(cbind(split_billboard_text[2:101, ], rep(i,100)), stringsAsFactors=FALSE) #binding the data for each year
     # row bind this year's data to all the data
     bill_music <- rbind(bill_music, billboard) 
     
}
colnames(bill_music) <- c("Rank", "Song", "Artist", "Year") #col names of the csv
bill_music$Song <- gsub('\\"', "", bill_music$Song) #data cleaning
bill_music$Song <- tolower(gsub("[^[:alnum:] ]", "", bill_music$Song))
bill_music$Song <- gsub("\\'", "", iconv(bill_music$Song, to='ASCII//TRANSLIT')) # fix special accent chars

bill_music$Artist <- tolower(gsub("[^[:alnum:] ]", "", bill_music$Artist)) #lowercase
bill_music$Artist <- gsub("'e", "e", iconv(bill_music$Artist, to='ASCII//TRANSLIT')) # fix special accent chars
bill_music$Artist<- gsub("'o", "o", bill_music$Artist)

# new variables
bill_music$Lyrics <- "" 
bill_music$Source <- ""

################### SCRAPE LYRICS ###################
### source: multiple. 1=metorlyics.com, 3=songlyrics.com, 5=lyricsmode.com
for (s in 1:length(bill_music$Song))  {
     
     lyrics <- "Not set yet."
     results <- 12 # arbitrary number
     
     # clean up the artist field to fit in the URL
     artist <- strsplit(bill_music$Artist[s], " featuring | feat | feat. | with | duet | and ")
     artist <- unlist(artist)[[1]] #unlist a list of vectors into a single vector for artists
     artist_collaboration <- gsub("the ", "", artist) #data cleaning
     firstletter <- substring(artist_collaboration, 1, 1) 
     
     # make URLs
     metroURL <- paste("http://metrolyrics.com/",bill_music$Song[s],"-lyrics-",artist_collaboration,".html",sep="")
     songURL <- paste("http://songlyrics.com/",artist_collaboration,"/",bill_music$Song[s],"-lyrics",sep="")
     modeURL <- paste("http://www.lyricsmode.com/lyrics/", firstletter, "/", artist_collaboration, "/", bill_music$Song[s], ".html", sep="")
     
     
     URLs <- c(metroURL, songURL, modeURL)
     
     lyric_id <- c("//div[@id='lyrics-body-text']", 
                    "//p[@id='songLyricsDiv']", 
                    "//p[@id='lyrics_text']")
     
     for (b in 1:length(URLs)) {
          bill_music$Lyrics[s] <- "Not set yet."
          
          results <- 12 # arbitrary number
          
          if(b!=3) URL <- tolower(gsub(" ", "-", URLs[b]))
          if(b==3) URL <- URLs[b]
          
          tryCatch({ 
               results <- htmlTreeParse(URL, useInternal=TRUE, isURL=TRUE)
               lyrics <- xpathSApply(results, lyric_id[b], xmlValue) },
               error = function(x) { 
                    message(paste(s, "failed")) },
               finally={ 
                    if (!is.numeric(results)) { 
                         if (length(lyrics)!=0) { 
                              bill_music$Lyrics[s] <- lyrics[[1]]
                              message(paste(s, "success"))
                              bill_music$Source[s] <- b
                              break
                         }
                    } 
               }) # end tryCatch
     } # end URL for
} # end for

bill_music$Lyrics <- gsub("\\\n|\\\t"," ",bill_music$Lyrics)
bill_music$Lyrics <- tolower(gsub("[^[:alnum:] ]", "", bill_music$Lyrics))
missing <- round(length(bill_music[bill_music$Lyrics=="not set yet", 1])/length(bill_music[,1]), 4)*100

bill_music$Lyrics <- gsub("not set yet", "NA", bill_music$Lyrics)
bill_music$Lyrics <- gsub("we are not in a position to display these lyrics due to licensing restrictions sorry for the inconvenience", "NA", bill_music$Lyrics)

 setwd("D:/Documents/Modules/Capstone/")
 write.csv(bill_music, "billboard_lyrics_1964-2017.csv", row.names=FALSE)
 bill_music <- read.csv("billboard_lyrics_1964-2017.csv", stringsAsFactors=FALSE)
 
 bill_music




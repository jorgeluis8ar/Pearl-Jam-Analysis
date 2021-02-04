
rm(list = ls())
packages <- c('spotifyr','lubridate','ggplot2','dplyr','tidytext',
              'stringr','tidyr','viridis','wordcloud', "tm",'forcats','fmsb',
              'scales','radarchart','qdap','knitr','geniusr','tidyverse','plotly')
lapply(packages,require,character.only=T)
rm(packages)
setwd("~/Documents/Universidad/Taller de R")

# pj <-readRDS(file = 'Pearl-Jam-Analysis/Data/pearl.RDS')
# 
# random <- sample(nrow(pj))
# pj <- pj[random,]
# 
# albums <- c("Riot Act", "Ten", "Yield", "Gigaton", "Backspacer", "Vs.", "Pearl Jam", "No Code", "Vitalogy", "Binaural", "Lightning Bolt")
# pj <- pj[pj$album_name %in% albums,]
# pj <- mutate(pj, dupli=ifelse(duplicated(pj$track_name)==T,1,0))
# pj <- subset(pj,dupli == 0)
# head(pj,5) %>% select(track_name, album_name,artist_name,album_release_date,danceability,instrumentalness,energy,valence, key_name,mode_name) %>%  kable(format = "simple", col.names = str_to_title(gsub("[_]", " ", colnames(.))),align = 'lccccccccc',caption = "An example table caption.",digits = 3)


# lyrics <- readRDS(file = 'Pearl-Jam-Analysis/Data/lyrics.RDS')

pj <- readRDS(file = 'Proyecto/Datos/pearl jam 2.RDS')
# pj <- merge(x = pj,y = lyrics,by = 'track_id')

songs <- c("Arc","Aya Davanita - Remastered","Cready Stomp - bonus track")
pj[pj$track_name %in% songs,grep('lyrics', colnames(pj))]
pj[pj$track_name %in% songs,grep('lyrics', colnames(pj))] <- NA

pj$lyrics <- str_replace_all(pj$lyrics,'/','')
pj$lyrics <- str_replace_all(pj$lyrics,' x ','')
pj$lyrics <- str_replace_all(pj$lyrics,' x','')



letras <- Corpus(VectorSource(pj$lyrics))
letras <- tm_map(letras, content_transformer(tolower))
letras <- tm_map(letras, removePunctuation)
letras <- tm_map(letras, removeWords, stopwords("english"))
letras <- tm_map(letras, stripWhitespace)
letras <- letras %>% unlist()
pj$lyrics <- as.character(letras[1:146])
pj$lyrics <- str_trim(pj$lyrics)


pj$lyrics <- str_replace_all(pj$lyrics, 'bottomoh', ' bottom oh ')
pj$lyrics <- str_replace_all(pj$lyrics, ' heyim ', ' hey I am ')
pj$lyrics <- str_replace_all(pj$lyrics, ' deepoh ', ' deep oh ')
pj$lyrics <- str_replace_all(pj$lyrics, ' butterfliesdont ', ' butterflies do not ')
pj$lyrics <- str_replace_all(pj$lyrics, ' awaysomeday ', ' away someday ')
pj$lyrics <- str_replace_all(pj$lyrics, ' risingnext ', ' rising next ')
pj$lyrics <- str_replace_all(pj$lyrics, ' uhohoh ', ' oh ')
pj$lyrics <- str_replace_all(pj$lyrics, ' angellest ', ' angel lets ')
pj$lyrics <- str_replace_all(pj$lyrics, ' doodoodoodoodoodoodoo ', '')
pj$lyrics <- str_replace_all(pj$lyrics, ' yeah ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' yeah', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' ooh ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' oh ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' ah ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' hm ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' ye ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' ay ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' yeh ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' ya ', 'you')
pj$lyrics <- str_replace_all(pj$lyrics, ' mm ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' interlude ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' woo ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' mhm ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' oooh ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' ooooh ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' hey ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' uh ', ' ')
pj$lyrics <- str_replace_all(pj$lyrics, ' youll ', ' you will ')
pj$lyrics <- str_replace_all(pj$lyrics, ' cant ', ' can not ')
pj$lyrics <- str_replace_all(pj$lyrics, 'cant ', ' can not ')
pj$lyrics <- str_replace_all(pj$lyrics, ' im ', ' I am ')
pj$lyrics <- str_replace_all(pj$lyrics, ' ill ', ' I will ')
pj$lyrics <- str_replace_all(pj$lyrics, ' didnt ', ' did not ')
pj$lyrics <- str_replace_all(pj$lyrics, ' dont ', ' do not ')
pj$lyrics <- str_replace_all(pj$lyrics, ' hes ', ' he is ')
pj$lyrics <- str_replace_all(pj$lyrics, ' uhhuh ', '')
pj$lyrics <- str_replace_all(pj$lyrics, ' its' , '')
pj$lyrics <- str_replace_all(pj$lyrics, ' id ', ' I would ')
pj$lyrics <- str_replace_all(pj$lyrics, ' youre ', ' you are ')
pj$lyrics <- str_replace_all(pj$lyrics, ' youve ', ' you have ')
pj$lyrics <- str_replace_all(pj$lyrics, ' youd ', ' you would ')
pj$lyrics <- str_replace_all(pj$lyrics, ' theres ', ' there is ')
pj$lyrics <- str_replace_all(pj$lyrics, ' theyll ', ' they will ')
pj$lyrics <- str_replace_all(pj$lyrics, ' whats ', ' what is ')
pj$lyrics <- str_replace_all(pj$lyrics, ' doesnt ', ' does not ')
pj$lyrics <- str_replace_all(pj$lyrics, ' ive ', ' I have ')
pj$lyrics <- str_replace_all(pj$lyrics, ' hadnt ', ' had not ')
pj$lyrics <- str_replace_all(pj$lyrics, ' wouldnt ', ' would not ')
pj$lyrics <- str_replace_all(pj$lyrics, ' til ',' until ')
pj$lyrics <- str_replace_all(pj$lyrics, ' wouldve ', ' would have ')
pj$lyrics <- str_replace_all(pj$lyrics, ' shes ', ' she is ')
pj$lyrics <- str_replace_all(pj$lyrics, ' thats ', ' that is ')

album_levels <- c(
  "Ten", "Vs.", "Vitalogy", "No Code", "Yield", "Binaural", 
  "Riot Act", "Pearl Jam", "Backspacer", "Lightning Bolt", "Gigaton")


pearl_songs <- pj %>% unnest_tokens(word, lyrics)

cleaned_pearl <- pearl_songs %>% anti_join(stop_words)

bing <- get_sentiments(lexicon = "nrc")

pearl_sentiment <- cleaned_pearl %>%
  inner_join(bing,by = c('word')) %>%
  count(track_name, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(positive + negative))

columns <- c(grep('album_name', colnames(pj)),
             grep('track_name', colnames(pj)))

pearl_sentiment <- inner_join(x = pearl_sentiment, y = pj[,columns], by = "track_name")

pj <- inner_join(pj, pearl_sentiment)

pj <- arrange(pj, album_release_year)
pj <- pj %>% unnest_tokens(word, lyrics) %>%
  group_by(track_name) %>% summarise(distinct_words = n_distinct(word)) %>% 
  inner_join(pj)


pj <- mutate(pj, words = sapply(strsplit(pj$lyrics, " "), length))
pj <- mutate(pj, seconds = duration_ms/1000)
pj <- mutate(pj, words_per_second = words/seconds)
pj <- mutate(pj, turbulent_words = sadness/distinct_words)
pj <- mutate(pj, dist_per = (distinct_words/words))
pj <- mutate(pj, index = ((1-valence)+turbulent_words*(1+dist_per))/2)

rank <- pj %>% group_by(album_name,track_name,album_release_year) %>% 
  summarise(average = mean(index)) %>% arrange(album_release_year)
rank <-  inner_join(rank, rank %>% group_by(album_name) %>% summarise(gloom_index = mean(average)))
rank <- as.data.frame(rank)


rank %>% group_by(album_name) %>% summarise(gloom_index = mean(average)) %>% arrange(desc(gloom_index)) %>% kable()
rank %>% group_by(track_name,album_name) %>% summarise(gloom_index = mean(average)) %>% arrange(desc(gloom_index)) %>% head(10) %>% kable()
pj %>% group_by(track_name,album_name) %>% summarise(word_per_song = mean(words),
                                                     distinct_words = mean(distinct_words),
                                                     words_per_second = mean(words_per_second),
                                                     ratio = mean(dist_per)) %>% arrange(desc(distinct_words)) %>% head(20) %>% kable()
pj %>% group_by(album_name) %>% summarise(total_words   = sum(words),
                                          songs         = n(),
                                          word_per_song = mean(words),
                                          words_per_second = mean(words_per_second),
                                          ratio = mean(dist_per)) %>% arrange(desc(word_per_song)) %>% kable()



pj %>% group_by(track_name) %>% summarise(ta = (((1- mean(valence))+mean(turbulent_words)*(1+mean(dist_per))))/2,
                                          valence = mean(valence),
                                          turbulent_words = mean(turbulent_words),
                                          dist_per = mean(dist_per)) %>% arrange(desc(ta)) %>% 
      head(10) %>% kable()

pj <- mutate(pj, gloom_index = round(rescale(1 - ((1 - valence) + (turbulent_words * (1 + dist_per))) / 2, to = c(1, 100)), 2))

rank$album_name <- factor(rank$album_name, levels = album_levels)
pj$album_name <- factor(pj$album_name, levels = album_levels)

pal <- c("turquoise4","navyblue","peru","goldenrod2","forestgreen","darkgray",
         "orange","magenta","lightslateblue","yellowgreen","mediumblue")

fig <- plot_ly(data = rank,
              x = ~album_name, y = ~average,type = 'scatter',mode = 'markers',
              marker = list(size = 10),
              color = ~album_name, colors = pal, span = I(1),stroke = I("black"),
              text = ~paste("Gloom Index score: ", round(average,3), '<br>Track:', track_name))

fig <- fig %>% add_trace(y = ~gloom_index,mode = 'lines+markers',type = 'scatter',
                      color = I('black'),stroke = I("black"),marker = list(size = 10),
                      text = ~paste("Album average valence:", round(gloom_index,3)))

fig %>% layout(xaxis = list(zeroline = T, title ="Album Name"),
               yaxis = list(hoverformat = '.3f', title = 'Gloom Index'),
showlegend = F)

pearl_songs <- pj %>% unnest_tokens(word, lyrics)
pearl_songs$album_name <- factor(pearl_songs$album_name, levels = album_levels)
pj$album_name <- factor(pj$album_name, levels = album_levels)

pearl_songs %>% group_by(album_name,track_name) %>% summarise(number_words = n()) %>% 
  ggplot(data = ., aes(x = factor(album_name), y = number_words)) + geom_violin(alpha = 0.7, color = 'black', size = 0.2)+
  geom_jitter(aes(size = number_words),height = 0, width = 0.1, alpha =0.2) + geom_boxplot(aes(fill = album_name), alpha = 0.6) +
  labs(title = "Lexical Diversity - Vocabulary", x = "", y = 'Distinct words count') + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        panel.grid.major = element_line(size = 0.4)) + guides(fill = F) + scale_size('Number of words')

pearl_songs %>% group_by(album_name,track_name) %>% summarise(number_words = n()) %>% 
  plot_ly(x = ~album_name, y = ~number_words,type = 'scatter',mode = 'markers', size = ~number_words)

p <- pearl_songs %>% group_by(album_name,track_name) %>% summarise(number_words = n()) %>% 
     plot_ly(y = ~number_words,color = I("black"), 
     alpha = 0.75, boxpoints = "suspectedoutliers") 
p %>% add_boxplot(x = ~album_name, color = ~album_name, colors = pal)


fig <- pearl_songs %>% group_by(album_name,track_name) %>% summarise(number_words = n()) %>% 
  plot_ly(y = ~number_words, type = 'violin', box = list(visible = T),
    meanline = list(visible = T),x = ~album_name, color = ~album_name, colors = pal) 

fig <- fig %>%
  layout(
    yaxis = list(
      title = "",
      zeroline = F
    )
  )

fig


matri <-pj %>% group_by(album_name) %>% 
               summarise(index = mean(index),
                valence = mean(valence),
                `sad words` = mean(turbulent_words),
                `distinct percentage words` = mean(dist_per),
                `release year`=mean(album_release_year)) %>% 
               arrange(`release year`) %>% select(album_name,index,`release year`)


rollm <- function(data){
  data <- mutate(data,roll_mean = 0)
  for (element in 1:nrow(data)) {
    data$roll_mean[element] <- mean(data$index[1:element]) 
  }
  return(data)
}

confi <- function(data){
  data <- mutate(data,conf_plus = 0)
  data <- mutate(data,conf_minu = 0)
  
  for (element in 1:nrow(data)) {
    if (element == 1) {
      data$conf_plus[element] <- data$roll_mean[element]
      data$conf_minu[element] <- data$roll_mean[element]
    }
    else{
      data$conf_plus[element] <- mean(data$roll_mean[1:element]) +1.96*(sd(data$roll_mean[1:element])/sqrt(element+1))
      data$conf_minu[element] <- mean(data$roll_mean[1:element]) -1.96*(sd(data$roll_mean[1:element])/sqrt(element+1))      
    }
  }
  return(data)
}

matri <- rollm(matri)
matri <- confi(matri)
matri$album_name <- factor(matri$album_name, levels = album_levels)


fig2 <- plot_ly(data = matri,
                x = ~album_name, y = ~index,type = 'scatter',mode = 'lines+markers',
                marker = list(size = 10),
                color = ~album_name, colors = pal,
                span = I(1),stroke = I("black"),
                text = ~paste("Gloom Index score: ", round(index,3), '<br>Track:', album_name,'<br>Year:',`release year`))

fig2 %>% add_trace(y = ~roll_mean,mode = 'lines+markers',type = 'scatter', name = 'Moving average',
                   color = I('black'),stroke = I("black"),marker = list(size = 10),
                   text = ~paste("Album average valence:", round(roll_mean,3),'<br>Track:', album_name,'<br>Year:',`release year`))

![GitHub](https://img.shields.io/github/license/jorgeluis8ar/Pearl-Jam-Analysis) [![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/Pearl-Jam-Analysis/jorgeluis8ar/issues) ![](https://img.shields.io/github/followers/jorgeluis8ar?style=social)

<img src="https://avatars0.githubusercontent.com/u/69487641?s=400&v=4" align="right" width=120 height=120 alt="" />

### Colaboradores 
<a href="https://github.com/jorgeluis8ar/Pearl-Jam-Analysis/graphs/contributors">
  <img src="https://contributors-img.web.app/image?repo=jorgeluis8ar/Pearl-Jam-Analysis" />
</a>


# Pearl Jam Analysis in R
## Musical and lyrical analysis of Pearl Jam's songs

This repository comprises a musical and lyrical analysis of Pearl Jam's songs. I use data from Spotify's and Genius's APIs. I use scripts to query information on the musical propety of all of the 11 Pearl Jam albums. I later use a script to scrape the lyrycs to several songs and finally use the Genius API to download the remaining. Related works include [Kendrick Lamar sentiment analysis](https://github.com/davidklaing/kendrick/blob/master/README.md), [Gloom Index](https://www.rcharlie.com/blog/fitter-happier/) to find Radiohead's most depressing song by [Charlie Thompson](https://www.rcharlie.com), [Bob Dylan lyrical analysis](https://rpubs.com/paul_reiners/406359) by Paul Reiners, [tidy sentiment analysis](https://www.datacamp.com/community/tutorials/sentiment-analysis-R) on Prince's music by Debbie Liske and [Musical Lyrics Analysis](https://rpubs.com/BreeMcLennan/music_lyric_analysis) on several artists by Bree McLennan.

The repo is organized as follows:


* ***Data Assessment***
  + ***Intellectual Property (Copyrigths)***
  + ***Songs selection: Spotify API***
  + ***Lyrics: Scraping and using the Genius API***
  + ***Processing the lyrics***
  + ***Final Data set***

* ***Data exploration***
  + ***Word counts by album and song***
  + ***Wordclouds***
  + ***Vocabulary diversity***
  + ***Term Frequency Inverse Document Frequency (TF-IDF)***

* ***Sentiment Analysis and Natural Language Processing***
  + ***NCR Sentiment***
  + ***Bi-grams***
  
# Data Assessment
## Intellectual Property (Copyrigths)

I would like to acknowledge that every single content exposed in this repository is based on protected songs. All the rigths of the data used come from the Spotify API, Genius API, but mainly the sonwriters and composers of the songs. In no way it is intented to make it as my own. All the mistakes made are my own and no one else.


## Songs selection: Spotify API

I first asing all the packages needed into the vector `packages`. In this section the important package to query the Spotify API is `spotifyr`.

```{r, eval=T ,include=F}
packages <- c('spotifyr','lubridate','ggplot2','dplyr','tidytext',
              'stringr','tidyr','viridis','wordcloud', "tm",'forcats','fmsb',
              'scales','radarchart','qdap','knitr','geniusr','tidyverse')
lapply(packages,require,character.only=T)
rm(packages)
```

```{r, eval=F ,include=T}
packages <- c('spotifyr','lubridate','ggplot2','dplyr','tidytext',
              'stringr','tidyr','viridis','wordcloud', "tm",'forcats','fmsb',
              'scales','radarchart','qdap','knitr','geniusr','tidyverse')
lapply(packages,require,character.only=T)
rm(packages)
```
The package requieres a `Client ID` and a `Client Secret` to query the API. To do so, the user must have a premium account in order to create a developers account. The proccess could be done [here](https://developer.spotify.com/dashboard/). Once this process is done, you can pull spotify access token into R with `get_spotify_access_token()`. Note that you could pass your ID and secret in order to set your credentials into System Environment. For more information on the packages reference to Charlie Thompson's `spotifyr` Github [repository](https://github.com/charlie86/spotifyr).

```{r, eval=F}
Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')

access_token <- get_spotify_access_token()
```

To get Pearl Jam's songs audio features we use the function `get_artist_audio_features()`. This functions queries the Spotify API and returns information on several characteristics such as: danceability, energy, instrumentalness, valene, explicit content, track name, track album, etc. Now, I'll only keep songs featuring in the 11 albums. The reason is to eliminate live performances and covers and focus on self-written songs. Lastly we verify there are no repeated sing in the data set.

```{r, eval=T}
albums <- c("Riot Act", "Ten", "Yield", "Gigaton", "Backspacer", "Vs.", "Pearl Jam", "No Code", "Vitalogy", "Binaural", "Lightning Bolt")
pj <- pj[pj$album_name %in% albums,]
pj <- mutate(pj, dupli=ifelse(duplicated(pj$track_name)==T,1,0))
pj <- subset(pj,dupli == 0)
head(pj,5) %>% select(track_name, album_name,artist_name,album_release_date,danceability,instrumentalness,energy,valence, key_name,mode_name) %>%  kable(format = "simple", col.names = str_to_title(gsub("[_]", " ", colnames(.))),align = 'lccccccccc',caption = "An example table caption.",digits = 3)
```

## Lyrics: Genius API

As for Spotify's API, the Genius API requieres a developers account in order to query information. To authenticate the information the user must: 1. Create a `Genius API client` [here](https://genius.com/api-clients/new), 2. generate a `client access token` form the [API Clients Page](https://genius.com/api-clients) and 3. set your credential in the System environment variable `GENIUS_API_TOKE` calling the function `genius_token()`. Now, in order to fecth the lyrics for each song I created a loop that goes trhough every song in the data set and retrieves the lyrics in a single vector and then adds it to the create variable `lyrics2`.

```{r, eval = F} 
pj <- mutate(pj, lyrics2 = "")
for (element in (1:nrow(pj))) {
  
  # I created the loop with two cicles in it in order to double check all the songs 
  # get their corresponding lyrics. Note that the loop isolates each song and
  # retrieves the information with the function et_lyrics_search()
  
  title <- str_to_title(pj$track_name[element]) 
  print(title)
  lyrics <- get_lyrics_search(artist_name = "Pearl Jam",
                              song_title = title)
  if (nrow(lyrics)!=0) {
    # I optedto reduce the dimensions of the retrieved data set to  one observation.
    # Now I save the first line and add the rest of the lines of the song
  lyrics2 <- ""
    for (piece in (1:nrow(lyrics))) {
    if (piece ==1) {
      lyrics2 <- lyrics$line[piece]
    }
    else{
      lyrics2 <- paste(lyrics2, lyrics$line[piece], collapse = " ")
    }
    print(title)
    print(lyrics2)
    }
  }
  
  # If any song had any problem with querying the lyrics the second part of the loop
  # repeats the procces to guarantee that all songs are assinged to their lyrics.
  
  if (nrow(lyrics)==0) {
    lyrics <- get_lyrics_search(artist_name = "Pearl Jam",
                                song_title = title)
    lyrics2 <- ""
    for (piece in (1:nrow(lyrics))) {
      if (piece ==1) {
        lyrics2 <- lyrics$line[piece]
      }
      else{
        lyrics2 <- paste(lyrics2, lyrics$line[piece], collapse = " ")
      }
      print(title)
      print(lyrics2)
    }
  }
  
  # Finally the song is added to the original data
  
  pj$lyrics2[element] <- lyrics2
}
```

Our data set now contains lyrics to all the 146 songs in the 11 albums. A glimpse to my top two Pearl Jam songs (according to spotify rankings) Black and Even flow:

``` {r, eval=T ,include=F} 
pj <- inner_join(pj,lyrics)
colnames(pj)[ncol(pj)] <- 'lyrics'
```

``` {r}
songs <- c('Black', 'Even Flow')
pj[pj$track_name %in% songs,] %>% select(artist_name, album_name, track_name, lyrics) %>% 
  kable(format = "simple",col.names = str_to_title(gsub("[_]", " ", colnames(.))),align = 'lccccccccc')
```

Finally, we have to take into account instumental songs. Even though Pearl Jam doesn't have many instrumental songs 3 cases need to be adressed. these are ***Arc***, ***Aya Davanita*** and ***Cready Stomp***. I proced then to assing a missing value to these songs in order to implement the followong analysis.
``` {r, eval = T}
songs <- c("Arc","Aya Davanita - Remastered","Cready Stomp - bonus track")
pj[pj$track_name %in% songs,grep('lyrics', colnames(pj))]
pj[pj$track_name %in% songs,grep('lyrics', colnames(pj))] <- NA
```


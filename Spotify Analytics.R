# Spotify Data Analytics
# Justin Chow

install.packages("Rspotify") # Install RSpotify package
library (Rspotify)

library(tidyverse)
library(knitr)

library(dplyr)

install.packages("car") # Need this for Vif Function
library (car)
library(plotly)

################################### Accessing Spotify API ############################ 

# Use the spotifyOAuth() function to save authentication as an object. 
# We will use this everytime we want to access Spotify data.
# app_id: can be anything
# client_id and client_secret: must be the keys generated after creating your app in the Spotify web UI
keys <- spotifyOAuth('Spotify Analysis','13844449779f4e27a224c043839a861c', '527504c16b4b4ac086eec47074367c0d')

  
# Top 100 Songs Canada 2019 Playlist ID: 37i9dQZF1DX82re5NxbwyO
top_100_song_ids <- getPlaylistSongs("spotify","37i9dQZF1DX82re5NxbwyO",token = keys) # %>% select(tracks, popularity, artist)

View(top_100_song_ids)

################################### Data Wrangling  ############################ 

# Loop through the ID's of the Top 100 Playlist
current_id <- top_100_song_ids[1,2]

# Initilize the first row of dataframe. Grab the features of the first row's ID
master_features_df <- getFeatures(current_id, token = keys)

# Eliminate last two columns
master_features_df_new <- master_features_df %>% select (-uri, -analysis_url)

View(master_features_df_new)

# Loop through the ID's of the Top 100 Songs and add to dataframe
for (i in 2:nrow(top_100_song_ids))
{
  # Set the current ID by look at the df
  current_id <- top_100_song_ids[i,2]
  # Retrieve current ID features 
  song_features <- getFeatures(current_id, token = keys)
  # Eliminate the last two columns 
  song_features_new <- song_features %>% select (-uri, -analysis_url)
  # Combine dataframes
  master_features_df_new <- rbind(master_features_df_new, song_features_new)
  # Iterate index
  i <- i + 1
}

View(master_features_df_new)

# Create a new vector for Popularity field
popularity_vec <- c()

# Iterate through our master data frame
for (i in 1:nrow(master_features_df_new))
{
  # Set the current id
  current_id <- master_features_df_new[i,1]
  # Get the track info of current track
  current <- getTrack (current_id, token = keys)
  # Grab only the Popularity field
  current_popularity <- current$popularity
  # Add to Popularity vector
  popularity_vec[i] <- current_popularity
  # Increment index
  i <- i + 1
}
# Add Popularity vector to master df
master_features_df_new["Popularity"] <- popularity_vec

View(master_features_df_new)

# The code below renames the columns so the R can attach the fields. This way it would be easier to read when we look at our regression summary
master_df <- master_features_df_new
names(master_df) <- c("Song_id", "danceability", "Energy", "Key", "Loudness", "Mode", "Speechiness", "Acousticness", "Instrumentalness", "Liveness", "Valence", "Tempo", "Duration(ms)", "Time_Signature", "popularity")     
attach (master_df)

################################### Data Visualization ############################ 

## Question 1: Is there a certain sweet spot in whether songs should have a certain level of dancability to them?
ggplot(master_df, aes(x = danceability)) + geom_histogram(bins = 25) + theme_minimal()
ggplot(master_df, aes(x = energy)) + geom_histogram(bins = 25) + theme_minimal()


# The code below pulls the Top 100 Songs of 2019 and finds the genre from each artist
playlist_songs <- getPlaylistSongs("spotify", "37i9dQZF1DX82re5NxbwyO", token = keys)

# Extract only the artist ID field as a vector
artist_id <- playlist_songs$artist_id

# Extract only the artist field as a vector 
artist <- playlist_songs$artist

# Extract only the popularity field as a vector
popularity <- playlist_songs$popularity

# Extract only the popularity field as a vector
song_name <- playlist_songs$tracks

# Create an empty vector called genre
genre <- c()

# Loop the Top 100 Playlist and find the genre for each artist
for (i in 1:nrow(playlist_songs))
{
  # At each iteration, set the artist ID
  current_artist_id <- artist_id[i]
  
  # Get the information of the artist use getArtist() and use keys as token for authentication
  artist_info <- getArtist(current_artist_id, token = keys)
  
  # Genre data is stored like pop; hip hop; r&b
  # Remove the genres after the first ;
  temp <- gsub("\\;.*","",artist_info$genres)
  
  # Store in genre vector
  genre[i] <- temp
  
  # Iterate index
  i <- i + 1
}

# Create a new dataframe for visualization. Add data stored in master_df
viz_df <- master_df

# Add Artist vector into viz dataframe
viz_df["Artist"] <- artist

# Add Genre vector into viz df
viz_df["Genre"] <- genre

# Add song vector into viz df 
viz_df["Song"] <- song_name

View(viz_df)

# The first data visualization
# Set X axis to danciability
# Set y axis to Popularity
# Set the colour as the genre
# Set size of bubblew to ernegy
plot_1 <- plot_ly(viz_df, 
                x = ~Danceability, 
                y = ~Popularity, 
                type = 'scatter', 
                mode = 'markers',
                color = ~viz_df$Genre,
                size = ~Energy,
                sizes = c(1,25),
                colors = 'Paired',
                text = ~paste('Artist:', Artist, '<br>Song Name:', Song),
                marker = list(opacity=0.5, sizemode = 'diameter')) %>% layout(title = "Dancibility vs. Popularity per Genre and Energy of Spotify's Top 100 Songs", 
                                                                                 xaxis = list(showgrid = FALSE),
                                                                                 yaxis = list(showgrid = FALSE))

plot_1


# The second data visualization
# Set X axis to danciability
# Set y axis to Popularity
# Set the colour as the valence
# Set size of bubblew to ernegy
plot_2 <- plot_ly(viz_df, 
                  x = ~Danceability, 
                  y = ~Popularity, 
                  type = 'scatter', 
                  mode = 'markers',
                  color = ~Valence,
                  size = ~Energy,
                  sizes = c(1,35),
                  colors = 'Reds',
                  text = ~paste('Artist:', Artist, '<br>Song Name:', Song),
                  marker = list(opacity=0.5, sizemode = 'diameter')) %>% layout(title = "Dancibility vs. Popularity per Valence(Emotion) and Energy of Spotify's Top 100 Songs", 
                                                                                xaxis = list(showgrid = FALSE),
                                                                                yaxis = list(showgrid = FALSE))

plot_2

 ################################### Analysis ##################################### 

# **************************** Correlation of Variables ****************************

# This code looks at how correlated the variables are in the Top 100 Songs variables
master_df_no_id <- master_df %>% select(-Song_id)
cor(master_df_no_id)
plot(master_df_no_id, pch = 15 , col = "blue")

# **************************** Correlation of Variables ****************************



# **************************** Categorical Variables ****************************

# This code looks at categoical variables of the Top 100 songs
# Here we can see that the P-value is large for all keys except for the key of 5.
# This means that the popularity of songs are not significantly different to songs with the key of 0.
data_cat <- master_df[, c("popularity", "Key")]
data_cat$Key <- as.factor(data_cat$Key)
names(data_cat) <- c("popularity", "Key")
cat_fit <- lm(data_cat$popularity ~ ., data = data_cat)
summary (cat_fit)


# This code looks at categoical variables of the Top 100 songs
# Here we can see that the P-value is large for both types of Modes for songs
# This means that the popularity of songs are not significantly different to songs with the Mode of 0.
data_cat <- master_df[, c("popularity", "Mode")]
data_cat$Mode <- as.factor(data_cat$Mode)
names(data_cat) <- c("popularity", "Mode")
cat_fit <- lm(data_cat$popularity ~ ., data = data_cat)
summary (cat_fit)

# **************************** Categorical Variables ****************************



# **************************** Building a regression model ****************************
# The code below performs the regression

# Create a regression model and do not include the categoical variables
model_1 <- lm (popularity ~ danceability + Energy + Loudness + Speechiness + Acousticness + Instrumentalness + Liveness + Valence + Tempo + `Duration(ms)`)
summary (model_1)


# Use the step function
# After running the code below we find that with 95% confidence that Energy and Loudness 
# are the most signficant and influenial predictor variables in determining popularity.

# If we want to decrease our confidence interval to 85% we can say that the Energy, Loudness,
# the emotion conveyed and how much instruments are within the song are statisically signficant to determine popularity.

step.backward <- step (model_1, direction = "backward")
summary (step.backward) 

step.both <- step (model_1, direction = "both")
summary (step.both)

# **************************** Building a regression model ****************************




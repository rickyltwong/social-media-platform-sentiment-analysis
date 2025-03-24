# Functions for sentiment analysis project

# Function to extract hashtags from text
extract_hashtags <- function(hashtags) {
  unlist(strsplit(hashtags, " "))
}

# Function to process sentiment data
process_sentiment_data <- function(df) {
  # Remove unnecessary columns
  df <- df[, -c(1, 2)]
  df$Country <- as.factor(stringr::str_trim(df$Country))
  
  # Define sentiment label categories
  positive <- c("Positive", "Happiness", "Joy", "Love", "Amusement", "Enjoyment", "Admiration", "Affection", "Awe",
                "Acceptance", "Adoration", "Calmness", "Excitement", "Kind", "Pride", "Elation", "Euphoria", 
                "Contentment", "Serenity", "Gratitude", "Hope", "Empowerment", "Compassion", "Tenderness", 
                "Enthusiasm", "Fulfillment", "Reverence", "Hopeful", "Proud", "Grateful", "Empathetic", 
                "Compassionate", "Playful", "Free-spirited", "Inspired", "Confident", "Thrill", "Overjoyed", 
                "Inspiration", "Motivation", "JoyfulReunion","Satisfaction", "Blessed", "Appreciation", 
                "Confidence", "Accomplishment", "Wonderment", "Optimism", "Enchantment", "Mindfulness", 
                "Elegance", "Whimsy", "Harmony", "Creativity", "Radiance", "Wonder", "Rejuvenation", 
                "Coziness", "Adventure", "Melodic", "FestiveJoy", "InnerJourney", "Freedom", "Dazzle", 
                "Adrenaline", "ArtisticBurst", "CulinaryOdyssey", "Resilience", "Immersion", "Spark", 
                "Marvel", "Positivity", "Kindness", "Friendship", "Success", "Exploration", "Amazement", 
                "Romance", "Captivation", "Tranquility", "Grandeur", "Emotion", "Energy", "Celebration", 
                "Charm", "Ecstasy", "Colorful", "Hypnotic", "Connection", "Iconic", "Journey", "Engagement", 
                "Touched", "Triumph", "Heartwarming", "Solace", "Breakthrough", "Imagination", "Vibrancy", 
                "Mesmerizing", "Creative Inspiration", "Nature's Beauty", "Celestial Wonder", "Happy", 
                "PlayfulJoy", "DreamChaser", "Blessing", "Sympathy", "Renewed Effort", "Culinary Adventure", 
                "Determination", "Zest", "Joy in Baking", "Challenge", "Winter Magic", "Thrilling Journey", 
                "Runway Creativity", "Ocean's Freedom", "Relief", "Curiosity"
  )
  
  negative <- c("Negative", "Anger", "Fear", "Sadness", "Disgust", "Disappointed", "Bitter", "Shame", 
                "Despair", "Grief", "Loneliness", "Jealousy", "Resentment", "Frustration", "Boredom", 
                "Anxiety", "Intimidation", "Helplessness", "Envy", "Regret", "Melancholy", "Fearful", 
                "Apprehensive", "Overwhelmed", "Devastated", "Frustrated", "Envious", "Dismissive", 
                "Bitterness", "Heartbreak", "Betrayal", "Suffering", "EmotionalStorm", "Isolation", 
                "Disappointment", "LostLove", "Exhaustion", "Sorrow", "Darkness", "Desperation", 
                "Ruins", "Desolation", "Loss", "Heartache", "Solitude", "Obstacle", "Pressure", 
                "Miscalculation", "Sad", "Hate", "Bad", "Jealous", "Embarrassed"
  )
  
  neutral <- c("Neutral", "Surprise", "Anticipation", "Confusion", "Arousal", "Indifference", 
               "Numbness", "Nostalgia", "Ambivalence", "Contemplation", "Reflection", "Intrigue", 
               "Whispers of the Past", "Pensive", "Bittersweet", "Suspense", "Envisioning History", 
               "Mischievous", "Yearning"
  )
  
  # Add a label column based on category definition
  df <- df %>% dplyr::mutate(
    Sentiment = trimws(Sentiment),
    Sentiment_Label = dplyr::case_when(
      Sentiment %in% positive ~ "Positive",
      Sentiment %in% negative ~ "Negative",
      Sentiment %in% neutral ~ "Neutral"
    )
  )
  
  return(df)
}

# Function to analyze sentiment by platform
analyze_sentiment_by_platform <- function(df) {
  sentiment_platform <- df %>%
    dplyr::mutate(Platform = trimws(Platform)) %>% 
    dplyr::group_by(Platform, Sentiment_Label) %>%
    dplyr::summarise(Count = n())
  
  return(sentiment_platform)
}

# Function to analyze hashtags
analyze_hashtags <- function(df) {
  all_hashtags <- unlist(lapply(df$Hashtags, extract_hashtags))
  all_hashtags <- all_hashtags[all_hashtags != "" & all_hashtags != " "]
  hashtag_counts <- as.data.frame(table(all_hashtags))
  
  colnames(hashtag_counts) <- c("Hashtag", "Frequency")
  sorted_hashtag_counts <- hashtag_counts %>%
    dplyr::arrange(desc(Frequency))
  
  return(sorted_hashtag_counts)
}

# Function to analyze sentiment by region
analyze_sentiment_by_region <- function(df) {
  sentiment_by_region <- df %>%
    dplyr::group_by(Country, Sentiment_Label) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::ungroup()
  
  top_countries <- sentiment_by_region %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize(total_count = sum(count)) %>%
    dplyr::top_n(10, total_count) %>%
    dplyr::pull(Country)
  
  top_sentiment_by_region <- sentiment_by_region %>%
    dplyr::filter(Country %in% top_countries)
  
  top_sentiment_by_region <- top_sentiment_by_region %>%
    dplyr::group_by(Country) %>%
    dplyr::mutate(percentage = count / sum(count) * 100) %>%
    dplyr::ungroup()
  
  top_sentiment_by_region <- top_sentiment_by_region %>%
    dplyr::left_join(
      sentiment_by_region %>%
        dplyr::group_by(Country) %>%
        dplyr::summarize(total_count = sum(count)),
      by = "Country"
    )
  
  return(top_sentiment_by_region)
}

# Function to analyze sentiment over time
analyze_sentiment_over_time <- function(df) {
  df$Timestamp <- as.POSIXct(df$Timestamp, format="%Y-%m-%d %H:%M:%S")
  
  # Extract year and month for time series analysis
  df <- df %>%
    dplyr::mutate(YearMonth = format(Timestamp, "%Y-%m"))
  
  # Filter data to include only from 2020 onward
  df_2020 <- df %>%
    dplyr::filter(format(Timestamp, "%Y") >= 2020)
  
  # Group by YearMonth and Sentiment to get the count of each sentiment per month
  sentiment_over_time <- df_2020 %>%
    dplyr::group_by(YearMonth, Sentiment_Label) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::ungroup()
  
  return(sentiment_over_time)
}

# Function to prepare data for text analysis
prepare_text_data <- function(df) {
  # Cleaning and splitting text
  text_data <- df %>%
    dplyr::mutate(Text = tolower(Text)) %>%
    tidytext::unnest_tokens(word, Text)
  
  # Cleaning and splitting hashtags
  hashtag_data <- df %>%
    dplyr::mutate(Hashtags = stringr::str_replace_all(Hashtags, " #", ",")) %>%
    tidyr::separate_rows(Hashtags, sep = ",") %>%
    dplyr::filter(Hashtags != "") %>%
    dplyr::mutate(word = tolower(Hashtags))  # Create a uniform column name for merging
  
  # Combine both datasets
  combined_data <- dplyr::bind_rows(text_data, hashtag_data)
  
  # Remove stop words
  stop_words <- tidytext::get_stopwords()
  cleaned_data <- combined_data %>%
    dplyr::anti_join(stop_words, by = "word")
  
  return(cleaned_data)
}

# Function to analyze keywords by sentiment
analyze_keywords_by_sentiment <- function(cleaned_data) {
  keyword_sentiment <- cleaned_data %>%
    dplyr::group_by(word, Sentiment_Label) %>%
    dplyr::summarise(Count = n(), .groups = 'drop')
  
  # Filter for positive and negative sentiments
  positive_words <- keyword_sentiment %>%
    dplyr::filter(Sentiment_Label == "Positive") %>%
    dplyr::arrange(desc(Count))
  
  negative_words <- keyword_sentiment %>%
    dplyr::filter(Sentiment_Label == "Negative") %>%
    dplyr::arrange(desc(Count))
  
  return(list(
    all_keywords = keyword_sentiment,
    positive_words = positive_words,
    negative_words = negative_words
  ))
} 
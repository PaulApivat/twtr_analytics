library(tidyverse)
library(lubridate)

# 
df <- read_csv("tweet_activity.csv")

# you actually need more data than "past 28 days"
# you need data from January
jan <- read_csv("tweet_activity_jan.csv")
feb <- read_csv("tweet_activity_feb.csv")
mar <- read_csv("tweet_activity_mar.csv")


# no need to compare time, just rbind
rbind(mar, feb, jan) %>% View()

df2 <- rbind(mar, feb, jan)

# select most relevant variables
df3 <- df2 %>%
    select(time, `Tweet text`,impressions, `media engagements`, likes) %>%
    mutate(
        mepi = `media engagements`/impressions,
        likes_ratio = likes/`media engagements`
    ) %>%
    arrange(desc(`media engagements`)) %>%
    # filter out all tweets with 0 media engagement 
    filter(`media engagements` > 0) %>% 
    # filter out tweets that begin with a number (1/ or 2/ or 3/) - these are threads
    filter(!grepl("^[0-9]", `Tweet text`)) %>%
    # filter out tweets with 'Day' - none of my atomic essays have the word Day in it
    filter(!grepl("Day", `Tweet text`)) %>%
    # filter out tweets with "@" 
    filter(!grepl("@", `Tweet text`)) %>%
    filter(!grepl("Step", `Tweet text`)) %>% 
    filter(!grepl("#TidyTuesday", `Tweet text`)) %>%
    filter(!grepl("#Rstats", `Tweet text`)) %>% 
    filter(!grepl("Link", `Tweet text`)) %>%
    arrange(time)


# Visualize Impressions
df3 %>%
    ggplot(aes(x = time, y = impressions)) +
    geom_point() +
    theme(
        axis.text.x = element_text(angle = 90)
    )

# Visualize Media Engagements
df3 %>%
    ggplot(aes(x = time, y = `media engagements`)) +
    geom_point() +
    theme(
        axis.text.x = element_text(angle = 90)
    ) 

# Visualize MEPI
df3 %>%
    ggplot(aes(x = time, y = mepi)) +
    geom_point(color = ifelse(df3$mepi > 0.05, 'red', 'black')) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )
    
# Visualize Likes Ratio 
library(lubridate)

df3 %>%
    mutate(
        time = as.Date(time)
    ) %>%
    ggplot(aes(x = time, y = likes_ratio)) +
    geom_point(color = ifelse(df3$likes_ratio > 1, 'red', 'black')) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )
    
library(tidyverse)
library(lubridate)

# NOTE: The purpose is to analyze Copy writing (lead-in Tweets) & Atomic Essay (writing)
# Copy writing effectiveness = Media Engagement per Impression (How many people click your essay because of your tweet)
# Essay writing effectiveness = Likes per Media Engagement (After they read your essay, how many clicked like?)


# Data: Manual Data Collection Day 1 - Day 60
df <- readxl::read_xlsx("./data/data_driven_writing.xlsx")

# Plot Data that was Manually Collected
# Compare visual plot to Excel's Point and Click

df %>%
    ggplot(aes(x = Day, y = `mepi - copy writing`)) +
    geom_point(color = ifelse(df$`mepi - copy writing` > 0.059, '#E08963', '#5E96AE'),
               size = ifelse(df$`mepi - copy writing` > 0.059, 5, 4)) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#B2EBE0"),
        plot.background = element_rect(fill = "#B2EBE0"),
        plot.title = element_text(colour = "#E08963", size = 20, face = "bold"),
        plot.subtitle = element_text(colour = "#5E96AE", size = 12, face = "italic", margin = margin(0,0,30,0))
    ) +
    scale_x_continuous(breaks = seq(0, 60, by = 10)) +
    labs(
        title = "Copy Writing Effectiveness",
        subtitle = "Media Engagement per Impression: Atomic Essays 1 - 60",
        y = "MEPI",
        x = "Atomic Essays",
        caption = "Data & Graphic: @paulapivat"
    )




# Data: January to March
jan <- read_csv("./data/tweet_activity_jan.csv")
feb <- read_csv("./data/tweet_activity_feb.csv")
mar <- read_csv("./data/tweet_activity_mar.csv")


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
    
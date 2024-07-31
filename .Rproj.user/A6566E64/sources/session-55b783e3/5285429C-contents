
# In this analysis, I want to answer these questions:
# - What's the total of entries of the emojis per range of age? []
# - What's the total of entries of the emojis per platafform [x]
# - What's the total of entries of each emoji? [x]
# - What's the feeling most used in young male guys? []


library('tidyverse')
library('janitor')

df <- read_csv('emoji_usage_dataset.csv')

# Exploring the data
glimpse(df)

# Cleaning the data
df <- clean_names(df)
df <- remove_empty(df, which='cols', cutoff=1, quiet = FALSE)

# Total entries of emojis per plataform
df %>% group_by(emoji, platform) %>% 
  summarise(total_entries=n()) %>% 
  arrange(desc(total_entries)) %>% 
  View()

# Total entries of each emoji
df %>% group_by(emoji) %>% 
  summarise(total_entries=n()) %>% 
  arrange(desc(total_entries)) %>% 
  View()


min(df$user_age)
max(df$user_age)
# My age is from 13 to 64
df %>% mutate(category= case_when(
  user_age>12 & user_age<30 ~ 'young',
  user_age>29 & user_age<40 ~ 'middle-aged',
  user_age>40 ~ 'adult',
  .default='other' 
))








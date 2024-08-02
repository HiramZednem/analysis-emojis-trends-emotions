
# In this analysis, I want to answer these questions:
# - What's the total of entries of the emojis per range of age? [x]
# - What's the total of entries of the emojis per platafform [x]
# - What's the total of entries of each emoji? [x]
# - What's the feelling most used? [x]
# - What's the feeling most used in young male guys? [x]


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
  arrange(desc(total_entries)) -> emoji_per_plataform

# Total entries of each emoji
df %>% group_by(emoji) %>% 
  summarise(total_entries=n()) %>% 
  arrange(desc(total_entries)) -> emoji_total


min(df$user_age)
max(df$user_age)
# My age is from 13 to 64
df %>% mutate(category= case_when(
  user_age>12 & user_age<30 ~ 'young',
  user_age>29 & user_age<40 ~ 'middle-aged',
  user_age>39 ~ 'adult',
  .default='other' 
)) -> df_processed 


df_processed %>% group_by(emoji, category) %>% 
  summarize(total_entries=n()) -> emoji_per_category

df_processed %>% group_by(context) %>% 
  summarise(total_entries=n()) %>% 
  arrange(desc(total_entries)) -> feelings_total

df_processed %>% filter(category=='young' & user_gender=='Male') %>%
  group_by(context) %>% 
  summarise(total_entries=n()) %>% 
  arrange(desc(total_entries)) -> feelings_young_male

# once that we have the data that answer our questions let's plot
# I'll be do this process with tableau, so lets export the data

write.csv(emoji_total, file="./emoji_total.csv")
write.csv(emoji_per_category, file="./emoji_per_category.csv")
write.csv(emoji_per_plataform, file="./emoji_per_plataform.csv")
write.csv(feelings_young_male, file="./feelings_young_male.csv")
write.csv(feelings_total, file='./feelings_total.csv')

colnames(emoji_total)

# because I want to practice more, i'll do it with ggplot2 too



ggplot(emoji_total, aes(x = emoji , y = total_entries, fill=emoji, label='h')) +
  geom_bar(stat = "identity") + 
  geom_text(family = "EmojiOne", size = 6) +
  labs(title='Emoji Usage per Total Entrie')

# 'showtext' para usar fuentes externas
install.packages("showtext")
library(showtext)
?font_add
font_add("NotoEmojiColor","./NotoEmoji-Light.ttf")
showtext_auto()

# Crear la gráfica usando la fuente 'Noto Emoji' solo en el eje x
ggplot(emoji_total, aes(x = emoji, y = total_entries)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Uso de Emojis", x = "Emoji", y = "Entradas Totales") +
  theme_minimal() +
  theme(axis.text.x = element_text(family = "NotoEmojiColor", size = 12))


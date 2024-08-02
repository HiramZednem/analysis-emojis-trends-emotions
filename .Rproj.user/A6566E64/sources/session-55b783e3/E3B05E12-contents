library(tidyverse)
library(palmerpenguins)
data(penguins)


head(penguins)
colnames(penguins)

# Supongamos que quieres modelar la relación entre masa corporal y longitud de aletas en las tres especies de pingüino.
ggplot(data = penguins) + geom_point(mapping = aes(y = body_mass_g, x = flipper_length_mm, color=species, shape=sex))
# aes points = x,y,Color, Shape, Size, Alpha

help("read_csv")

#data
# figura geometrica, like pointst, bar, lines and more...
# Chose the look -> aesthetic 
# The mapping indicates what aesthetic we want to have

# CREAM MAS TIPOS DE VIUSALIZACIONES
ggplot(data=penguins) +
  geom_smooth(mapping=aes(x=flipper_length_mm, y=body_mass_g)) +
  geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g))

ggplot(data=penguins) +
  geom_smooth(mapping=aes(x=flipper_length_mm, y=body_mass_g, linetype = species)) 

ggplot(data=penguins) +
  geom_smooth(mapping=aes(x=flipper_length_mm, y=body_mass_g)) + # crea la linea de tendencia
  geom_jitter(mapping=aes(x=flipper_length_mm, y=body_mass_g)) # jitter añade ruido

# bar char
ggplot(diamonds) +
  # color pone de color el contorno, fill pone el color completo
  geom_bar(mapping=aes(x=cut, fill=cut))# de manera predeterminada centa el tipo ded atos que hay

# facet_wrap and facate_grid
# bar char
head(diamonds)
ggplot(diamonds) +
  geom_bar(mapping=aes(x=color, fill=cut)) +
  facet_wrap(~cut)

ggplot(data=penguins) +
  geom_point(mapping = aes(x=flipper_length_mm, y=body_mass_g, color=species)) +
  facet_grid(~sex)

ggplot(data=penguins) +
  geom_point(mapping = aes(x=flipper_length_mm, y=body_mass_g, color=species)) +
  facet_wrap(~sex)


# adding labels
ggplot(data=penguins) +
  geom_point(mapping = aes(x=flipper_length_mm, y=body_mass_g, color=species)) +
  labs(title='Palmer Penguins: Body Mass vs. Flipper Length', subtitle = 'Sample of 3 species of penguins',
       caption='Data collected by Dr. Kristen Gorman') +
  annotate('text', x=220, y=3500, label='The gentoos are the largest',
           fontface='bold', size=4.5, color='purple')

# save plots
# 1. In the Plot, click export
# 2. using ggsave -> autmatically save the last plot
help(gg_save)
ggsave('PenguinChar.png')








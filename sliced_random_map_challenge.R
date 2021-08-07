library(tidyverse)
library(here)
library(sf)
library(tigris)
library(leaflet)



df <- read_csv("https://raw.githubusercontent.com/magi-1/presidential-elections/main/Data/TAMIDS%20Given/county_level.csv")


ct <- tigris::counties(cb = TRUE)


df_sp <- ct %>%
  right_join(df, by = c("GEOID"= "county_fips")) %>% 
  filter(STATEFP != 15) %>%
  mutate(p_diff = votes_gop/total_votes) 

df_sp %>%
  ggplot(aes(fill = p_diff)) +
  geom_sf(lwd = 0.4) +
  borders("state") +  
  scale_fill_distiller(palette = "RdBu") + 
  coord_sf(crs = 5070) +
  theme(plot.background = element_rect(fill = "black", color = "black")) +
  theme_minimal()

o

ggsave("gop_plot.png", device = "png", dpi = 300, bg = "white")
 








































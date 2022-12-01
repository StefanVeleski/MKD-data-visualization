library(tidyverse)
library(readr) 
#### Merge all the datasets together ####

setwd("C:/Users/Stefan/OneDrive/Github/MKD census/Unemployment data/Tidy")
main_dataset <- list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))

setwd("C:/Users/Stefan/OneDrive/Github/MKD census")

#### Animated plot ####
library(gganimate)
library(gifski)
library(wesanderson)
library(tidyr)
library(ggrepel)

main_dataset$Ethnicity <- as.factor(main_dataset$Ethnicity)

main_dataset$Ethnicity <- fct_collapse(main_dataset$Ethnicity,
  Other = c("Serbian","Vlachs", "Bosniak", "Other"))

main_dataset1 <- main_dataset %>% 
  mutate(Year = as.numeric(Year)) %>% 
  drop_na() %>% 
  group_by(Year, Ethnicity) %>% 
  summarise(Total = sum(Men, Women))
  
  
plot2 <- main_dataset1 %>% 
  ggplot(aes(x = Year, y = Total, color = Ethnicity)) +
  geom_line(size = 2, alpha = 0.75) +
  labs(title = "Total number of unemployed by ethnic background in Macedonia",
       subtitle = "2003-2021",
       caption = "Source: Employment Service Agency of The Republic of North Macedonia (https://av.gov.mk/pregled-na-evidentirani-nevraboteni-lica-spored-nacionalna-pripadnost.nspx)",
       x = "Year",
       y = "Number of Unemployed") +
  geom_label_repel(aes(label = Ethnicity), nudge_x = 0.35, size = 4) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5)) +
  labs(x = "Year", y = "Number of Unemployed") +
  theme_minimal() +
  theme(legend.position = "none")+
  geom_point() +
  scale_x_continuous(breaks=0:2022) +
  scale_y_continuous(breaks = seq(0,250000,by=50000))
  

plot2animation = plot2 +
  transition_reveal(along = Year, range = c(2003,2021))+
  view_follow(fixed_y = TRUE)

animate(plot2animation, fps = 30, height = 1000, width = 1600, duration = 10, res = 100, end_pause = 90)
anim_save("Plots/unemployment_mkd.gif")
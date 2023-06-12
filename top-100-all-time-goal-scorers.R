# ------------------------
# Packages 
# ------------------------

library(tidyverse)

# ------------------------
# Colour Themes 
# ------------------------

colour_theme <- 
  c("A" = "#F9F7DC", 
    "B" = "#FFCB65",
    "D" = "#EF8535",
    "D" = "#CD6301",
    "C" = "#C43C1E",
    "E" = "#763C00"
  )

afl.colours <- 
  c(
    "modern greats" = "#fc7108",
    "all time greats" = "#cc3918",
    "the GOAT" = "#ed3f18",
    "historic greats" = "#2d8dc4"
  )





# ------------------------
# top goal kickers of all time
# ------------------------

# plugger
# 9423 
afl.all <- get_fryzigg_stats(start = 1897, 
                             end = 2023)
top_goal_kickers <- 
  afl.all %>% 
  select(player_first_name, player_last_name, player_id, goals) %>%  
  group_by(player_first_name, player_last_name, player_id) %>% 
  summarise(total_goals = sum(goals)) %>% 
  arrange(desc(total_goals)) %>% # arrange in order of value
  head(n = 100) %>% 
  as.data.frame() # could also limit to top 'n' values if many categories 

list <- select(top_goal_kickers, player_id) 

# rm(afl.top100)

# https://stackoverflow.com/questions/12925063/numbering-rows-within-groups-in-a-data-frame

afl.top100 <- 
  afl.all %>% 
  select(player_first_name, player_last_name, player_id, match_id, goals) %>% 
  inner_join(list, by = c("player_id" = "player_id")) %>% 
  arrange(player_id) %>% 
  as.data.frame() 

afl.top100$player_id <- as.factor(afl.top100$player_id)

# str(afl.top100)

afl.top100 <-
  afl.top100 %>%
  group_by(player_id) %>%
  mutate(match = 1:n())

afl.top100$total_goals <- ave(afl.top100$goals, afl.top100$player_id, FUN=cumsum)

afl.top100$player_id <- as.character(afl.top100$player_id)


# Peter Hudson -- 727 goals in 152 games! 

afl.top100 %>% 
  # filter(match <= 200) %>% 
  # filter(Player != "Charlie Curnow") %>% 
  ggplot() + 
  geom_line(aes(y = total_goals, 
                x = match, 
                group = player_id), 
            size = 0.6) +
  
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400)) +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) + # expand axis border with mult
  # scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350)) +
  
  theme_plot +
  theme(axis.line.x = element_line(color="#4E4F4E", size = 0.2),
        axis.line.y = element_line(color="#4E4F4E", size = 0.2),
        legend.key = element_rect(fill = "white"), # colour of legend line background 
        
        # Plot margins / border / fill-colour 
        # plot.background = element_rect(fill = "#e8b14c"),
        # legend.background = element_rect(fill = "#e8b14c"),
        
        panel.border = element_blank(),
        legend.position = "none"
        
  )





lf_text <- "Lance Franklin | 1051" 
tl_text <- "Tony Lockett | 1360" 
bq_text <- "Bernie Quinlan | 817" 
ph_text <- "Peter Hudson | 727" 
jc_text <- "John Coleman | 537" 
jd_text <- "Jason Dunstall | 1254" 
jr_text <- "Jack Riewoldt | 764" 
th_text <- "Tom Hawkins | 741" 
pm_text <- "Peter Mckenna | 874"
tm_text <- "Tony Modra | 588"



labs(y = "Mean length of odontobast cells (mm)",
     title = paste0("Top 100 VFL/AFL goal kickers of all time, focused on 
                    <span style='color:", afl.colours["modern greats"], "'>modern greats</span>,
                    <span style='color:", afl.colours["all time greats"], "'>all time greats</span>,
                      compared to equivalent doses of <span style='color:",
                    vit_c_palette["Vitamin C"], "'>Vitamin C</span>"),
     subtitle = "With the highest dose, the mean recorded length was almost identical.") +

  # Our colour palette (see slides for how we landed here!)
  vit_c_palette <- c("Vitamin C" = "#E93603", 
                     "Orange Juice" = "#fab909",
                     light_text = "#323A30",
                     dark_text =  "#0C1509")



# Standard .csv export 
write.csv(afl.top100, file = "afl_top_100.csv",
          na = "", 
          row.names = FALSE)

#F9F7DC
#fcdc9a

afl.top100 %>% 
  # filter(match <= 200) %>% 
  # filter(Player != "Charlie Curnow") %>% 
  ggplot(aes(y = total_goals, 
             x = match, 
             group = player_id)) + 
  geom_line(size = 0.2, colour = "#fcfbeb") + 
  # all time greats
  geom_line(data = afl.top100[afl.top100$player_id==9423,], colour = "#ed3f18", size = 1.0) + # tony lockett 
  geom_line(data = afl.top100[afl.top100$player_id==9595,], colour = "#a63117", size = 0.6) + # jason dunstall 
  geom_line(data = afl.top100[afl.top100$player_id==7905,], colour = "#2d8dc4", size = 0.6) + # peter hudson
  geom_line(data = afl.top100[afl.top100$player_id==8185,], colour = "#a63117", size = 0.6) + # bernie quinlan - kicked most of his goals after game 250! 
  geom_line(data = afl.top100[afl.top100$player_id==5860,], colour = "#2d8dc4", size = 0.6) + # john coleman 
  geom_line(data = afl.top100[afl.top100$player_id==7653,], colour = "#a63117", size = 0.6) + # peter mckenna 
  geom_line(data = afl.top100[afl.top100$player_id==10312,], colour = "#a63117", size = 0.6) + # tony modra 
  
  # modern greats
  geom_line(data = afl.top100[afl.top100$player_id==11385,], colour = "#fc7108", size = 0.6) + # lance franklin 
  geom_line(data = afl.top100[afl.top100$player_id==11582,], colour = "#fc7108", size = 0.6) + # jack riewoldt
  geom_line(data = afl.top100[afl.top100$player_id==11554,], colour = "#fc7108", size = 0.6) + # tom hawkins
  
  
  
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400)) +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) + # expand axis border with mult
  # scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350)) +
  
  labs(x = "matches played",
       y = "goals",
       title = "Top 100 VFL/AFL goal scorers of all time",
       subtitle = "Daily count of song plays between 2016 & 2022",
       caption = "Fitzroy") +
  
  
  # Lance Franklin 1051 
  geom_text(geom = "label", 
            x = 375, y = 1051,
            label = lf_text,
            hjust = "left", 
            color = "#fc7108") +
  geom_segment(geom = "segment",
               x = 350, y = 1051, 
               xend = 370, yend = 1051, 
               color = "#fc7108", linetype = 2) + 
  # Jack Riewoldt 764
  geom_text(geom = "label", 
            x = 362, y = 766,
            label = jr_text,
            hjust = "left", 
            color = "#fc7108") +
  geom_segment(geom = "segment",
               x = 337, y = 766, 
               xend = 357, yend = 766, 
               color = "#fc7108", linetype = 2) + 
  # Tom Hawkins 741
  geom_text(geom = "label", 
            x = 362, y = 739,
            label = th_text,
            hjust = "left", 
            color = "#fc7108") +
  geom_segment(geom = "segment",
               x = 337, y = 739, 
               xend = 357, yend = 739, 
               color = "#fc7108", linetype = 2) + 
  # Tony Lockett 1360
  geom_text(geom = "label", 
            x = 310, y = 1360,
            label = tl_text,
            hjust = "left", 
            color = "#ed3f18") +
  geom_segment(geom = "segment",
               x = 285, y = 1360, 
               xend = 305, yend = 1360, 
               color = "#ed3f18", linetype = 2) + 
  # Jason Dunstall 1254
  geom_text(geom = "label", 
            x = 300, y = 1254,
            label = jd_text,
            hjust = "left", 
            color = "#a63117") +
  geom_segment(geom = "segment",
               x = 275, y = 1254, 
               xend = 295, yend = 1254, 
               color = "#a63117", linetype = 2) + 
  # Bernie Quinlan 817
  geom_text(geom = "label", 
            x = 395, y = 817,
            label = bq_text,
            hjust = "left", 
            color = "#a63117") +
  geom_segment(geom = "segment",
               x = 390, y = 817, 
               xend = 370, yend = 817, 
               color = "#a63117", linetype = 2) + 
  # Peter Hudson 727
  geom_text(geom = "label", 
            x = 60, y = 727,
            label = ph_text,
            hjust = "left", 
            color = "#2d8dc4") +
  geom_segment(geom = "segment",
               x = 120, y = 727, 
               xend = 100, yend = 727, 
               color = "#2d8dc4", linetype = 2) + 
  # John Coleman 537
  geom_text(geom = "label", 
            x = 33, y = 537,
            label = jc_text,
            hjust = "left", 
            color = "#2d8dc4") +
  geom_segment(geom = "segment",
               x = 93, y = 537, 
               xend = 73, yend = 537, 
               color = "#2d8dc4", linetype = 2) +  
  # Peter Mckenna 874
  geom_text(geom = "label", 
            x = 125, y = 874,
            label = pm_text,
            hjust = "left", 
            color = "#a63117") +
  geom_segment(geom = "segment",
               x = 185, y = 874, 
               xend = 165, yend = 874, 
               color = "#a63117", linetype = 2) +  
  # Tony Modra 588
  geom_text(geom = "label", 
            x = 50, y = 588,
            label = tm_text,
            hjust = "left", 
            color = "#a63117") +
  geom_segment(geom = "segment",
               x = 160, y = 588, 
               xend = 90, yend = 588, 
               color = "#a63117", linetype = 2) +  
  
  
  theme_plot +
  theme(axis.line.x = element_line(color="#1c1b1a", size = 0.2),
        axis.line.y = element_line(color="#1c1b1a", size = 0.2),
        
        axis.title.y = element_text(color="#1c1b1a"),
        axis.text.y = element_text(color = "#1c1b1a"),
        axis.text.x = element_text(color = "#1c1b1a"),
        plot.title = element_text(colour = "#1c1b1a",
                                  hjust = 0,
                                  size = 14,
                                  face = "bold"),
        plot.subtitle = element_text(colour = "#1c1b1a",
                                     hjust = 0,
                                     size = 12),
        
        legend.key = element_rect(fill = "white"), # colour of legend line background 
        
        # Plot margins / border / fill-colour 

        panel.background = element_rect(fill = "#fcdc9a",
                                        colour = "#fcdc9a"),
        plot.background = element_rect(fill = "#fcdc9a",
                                       colour = "#fcdc9a"),
        panel.grid.major=element_line(colour="#fcdc9a"),
        panel.grid.minor=element_line(colour="#fcdc9a"),
        
        # Plot margins / border / fill-colour 
        # plot.background = element_rect(fill = "#e8b14c"),
        # legend.background = element_rect(fill = "#e8b14c"),
        
        panel.border = element_blank(),
        legend.position = "none"
        
  )

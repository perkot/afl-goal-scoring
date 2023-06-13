# ------------------------
# Packages 
# ------------------------

library(tidyverse)
library(fitzRoy)
library(ggtext)

# ------------------------
# Colour Themes 
# ------------------------

afl.colours <- 
  c(
    "peers" = "#15ceeb",
    "all time greats" = "#f7c779",
    "historic greats" = "#ff8d36"
    )

# ------------------------
# Data Prep 
# ------------------------

# Retrieve all player data via fitzroy  
afl.all <- fetch_player_stats(c(1897:2023), source = "fryzigg")

# Extract out top 100 goal kickers of all time 
top_goal_kickers <- 
  afl.all %>% 
  select(player_first_name, player_last_name, player_id, goals) %>%  
  group_by(player_first_name, player_last_name, player_id) %>% 
  summarise(total_goals = sum(goals)) %>% 
  arrange(desc(total_goals)) %>% # arrange in order of value
  head(n = 100) %>% 
  as.data.frame() # could also limit to top 'n' values if many categories 

# Extract list of players 
list <- select(top_goal_kickers, player_id) 

# Using top 100 list, retrieve game x game data 
afl.top100 <- 
  afl.all %>% 
  select(player_first_name, player_last_name, player_id, match_id, goals) %>% 
  inner_join(list, by = c("player_id" = "player_id")) %>% 
  arrange(player_id) %>% 
  as.data.frame() 

# as factor 
afl.top100$player_id <- as.factor(afl.top100$player_id)

# determine match played, per player 
afl.top100 <-
  afl.top100 %>%
  group_by(player_id) %>%
  mutate(match = 1:n())

# Extract out matches played 
total_matches <- 
afl.top100 %>% 
  group_by(player_id) %>% 
  summarise(Value = max(match))

# factor 
total_matches$player_id <- as.factor(total_matches$player_id)
top_goal_kickers$player_id <- as.factor(top_goal_kickers$player_id)

# return for each player 
top_goal_kickers <- left_join(top_goal_kickers, total_matches, by = "player_id")

# get cumulative sum of goals kicked 
afl.top100$total_goals <- ave(afl.top100$goals, afl.top100$player_id, FUN=cumsum)

afl.top100$player_id <- as.character(afl.top100$player_id)


# text for visualisation 
lf_text <- paste("(4) Lance Franklin* |", top_goal_kickers$total_goals[top_goal_kickers$player_id == 11385])
tl_text <- paste("(1) Tony Lockett |", top_goal_kickers$total_goals[top_goal_kickers$player_id == 9423])
gc_text <- paste("(2) Gordon Coventry |", top_goal_kickers$total_goals[top_goal_kickers$player_id == 2567])
bq_text <- paste("(11) Bernie Quinlan |", top_goal_kickers$total_goals[top_goal_kickers$player_id == 8185])
ph_text <- paste("(20) Peter Hudson |", top_goal_kickers$total_goals[top_goal_kickers$player_id == 7905])
jc_text <- paste("(55) John Coleman |", top_goal_kickers$total_goals[top_goal_kickers$player_id == 5860])
jd_text <- paste("(3) Jason Dunstall |", top_goal_kickers$total_goals[top_goal_kickers$player_id == 9595])
jr_text <- paste("(13) Jack Riewoldt* |", top_goal_kickers$total_goals[top_goal_kickers$player_id == 11582])
th_text <- paste("(15) Tom Hawkins* |", top_goal_kickers$total_goals[top_goal_kickers$player_id == 11554])
pm_text <- paste("(10) Peter Mckenna |", top_goal_kickers$total_goals[top_goal_kickers$player_id == 7653])
# tm_text <- paste("Tony Modra |", top_goal_kickers$total_goals[top_goal_kickers$player_id == 10312])


# ------------------------
# Plot theme 
# ------------------------

# Theme 
theme_plot <-
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5), 
        
        axis.line.x = element_line(color="#d6d6d6", size = 0.2),
        axis.line.y = element_line(color="#d6d6d6", size = 0.2),
        
        axis.title.y = element_text(color="#d6d6d6", size = 14),
        axis.title.x = element_text(color="#d6d6d6", size = 14),
        
        axis.ticks.x =  element_line(color="#d6d6d6"),
        axis.ticks.y =  element_line(color="#d6d6d6"),
        
        axis.text.y = element_text(color = "#d6d6d6", size = 12),
        axis.text.x = element_text(color = "#d6d6d6", size = 12),
        plot.caption = element_text(colour = "#d6d6d6"),
        legend.key = element_rect(fill = "#ba001c"), # colour of legend line background 
        # Plot margins / border / fill-colour 
        panel.background = element_rect(fill = "#ba001c",
                                        colour = "#ba001c"),
        plot.background = element_rect(fill = "#ba001c",
                                       colour = "#ba001c"),
        panel.grid.major=element_line(colour="#ba001c"),
        panel.grid.minor=element_line(colour="#ba001c"),
        
        panel.border = element_blank(),
        legend.position = "none"
  )


# ------------------------
# Visualisation 
# ------------------------

p <- 
  afl.top100 %>% 
  ggplot(aes(y = total_goals, 
             x = match, 
             group = player_id)) + 
  geom_line(size = 0.2, colour = "#bf8080") + 
  
  # buddy franklin 
  geom_line(data = afl.top100[afl.top100$player_id==11385,], colour = "#f2f2f2", size = 1.0) + # lance franklin 
  # all time greats
  geom_line(data = afl.top100[afl.top100$player_id==9423,], colour = "#f7c779", size = 0.6) + # tony lockett 
  geom_line(data = afl.top100[afl.top100$player_id==9595,], colour = "#f7c779", size = 0.6) + # jason dunstall 
  geom_line(data = afl.top100[afl.top100$player_id==2567,], colour = "#f7c779", size = 0.6) + # gordon coventry
  # peers
  geom_line(data = afl.top100[afl.top100$player_id==11582,], colour = "#15ceeb", size = 0.6) + # jack riewoldt
  geom_line(data = afl.top100[afl.top100$player_id==11554,], colour = "#15ceeb", size = 0.6) + # tom hawkins
  # historic greats 
  geom_line(data = afl.top100[afl.top100$player_id==7905,], colour = "#ff9054", size = 0.6) + # peter hudson
  geom_line(data = afl.top100[afl.top100$player_id==8185,], colour = "#ff9054", size = 0.6) + # bernie quinlan - kicked most of his goals after game 250! 
  geom_line(data = afl.top100[afl.top100$player_id==5860,], colour = "#ff9054", size = 0.6) + # john coleman 
  geom_line(data = afl.top100[afl.top100$player_id==7653,], colour = "#ff9054", size = 0.6) + # peter mckenna 
  
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400)) +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) + # expand axis border 
  
  labs(x = "games played",
       y = "goals",
       title = "Lance Franklin's ascent to 4th on the all time goal kicking list",
       subtitle = paste0("Depicts the top 100 goal kickers with emphasis on <span style='color:", afl.colours["all time greats"], "'>the top 3 all-time goal kickers</span>,
               <span style='color:", afl.colours["historic greats"], "'>historic greats</span> & Franklin's closest
               <span style='color:", afl.colours["peers"], "'>peers</span>"),
       caption = "*Current player") +
  
  # Lance Franklin 1051 
  geom_text(geom = "label", 
            x = 385, y = 1060,
            label = lf_text,
            hjust = "left", 
            color = "#f2f2f2",
            size = 5) +
  geom_segment(geom = "segment",
               x = 355, y = 1060, 
               xend = 380, yend = 1060, 
               color = "#f2f2f2", linetype = 2, size = 0.4) + 
  # Jack Riewoldt 764
  geom_text(geom = "label", 
            x = 385, y = 776,
            label = jr_text,
            hjust = "left", 
            color = "#15ceeb",
            size = 4) +
  geom_segment(geom = "segment",
               x = 347, y = 776, 
               xend = 380, yend = 776, 
               color = "#7e9ea3", linetype = 2, size = 0.1) + 
  # Tom Hawkins 741
  geom_text(geom = "label", 
            x = 385, y = 749,
            label = th_text,
            hjust = "left", 
            color = "#15ceeb",
            size = 4) +
  geom_segment(geom = "segment",
               x = 347, y = 749, 
               xend = 380, yend = 749, 
               color = "#7e9ea3", linetype = 2, size = 0.1) + 
  # Tony Lockett 1360
  geom_text(geom = "label", 
            x = 385, y = 1360,
            label = tl_text,
            hjust = "left", 
            color = "#f7c779",
            size = 4) +
  geom_segment(geom = "segment",
               x = 285, y = 1360, 
               xend = 380, yend = 1360, 
               color = "#c9c0b1", linetype = 2, size = 0.1) + 
  # Gordon Coventry 1299
  geom_text(geom = "label", 
            x = 385, y = 1299,
            label = gc_text,
            hjust = "left", 
            color = "#f7c779",
            size = 4) +
  geom_segment(geom = "segment",
               x = 310, y = 1299, 
               xend = 380, yend = 1299, 
               color = "#c9c0b1", linetype = 2, size = 0.1) + 
  # Jason Dunstall 1254
  geom_text(geom = "label", 
            x = 385, y = 1254,
            label = jd_text,
            hjust = "left", 
            color = "#f7c779",
            size = 4) +
  geom_segment(geom = "segment",
               x = 275, y = 1254, 
               xend = 380, yend = 1254, 
               color = "#c9c0b1", linetype = 2, size = 0.1) + 
  # Bernie Quinlan 817
  geom_text(geom = "label", 
            x = 385, y = 817,
            label = bq_text,
            hjust = "left", 
            color = "#ff9054",
            size = 4) +
  geom_segment(geom = "segment",
               x = 370, y = 817, 
               xend = 380, yend = 817, 
               color = "#c9c0b1", linetype = 2, size = 0.1) + 
  # Peter Hudson 727
  geom_text(geom = "label", 
            x = 385, y = 720,
            label = ph_text,
            hjust = "left", 
            color = "#ff9054",
            size = 4) +
  geom_segment(geom = "segment",
               x = 135, y = 720, 
               xend = 380, yend = 720, 
               color = "#c9c0b1", linetype = 2, size = 0.1) + 
  # John Coleman 537
  geom_text(geom = "label", 
            x = 385, y = 537,
            label = jc_text,
            hjust = "left", 
            color = "#ff9054",
            size = 4) +
  geom_segment(geom = "segment",
               x = 103, y = 537, 
               xend = 380, yend = 537, 
               color = "#c9c0b1", linetype = 2, size = 0.1) +  
  # Peter Mckenna 874
  geom_text(geom = "label", 
            x = 385, y = 874,
            label = pm_text,
            hjust = "left", 
            color = "#ff9054",
            size = 4) +
  geom_segment(geom = "segment",
               x = 195, y = 874, 
               xend = 380, yend = 874, 
               color = "#c9c0b1", linetype = 2, size = 0.1) +  
  
  theme_plot +
  theme(plot.title = element_markdown(lineheight = 1.1, 
                                      hjust = 0, 
                                      size = 16, 
                                      face = "bold", 
                                      colour = "#d6d6d6"),
        plot.subtitle = element_markdown(colour = "#d6d6d6",
                                         hjust = 0,
                                         size = 14)
  )

# preview
p
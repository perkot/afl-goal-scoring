# ideas 
  # bucket # of goals 
  # --> increments of 20 matches?
  # or 1-20, 20-50,50-100,100-200,200+

  # look at average goals per game over different periods of career?

# ------------------------
# DEPENDENCIES 
# ------------------------

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(fitzRoy)
library(shiny)
library(shinyWidgets)
library(ggthemes)
library(tableHTML)

# ------------------------
# Colour Theme 
# ------------------------

colour_theme <- 
  c("A" = "#2b6389", 
    "B" = "#e8b14c",
    "C" = "#e5963c",
    "D" = "#e2754a",
    "E" = "#b47462"
  )

# ------------------------
# Data
# ------------------------

# Generic Fitzroy command to get all AFL data 

afl.1 <- get_fryzigg_stats(start = 2005, 
                           end = 2023)

# ------------------------
# Players cumulative sum 
# ------------------------

jr <- afl.1 %>% 
  filter(player_first_name == "Jack" & player_last_name == "Riewoldt") %>% 
  mutate(total_goals = cumsum(goals)) %>% 
  select(total_goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(total_goals, .after = match) %>% 
  rename("Jack Riewoldt" = "total_goals") 

th <- afl.1 %>% 
  filter(player_first_name == "Tom" & player_last_name == "Hawkins") %>% 
  mutate(total_goals = cumsum(goals)) %>% 
  select(total_goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(total_goals, .after = match) %>% 
  rename("Tom Hawkins" = "total_goals") 

lf <- afl.1 %>% 
  # filter(player_id == 11385)
  filter(player_first_name == "Lance" & player_last_name == "Franklin") %>% 
  mutate(total_goals = cumsum(goals)) %>% 
  select(total_goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(total_goals, .after = match) %>% 
  rename("Lance Franklin" = "total_goals") 

tw <- afl.1 %>% 
  filter(player_first_name == "Taylor" & player_last_name == "Walker") %>% 
  mutate(total_goals = cumsum(goals)) %>% 
  select(total_goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(total_goals, .after = match) %>% 
  rename("Taylor Walker" = "total_goals") 

jc <- afl.1 %>% 
  filter(player_first_name == "Jeremy" & player_last_name == "Cameron") %>% 
  mutate(total_goals = cumsum(goals)) %>% 
  select(total_goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(total_goals, .after = match) %>% 
  rename("Jeremy Cameron" = "total_goals") 

cc <- afl.1 %>% 
  filter(player_first_name == "Charlie" & player_last_name == "Curnow") %>% 
  mutate(total_goals = cumsum(goals)) %>% 
  select(total_goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(total_goals, .after = match) %>% 
  rename("Charlie Curnow" = "total_goals") 

tl <- afl.1 %>% 
  filter(player_first_name == "Tom" & player_last_name == "Lynch" & player_id == 11953) %>% 
  mutate(total_goals = cumsum(goals)) %>% 
  select(total_goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(total_goals, .after = match) %>% 
  rename("Tom Lynch" = "total_goals") 

jk <- afl.1 %>% 
  filter(player_first_name == "Josh J." & player_last_name == "Kennedy" & player_id == 11492) %>% 
  mutate(total_goals = cumsum(goals)) %>% 
  select(total_goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(total_goals, .after = match) %>% 
  rename("Josh J. Kennedy" = "total_goals") 

compare2 <- join_all(list(jr,th,lf,tw,jc,cc,tl,jk), by='match', type='full') %>% 
  pivot_longer(-match,
               names_to = "Player",
               values_to = "Goals")


# ------------------------
# Players total sum 
# ------------------------

jr <- afl.1 %>% 
  filter(player_first_name == "Jack" & player_last_name == "Riewoldt") %>% 
  select(goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(goals, .after = match) %>% 
  rename("Jack Riewoldt" = "goals") 

th <- afl.1 %>% 
  filter(player_first_name == "Tom" & player_last_name == "Hawkins") %>% 
  select(goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(goals, .after = match) %>% 
  rename("Tom Hawkins" = "goals") 

lf <- afl.1 %>% 
  # filter(player_id == 11385)
  filter(player_first_name == "Lance" & player_last_name == "Franklin") %>% 
  select(goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(goals, .after = match) %>% 
  rename("Lance Franklin" = "goals") 

tw <- afl.1 %>% 
  filter(player_first_name == "Taylor" & player_last_name == "Walker") %>% 
  select(goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(goals, .after = match) %>% 
  rename("Taylor Walker" = "goals") 

jc <- afl.1 %>% 
  filter(player_first_name == "Jeremy" & player_last_name == "Cameron") %>% 
  select(goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(goals, .after = match) %>% 
  rename("Jeremy Cameron" = "goals") 

cc <- afl.1 %>% 
  filter(player_first_name == "Charlie" & player_last_name == "Curnow") %>% 
  select(goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(goals, .after = match) %>% 
  rename("Charlie Curnow" = "goals") 

tl <- afl.1 %>% 
  filter(player_first_name == "Tom" & player_last_name == "Lynch" & player_id == 11953) %>% 
  select(goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(goals, .after = match) %>% 
  rename("Tom Lynch" = "goals") 

jk <- afl.1 %>% 
  filter(player_first_name == "Josh J." & player_last_name == "Kennedy" & player_id == 11492) %>% 
  select(goals) %>% 
  mutate(match = row_number()) %>% 
  relocate(goals, .after = match) %>% 
  rename("Josh J. Kennedy" = "goals") 

compare <- join_all(list(jr,th,lf,tw,jc,cc,tl,jk), by='match', type='full') %>% 
  pivot_longer(-match,
               names_to = "Player",
               values_to = "Match Goals")

# ------------------------
# Combine
# ------------------------

compare3 <- left_join(compare2, compare, by = c("match" = "match", "Player" = "Player"))


# ------------------------
# Plot
# ------------------------

# Theme for plots
theme_plot <-
  theme(
    plot.title = element_text(size = 11, hjust = 0, colour = "#4E4F4E", face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0, colour = "#4E4F4E"),
    axis.title = element_text(size = 10, colour = "#4E4F4E"),
    axis.text = element_text(size = 10, colour = "#4E4F4E"),
    panel.background = element_rect(fill = "#fcffff",
                                    colour = "#fcffff"),
    plot.background = element_rect(fill = "#fcffff",
                                   colour = "#fcffff"),
    plot.margin = margin(5.5, 40, 5.5, 5.5), 
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(colour = "#4E4F4E",
                                size=10,
                                face="bold"),
    legend.margin = margin(grid::unit(0,"cm")),
    legend.text = element_text(colour = "#4E4F4E",
                               size = 10),
    legend.key.height = grid::unit(0.8,"cm"),
    legend.key.width = grid::unit(0.2,"cm")
  )




compare3 %>%
  group_by(Player) %>%
  slice(which.max(`Match Goals`))

compare3 %>% 
  drop_na() %>% 
  group_by(Player) %>% 
  summarise(goals = max(Goals, na.rm=TRUE),
            match = max(match, na.rm=TRUE))



compare3 %>% 
  filter(match <= 200) %>% 
  # filter(Player != "Charlie Curnow") %>% 
  ggplot() + geom_line(aes(y = Goals, x = match, colour = Player)) +
  # Specify custom axis labels 
  # modify to "year" view for y-axis 
  
  geom_segment(data = compare2 %>% 
                 filter(match <= 200) %>% 
                 drop_na() %>% 
                 group_by(Player) %>% 
                 summarise(goals = max(Goals, na.rm=TRUE),
                           match = max(match, na.rm=TRUE)),
               aes(y = goals, x = match, xend = 220, yend = goals), linetype = 2, colour = 'grey') + 
  # geom_point(size = 8) + 
  
  geom_text(data = compare2 %>% 
              filter(match <= 200) %>% 
              drop_na() %>% 
              group_by(Player) %>% 
              summarise(goals = max(Goals, na.rm=TRUE),
                        match = max(match, na.rm=TRUE)),
            aes(label = Player,
                x = 230,
                y = goals,
                color = Player)) +
  
  geom_point(data = compare3 %>%
               group_by(Player) %>%
               slice(which.max(`Match Goals`)),
             aes(label = `Match Goals`,
                 x = match,
                 y = Goals,
                 color = Player)) +
  
  geom_text(data = compare3 %>%
               group_by(Player) %>%
               slice(which.max(`Match Goals`)),
             aes(label = `Match Goals`,
                 x = match,
                 y = Goals,
                 color = Player),vjust = -1) +
  
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200)) + # 200 game filter 
  # scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350)) +
  # scale_colour_manual(values = c("#2b6389", "#e2754a", "#b47462")) +
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




















# filter

jr_goals <- afl.1 %>%
  filter(player_first_name == "Jack" & player_last_name == "Riewoldt") %>%
  select(player_last_name, goals) %>%
  mutate(total_goals = cumsum(goals)) %>%
  mutate(match = row_number()) %>%
  select(match, player_last_name, total_goals)

th_goals <- afl.1 %>%
  filter(player_first_name == "Tom" & player_last_name == "Hawkins") %>%
  select(player_last_name, goals) %>%
  mutate(total_goals = cumsum(goals)) %>%
  mutate(match = row_number()) %>%
  select(match, player_last_name, total_goals)

lf_goals <- afl.1 %>%
  filter(player_first_name == "Lance" & player_last_name == "Franklin") %>%
  select(player_last_name, goals) %>%
  mutate(total_goals = cumsum(goals)) %>%
  mutate(match = row_number()) %>%
  select(match, player_last_name, total_goals)

compare <- full_join(jr, th, by = "match") %>% 
  pivot_longer(-match,
               names_to = "Player",
               values_to = "Goals")


compare2 <- join_all(list(jr,th,lf,tw,jc,cc,tl,jk), by='match', type='full') %>% 
  pivot_longer(-match,
               names_to = "Player",
               values_to = "Goals")

compare3 <- join_all(list(jr,th,lf,tw,jc,cc,tl,jk), by='match', type='full')


str(compare)

compare$match <- as.numeric(compare$match)
compare$Goals <- as.numeric(compare$Goals)

# Theme for plots
theme_plot <-
  theme(
    plot.title = element_text(size = 11, hjust = 0, colour = "#4E4F4E", face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0, colour = "#4E4F4E"),
    axis.title = element_text(size = 10, colour = "#4E4F4E"),
    axis.text = element_text(size = 10, colour = "#4E4F4E"),
    panel.background = element_rect(fill = "#fcffff",
                                    colour = "#fcffff"),
    plot.background = element_rect(fill = "#fcffff",
                                   colour = "#fcffff"),
    plot.margin = margin(5.5, 40, 5.5, 5.5), 
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(colour = "#4E4F4E",
                                size=10,
                                face="bold"),
    legend.margin = margin(grid::unit(0,"cm")),
    legend.text = element_text(colour = "#4E4F4E",
                               size = 10),
    legend.key.height = grid::unit(0.8,"cm"),
    legend.key.width = grid::unit(0.2,"cm")
  )

compare %>% 
  ggplot() + geom_line(aes(y = Goals, x = match, colour = Player)) +
  # Specify custom axis labels 
  # modify to "year" view for y-axis 
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800)) +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300)) +
  scale_colour_manual(values = c("#2b6389", "#e2754a")) +
  theme_plot +
  theme(axis.line.x = element_line(color="#4E4F4E", size = 0.2),
        axis.line.y = element_line(color="#4E4F4E", size = 0.2),
        legend.key = element_rect(fill = "white"), # colour of legend line background 
  
  # Plot margins / border / fill-colour 
  # plot.background = element_rect(fill = "#e8b14c"),
  # legend.background = element_rect(fill = "#e8b14c"),
  
  panel.border = element_blank()
  
  )

library(geomtextpath)




compare2 %>% 
  drop_na() %>% 
  group_by(Player) %>% 
  summarise(goals = max(Goals, na.rm=TRUE),
            match = max(match, na.rm=TRUE))



compare2 %>% 
  filter(match <= 200) %>% 
  ggplot() + geom_line(aes(y = Goals, x = match, colour = Player)) +
  # Specify custom axis labels 
  # modify to "year" view for y-axis 

  # geom_segment(aes(y = Goals, x = match, xend = 350, yend = Goals), linetype = 2, colour = 'grey') + 
  # geom_point(size = 8) + 
  # geom_text(aes(y = max(Goals), x = 300, label = Player, size = 12), hjust = 0) + 

  geom_text(data = compare2 %>% 
              filter(match <= 200) %>% 
              drop_na() %>% 
              group_by(Player) %>% 
              summarise(goals = max(Goals, na.rm=TRUE),
                        match = max(match, na.rm=TRUE)),
            aes(label = Player,
                x = match,
                y = goals,
                color = Player), vjust= 1, hjust = 1) +
  
  
  # geom_textline(aes(x = match, y = Goals, group = Player, colour = Player, label = Player),
  #               hjust = 1) +
  
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200)) + # 200 game filter 
  # scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350)) +
  # scale_colour_manual(values = c("#2b6389", "#e2754a", "#b47462")) +
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
  
dt %>% group_by(subject)%>% 
  summarise(first_value = min(value, na.rm=TRUE),
            last_value = max(value, na.rm=TRUE))
















compare %>% 
  ggplot(aes(group = PatientID, color = ChangeType, x = Timepoint, y = PainSeverity))+ 
  stat_summary(fun.y=mean, colour="black", geom="line", aes(group = 1), size = 1.5)+
  labs(list(title = "Change to outcomes from pre-program to discharge",
            subtitle= "Pain Severity", 
            y = paste("Pain Severity Score"))) +
  scale_x_discrete(labels=c("Pre-Program", "Discharge")) +
  scale_colour_manual(values=c("#F15151", "#6CCA62", "#FFB165")) +
  geom_line(size=0.5)+
  geom_point(size = 0.5, colour="000033")
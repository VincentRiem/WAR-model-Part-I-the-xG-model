install.packages("jsonlite")
library(jsonlite)
library(dplyr)
library(purrr)

list.files()
getwd()
setwd("~/Downloads/events")

start_game <- 122838
end_game   <- 123143

files <- paste0("events_", start_game:end_game, ".json")

load_game <- function(file) {
  tryCatch({
    fromJSON(file, flatten = TRUE)
  }, error = function(e) {
    message(paste("Error loading:", file))
    return(NULL)
  })
}

games_list <- map(files, load_game)

all_games <- bind_rows(games_list)


all_games <- all_games %>%
  mutate(next_actionType = lead(actionType))

shots <- all_games %>%
  filter(actionType == "SHOT") %>%
  mutate(goal = ifelse(next_actionType == "GOAL", 1, 0))

shots <- shots %>%
  mutate(close_opponent = ifelse(
    distanceToOpponent %in% c("ONE_METER", "LESS_THAN_ONE_METER"),
    1,
    0
  ))

shots_model <- shots %>%
  mutate(
    
    body_part = factor(bodyPartExtended),
    
    phase = factor(phase),
    
    one_vs_one = ifelse(action == "one_vs_one_against_gk", 1, 0),
    
    opponents_goal = as.numeric(opponents),
    
  )

xg_model <- glm(
  goal ~ 
    shot.distance +
    shot.angle +
    body_part +
    phase +
    one_vs_one +
    opponents_goal +
    close_opponent,
  
  data = shots_model,
  family = binomial()
)

shots_model$xG <- predict(xg_model, type = "response")

install.packages("pROC")
library(pROC)

roc_curve <- roc(shots_model$goal, shots_model$xg)
auc(roc_curve)

shots_model <- shots_model %>%
  mutate(
    shot_side = ifelse(start.coordinates.y > 0, "left", "right")
  )

#Test model 2
xg_model2 <- glm(
  goal ~ 
    shot.distance +
    shot.angle +
    body_part +
    phase +
    one_vs_one +
    opponents_goal +
    close_opponent +
    shot.distance:body_part,
  
  data = shots_model,
  family = binomial()
)

shots_model$xg2 <- predict(xg_model2, type = "response")

roc_curve2 <- roc(shots_model$goal, shots_model$xg2)
auc(roc_curve)

library(ggplot2)

ggplot(shots_model, aes(start.coordinates.x, start.coordinates.y, color = xg)) +
  geom_point(alpha = 0.4) +
  scale_color_viridis_c()


ggplot(shots_model, aes(start.coordinates.x, start.coordinates.y, color = xG)) +
  geom_point(alpha = 0.4) +
  
  # new xG color palette
  scale_color_viridis_c(option = "inferno") +
  
  # outer pitch
  annotate("rect", xmin = -52.5, xmax = 52.5, ymin = -34, ymax = 34,
           fill = NA, colour = "white", linewidth = 0.9) +
  
  # halfway line
  annotate("segment", x = 0, xend = 0, y = -34, yend = 34,
           colour = "white", linewidth = 0.7) +
  
  # centre circle
  annotate("path",
           x = 9.15 * cos(seq(0, 2*pi, length.out = 200)),
           y = 9.15 * sin(seq(0, 2*pi, length.out = 200)),
           colour = "white", linewidth = 0.7) +
  
  # penalty boxes
  annotate("rect", xmin = -52.5, xmax = -36, ymin = -20.16, ymax = 20.16,
           fill = NA, colour = "white", linewidth = 0.7) +
  annotate("rect", xmin = 36, xmax = 52.5, ymin = -20.16, ymax = 20.16,
           fill = NA, colour = "white", linewidth = 0.7) +
  
  # six-yard boxes
  annotate("rect", xmin = -52.5, xmax = -47, ymin = -9.16, ymax = 9.16,
           fill = NA, colour = "white", linewidth = 0.7) +
  annotate("rect", xmin = 47, xmax = 52.5, ymin = -9.16, ymax = 9.16,
           fill = NA, colour = "white", linewidth = 0.7) +
  
  # penalty spots
  annotate("point", x = -42, y = 0, size = 2, colour = "white") +
  annotate("point", x = 42, y = 0, size = 2, colour = "white") +
  
  # LEFT penalty arc
  annotate("path",
           x = -42 + 9.15 * cos(seq(-pi/4, pi/4, length.out = 200)),
           y = 0 + 9.15 * sin(seq(-pi/4, pi/4, length.out = 200)),
           colour = "white", linewidth = 0.7) +
  
  # RIGHT penalty arc
  annotate("path",
           x = 42 + 9.15 * cos(seq(3*pi/4, 5*pi/4, length.out = 200)),
           y = 0 + 9.15 * sin(seq(3*pi/4, 5*pi/4, length.out = 200)),
           colour = "white", linewidth = 0.7) +
  
  coord_fixed(xlim = c(-52.5, 52.5), ylim = c(-34, 34), expand = FALSE) +
  
  labs(
    title = "xG for every shot",
    x = "",
    y = ""
  ) +
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#2E8B57", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white"),
    plot.title = element_text(color = "black", face = "bold", size = 18)
  )

library(dplyr)

shots_model %>%
  mutate(bin = cut(xg, breaks = seq(0,1,0.05))) %>%
  group_by(bin) %>%
  summarise(
    predicted = mean(xg),
    actual = mean(goal)
  ) %>%
  ggplot(aes(predicted, actual)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  labs(
    title = "xG Model Calibration",
    x = "Predicted Goal Probability",
    y = "Actual Goal Frequency"
  ) +
  theme_minimal()


#Calibration plot
calibration <- shots_model %>%
  mutate(bin = cut(xg, breaks = seq(0,1,0.05))) %>%
  group_by(bin) %>%
  summarise(
    predicted = mean(xg),
    actual = mean(goal),
    n = n()
  )

ggplot(calibration, aes(predicted, actual)) +
  
  # perfect calibration line
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    color = "#d62728",
    linewidth = 1
  ) +
  
  # points
  geom_point(
    aes(size = n),
    color = "#2E8B57",
    alpha = 0.8
  ) +
  
  scale_size(range = c(2,6), name = "Amount of Shots") +
  
  labs(
    title = "xG Model Calibration",
    x = "Predicted Goal Probability (xG)",
    y = "Actual Goal Frequency"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "right"
  )

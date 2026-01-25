library(tidyverse)
options(scipen = 999)

games <- read_csv("data/cfb-results-2025.csv")
as_of <- "2026-01-14" %>% as_date()

results <- games %>% 
  janitor::clean_names() %>% 
  filter(mdy(date) <= as_of) %>% 
  separate(winner, 
           into = c(NA, "winner"), 
           sep = "\\([0-9]+\\)", 
           fill = "left") %>% 
  separate(loser,
           into = c(NA, "loser"),
           sep = "\\([0-9]+\\)",
           fill = "left") %>% 
  mutate(winner = str_trim(winner),
         loser = str_trim(loser)) %>% 
  select(winner, loser)

teams <- unique(c(games_clean$winner, games_clean$loser))

results_by_team_win <- results %>% 
  group_by(winner, loser) %>% 
  tally(name = "wins") %>% 
  rename(team = winner,
         opponent = loser)

results_by_team_loss <- results %>% 
  group_by(loser, winner) %>% 
  tally(name = "losses") %>% 
  rename(team = loser,
         opponent = winner)

results_by_team <- full_join(results_by_team_win,
                             results_by_team_loss,
                             join_by(team, opponent)) %>% 
  replace_na(list(wins = 0, losses = 0)) %>% 
  group_by(team, opponent) %>% 
  summarize(wins = sum(wins), losses = sum(losses)) %>% 
  mutate(games_played = wins + losses)

add_dummy_team <- function(results, alpha){
  dummy_wins <- list()
  dummy_losses <- list()
  for(i in 1:length(teams)){
    dummy_wins[[i]] <- tibble(team = teams[i], 
                              opponent = "DUMMY_TEAM", 
                              wins = alpha, 
                              losses = alpha)
    dummy_losses[[i]] <- tibble(team = "DUMMY_TEAM", 
                                opponent = teams[i], 
                                wins = alpha, 
                                losses = alpha)
  }
  dummy_rows <- bind_rows(dummy_wins, dummy_losses)
  
  results_regularized <- rbind(results, dummy_rows) %>% 
    arrange(team, opponent)
  
  return(results_regularized)
}

results_regularized <- add_dummy_team(results_by_team, 0.85)
# regularization parameter


# convert from long to wide
results_table <- results_regularized %>% 
  ungroup() %>% 
  complete(team, opponent, fill = list(wins = 0, losses = 0)) %>% 
  mutate(wins = if_else(team == opponent, NA_real_, wins),
         losses = if_else(team == opponent, NA_real_, losses)) %>% 
  select(-c(losses, games_played)) %>% 
  pivot_wider(names_from = opponent, values_from = wins) %>% 
  column_to_rownames(var = "team")

# method of normalizing ratings
geometric_mean <- function(vec){return(exp(mean(log(vec))))}

normalize_ratings <- function(ratings){
  return(ratings/geometric_mean(ratings)) # normalize to geometric mean of 1
}

ratings_init <- rep(1, times = length(teams)+1)

calculate_ratings <- function(result_tbl,
                              ratings_init,
                              max_iter = 100,
                              precision = 0.001){
  ratings <- ratings_init
  
  for(iter in 1:max_iter){
    new_ratings <- ratings
    
    for(i in 1:length(ratings)){
      # initialize numerator and denominator
      numer <- 0
      denom <- 0
      for(j in 1:length(ratings)){
        if(i != j){
          # numerator = wins by i over j, multiplied by j's rating, divided by the sum of their ratings
          numer <- numer + (result_tbl[i,j]*new_ratings[j]/(new_ratings[i]+new_ratings[j]))
          # denominator = wins by j over i, divided by the sum of their ratings
          denom <- denom + (result_tbl[j,i]/(new_ratings[i] + new_ratings[j]))
        }
      }
      new_ratings[i] <- numer / denom
    }
    
    ratings_normalized <- normalize_ratings(new_ratings)
    
    if(abs(sum(ratings_normalized) - sum(ratings)) < precision){
      message(paste0("Converged after ", iter, " iterations."))
      return(ratings_normalized)
    }
    ratings <- ratings_normalized
  }
  stop(paste0("Did not converge after ", max_iter, " iterations."))
}

ratings <- tibble(team = rownames(results_table),
                  rating = round(100 * calculate_ratings(results_table, 
                                                         ratings_init), 
                                 2)) %>% 
  filter(team %in% teams) %>% 
  arrange(desc(rating)) %>% 
  mutate(rank = row_number()) %>% 
  select(rank, team, rating)

# round robin winning percentage
calculate_rrwp <- function(ratings){
  rrwp <- rep(0, times = length(ratings))
  for(i in 1:length(ratings)){
    xw <- 0 # expected wins
    for(j in 1:length(ratings)){
      if(i != j){
        xw <- xw + ratings[i]/(ratings[i] + ratings[j]) # team's rating divided by sum of team + opponent's ratings
      }
    }
    rrwp[i] <- xw / (length(ratings) - 1) # winning percentage = total expected wins / games played
  }
  return(round(rrwp, 4))
}

strength_of_schedule <- results_by_team %>% 
  mutate(total_games = wins + losses) %>% 
  left_join(ratings, join_by(team)) %>% 
  rename(team_rating = rating) %>% 
  left_join(ratings, join_by(opponent == team)) %>% 
  rename(opponent_rating = rating) %>% 
  group_by(team, opponent, team_rating, opponent_rating) %>% 
  summarize(weighting_factor = total_games / (team_rating + opponent_rating)) %>% 
  group_by(team) %>% 
  summarize(strength_of_schedule = round(sum(opponent_rating * weighting_factor/sum(weighting_factor)), 2))

winning_ratio <- results_by_team %>% 
  group_by(team) %>% 
  summarize(wins = sum(wins),
            losses = sum(losses),
            winning_ratio = wins / losses) %>% 
  select(team, winning_ratio)

ratings_table <- ratings %>% 
  mutate(rrwp = calculate_rrwp(rating))
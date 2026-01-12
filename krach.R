library(tidyverse)
options(scipen = 999)

games <- read_csv("data/wih-results-2025-26.csv")
# UPDATE
as_of <- "2026-01-12"

teams <- sort(unique(c(games$away, games$home)))
team_info <- read_csv("data/team-info.csv")

# convert game results into team-by-team results versus each opponent
results <- games %>% filter(date <= as_of)
if(any(is.na(results$away_win))) warning("Not all games as of specified date have a recorded result.")

results_by_team_home <- results %>% 
  group_by(home, away) %>% 
  summarize(home_wins = sum(home_win), home_losses = sum(away_win)) %>% 
  rename(team = home, opponent = away)

results_by_team_away <- results %>% 
  group_by(away, home) %>% 
  summarize(away_wins = sum(away_win), away_losses = sum(home_win)) %>% 
  rename(team = away, opponent = home)

# combine home and away results
results_by_team <- full_join(results_by_team_home,
                             results_by_team_away,
                             join_by(team, opponent)) %>% 
  replace_na(list(home_wins = 0, home_losses = 0, 
                  away_wins = 0, away_losses = 0)) %>% 
  group_by(team, opponent) %>% 
  summarize(wins = sum(home_wins) + sum(away_wins),
            losses = sum(home_losses) + sum(away_losses))

# convert from long to wide
results_table <- results_by_team %>% 
  ungroup() %>% 
  complete(team, opponent, fill = list(wins = 0, losses = 0)) %>% 
  mutate(wins = if_else(team == opponent, NA_real_, wins),
         losses = if_else(team == opponent, NA_real_, losses)) %>% 
  select(-losses) %>% 
  pivot_wider(names_from = opponent, values_from = wins) %>% 
  column_to_rownames(var = "team")

# method of normalizing ratings
geometric_mean <- function(vec){return(exp(mean(log(vec))))}

normalize_ratings <- function(ratings){
  return(ratings/geometric_mean(ratings)) # normalize to geometric mean of 1
}

ratings_init <- rep(1, times = length(teams))

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

ratings <- tibble(
  team = teams,
  rating = round(calculate_ratings(results_table, ratings_init) * 100, 2))

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
  mutate(rrwp = calculate_rrwp(rating)) %>% 
  arrange(desc(rating)) %>% 
  mutate(ranking = row_number()) %>% 
  left_join(team_info, join_by(team == abbr)) %>% 
  select(ranking, team = team.y, abbr = team, conference, rating, rrwp)

write_csv(ratings_table,
          paste0("output/wih_krach_rankings_", as_of, ".csv"))


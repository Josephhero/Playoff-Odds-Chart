library(tidyverse)
library(oddsapiR)
library(nflverse)
library(gt)
library(gtExtras)
library(shadowtext)


# Data-----

if (!dir.exists("./images")) dir.create("./images")
options(scipen=999)

YEAR <- get_current_season()
WEEK <- get_current_week()
sched <- load_schedules(seasons = YEAR)

week_dates <- sched |> 
  filter(week == WEEK) |> 
  mutate(gameday = ymd(gameday)) |> 
  pull(gameday)

# Get odds offered for sport by market (h2h, spreads, totals): 
# You will need an API key for this. You can follow the instructions 
# from the {oddsapiR} package here: https://oddsapir.sportsdataverse.org/
h2h_lines_raw <- toa_sports_odds(sport_key = 'americanfootball_nfl', 
                                 regions = 'us', 
                                 markets = 'h2h', 
                                 odds_format = 'american',
                                 date_format = 'iso' 
) |> 
  filter(bookmaker_key == "draftkings")

h2h_lines <- h2h_lines_raw |> 
  mutate(commence_ny = lubridate::as_date(lubridate::ymd_hms(commence_time, tz = "America/New_York")), 
         .after = commence_time) |> 
  filter(commence_ny %in% week_dates)
# slice_head(n = (games * 2))

spread_lines_raw <- toa_sports_odds(sport_key = 'americanfootball_nfl', 
                                    regions = 'us', 
                                    markets = 'spreads', 
                                    odds_format = 'american',
                                    date_format = 'iso'
) |> 
  rename(spread = outcomes_point) |> 
  filter(bookmaker_key == "draftkings")

spread_lines <- spread_lines_raw |> 
  mutate(commence_ny = lubridate::as_date(lubridate::ymd_hms(commence_time, tz = "America/New_York")), 
         .after = commence_time) |> 
  filter(commence_ny %in% week_dates)

total_lines_raw <- toa_sports_odds(
  sport_key = 'americanfootball_nfl',  
  regions = 'us', 
  markets = 'totals',
  odds_format = 'american',
  date_format = 'iso'
) |> 
  rename(total = outcomes_point) |> 
  filter(bookmaker_key == "draftkings")

total_lines <- total_lines_raw |> 
  mutate(commence_ny = lubridate::as_date(lubridate::ymd_hms(commence_time, tz = "America/New_York")), 
         .after = commence_time) |> 
  filter(commence_ny %in% week_dates)

# Get vig free head to head and turn into percentages to win.
h2h_home <- h2h_lines |> 
  filter(home_team == outcomes_name) |> 
  select(home_team, away_team, ml_home = outcomes_price)

h2h_away <- h2h_lines |> 
  filter(away_team == outcomes_name) |> 
  select(home_team, away_team, ml_away = outcomes_price)

h2h <- left_join(h2h_home, h2h_away,  
                 by = c("home_team", "away_team")) |> 
  mutate(calc_home = if_else(ml_home < ml_away, abs(ml_home), 100)) |> 
  mutate(calc_away = if_else(ml_away < ml_home, abs(ml_away), 100)) |> 
  mutate(vig_home = calc_home / (abs(ml_home) + 100)) |> 
  mutate(vig_away = calc_away / (abs(ml_away) + 100)) |> 
  mutate(home_wp = vig_home / (vig_home + vig_away)) |> 
  mutate(away_wp = vig_away / (vig_home + vig_away))


# Get spreads and turn into implied points
implied_points <- left_join(
  select(spread_lines, commence_time, id, team = outcomes_name, spread), 
  select(total_lines, id, total), 
  by = "id", 
  relationship = "many-to-many"
) |> 
  distinct() |> 
  mutate(exp_points = (total + (spread * -1)) / 2)

week_preview1 <- 
  left_join(h2h, select(implied_points, commence_time, team, home_spread = spread, total, 
                        home_score = exp_points), 
            by = c("home_team" = "team")) |> 
  left_join(select(implied_points, team, away_spread = spread, 
                   away_score = exp_points), 
            by = c("away_team" = "team")) |> 
  left_join(select(load_teams(), team_name, home_abbr = team_abbr, team_conf, 
                   home_fill = team_color), 
            by = c("home_team" = "team_name")) |> 
  left_join(select(load_teams(), team_name, away_abbr = team_abbr, 
                   away_fill = team_color, away_fill2 = team_color2), 
            by = c("away_team" = "team_name")) |> 
  mutate(away_fill = if_else(home_fill == away_fill, away_fill2, away_fill)) |> 
  mutate(fav_spread = if_else(away_wp > 0.5, 
                              paste(away_abbr, away_spread, sep = "  "), 
                              paste(home_abbr, home_spread, sep = "  "))) |> 
  mutate(win_probability = paste0(away_abbr, "@", home_abbr)) |> 
  mutate(separator = "-") |> 
  mutate(eastern_time = lubridate::as_datetime(commence_time, tz = "US/Eastern")) |> 
  mutate(gameday_full = wday(eastern_time, label = TRUE, abbr = FALSE)) |> 
  mutate(gameday = if_else(row_number() == 1, gameday_full, ""), .by = gameday_full) |> 
  arrange(commence_time) |> 
  select(gameday, away_abbr, win_probability, home_abbr, fav_spread, total,
         fav_spread, total, away_score, separator, home_score, home_fill, away_fill,
         home_wp, away_wp) 


week_preview <- week_preview1 |> 
  select(-home_fill:-away_wp) |> 
  mutate(text_style = if_else(home_score > away_score, 1, 0))

# Set up function for stacked bar chart

plot_stacked_bar <- function(df, win_prob) {
  
  win <- week_preview1 |> 
    select(win_probability, home_abbr, away_abbr, home_wp, away_wp) |> 
    pivot_longer(cols = c(home_wp, away_wp), 
                 names_to = "win_type", 
                 values_to = "win") |> 
    mutate(team_wp = if_else(win_type == "home_wp", home_abbr, away_abbr))
  
  fill <- week_preview1 |> 
    select(win_probability, home_abbr, away_abbr, home_fill, away_fill) |> 
    pivot_longer(cols = c(home_fill, away_fill), 
                 names_to = "fill_type", 
                 values_to = "fill_col") |> 
    mutate(team_wp = if_else(fill_type == "home_fill", home_abbr, away_abbr)) |> 
    select(-home_abbr, -away_abbr)
  
  data_gg <- left_join(win, fill, by = c("win_probability", "team_wp")) |> 
    mutate(text = paste0(round(win * 100, 0), "%")) |> 
    mutate(text_loc = if_else(away_abbr == team_wp, win / 2, 1 - (win / 2))) |> 
    mutate(sort = row_number()) 
  
  data_gg |>
    filter(win_probability == {{win_prob}}) |> 
    mutate(fill_col = fct_reorder(fill_col, sort, .desc = FALSE)) |> 
    ggplot(aes(x = win, y = win_probability, fill = fill_col)) + 
    geom_col(position = "stack", color = "white", linewidth = 7, 
             alpha = 0.8) +
    geom_shadowtext(aes(x = text_loc, y = 1, label = text), 
                    bg.color = "gray10", 
                    bg.r = .2, 
                    fontface = "bold",
                    size = 50) + 
    scale_fill_identity() +
    theme_void() 
  
}

odds_date <- paste0(
  format(lubridate::as_datetime(
    ymd_hms(h2h_lines$bookmaker_last_update[1], tz = "US/Eastern")), 
    "%b %d, %Y, %I:%M %p"), 
  " EST")


(
  tab <- 
    gt(week_preview) |> 
    gt_theme_538() |> 
    cols_hide("text_style") |> 
    gt_nfl_logos(c("home_abbr", "away_abbr")) |>  
    cols_label(gameday = "",
               home_abbr = "home",
               away_abbr = "away", 
               home_score = "home", 
               away_score = "away", 
               fav_spread = "spread", 
               separator = ""
    ) |> 
    # Header text and format
    tab_header(title = paste0("DraftKings Week ", WEEK, " Odds"), 
               subtitle = paste0("Odds last updated: ", odds_date)) |> 
    tab_style(
      style = list(
        cell_text(weight = "bold", 
                  align = "center")
      ),
      locations = cells_title(groups = c("title", "subtitle"))
    ) |> 
    # Column Label format
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(everything())
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(
        columns = everything())
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = "gameday")
    ) |>
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body(
        columns = c("away_abbr", "home_abbr"))
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = "home_score", 
        rows = text_style == 1)
    ) |>
    tab_style(
      style = cell_text(whitespace = "pre"),
      locations = cells_body(
        columns = "fav_spread") 
    ) |>    
    tab_style(
      style = cell_text(color = "red", style = "italic"),
      locations = cells_body(
        columns = "home_score", 
        rows = text_style == 0)
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = "away_score", 
        rows = text_style == 0)
    ) |>
    tab_style(
      style = cell_text(color = "red", style = "italic"),
      locations = cells_body(
        columns = "away_score", 
        rows = text_style == 1)
    ) |>
    tab_spanner(
      label = md("**Expected Score**"),
      columns = c("away_score":"home_score"),
    ) |>
    # Turn Win Probability into bar chart
    text_transform(
      locations = cells_body(columns = "win_probability"),
      fn = function(column) {
        map(column, function(x) plot_stacked_bar(x, df = week_preview1))|>
          ggplot_image(height = px(25), aspect_ratio = 7)
      }
    ) |> 
    tab_options(data_row.padding = px(0)) |> 
    tab_style(
      style = cell_borders(sides = "all", color = "transparent"),
      locations = cells_body(columns = "gameday")
    ) |> 
    # Optional: You can also adjust the padding if you want to create more space
    # tab_style(
    #   style = cell(padding = px(5)),
    #   locations = cells_body(columns = "gameday")
    # ) |> 
    # Add footnote and style it
    tab_footnote(
      footnote = paste0("Win Probability is the moneyline converted to a percentage with the vig removed."),
      locations = cells_column_labels(
        columns = "win_probability"
      )
    ) |>
    tab_style(
      style = list(
        cell_text(style = "italic", 
                  size = px(12))
      ),
      locations = cells_footnotes()
    ) |>
    tab_footnote(
      footnote = paste0("Expected score is the implied score based on the spread and the total."),
      locations = cells_column_spanners(),
    ) |>
    tab_style(
      style = list(
        cell_text(style = "italic", 
                  size = px(12))
      ),
      locations = cells_footnotes()
    ) |>
    tab_style(
      style = list(
        cell_borders(
          side = c("top"),
          color = "gray80",
          weight = px(1)
        )
      ),
      locations = cells_source_notes()
    ) |>
    tab_options(footnotes.padding = px(0)) |> 
)

gtsave(tab, 
       path = "./images", 
       filename = paste0(YEAR, " DraftKings Week ", WEEK, " Odds.png"), 
       expand = 10)




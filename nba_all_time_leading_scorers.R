devtools::install_github("emilykuehler/basketballstatsR")
library(tidyverse); library(basketballstatsR); library(gganimate); library(ggpomological)

suffix <- c(seq(53,99),'00','01','02','03','04','05','06','07','08','09',seq(10,19))
seasons <- paste0(seq(1952,2018),'-',suffix)

pts_df <- get_league_leaders(season='1951-52')
pts_df <- pts_df %>% mutate(season = '1951-52')

for (i in 1:length(seasons)) {
  curr_df <- get_league_leaders(season = seasons[i]) %>%
    mutate(season = seasons[i])

  pts_df <- pts_df %>% bind_rows(curr_df)

  print (seasons[i])

  Sys.sleep(runif(n=1,min = 0.5,max = 2))
}

pts_df <- pts_df %>%
  select(player_id, player, pts, season)

#need to calculate rolling sum
pts_df_sum <- pts_df %>%
  separate(season, into = c('year', 'year2'), sep = '-') %>%
  select(-year2) %>%
  mutate(pts = as.numeric(pts), year = as.numeric(year)) %>%
  group_by(player_id) %>%
  mutate(cum_sum_pts = cumsum(pts)) %>%
  ungroup()


pts_df_sum2 <- pts_df_sum %>%
  filter(year==1951) %>%
  top_n(10, wt=cum_sum_pts) %>%
  arrange(desc(cum_sum_pts)) %>%
  mutate(curr_year = 1951,
         ordering = as.double(rev(seq(10:1))) * 1.0)


yrs <- sort(unique(pts_df_sum$year))
for (i in 2:length(yrs)) {
  tmp_df <- pts_df_sum %>%
    filter(year <= yrs[i]) %>%
    group_by(player) %>%
    filter(cum_sum_pts==max(cum_sum_pts)) %>%
    ungroup() %>%
    top_n(10, wt=cum_sum_pts) %>%
    arrange(desc(cum_sum_pts)) %>%
    mutate(curr_year = yrs[i],
           ordering = as.double(rev(seq(10:1))) * 1.0)
  pts_df_sum2 <- pts_df_sum2 %>%
    bind_rows(tmp_df)
  print(yrs[i])
}

pts_df_sum2 <- pts_df_sum2 %>%
  mutate(yr_suffix = curr_year %% 100 + 1,
         yr_suffix = ifelse(yr_suffix <= 9, paste0('0', yr_suffix), as.character(yr_suffix)),
         yr_label = paste0(curr_year, '-', yr_suffix))

player_position_df <- get_basic_player_info(id = unique(pts_df_sum2$player_id))
player_position_df2 <- player_position_df %>%
  select(person_id, position) %>%
  rename(player_id = person_id)

merged_pts_pos <- pts_df_sum2 %>%
  left_join(player_position_df2, by = c('player_id'))


my_font <- 'Quicksand'
my_background <- unlist(ggpomological:::pomological_base)[2]
pom_pal <- ggpomological:::pomological_palette[2:5]
my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  plot.title = element_text(face = 'bold', size = 20),
                  plot.subtitle = element_text(size = 14),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_line(color = 'grey75'),
                  panel.grid.minor.x = element_line(color = 'grey75'),
                  legend.position = 'none',
                  plot.caption = element_text(size = 8),
                  axis.ticks = element_blank(),
                  axis.text.y =  element_blank())

theme_set(theme_light() + my_theme)

merged_pts_pos2 <- merged_pts_pos %>% filter(curr_year >= 1960)

barplot_race_nba <- ggplot(aes(ordering, group = player), data = merged_pts_pos2) +
  geom_tile(aes(y = cum_sum_pts / 2,
                height = cum_sum_pts,
                width = 0.9, fill = position), alpha = 0.9) +
  scale_fill_manual(values = pom_pal) +
  geom_text(aes(y = cum_sum_pts, label = player), family=my_font, nudge_y = -3750, size = 3) +
  geom_text(aes(y = cum_sum_pts, label = as.character(cum_sum_pts)), family=my_font, nudge_y = 1500) +
  geom_text(aes(x=1,y=37500, label=paste0(yr_label)), family=my_font, size=8, color = 'gray45') +
  coord_cartesian(clip = "off", expand = FALSE) +
  coord_flip() +
  labs(title = 'All-Time NBA Scoring Leaders',
       caption = 'data source: stats.nba.com | plot by @emilykuehler',
       x = '',
       y = '') +
  transition_states(curr_year,
                    transition_length = 4, state_length = 3) +
  ease_aes('cubic-in-out')

animate(barplot_race_nba, nframes = 1000, fps = 30, width = 600, height = 400, res=80, detail = 3)

anim_save("nba_scoring_leaders.gif")

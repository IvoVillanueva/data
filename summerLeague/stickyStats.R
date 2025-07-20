library(tidyverse)
library(hoopR)
library(janitor)
library(hablar)
library(gt)
library(gtExtras)
library(extrafont)



twitter <- "<span style='color:#000000;font-family: \"Font Awesome 6 Brands\"'>&#xE61A;</span>"
tweetelcheff <- "<span style='font-weight:bold;'>*@elcheff*</span>"
insta <- "<span style='color:#E1306C;font-family: \"Font Awesome 6 Brands\"'>&#xE055;</span>"
instaelcheff <- "<span style='font-weight:bold;'>*@sport_iv0*</span>"
github <- "<span style='color:#000000;font-family: \"Font Awesome 6 Brands\"'>&#xF092;</span>"
githubelcheff <- "<span style='font-weight:bold;'>*IvoVillanueva*</span>"
substack <- "<img src='https://substackcdn.com/image/fetch/$s_!xBQa!,w_10,c_limit,f_auto,q_auto:good,fl_progressive:steep/https%3A%2F%2Fsubstack.com%2Fimg%2Fsubstack.png' />"
caption <- glue::glue("**Datos**: *NBA* | **Gráfico**: *Ivo Villanueva* • {twitter} {tweetelcheff} • {insta} {instaelcheff} • {github} {githubelcheff}")


gamelogs <- nba_leaguegamelog(season = "2025-26", league_id = 15, player_or_team = 'P')

equipos <- nba_teams()


equipos_nba <- equipos %>%
  filter(league_id == "00") %>%
  transmute(team_id, team = team_abbreviation, color = paste0("#",color), alternate_color = paste0("#", alternate_color)) %>%
  mutate(alternate_color = ifelse(team == "SAS", color, alternate_color),
         color = ifelse(team == "SAS", "white",color),
         alternate_color = ifelse(team == "UTA", "#4e008e", alternate_color),
         color = ifelse(team == "UTA", "#79a3dc",color),)


box_scores <- gamelogs$LeagueGameLog %>%
  clean_names() %>%
  retype()


sticky_stats <- box_scores %>%
  mutate(
    minutes_played = ifelse(min == 0, NA, min),
    multiplier = 36 / minutes_played,

    fg3a_per36 = fg3a * multiplier,
    blk_per36  = blk * multiplier,
    oreb_per36 = oreb * multiplier,
    ast_per36  = ast * multiplier,

    fg_attempts = ifelse(fga == 0, NA, fga),
    fg3a_ratio = fg3a / fg_attempts  # 3PAr = 3PA / FGA
  ) %>%
  select(player_name, team_abbreviation, game_date, minutes_played,
         fg3a_per36, blk_per36, oreb_per36, ast_per36, fg3a_ratio)


sticky_stats_summary <- sticky_stats %>%
  group_by(player_name, team = team_abbreviation) %>%
  summarise(
    minutes_played = sum(minutes_played, na.rm = TRUE),
    fg3a_per36 = mean(fg3a_per36, na.rm = TRUE),
    blk_per36  = mean(blk_per36, na.rm = TRUE),
    oreb_per36 = mean(oreb_per36, na.rm = TRUE),
    ast_per36  = mean(ast_per36, na.rm = TRUE),
    fg3a_ratio = mean(fg3a_ratio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(minutes_played > 50)


fg3 <- sticky_stats_summary %>%
 select(player_name, team, fg3a_per36, fg3a_ratio) %>%
  arrange(desc(fg3a_ratio)) %>%
  head(15) %>%
  mutate(rk = row_number())

blk_per36 <- sticky_stats_summary %>%
  select(blk_per36_player_name = player_name,  blk_per36_team = team, blk_per36) %>%
  arrange(desc(blk_per36)) %>%
  head(15)%>%
  mutate(rk = row_number())

oreb_per36 <- sticky_stats_summary %>%
  select(oreb_per36_player_name = player_name,  oreb_per36_team = team, oreb_per36) %>%
  arrange(desc(oreb_per36)) %>%
  head(15)%>%
  mutate(rk = row_number())

ast_per36 <- sticky_stats_summary %>%
  select(player_ast_per36 =player_name, ast_per36_team = team, ast_per36) %>%
  arrange(desc(ast_per36)) %>%
  head(15)%>%
  mutate(rk = row_number())

final_table <- fg3  %>%
  left_join(equipos_nba, join_by(team)) %>%
  left_join(ast_per36 , join_by(rk)) %>%
  left_join(oreb_per36, join_by(rk)) %>%
  left_join(blk_per36, join_by(rk)) %>%
  left_join(equipos_nba %>%
              rename(ast_per36_team = team, ast_per36_id = team_id, ast_per36_color = color, ast_per36_alternate_color = alternate_color), join_by(ast_per36_team)) %>%
  left_join(equipos_nba %>%
              rename(oreb_per36_team = team, oreb_per36_id = team_id, oreb_per36_color = color, oreb_per36_alternate_color = alternate_color), join_by(oreb_per36_team)) %>%
  left_join(equipos_nba %>%
              rename(blk_per36_team = team, blk_per36_id = team_id, blk_per36_color = color, blk_per36_alternate_color = alternate_color), join_by(blk_per36_team))



add_photo_frame <- function(color = color, alternate_color = alternate_color, team_id = team_id, player_name = player_name) {
  glue::glue(
    "<div style='display: flex; align-items: center; justify-content: letf; height: 100%;text-align:left;'>
       <img style='
         width: 50px;
         height: 50px;
         border-radius: 20%;
         background-color: {color};
         border: 2px solid {alternate_color};'
         src='https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg'/>
         <span style='font-weight:bold; font-variant:small-caps; padding-left: 10px; font-size:20px;'>{player_name}</span>
     </div>"
  )
}


add_rk <- function(rk) {
  div_out <- htmltools::div(
    style = (
      "background: linear-gradient(135deg, #fff 0%, #0268d6 100%);
                              margin: 1px;
  text-align: center;
  width: 30px;
  height: 30px;
  border-radius: 20%;
  font-size:20px;
  font-weight: 900;
  vertical-align:middle;
  background-color: #000000;
  color:white;
    position: relative;"
    ),
    paste(rk)
  )

  as.character(div_out) %>%
    gt::html()
}


final_table %>%
  mutate(
    ranking = map(rk, add_rk),
    combo_img = add_photo_frame(color, alternate_color, team_id, player_name),
    combo_img = map(combo_img, gt::html),
    combo_img2 = add_photo_frame(ast_per36_color, ast_per36_alternate_color, ast_per36_id, player_ast_per36),
    combo_img2 = map(combo_img2, gt::html),
    combo_img3 = add_photo_frame(oreb_per36_color, oreb_per36_alternate_color, oreb_per36_id, oreb_per36_player_name),
    combo_img3 = map(combo_img3, gt::html),
    combo_img4 = add_photo_frame(blk_per36_color, blk_per36_alternate_color, blk_per36_id, blk_per36_player_name),
    combo_img4 = map(combo_img4, gt::html)
  ) %>%
  select(ranking, combo_img,fg3a_per36, fg3a_ratio, combo_img2, ast_per36,
         combo_img3, oreb_per36, combo_img4,blk_per36) %>%
  gt() %>%
  fmt_number(
    columns = c(fg3a_per36, ast_per36, oreb_per36,blk_per36),  # O selecciona columnas con tidyselect
    decimals = 1 # Número de decimales que quieres mostrar
  ) %>%
  fmt_percent(
    columns = fg3a_ratio,
    decimals = 1
  ) %>%
  cols_label(
    ranking = "RK",
    combo_img = "",
    fg3a_per36 = "3FGA/36",
    fg3a_ratio = "3PAr",
    combo_img2 = "",
    ast_per36	 = "AST/36",
    combo_img3 = "",
    oreb_per36 = "ORB/36",
    combo_img4 = "",
    blk_per36 = "BLK/36"
  ) %>%
  cols_align(
    align = "right",
    columns = c(fg3a_per36,fg3a_ratio, ast_per36,
                oreb_per36, blk_per36)
  ) %>%
  tab_spanner(columns = combo_img:fg3a_ratio, "TRIPLE") %>%
  tab_spanner(columns = c(combo_img2,ast_per36), "ASISTENCIAS") %>%
  tab_spanner(columns = c(combo_img3, oreb_per36), "REBOTE OFENSIVO") %>%
  tab_spanner(columns = c(combo_img4,blk_per36), "TAPONES") %>%
  tab_options(
    heading.border.bottom.style = "none",
    table.border.top.style = "none", # transparent
    table.border.bottom.style = "none",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.color = "black",
    row_group.border.top.style = "none",
    row_group.border.top.color = "black",
    table.font.size =20,
    footnotes.font.size =15,
    heading.title.font.weight = "bold",
    column_labels.font.size = 11,
    column_labels.font.weight = "bold",
    source_notes.font.size = 17,
    data_row.padding = px(2.4),
    table_body.hlines.color = 'gray90',
    table.font.names = "Oswald",
    table.additional_css = ".gt_table {
                margin-bottom: 20px;
              }"
  )    %>%
  tab_header(
    title = md("<div style='display: flex; align-items: center; justify-content: center; height: 134px; text-align: center; font-weight: 600; font-size: 104px;'>
  <img src='https://baloncestofuenlabrada.com/wp-content/uploads/2024/10/BFuenlabradaLogo.png' style=' height: 134px;'>
  <span>Lideres en Sticky Stats</span></div>"),
    subtitle = md("<span style='display:block;text-align:middle;font-weight:400;color:#8C8C8C;font-size:28px'>
                    Jugadores con más de 50 minutos jugados | incluye todas las ligas de veranos 2025</span>")
  ) %>%
  tab_source_note(
    source_note = md(caption)
  ) |>
 gtsave(here::here("/Users/ivo/RStudio/ACB/2025/substack/summer_league/png/stikystats.png"),
         vwidth = 3000, vheight = 1500, expand = 100)




library(tidyverse)#el pandas de R
library(rvest) # para extraer los datos de las webs
library(janitor) # limpiar textos encabezados y subir una fila las tablas
library(ggtext) # para leer html
library(gt) # tabla
library(gtExtras) # accesorios para la tabla


# para dar un nombre mas corto a la fuente aunque en esta tabla no lo uso
#
# # sysfonts::font_add("fs", "/Users/ivo/Library/Fonts/Font Awesome 6 Free-Solid-900.otf")
# # sysfonts::font_add("fb", "Users/ivo/Library/Fonts/Font Awesome 6 Free-Regular-400.otf")
#
# font_files() |>
#   tibble() |>
#   filter(str_detect(family, "Roboto"))#para encontrar como se llama la fuente


# datos y data wrangling-------------------------------------------------------------------

# data of logos colors and names de la ACB

tm_acb <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/acbTeams2022/main/logos2024.csv") |>
  select(eq = abb, equipo, logo) %>%
  mutate( logo = ifelse(eq == "UNI", "https://static.acb.com/img/www/clubes2024/2324UnicajaLogo.png", logo))

caras <- read.csv("nombres_fotos_2024_caras.csv") |>
  select(jugador =nombre , foto, lic, codigo) %>%
  add_row(jugador = "Shannon Evans", codigo = 30001134, foto = "https://static.acb.com/media/PRO/00/00/52/36/66/0000523666_5-6_03.jpg", lic = "COT") %>%
  mutate(jugador =ifelse(jugador == "Lluís Costa", "Lluis Costa", jugador))



# listado de jugadores con sus fotos
#
#
# caras <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/acb/main/nombres_fotos_2024_1.csv") |>
#   select(jugador = nombre, lic, foto, codigo) %>%
#   mutate(lic = case_when(
#     str_detect(lic, "JFL") ~ "JFL",
#     str_detect(lic, "COT") ~ "COT",
#     str_detect(lic, "EXT") ~ "EXT",
#     str_detect(lic, "EUR") ~ "EUR",
#     TRUE ~ lic
#   ),
#   jugador = str_squish(jugador))


# tabla con la clasificación por equipo

clasificacion <- "https://www.acb.com/resultados-clasificacion/ver/temporada_id/2023/competicion_id/1/jornada_numero/28" |>
  read_html() |>
  html_element("table") |>
  html_table() |>
  janitor::clean_names() |>
  mutate(equipo = substr(equipo, 1, 3)) |> # las iniciales del equipo vienen unidas al nombre del equipo
  select(tm = equipo, v, d) |>
  mutate(wl = glue::glue("{v}-{d}")) |> # columna resumen
  select(tm, wl)

# tabla con los resultados

wins <- read.csv(here::here("2024", "PlayByPlay_2024", "csv", "iddf_df24.csv")) %>%
  filter(id_phase == 107) |>
  select(id_match = id, id_phase, local_team = local_team_team_abbrev_name, visitor_team = visitor_team_team_abbrev_name, score_local, score_visitor, matchweek_number)|>
  filter(matchweek_number >= 24) |>
  mutate(
    result = score_local - score_visitor
  ) %>%
  pivot_longer(contains("team"), names_to = "home_away", values_to = "team", names_prefix = ("team_")) %>%
  mutate(
    result = ifelse(home_away == "local_team", result, -result),
    win = ifelse(result > 0, 1, 0),
    outcome_icon = case_when(
      win == 1 ~ "<span style='font-size:25px;color:#29C458;font-family: \"Font Awesome 6 Free\"'>&#xf058;</span>", # esto son los puntos verdes si es uno punto verde
      win == 0 ~ "<span style='font-size:25px;color:#D31818;font-family: \"Font Awesome 6 Free\"'>&#xf057;</span>" # esto son los puntos rojos si es cero o sea derrota punto rojo
    )
  ) |>
  group_by(team) %>%
  rename(tm = team) %>%
  summarise(
    Wins = length(win[win == 1]),
    Losses = length(win[win == 0]),
    outcomes = list(outcome_icon),
    .groups = "drop"
  )



# outcome_icon = case_when(w==1 ~ "<span style='font-family:fs;color:#29C458;'>&#xf058;</span>",
#                          d==1 ~ "<span style='font-family:fs;color:#6F6F6F;'>&#xf056;</span>",
#                          l==1 ~ "<span style='font-family:fs;color:#D31818;'>&#xf057;</span>"))|>

# extraer calendario por equipo -------------------------------------------

url <- "https://www.proballers.com/es/baloncesto/liga/30/spain-liga-endesa/equipos" |>
  read_html()


cal <- tibble(
  calendario = url |> html_elements("a[title='Calendário']") |>
    html_attr("href")
) |>
  mutate(
    calendario = paste0("https://www.proballers.com", calendario),
    cod = str_extract(calendario, "[0-9]+"),
    eq = case_when( # para que coincida con el archivo de los logos
      cod == 155 ~ "BKN",
      cod == 13356 ~ "GIR",
      cod == 405 ~ "BAX",
      cod == 671 ~ "CAZ",
      cod == 2111 ~ "MBA",
      cod == 218 ~ "COV",
      cod == 148 ~ "BAR",
      cod == 1562 ~ "ZPA",
      cod == 157 ~ "DGC",
      cod == 154 ~ "JOV",
      cod == 917 ~ "LNT",
      cod == 1986 ~ "MOB",
      cod == 160 ~ "RMB",
      cod == 149 ~ "BRE",
      cod == 844 ~ "SBB",
      cod == 457 ~ "UCM",
      cod == 161 ~ "UNI",
      cod == 153 ~ "VBC",
      TRUE ~ cod
    )
  )



p <- cal$calendario

rival <- function(p) {
  url <- p |>
    read_html()
  df <- tibble(
    logo = url |> html_elements(".show img") |>
      html_attr("src")
  ) |>
    mutate(equip = str_extract(logo, "[0-9]+"))
  df$equip2 <- p
  return(df)
}

pdf <- map_df(p, rival)

# divido en partidos jugados 3 ultimos o los que sean

calen_pas <- pdf |>
  mutate(equip2 = str_extract(equip2, "[0-9]+")) |>
  group_by(equip2) |>
  slice(24:28) |>
  left_join(cal |> select(cod, tm = eq), by = c("equip2" = "cod")) |> # incluyo los logos al equipo
  left_join(cal |> select(cod, tm1 = eq), by = c("equip" = "cod")) |> # incluyo los logos de los rivales de ese equipo
  left_join(tm_acb |> select(tm1 = eq, logo1 = logo)) |>
  ungroup() |>
  group_by(tm) |>
  summarise(logo_p = list(logo1)) |>
  ungroup()

# divido en partidos por jugar  5 proximos o los que sean

calen_futur <- pdf |>
  mutate(equip2 = str_extract(equip2, "[0-9]+")) |>
  group_by(equip2) |>
  slice(29:34) |>
  left_join(cal |> select(cod, tm = eq), by = c("equip2" = "cod")) |> # incluyo los logos al equipo
  left_join(cal |> select(cod, tm1 = eq), by = c("equip" = "cod")) |> # incluyo los logos de los rivales de ese equipo
  left_join(tm_acb |> select(tm1 = eq, logo1 = logo)) |>
  ungroup() |>
  group_by(tm) |>
  summarise(logo_f = list(logo1)) |>
  ungroup()


# valoracion media por jugador este scrap no le digo a nadie como se hace ;)--------------------------------------------


jor <- c(1:28)

jornada <- function(jor) {
  url <- "https://www.rincondelmanager.com/smgr/valoracion.php"
  pgsession <- session(url)
  pgform <- html_form(pgsession)[[5]]
  filled_form <- html_form_set(pgform,
                               "de_jor" = "1",
                               "a_jor" = jor,
                               "cancha" = "2"
  )

  df <- session_submit(pgsession, form = filled_form, POST = url)
  df <- df %>%
    html_element(glue::glue("table#tTodos")) %>%
    html_table(dec = ",") |>
    mutate(Broker = as.numeric(str_replace_all(Broker, "\\.", ""))) |>
    janitor::clean_names() |>
    mutate(eq = case_when(
      eq == "CBG" ~ "COV",
      eq == "OBR" ~ "MOB",
      eq == "RMA" ~ "RMB",
      eq == "BAS" ~ "BKN",
      eq == "FCB" ~ "BAR",
      eq == "CAN" ~ "LNT",
      eq == "ZAR" ~ "CAZ",
      eq == "BLB" ~ "SBB",
      eq == "MUR" ~ "UCM",
      eq == "MAN" ~ "BAX",
      eq == "PAL" ~ "ZPA",
      eq == "GCA" ~ "DGC",
      eq == "AND" ~ "MBA",
      TRUE ~ eq))|>
    pivot_longer(names_to = "jornada",
                 values_to = "val",
                 cols =c(paste0("j",jor):paste0("j",jor))) |>
    arrange(desc(jornada))


  return(df)
}





pos <- c("Bases", "Aleros", "Pívots") # en la pagina hay una pestaña por jugador

recPosloc <- function(pos) {
  url <- "https://www.rincondelmanager.com/smgr/valoracion.php"
  pgsession <- session(url)
  pgform <- html_form(pgsession)[[5]]
  filled_form <- html_form_set(pgform,
                               "de_jor" = "1",
                               "a_jor" = "28",
                               "cancha" = "2"
  )

  df <- session_submit(pgsession, form = filled_form, POST = url)
  df <- df %>%
    html_element(glue::glue("table#t{pos}")) %>%
    html_table(dec = ",") |>
    mutate(Broker = as.numeric(str_replace_all(Broker, "\\.", ""))) |>
    janitor::clean_names() |>
    mutate(eq = case_when(
      eq == "CBG" ~ "COV",
      eq == "OBR" ~ "MOB",
      eq == "RMA" ~ "RMB",
      eq == "BAS" ~ "BKN",
      eq == "FCB" ~ "BAR",
      eq == "CAN" ~ "LNT",
      eq == "ZAR" ~ "CAZ",
      eq == "BLB" ~ "SBB",
      eq == "MUR" ~ "UCM",
      eq == "MAN" ~ "BAX",
      eq == "PAL" ~ "ZPA",
      eq == "GCA" ~ "DGC",
      eq == "AND" ~ "MBA",
      TRUE ~ eq), media_sm = as.numeric(media_sm),
      pos = paste0(pos)
    )


  return(df)
}

recPosloc_df <- map_df(pos, recPosloc)


df1 <- recPosloc_df |>
  arrange(desc(media_sm)) |>
  pivot_longer(
    names_to = "jornada",
    values_to = "val",
    cols = c("j24":"j28")
  ) |>
  mutate(jornada = as.numeric(str_remove(jornada, "j"))) |>
  arrange(jornada) |>
  ungroup() |>
  group_by(jugador, jug) |>
  #filter(jugador != "Juan Fernández") |>
  summarise(
    val = list(val),
    .groups = "drop"
  )





# tabla final -------------------------------------------------------------


df <- recPosloc_df |>
  #filter(!jugador %in% c("Jaylen Hands", "Jacob Wiley")) %>%
  group_by(pos) |>
  arrange(desc(ef)) |>
  drop_na() |>
  slice(1:5) |>
  ungroup() |>
  arrange(match(pos, c("Bases", "Aleros", "Pívots"))) |>
  select(jugador:media_sm, ef:reg, pos) |>
  rename(tm = eq) |>
  left_join(df1) |>
  left_join(calen_pas) |>
  left_join(calen_futur) |>
  left_join(clasificacion) |>
  left_join(caras) |>
  left_join(tm_acb %>% rename(tm = eq)) |>
  left_join(wins) |>
  mutate(foto_num = str_replace(foto, ".*[/]([^.]+)[.].*", "\\1"),
         foto = paste0("/Users/ivo/R/RStudio/ACB/2024/headshots_2024/pngheadshots2024_circulares", "/",foto_num ,"_",codigo,".png"),
    wl2 = paste0(Wins, "-", Losses)
  )

# coordPolar funcion --------------------------------------------------------------esto es lo que hace los radares
library(geomtextpath)
players <- df$jug # listado de jugadores
c_cir <- recPosloc_df |>
  select(jug, eq, media, rent, ef, reg) |>
  mutate(across(c(media:reg), rank) / nrow(recPosloc_df) * 100) |> # esto crea el percentil
  filter(jug %in% c(players)) |>
  rename(MED = media, EF = ef, RENT = rent, REG = reg) |>
  pivot_longer(-c(jug:eq), names_to = "stats", values_to = "value") |>
  mutate(juga = jug)


circle_df <- function(name) {
  segments <- data.frame(
    x1 = rep(0, 5),
    x2 = rep(3.5, 5),
    y1 = c(0, 25, 50, 75, 100),
    y2 = c(0, 25, 50, 75, 100)
  )

  labels <- data.frame(
    y = c(25, 50, 75, 100),
    x = rep(0.25, 4)
  )

  df1 <- c_cir |>
    filter(jug == name) |>
    ggplot(aes(x = stats, y = value, fill = stats, alpha = .67)) +
    coord_polar() +
    theme_void() +
    geom_textpath(
      inherit.aes = FALSE,
      mapping = aes(x = stats, label = stats, y = 130),
      fontface = "bold", upright = TRUE, text_only = TRUE, size = 13
    ) +
    geom_segment(
      inherit.aes = FALSE,
      data = segments,
      mapping = aes(x = x1, xend = x2, y = y1, yend = y2), linewidth = 0.45
    ) +
    geom_col(width = .85, show.legend = FALSE) +
    scale_y_continuous(limits = c(-10, 130)) +
    geom_textsegment(
      inherit.aes = FALSE,
      data = labels,
      mapping = aes(x = 3.5, xend = 4.5, y = y, yend = y, label = y),
      linewidth = 0.35, size = 2.5
    ) +
    scale_fill_manual(values = c("#E1341A", "#FF903B", "#ffe850", "#4bd8ff"))
  return(df1)
}

# funcion ivo_sparkline (esto es un proyecto sin acabar---------------------------------------------------
#
# dollar<- ggplot(data,aes(x=Year,y=Dollar))+
#   geom_line(size=2.5,color="#fcc425")+
#   geom_point(size=6)
#
#
#
#
# #
#
# p11 <- recPosloc_df%>%
#   drop_na() |>
#   select(jug, j10:j12) |>
#   pivot_longer(-jug) |>
#   filter(jug == "T. Carter")
# p11 |>
#    ggplot(p11, aes(x=name,y=value, group = 1))+
# geom_line(size=2.5,color="#fcc425")+
#   geom_text(aes(y = value+.5, label = value))+
#
#   geom_ribbon(aes(ymin = min(value), ymax = value), fill = "#fcc425", alpha =.65)+
#   geom_point(size=4)+
#   theme_void()


# tabla -------------------------------------------------------------------



combine_word <- function(jug, tm, logo, wl2) { # funcion que une los logos el nombre y las victorias en formato html
  glue::glue(
    "<div style='line-height:20px'><span style='font-weight:bold;font-variant:small-caps;horizontal-align:left;font-size:23px'>{jug}</span></div>
        <div style='font-size:20px;line-height:18px;vertical-align:middle;horizontal-align:left'><span style ='font-weight:bold;color:grey;font-size:20px'><img src='{logo}'
    style='width:26px;vertical-align:middle;horizontal-align:left'> {wl2} {tm}</span></div>"
  )
}

path <- here::here("2024","basicos 2024", "png", "radaresJ29")#esto es donde lo guardo en el equipo


tibble_plot <- df %>%
  mutate(plot = map(jug, circle_df)) %>%
  mutate(ggplot = NA) # aqui es donde esta la magia

tibble_plot %>%
  mutate(plot = map(jug, circle_df)) %>%
  mutate(
    combo = combine_word(jug, tm, logo, wl2),
    combo = map(combo, gt::html),
    outcomes = map(outcomes, gt::html)
  ) %>%
  select(foto, combo, pos, broker ,ef, rent, reg,media_sm, val, ggplot,  logo_p, outcomes,   logo_f) %>%
  gt(groupname_col = "pos") %>%
 cols_nanoplot(columns = val,
               new_col_label = md("<span style='font-weight:bold;font-size:30px'> *Valoración* </span>"),
               after = "media_sm",
               options = nanoplot_options(reference_area_fill_color  = "#FFFFFF",
                                          data_line_stroke_color = "black"),
                  ) %>%
   cols_hide(val) %>%
  text_transform(
    locations = cells_body(columns = c(ggplot)),
    fn = function(x) {
      map(tibble_plot$plot, ggplot_image, height = px(90), aspect_ratio = 1)
    }
  ) %>%
  gt_img_rows(height =65, columns = foto, img_source  = "local") %>%
  gt_img_multi_rows(height = 55, columns = c(logo_p)) |>
  gt_img_multi_rows(height = 55, columns = c(logo_f)) |>

   fmt_currency(placement = "right",
                columns = c(broker),
                currency = "EUR",
                use_subunits = FALSE,
                sep_mark ="."

   ) %>%
  cols_label(
    foto = "",
    combo = "",
    broker = gt::html("<span style='font-weight:bold;font-size:30px'>💰br🤑ker💰</span>"),
    media_sm = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:30px'>MED u3</span>"),
    val = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:30px'>VALORACIÓN u3</span>"),
    outcomes = gt::html("<span style='font-weight:bold;font-size:30px;color:#29C458;'>Win </span>-<span style='
                       font-weight:bold;font-size:30px;color:#D31818;'> Loss</span>"),
    ggplot = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:30px'>Radar</span>"),
    logo_p = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:30px'>últimos</span>"),
    ef = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:30px'>EF</span>"),
    rent = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:30px'>RENT</span>"),
    reg = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:30px'>REG</span>"),
    logo_f = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:30px'>próximos 🖐👆</span>")
  ) |>
  cols_align(
    align = "left",
    columns = c(combo, ggplot)
  ) |>
  cols_align(
    align = "center",
    columns = c(-combo)
  ) %>%
  cols_width(
    c(foto) ~ px(56)
  ) %>%
  cols_width(
    columns =
      c(ef) ~ px(86)
  ) %>%
  cols_width(
    columns =
      c(ggplot, val) ~ px(126)
  ) %>%
  cols_width(
    columns =
      c(logo_p, logo_f) ~ px(400)
  ) |>
  gt_color_rows(ef,
                palette = c("white", "#3494dc"),
                domain = c(min(df$ef), max(df$ef))
  ) |>
  gtExtras::gt_theme_espn() |>
   tab_options(heading.align  = "center",
               table.font.names = "Roboto Condensed",
               table.background.color = "white",
               table.font.size = 25,
               data_row.padding = px(1),
               source_notes.font.size = 25,
               table.additional_css = ".gt_table {
                margin-bottom: 50px;
              }
                    #title-text {

                padding-bottom: 2px;
                margin-top: -10px;


               }"
   ) %>%
  tab_header(#mucho html y css, lo de md es para que lea el html
    title = md("<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Liga_Endesa_2019_logo.svg/800px-Liga_Endesa_2019_logo.svg.png'
               style='height:175px;'><img src='https://raw.githubusercontent.com/IvoVillanueva/acb_logo/main/Logo%20SM%20mosca%20340x340.png'
               style='height:175px;'><br><span style='font-weight:bold;font-size:74px'><br>Top 15 Mejor Estado de Forma (EF) Por Posición Últimos 5 partidos</span>"),
    subtitle = md("<span style='font-weight:400;color:#8C8C8C;font-size:49px'>Jugadores que han jugado las 5 Últimas Jornadas | SuperManager Desde la J24 a la J28<br><br></span>")
  ) %>%
  tab_source_note(
    source_note = md(paste0(" **Datos**: *@elrincondelsm* &nbsp;&nbsp; <img src='https://www.rincondelmanager.com/smgr/images/logo.png'
                     style='height:20px;'>, &nbsp;&nbsp;*@Proballers_com*&nbsp;&nbsp;<img src='https://www.proballers.com/images/proballers-blue.svg'
                     style='height:20.5px;'>,  *@ACBCOM*&nbsp;&nbsp;<img src='https://upload.wikimedia.org/wikipedia/commons/3/3f/Acb_2019_logo.svg'
                     style='height:20.5px;'><br>",
                     caption))
  ) %>%

gt::gtsave(glue::glue("{path}.png"), vwidth = 2600, vheight = 995,  expand =60)#en el path puedes cambiar lo que tu quieras

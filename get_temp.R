######################################################
#
# 47都道府県と観測所を指定して、
# 気温観測を開始した年から2023年までの各日の気象データ取得
#
######################################################


pacman::p_load(
  jmastats,
  tidyverse,
  janitor,
  here,
  ggridges,
  ggthemes,
  tools,
  sf,
  stringi)


PREF.list <- read.csv("https://raw.githubusercontent.com/igproj-fusion/R-gis/main/Prefecture_list.csv")

### confirm Prefecture ###
PREF.list |> 
  mutate(PREF_en = toTitleCase(tolower(PREF_en))) |>
  pull(PREF_en)

### Set Prefecture ###
PREFECTURE = "Chiba"


PREF.code <- PREF.list |> 
  mutate(CODE = formatC(CODE, width = 2, flag = "0")) |>
  mutate(PREF_en = toTitleCase(tolower(PREF_en))) |> 
  filter(PREF_en == PREFECTURE) |> 
  pull(CODE)


data("stations", package = "jmastats")

### confirm Station Name ###
stations |> 
  filter(pref_code == PREF.code) |> 
  filter(station_type %in% c("四", "三", "官")) |>
  group_by(block_no) |>
  distinct(block_no, .keep_all = TRUE) |> 
  mutate(Latn = 
           stri_trans_general(katakana, "Any-Latn")) |> 
  mutate(Latn = toTitleCase(tolower(Latn))) |> 
  pull(Latn)

### Set Station Name ###
STATION = "Tateyama"


BLOCK_NO <- stations |> 
  filter(pref_code == PREF.code) |> 
  filter(station_type %in% c("四", "三", "官")) |>
  group_by(block_no) |>
  distinct(block_no, .keep_all = TRUE) |> 
  mutate(Latn = 
           stri_trans_general(katakana, "Any-Latn")) |> 
  mutate(Latn = toTitleCase(tolower(Latn))) |> 
  filter(Latn == STATION) |>
  pull(block_no)


START.year <- 
  jma_collect(item = "annually",
              block_no = BLOCK_NO,
              year = 2023,
              month = 1,
              cache = FALSE) |> 
  filter(!is.na(temperature$`average(℃)`)) |> 
  filter(year == min(year)) |> 
  pull(year)

END.year <- 2023


temp.df <- set_names(START.year:END.year) |> 
  map_dfr(\(YEAR) 
          set_names(1:12) |> 
            map(\(MONTH) 
                jma_collect(item = "daily",
                            block_no = BLOCK_NO,
                            year = YEAR, 
                            month = MONTH, 
                            cache = FALSE)) |> 
            bind_rows()) 


df.name <- paste0(STATION, "_", START.year, "_", END.year)
RData.name <- paste0(df.name, ".RData")
assign(df.name, temp.df)

save(df.name, file = here("data", RData.name))

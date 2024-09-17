######################################################
#
# add 2024 data
#
######################################################


pacman::p_load(
  jmastats,
  units,
  tidyverse,
  janitor,
  here,
  tools,
  stringi)


PREF.list <- read.csv("https://raw.githubusercontent.com/igproj-fusion/R-gis/main/Prefecture_list.csv") |> 
  mutate(PREF_en = toTitleCase(tolower(PREF_en)))


### confirm Prefecture ###
PREF <- PREF.list |> pull(PREF_en)
PREF.NO <- menu(PREF, title = "Select one prefecture")


### Set Prefecture ###
PREFECTURE <- PREF[PREF.NO]


PREF.code <- PREF.list |> 
  mutate(CODE = formatC(CODE, width = 2, flag = "0")) |>
  filter(PREF_en == PREFECTURE) |> 
  pull(CODE)


data("stations", package = "jmastats")
stations.temp <- stations |>
  filter(station_type %in% c("四", "三", "官")) |>
  filter(pref_code == PREF.code) |> 
  group_by(block_no) |>
  distinct(block_no, .keep_all = TRUE) |> 
  mutate(Latn = 
           stri_trans_general(katakana, "Any-Latn")) |> 
  mutate(Latn = toTitleCase(tolower(Latn))) 


### confirm Station Name ###
STAT <- stations.temp |> pull(Latn)
STAT.NO <- menu(STAT, title = "Select one station")


### Set Station Name ###
STATION = STAT[STAT.NO]


BLOCK_NO <- stations.temp |>
  filter(Latn == STATION) |>
  pull(block_no)


df <- set_names(1:12) |>
  map(\(MONTH) {
    result <- tryCatch(
      {
        jma_collect(item = "daily",
                    block_no = BLOCK_NO,
                    year = 2024, 
                    month = MONTH,
                    cache = FALSE)
        },
      error = function(e) {
        message(paste0("\nError in collecting data for Month: ", MONTH))
        return(NULL)
        }
      )
    if (!is.null(result) && !is.data.frame(result)) {
      result <- as.data.frame(result)
      }
    result
    }) |>
  compact() |> 
  bind_rows()


##############################################################


START.year.org <- 
  jma_collect(item = "annually",
              block_no = BLOCK_NO,
              year = 2023,
              month = 1,
              cache = FALSE) |> 
  filter(!is.na(temperature$`average(℃)`)) |> 
  filter(year == min(year)) |> 
  pull(year)

END.year.org <- 2023


RDS.name <- paste0(PREFECTURE, STATION, "_", START.year.org, 
                   "_", END.year.org, ".rds")


tmp_dir <- tempdir()
RDS_file <- file.path(tmp_dir, RDS.name)
URL0 <- "https://raw.githubusercontent.com/igproj-fusion/JMAstats/main/Rds/"
URL <- paste0(URL0, RDS.name)
download.file(URL, destfile = RDS_file)

df.all <- readRDS(file = RDS_file) |> 
  rbind(df)

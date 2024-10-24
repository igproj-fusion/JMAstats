pacman::p_load(
  jmastats,
  tidyverse,
  here,
  tools,
  stringi,
  rvest)



############################################################
#
# https://github.com/igproj-fusion/JMAstats/tree/main/Rds47
# ここに都県ごとにRdsファイルが置いてある
# そのリストを取得する
#
############################################################

github.rds.url = "https://github.com/igproj-fusion/JMAstats/tree/main/Rds47"
github.dir = "/igproj-fusion/JMAstats/blob/main/Rds47/"

result <- data.frame(value = NULL)
while(length(result$value) == 0){
  Sys.sleep(5)

  result <- read_html(github.rds.url) |>
    html_nodes("a") |>
    html_attr("href") |>
    str_subset("\\.rds") |> 
    as_tibble() |>
    distinct(value, .keep_all = TRUE)
}


Today <- today() |> 
  print() |> 
  as_tibble() |> 
  mutate(Year = substring(value, 1, 4)) |> 
  mutate(Month = substring(value, 6, 7))

ThisYear <- as.integer(Today$Year)
ThisMonth <- as.integer(Today$Month)

URL.dl <- "https://raw.githubusercontent.com/igproj-fusion/JMAstats/main/Rds47/"

rds.file <- result |> 
  mutate(value2 = gsub(github.dir, "", value)) |> 
  mutate(value3 = gsub("JMA_", "", value2)) |>
  mutate(value3 = gsub(".rds", "", value3)) |>
  separate(
    value3,
    into = c("PREFECTURE", "START.ym"),
    sep = "_",
    remove = TRUE) |>
  mutate(Year = as.integer(substring(START.ym, 1, 4))) |> 
  mutate(Month = as.integer(substring(START.ym, 5, 6))) |> 
  mutate(URL_DL = paste0(URL.dl, value2)) |> 
  mutate(FLAG = ifelse(ThisMonth - Month >= 2,
                       1, 0)) |> 
  mutate(FLAG = ifelse(ThisYear > Year, 1, FLAG)) |> 
  filter(FLAG == 1) |> 
  mutate(nextYear = Year) |> 
  mutate(nextMonth = Month + 1) |> 
  mutate(nextYear = ifelse(nextMonth == 13,
                           Year + 1, nextYear)) |> 
  mutate(nextMonth = ifelse(nextMonth == 13, 
                            nextMonth - 12, nextMonth)) |> 
  mutate(newRDS = paste0("JMA_", PREFECTURE, "_", nextYear, 
                         formatC(nextMonth, 
                                 width = 2, flag = "0"), ".rds"))

                
############################################################
#
# 更新
#
############################################################

N <- nrow(rds.file)
if(N != 0){
  
  tmp_dir <- tempdir()
  RDS_file <- file.path(tmp_dir, rds.file$value2)

  for(i in 1:N){
    download.file(rds.file$URL_DL[i], destfile = RDS_file[i]) 
    RDS.org <- readRDS(RDS_file[i]) 
    
    PREFECTURE <- rds.file$PREFECTURE[i]
    
    block_no <- RDS.org |> 
      dplyr::select(block_no) |> 
      unique() |> 
      pull()
    
    df <- set_names(block_no) |>
      map(\(BLOCK_NO) {
        result <- tryCatch(
          {
            jma_collect(item = "daily",
                        block_no = BLOCK_NO,
                        year = rds.file$nextYear[i], 
                        month = rds.file$nextMonth[i],
                        cache = FALSE,
                        quiet = TRUE)
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
        result |> 
          mutate(PREFECTURE = PREFECTURE) |> 
          mutate(block_no = BLOCK_NO)
      }) |>
      compact() |>
      bind_rows()
    
    df.all <- bind_rows(RDS.org, df)
    RDS.name <- here("data/pref-latestRds",
                          rds.file$newRDS[i])
    saveRDS(df.all, file = RDS.name)
  }
}


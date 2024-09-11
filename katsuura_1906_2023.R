pacman::p_load(
  jmastats,
  tidyverse,
  janitor)


BLOCK_NO <- "47674"
YEAR_range <- 1906:2023
MONTH_range <- 1:12

katsuura_1906_2023 <- set_names(YEAR_range) |> 
  map_dfr(\(YEAR) 
    set_names(MONTH_range) |> 
      map(\(MONTH) 
          jma_collect(item = "daily",
                      block_no = BLOCK_NO,
                      year = YEAR, 
                      month = MONTH, 
                      cache = FALSE)) |> 
      bind_rows()) |>
  unnest(cols = c(pressure, precipitation, 
                  temperature, humidity, wind,
                  sunshine, snow, weather_time)) |> 
  clean_names()


save(katsuura_1906_2023, 
     file = hear("data", "katsuura_1906_2023.RData"))

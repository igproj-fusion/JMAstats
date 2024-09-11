pacman::p_load(
  jmastats,
  tidyverse,
  janitor,
  here,
  ggridges,
  ggthemes)




BLOCK_NO <- "47674"
YEAR_range <- 2024:2024
MONTH_range <- 1:9

katsuura_2024 <- set_names(YEAR_range) |> 
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


tmp_dir <- tempdir()
rData_file <- file.path(tmp_dir, "katsuura_1906_2023.RData")
URL <- "https://github.com/igproj-fusion/JMAstats/raw/main/katsuura_1906_2023.RData"
download.file(URL, destfile = rData_file)

load(rData_file)                     
katsuura.df <- rbind(katsuura_1906_2023, katsuura_2024)




df <- katsuura.df |>
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         yrday = yday(date))

colors <- ggokabeito::palette_okabe_ito()
colors[2] <- "cornflowerblue"


month_labs <- seq(as.Date("2001/1/1"), 
                  as.Date("2001/12/1"), "months") |>
  as_tibble() |> 
  mutate(yrday = yday(value)) |> 
  mutate(month_lab = month(value, label = TRUE, abbr = TRUE)) |> 
  select(yrday, month_lab) |> 
  rbind(data.frame(yrday = 365, month_lab = "Dec31"))


avg <- df |>
  dplyr::filter(year > 1990 & year < 2021) |>
  group_by(yrday) |>
  dplyr::filter(yrday != 366) |>
  summarize(mean_9120 = mean(average_c, na.rm = TRUE),
            sd_9120 = sd(average_c, na.rm = TRUE)) |>
  mutate(fill = colors[2],
         color = colors[2])

df_out <- df |>
  #dplyr::filter(year > 1980) |>
  mutate(year_flag = case_when(
    year == 2023 ~ "2023",
    year == 2024 ~ "2024",
    .default = "All other years"))


today <- df |>
  dplyr::filter(year == 2024) |> 
  dplyr::filter(month == max(month)) |> 
  dplyr::filter(!is.na(average_c)) |> 
  dplyr::filter(day == max(day))

LastDate <- gsub("-", "/", gsub("-0", "/", today$date))


yIntercept <- c(0, 10, 20, 30)

ggplot() +
  geom_hline(yintercept = yIntercept, color = "gray60", 
             linetype = "dashed") +
  geom_line(data = avg,
            mapping = aes(x = yrday,
                          y = mean_9120,
                          color = color),
            linewidth = 1.5,
            inherit.aes = FALSE) +
  scale_color_identity(name = "Mean Temp. 1991-2020", guide = "legend",
                       breaks = unique(avg$color), labels = "") +
  scale_fill_identity(name = "Mean Temp. 1991-2020", guide = "legend",
                      breaks = unique(avg$fill), labels = "") +
  ggnewscale::new_scale_color() +
  geom_line(data = df_out,
            mapping = aes(x = yrday, y = average_c, group = year, color = year_flag),
            inherit.aes = FALSE, alpha = 0.5) +
  scale_color_manual(values = c("darkorange", "darkmagenta", "grey80")) +
  scale_x_continuous(breaks = month_labs$yrday, 
                     labels = month_labs$month_lab) +
  scale_y_continuous(breaks = seq(0, 30, 10),
                     limits = c(-5, 30),
                     expand = expansion(mult = c(-0.05, 0.05))) +
  geom_line(linewidth = rel(1)) +
  geom_line(data = avg,
            mapping = aes(x = yrday,
                          y = mean_9120),
            linewidth = 1.5, color = "cornflowerblue") +
  geom_line(data = df |> dplyr::filter(year == 2023), 
            aes(x = yrday, y = average_c),
            color = "darkorange", linewidth = 0.7, alpha = 0.7) +
  geom_line(data = df |> dplyr::filter(year == 2024),
            aes(x = yrday, y = average_c),
            color = "darkmagenta", linewidth = 0.7, alpha = 0.7) +
  geom_point(data = today, aes(x = yrday, y = average_c),
             color = "darkmagenta", size = 2) +
  theme_classic() +
  guides(
    x = guide_axis(cap = "both"),
    y = guide_axis(minor.ticks = TRUE, cap = "both"),
    color = guide_legend(override.aes = list(linewidth = 1.5))
  ) +
  labs(x = "", y = "Mean Temperature (°Celsius)",
       color = "Year",
       title = "Mean Daily Temperature, Katsuura, Chiba",
       subtitle = paste0("1906/1/1~", 
                         LastDate),
       caption = "https://www.data.jma.go.jp/obd/stats/etrn/index.php") +
  theme(plot.margin= unit(c(1, 1, 1, 1), "lines"),
        axis.line = element_line(color = "gray30", linewidth = rel(1)),
        plot.title = element_text(size = rel(1.4)),
        plot.subtitle = element_text(size = rel(1.1)),
        plot.caption = element_text(size = rel(1.0)),
        axis.text = element_text(size = rel(0.9)),
        legend.position = "top")

df <- df.all |>
  select(date, temperature) |> 
  unnest(temperature) |> 
  clean_names() |> 
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
  filter(year > 1990 & year < 2021) |>
  group_by(yrday) |>
  filter(yrday != 366) |>
  summarize(mean_9120 = mean(average_c, na.rm = TRUE),
            sd_9120 = sd(average_c, na.rm = TRUE)) |>
  mutate(fill = colors[2],
         color = colors[2])

df_out <- df |>
  mutate(year_flag = case_when(
    year == 2023 ~ "2023",
    year == 2024 ~ "2024",
    .default = "All other years"))

lastDay <- df |>
  filter(date >= today() - 7) |> 
  filter(!is.na(average_c)) |>
  filter(date == max(date))

LastDate <- gsub("-", "/", lastDay |> pull(date))



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
  scale_y_continuous(breaks = seq(0, 35, 10),
                     limits = c(-5, 30),
                     expand = expansion(mult = c(-0.05, 0.05))) +
  geom_line(linewidth = rel(1)) +
  geom_line(data = avg,
            mapping = aes(x = yrday,
                          y = mean_9120),
            linewidth = 1.5, color = "cornflowerblue") +
  geom_line(data = df |> filter(year == 2023), 
            aes(x = yrday, y = average_c),
            color = "darkorange", linewidth = 0.7, alpha = 0.7) +
  geom_line(data = df |> filter(year == 2024),
            aes(x = yrday, y = average_c),
            color = "darkmagenta", linewidth = 0.7, alpha = 0.7) +
  geom_point(data = lastDay, aes(x = yrday, y = average_c),
             color = "darkmagenta", size = 2) +
  theme_classic() +
  guides(
    x = guide_axis(cap = "both"),
    y = guide_axis(minor.ticks = TRUE, cap = "both"),
    color = guide_legend(override.aes = list(linewidth = 1.5))) +
  labs(x = "", y = "Mean Temperature (°Celsius)",
       color = "Year",
       title = paste0("Mean Daily Temperature: ", STATION, ", ", PREFECTURE),
       subtitle = paste0(gsub("-", "/", df$date[1]), "~", LastDate),
       caption = "https://www.data.jma.go.jp/obd/stats/etrn/index.php") +
  theme(plot.margin= unit(c(1, 1, 1, 1), "lines"),
        axis.line = element_line(color = "gray30", linewidth = rel(1)),
        plot.title = element_text(size = rel(1.4)),
        plot.subtitle = element_text(size = rel(1.1)),
        plot.caption = element_text(size = rel(1.0)),
        axis.text = element_text(size = rel(0.9)),
        legend.position = "top")

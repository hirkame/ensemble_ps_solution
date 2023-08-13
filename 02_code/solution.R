# Load --------------------------------------------------------------------

data <- vroom::vroom(file = "01_data/owid-co2-data.csv")



# Filter ------------------------------------------------------------------

data <- data |> 
  dplyr::filter(
    year %% 5 == 0
  )

g7_countries <- c("United States", "United Kingdom", "Germany", "France", "Japan", "Canada", "Italy")

data <- data |> 
  dplyr::filter(
    country %in% g7_countries
  )

data <- data |> 
  dplyr::filter(
    year >= min(year[!is.na(gdp) & !is.na(co2_per_capita)]) & 
      year <= max(year[!is.na(gdp) & !is.na(co2_per_capita)]),
    .by = country
  ) 



# Identify NAs ------------------------------------------------------------------

table <- data |>
  dplyr::summarise(
    n = dplyr::n(),
    missing_gdp = sum(is.na(gdp)), 
    missing_co2 = sum(is.na(co2)), 
    missing_years_gdp = paste(year[is.na(gdp)], collapse = ", "),
    missing_years_co2 = paste(year[is.na(co2)], collapse = ", "),
    .by = "country"
  )

table



# Mutate ------------------------------------------------------------------

data <- data |> 
  dplyr::mutate(
    gdp_per_capita = gdp / population,
    .after = gdp
  ) |> 
  dplyr::mutate(
    min_year = min(year[!is.na(gdp) & !is.na(co2_per_capita)]),
    max_year = max(year[!is.na(gdp) & !is.na(co2_per_capita)]),
    .by = country,
    .after = year
  )

# Other solutions to mutate min_year and max_year columns
# data <- data |>
#   dplyr::filter(!is.na(gdp) & !is.na(co2_per_capita)) |>
#   dplyr::summarise(
#     min_year = min(year),
#     max_year = max(year),
#     .by = country
#   ) |>
#   dplyr::right_join(
#     data,
#     by = "country"
#   ) |>
#   dplyr::mutate(
#     gdp_per_capita = gdp / population,
#     .after = gdp
#   )



# Plot --------------------------------------------------------------------

ggplot2::ggplot(data, ggplot2::aes(x = log(gdp_per_capita), y = co2_per_capita, group = country)) + 
  ggplot2::geom_point(na.rm = FALSE) + 
  ggplot2::geom_path(na.rm = FALSE) + 
  ggplot2::facet_wrap(country ~ .) +  
  ggplot2::scale_x_continuous(
    limits = c(log(1000), log(55000)),
    labels = ~ round(exp(.x) / 1000),
    breaks = c(log(1000), log(2000), log(4000), log(8000), log(16000), log(32000), log(64000))
  ) +
  ggrepel::geom_label_repel(
    ggplot2::aes(label = ifelse(year == min_year | year == max_year, as.character(year), "")),
    size = 3,
    max.overlaps = Inf
  ) +
  ggplot2::labs(x = "GDP per Capita ($1,000)", y = "CO2 per Capita") 


# Plot version 2 --------------------------------------------------------------------

ggplot2::ggplot(data, ggplot2::aes(x = log(gdp_per_capita), y = co2_per_capita, group = country)) + 
  ggplot2::geom_point(na.rm = FALSE, color = "#3C8DAD") + 
  ggplot2::geom_path(na.rm = FALSE, color = "#3C8DAD") + 
  ggplot2::facet_wrap(country ~ .,
                      scales = "free"
                      ) +  
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.border       = ggplot2::element_blank(),
    axis.line.x.bottom = ggplot2::element_line(color = 'black'),
    axis.line.y.left   = ggplot2::element_line(color = 'black'),
    strip.background   = ggplot2::element_blank(),
    strip.text         = ggplot2::element_text(size = 10)
  ) +
  ggplot2::scale_x_continuous(
    limits = c(log(1000), log(55000)),
    labels = ~ round(exp(.x) / 1000),
    breaks = c(log(1000), log(2000), log(4000), log(8000), log(16000), log(32000), log(64000))
  ) +
  ggplot2::scale_y_continuous(
    limits = c(min(data$co2_per_capita), max(data$co2_per_capita)),
    labels = seq(0, 20, 5),
    breaks = seq(0, 20, 5)
  ) +
  ggrepel::geom_label_repel(
    ggplot2::aes(label = ifelse(year == min_year | year == max_year, as.character(year), "")),
    size = 3,
    max.overlaps = Inf
  ) +
  ggplot2::labs(x = "GDP per Capita ($1,000)",
                y = "CO2 per Capita") 



# Plot version 3 --------------------------------------------------------------------

ggplot2::ggplot(data, ggplot2::aes(x = log(gdp_per_capita), 
                                   y = co2_per_capita, 
                                   group = country
                                   )) + 
  ggplot2::geom_point(na.rm = FALSE,
                      ggplot2::aes(color = country)) + 
  ggplot2::geom_path(na.rm = FALSE,
                     ggplot2::aes(color = country)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.border       = ggplot2::element_blank(),
    axis.line.x.bottom = ggplot2::element_line(color = 'black'),
    axis.line.y.left   = ggplot2::element_line(color = 'black')
  ) +
  ggplot2::scale_x_continuous(
    limits = c(log(1000), log(55000)),
    labels = ~ round(exp(.x) / 1000),
    breaks = c(log(1000), log(2000), log(4000), log(8000), log(16000), log(32000), log(64000))
  ) +
  ggrepel::geom_label_repel(
    ggplot2::aes(label = ifelse(year == min_year | year == max_year, as.character(year), "")),
    size = 3,
    max.overlaps = Inf
  ) +
  ggplot2::scale_color_manual(
    name = ggplot2::element_blank(),
    labels = c("United States", "United Kingdom", "Germany", "France", "Japan", "Canada", "Italy"),
    values = c("#222222","#A3A3A3","#1c5d99","#639fab","#bbcde5","#e8c8c3","#eca64a")
    
  ) +
  ggplot2::labs(x = "GDP per Capita ($1,000)",
                y = "CO2 per Capita") 





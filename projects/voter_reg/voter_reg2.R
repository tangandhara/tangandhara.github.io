library(tidyverse)
library(lubridate)
library(ggrepel)
library(scales)
library(readr)
library(ggtext)
library(hrbrthemes)
library(systemfonts)
library(glue)
library(ggsci)
library(scales)
library(ggthemes)
library(ggbeeswarm)
library(zoo)
library(patchwork)

findoutlier <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

bkg_col      <- colorspace::lighten('white', 0.2)    
title_col    <- "#000000"            
subtitle_col <- "#000000"     
caption_col  <- "#000000"   
text_col     <- colorspace::darken("#343a40" , 0.15)  
col_palette  <- c("Under 25" = "#3F88C5", "25 to 34" = "#D00000", "35 to 44" = "#FFBA08", "45 to 54" = "#136F63", 
                  "55 to 64" = "#032b43", "65 to 74" = "#49dcb1", "Over 75" = "#ff8a5b")



# 2024 age groups ---------------------------------------------------------


df$age_group <- factor(df$age_group, levels = c("Under 25", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65 to 74", "Over 75"))
levels(df$age_group)


vote_age_groups <- read_csv("projects/voter_reg/Register_To_Vote_applications_by_age_group_01_04_2024_to_18_06_2024.csv")

vote_age_groups$Date <- dmy(vote_age_groups$Date)
vote_age_groups <- vote_age_groups |> pivot_longer(
  cols = (2:8),
  names_to = "age_group",
  values_to = "total"
)

df <- vote_age_groups |> filter(Date >= "2024-05-22") |> group_by(age_group) |>  arrange(Date) |> mutate(cs = cumsum(total))

#df <- vote_age_groups |> 
#  group_by(age_group) |> 
#  mutate(outlier = ifelse(findoutlier(total), total, NA))

# Total by age group
p2 <- df |> ggplot(aes(x = age_group, y = total, fill = age_group)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = col_palette) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "right",
    plot.background = element_rect("white"),
    legend.background = element_rect("white"),
    panel.background = element_rect("white"),
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(
    fill = guide_legend(
      direction = "horizontal",   # Set legend direction to horizontal
      nrow = 7,                   # Adjust the number of rows for better layout
      title = "Age groups",        # Set legend title text
      title.position = "top" 
    )
  )


# Cumulative over time by age group
p1 <- df |> ggplot(aes(y = cs, x = Date, group = age_group, colour = age_group))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = col_palette)+
  theme_fivethirtyeight()+
  theme(legend.position = "none",
        plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        axis.text.x = element_text(angle = 45, hjust = 1))

p1 + p2 + plot_annotation(
       title = "2024 voter demographics",
       subtitle = "The chart on the left shows the cumulative totals over the 28 day period between the election being called and registration closing, while the other shows the overall totals",
       caption = caption_text,
       #fill = "Age groups",
      theme = 
  theme(
    #plot.background = element_rect(text_col),
    legend.position = "off",
    axis.title.x.top = element_markdown(
      margin = margin(10, 2, 10, 2), 
      size   = rel(.9),
      color  = text_col, 
      family        = "Futura",
      face   = "bold"
    ),
    legend.title = element_markdown(
      size   = rel(.9), 
      hjust  = 0.5,
      family        = "Futura",
      face   = "bold",
      color  = text_col
    ),
    legend.text = element_text(
      size   = rel(.7),
      family        = "Futura",
      face   = "bold",
      color  = text_col
    ),
    legend.key.height  = unit(.8, "cm"),
    #legend.margin    = margin(t = 5, b = -5, unit = "pt"), #size of the box
    plot.title      = element_textbox(
      size          = rel(1),
      family        = "Futura",
      face          = "bold",
      color         = title_col,
      lineheight    = 1.1,
      margin        = margin(t = 5, b = 3)
    ),
    plot.subtitle   = element_textbox_simple(
      size          = rel(0.8), 
      family        = "Futura",
      #color         = subtitle_col,
      lineheight    = -0.4, 
      margin        = margin(t = 2, b = 8)
    ),
    plot.caption    = element_textbox_simple(family = "Futura",size = 7,margin = margin(2, 0, 0, 0, "lines")),
    axis.text  = element_text(size = rel(.5),family        = "Futura"),
    axis.title.x  = element_text(size = rel(0.5),family        = "Futura",face="bold", hjust = 0.5, vjust = -5),
    axis.title.y  = element_text(size = rel(0.5),family        = "Futura",face="bold", hjust = 0.5, vjust = 2),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_line(colour = "#ced4da", linewidth = 0.5)))

  # 2019 age group ----------------------------------------------------------


reg_Vote_2019 <- read_csv("projects/voter_reg/Register_To_Vote_applications_by_age_group_04_09_2019_to_12_12_2019.csv")
View(reg_Vote_2019)

reg_Vote_2019$Date <- dmy(reg_Vote_2019$Date)
reg_Vote_2019 <- reg_Vote_2019 |> pivot_longer(
  cols = (2:8),
  names_to = "age_group",
  values_to = "total"
)

reg_Vote_2019 <- reg_Vote_2019 |> arrange(Date) |> filter(Date>= "2019-10-30" & Date<= "2019-11-26")
options(scipen = 999)
library(scales)

reg_Vote_2019$age_group <- factor(reg_Vote_2019$age_group, levels = c("Under 25", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65 to 74", "Over 75"))

reg_Vote_2019 <- reg_Vote_2019 |>  group_by(age_group) |>  arrange(Date)|> mutate(cs = cumsum(total)) |> ungroup()

p3 <- reg_Vote_2019  |> ggplot(aes(x = age_group, y = total, fill = age_group))+
  geom_col()+
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = col_palette)+
  theme_fivethirtyeight() +
  theme(
    legend.position = "right",
    plot.background = element_rect("white"),
    legend.background = element_rect("white"),
    panel.background = element_rect("white"),
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(
    fill = guide_legend(
      direction = "horizontal",   # Set legend direction to horizontal
      nrow = 7,                   # Adjust the number of rows for better layout
      title = "Age groups",        # Set legend title text
      title.position = "top" 
    )
  )


p4 <- reg_Vote_2019 |> ggplot(aes(y = cs, x = Date, group = age_group, colour = age_group))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = col_palette)+
  theme_fivethirtyeight()+
  theme(legend.position = "none",
        plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        axis.text.x = element_text(angle = 45, hjust = 1))

p4 + p3+ plot_annotation(
  title = "2019 voter demographics",
  subtitle = "The chart on the left shows the cumulative totals over the 28 day period between the election being called and registration closing, while the other shows the overall totals",
  caption = caption_text,
  #fill = "Age groups",
  theme = 
    theme(
      #plot.background = element_rect(text_col),
      legend.position = "off",
      axis.title.x.top = element_markdown(
        margin = margin(10, 2, 10, 2), 
        size   = rel(.9),
        color  = text_col, 
        family        = "Futura",
        face   = "bold"
      ),
      legend.title = element_markdown(
        size   = rel(.9), 
        hjust  = 0.5,
        family        = "Futura",
        face   = "bold",
        color  = text_col
      ),
      legend.text = element_text(
        size   = rel(.7),
        family        = "Futura",
        face   = "bold",
        color  = text_col
      ),
      legend.key.height  = unit(.8, "cm"),
      #legend.margin    = margin(t = 5, b = -5, unit = "pt"), #size of the box
      plot.title      = element_textbox(
        size          = rel(1),
        family        = "Futura",
        face          = "bold",
        color         = title_col,
        lineheight    = 1.1,
        margin        = margin(t = 5, b = 3)
      ),
      plot.subtitle   = element_textbox_simple(
        size          = rel(0.8), 
        family        = "Futura",
        #color         = subtitle_col,
        lineheight    = -0.4, 
        margin        = margin(t = 2, b = 8)
      ),
      plot.caption    = element_textbox_simple(family = "Futura",size = 7,margin = margin(2, 0, 0, 0, "lines")),
      axis.text  = element_text(size = rel(.5),family        = "Futura"),
      axis.title.x  = element_text(size = rel(0.5),family        = "Futura",face="bold", hjust = 0.5, vjust = -5),
      axis.title.y  = element_text(size = rel(0.5),family        = "Futura",face="bold", hjust = 0.5, vjust = 2),
      panel.grid.major = element_blank(),
      panel.grid.major.y = element_line(colour = "#ced4da", linewidth = 0.5)))

# daily comparison --------------------------------------------------------

daily_2024 <- read_csv("projects/voter_reg/Register_To_Vote_applications_breakdown_01_04_2024_to_18_06_2024.csv", 
                       col_types = cols(`Paper forms` = col_skip()))
daily_2024$Date <- dmy(daily_2024$Date)

daily_2024 <- daily_2024 |> filter(Date>="2024-05-22") |> arrange(Date)
daily_2024$index <- 1:nrow(daily_2024)
daily_2024 <- daily_2024 |> 
  mutate(moving_avg = rollmean(Online, k=3, fill = NA, align = "right"))

daily_2019 <- read_csv("projects/voter_reg/Register_To_Vote_applications_breakdown_04_09_2019_to_12_12_2019.csv")

daily_2019$Date <- dmy(daily_2019$Date)

daily_2019 <- daily_2019 |> filter(Date>="2019-10-29" & Date<="2019-11-26") |> arrange(Date)
daily_2019$index <- 1:nrow(daily_2019)
daily_2019 <- daily_2019 |> 
  mutate(moving_avg = rollmean(Online, k=3, fill = NA, align = "right"))

ggplot()+
  geom_line(daily_2019, mapping = aes(x = index, y = Online), colour = "blue")+
  #geom_line(linetype = 2,daily_2019, mapping = aes(x = index, y = moving_avg), colour = "blue")+
  geom_line(daily_2024, mapping = aes(x = index, y = Online), colour = "red")
  #geom_line(linetype = 2,daily_2024, mapping = aes(x = index, y = moving_avg), colour = "red")




# Table -------------------------------------------------------------------
library(gt)

df_2024 <- vote_age_groups |> group_by(age_group) |> summarise(total = sum(total))
df_2019 <- reg_Vote_2019|> group_by(age_group) |> summarise(total = sum(total))

df_2019$age_group <- factor(df_2019$age_group, levels = c("Under 25", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65 to 74", "Over 75"))

df_2024$age_group <- factor(df_2024$age_group, levels = c("Under 25", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65 to 74", "Over 75"))

p5 <- df_2024 |> arrange(age_group) |>  gt() |>
  tab_header(
    title = "Voter registrations 2024",
    subtitle = "Online registrations from 22/05/2024 to 18/06/2024"
  ) |> 
  fmt_number(total, use_seps = TRUE,decimals = 0) |> 
  cols_label(
    age_group = "Age group",
    total = "Total"
  )
p5+p5

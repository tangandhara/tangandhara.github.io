library(tidyverse)
library(readr)

vote_june18 <- read_csv("projects/voter_reg/Register_To_Vote_live_usage_18_Jun_2024.csv")
View(vote_june18)

news_alert <- dmy_hms("18/06/2024 19:25:00")
jezza_tweet <- dmy_hms("18/06/2024 17:05:00")
tiktok_alert <- dmy_hms("18/06/2024 12:00:00")
c4debate <- dmy_hms("18/06/2024 19:00:00")
skynews <-  dmy_hms("18/06/2024 18:43:00")
sky <- "https://x.com/SkyNews/status/1803121493636788528"

new1 <- vote_june18 |> 
  mutate(date = if_else(`Online Applications`== "879", "19/06/2024", "18/06/2024"))

new1 <- new1 %>%
  mutate(dtm = dmy_hms(str_c(date, " ", Time)))

new1 <- new1 |> arrange(dtm)
View(new1)
new1 <- new1 |> 
  mutate(cumulative = cumsum(`Online Applications`))

new1 |> ggplot()+
  geom_line(lineend = "round",mapping = aes(x = dtm, y = cumulative))+
  theme_clean()+
  labs(x = "Time",
       y = "Number of applications submitted")


new1 |> ggplot()+
  geom_line(lineend = "round",mapping = aes(x = dtm, y = `Online Applications`))+
  #geom_line(mapping = aes(x = dtm, y = cumulative))+
  geom_point(x = news_alert, y = new1$`Online Applications`[234], colour = "red", size = 4)+
  annotate("text",label="Apple News\nnotification",x = news_alert, y = new1$`Online Applications`[234], vjust = -0.5)+
  geom_point(x = jezza_tweet,y = new1$`Online Applications`[205], colour = "blue", size = 4)+
  geom_point(x = tiktok_alert, y = new1$`Online Applications`[144],colour = "green", size = 4)+
  geom_point(x = c4debate, y = new1$`Online Applications`[228], colour = "yellow", size = 4)+
  annotate("curve",x = news_alert, y = new1$`Online Applications`[228], xend = dmy_hms("18/06/2024 19:35:00"), yend = 5990,arrow = arrow(length = unit(1.5, "mm")))+
  geom_point(x = skynews, y = new1$`Online Applications`[224],colour = "orange", size = 4)+
  annotate("text",label="Sky News\narticle",x = skynews, y = new1$`Online Applications`[224], vjust = -0.5)+
  annotate("text",label="TikTok\nnotification",x = tiktok_alert, y = new1$`Online Applications`[144], vjust = -0.5)+
  annotate("text",label="Jeremy Corbyn\ntweet",x = jezza_tweet, y = new1$`Online Applications`[205], vjust = -0.5)+
  scale_x_datetime(breaks = "3 hours", date_labels = "%H:%M")+
  #scale_y_continuous(sec.axis = sec_axis(~. * 300))+
  theme_minimal()

new1 |> ggplot()+
  geom_line(mapping = aes(x = dtm, y = cumulative))+
  scale_x_datetime(breaks = "2.4 hours", date_labels = "%H:%M")+
  #scale_y_continuous(sec.axis = sec_axis(~. * 300))+
  theme_minimal()

new1 |> ggplot( aes(x = `Online Applications`)) + 
  stat_ecdf(geom = "smooth", color = "firebrick", size = 1.5) 

new1 |> ggplot(aes(x = `Online Applications`))+
  geom_histogram()

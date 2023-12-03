rm(list = ls())

library(plotly)
library(tidyverse)

x_test <- readRDS("ChallengeData/data_rds_files/x_test")

turbines <- read_delim(
  "ChallengeData/Complementary_data/WindFarms_complementary_data.csv",
  delim = ";",
  skip = 1,
  col_names = c(
    "time",
    "windfarm",
    "windturbine",
    "avgpower",
    "cumulenergy",
    "winddir",
    "windspeed",
    "nacdir",
    "rotspeed")) %>%
  mutate(time = dmy_hm(time))


wf1 <- turbines %>%
  filter(windfarm == "WF1")

wf1 %>%
  ggplot(data = .,
         aes(x = windspeed, y = avgpower, col = windturbine)) +
  facet_wrap(~windturbine) +
  geom_point()

turbines %>%
  drop_na(windfarm, windturbine, avgpower) %>%
  group_by(windfarm, windturbine) %>%
  summarize(
    c_zeros = sum(avgpower == 0, na.rm = T),
    c_tot = n(),
    frac = c_zeros/c_tot
  ) %>%
  arrange(desc(frac))

turbines %>%
  drop_na(windfarm, windturbine, avgpower) %>%
  group_by(windfarm, hour = hour(time)) %>%
  summarize(
    c_zeros = sum(avgpower == 0, na.rm = T),
    c_tot = n(),
    frac = c_zeros/c_tot
  ) %>%
  ggplot(
    data = .,
    aes(x = hour, y = frac)) +
  facet_wrap(~windfarm) +
  geom_bar(position='stack', stat='identity')


wf1 %>%
  ggplot(
    data = .,
    aes(x = nacdir-winddir, y = cumulenergy, col = windturbine)) +
  geom_point()

turbines %>%
  drop_na(nacdir, winddir, cumulenergy, windfarm) %>%
  ggplot(
    data = .,
    aes(x = nacdir-winddir, y = cumulenergy, col = windturbine)) +
  geom_point()

wf1 %>%
  drop_na(winddir, cumulenergy, windfarm) %>%
  ggplot(
    data = .,
    aes(x = winddir, y = cumulenergy, col = windturbine)) +
  geom_point()

turbines %>%
  filter(
    !(windfarm %in% c("WF1", "WF6"))
  ) %>%
  plot_ly(
    x = ~rotspeed,
    y = ~abs(nacdir-winddir),
    z = ~cumulenergy,
    color = ~windfarm,
    size = .2,
    type = "scatter3d",
    mode = "markers")

turbines %>%
  filter(
    windfarm %in% c("WF1", "WF6")
  ) %>%
  plot_ly(
    x = ~rotspeed,
    y = ~abs(nacdir-winddir),
    z = ~cumulenergy,
    color = ~windfarm,
    size = .2,
    type = "scatter3d",
    mode = "markers")
  


turbines %>%
  drop_na(windfarm, rotspeed, cumulenergy) %>%
  ggplot(
    data = .,
    aes(x = rotspeed, y = cumulenergy)
  ) +
  facet_wrap(~windfarm, scales = "free") + 
  geom_point()


turbines %>%
  drop_na(windfarm, rotspeed, windspeed) %>%
  ggplot(
    data = .,
    aes(x = windspeed, y = rotspeed)
  ) +
  facet_wrap(~windfarm, scales = "free") + 
  geom_point()


# if we could (accurately) predict windspeed AND rotspeed from
#  the data we have, that would yield a (probably) very good prediction:
turbines %>%
  drop_na(windfarm, rotspeed, windspeed, cumulenergy) %>%
  plot_ly(
    x = ~windspeed,
    y = ~rotspeed,
    z = ~cumulenergy,
    color = ~windfarm,
    size = .2,
    type = "scatter3d",
    mode = "markers"
  )

turbines %>%
  drop_na(windfarm, rotspeed, windspeed, cumulenergy) %>%
  group_by(windfarm, time) %>%
  summarize(
    rotspeed = mean(rotspeed),
    windspeed = mean(windspeed),
    cumulenergy = sum(cumulenergy)
  ) %>%
  plot_ly(
    x = ~windspeed,
    y = ~rotspeed,
    z = ~cumulenergy,
    color = ~windfarm,
    size = .2,
    type = "scatter3d",
    mode = "markers"
  )

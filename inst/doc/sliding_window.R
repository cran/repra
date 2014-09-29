## ----, echo = FALSE, message = FALSE-------------------------------------
set.seed(26)
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE,
  fig.width = 12,
  fig.height = 6)
ggplot2::theme_set(ggplot2::theme_bw())

## ----, message = FALSE---------------------------------------------------
library(repra)
library(dplyr)
library(reshape2)
library(ggplot2)

## ------------------------------------------------------------------------
tdata <- repratime %>%
  filter(Area == "RMPA") %>%
  select(Area, Time, Load, Wind)
td <- format_timedata(tdata)
head(td)

## ------------------------------------------------------------------------
td.wide <- melt(td, 1:5, variable.name = "Type")
p <- ggplot(td.wide %>% filter(Day == 10), aes(x = Time, y = value, color = Type)) +
  geom_line() +
  labs(x = "Hour", y = "Power (MW)") +
  expand_limits(y = 0)
p

## ------------------------------------------------------------------------
td.3h <- td %>% sliding_window(win.h.size = c(-1, 1))
td.3h %>% filter(Day == 10) %>% arrange(Time) %>% head

## ------------------------------------------------------------------------
td.3h.wide <- melt(td.3h, 1:5, variable.name = "Type")
p + geom_point(data = td.3h.wide %>% filter(Day == 10), color = "black")

## ------------------------------------------------------------------------
ggplot(td %>% filter(Day == 10), aes(x = Time, y = Wind)) +
  geom_line(color = "blue") +
  geom_point(data = td.3h %>% filter(Day == 10), color = "black", size = 2) +
  labs(x = "Hour", y = "Power (MW)") +
  expand_limits(y = 0)

## ------------------------------------------------------------------------
td.5h <- td %>% sliding_window(win.h.size = c(-2, 2))
ggplot(td %>% filter(Day == 10), aes(x = Time, y = Wind)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "blue") +
  geom_point(data = td.5h %>% filter(Day == 10), color = "black", size = 2) +
  labs(x = "Hour", y = "Power (MW)") +
  expand_limits(y = 0)

## ------------------------------------------------------------------------
td.3d <- td %>% sliding_window(win.d.size = c(-1, 1))
ggplot(td %>% filter(Day == 10), aes(x = Time, y = Wind)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "blue") +
  geom_point(data = td.3d %>% filter(Day == 10), color = "black", size = 2) +
  labs(x = "Hour", y = "Power (MW)") +
  expand_limits(y = 0)

## ------------------------------------------------------------------------
td.3h.3d <- td %>% sliding_window(win.h.size = c(-1, 1), win.d.size = c(-1, 1))
ggplot(td %>% filter(Day == 10), aes(x = Time, y = Wind)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "blue") +
  geom_point(data = td.3h.3d %>% filter(Day == 10), color = "black", size = 2) +
  labs(x = "Hour", y = "Power (MW)") +
  expand_limits(y = 0)


## ---- echo = FALSE, message = FALSE--------------------------------------
set.seed(26)
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE,
  fig.width = 7,
  fig.height = 6)

## ---- message = FALSE----------------------------------------------------
library(repra)
library(dplyr)
library(reshape2)
library(ggplot2)

## ------------------------------------------------------------------------
gens <- repragen %>%
  filter(Area == "AZ-NM-NV")
head(gens)
out.table <- outage_table(gens)
head(out.table)

## ------------------------------------------------------------------------
plot(out.table)
summary(out.table)

## ------------------------------------------------------------------------
tdata <- repratime %>% filter(Area == "AZ-NM-NV")
head(tdata)
td <- format_timedata(tdata)
head(td)

## ------------------------------------------------------------------------
td2 <- td %>%
  mutate(NetLoad = 2.02 * Load)
calculate_metrics(td2, out.table)

## ------------------------------------------------------------------------
cv <- calculate_metrics(td2, out.table, raw = TRUE)
cv %>%
  arrange(-Capacity) %>%
  as.data.frame() %>%
  head()

## ------------------------------------------------------------------------
td2.wind <- td %>%
  mutate(NetLoad = 2.02 * Load - Wind)
calculate_metrics(td2.wind, out.table)

## ------------------------------------------------------------------------
elcc <- calculate_elcc(td, out.table)
as.data.frame(elcc)

## ------------------------------------------------------------------------
elcc2 <- calculate_elcc(td, out.table, ignore = "Wind")
as.data.frame(elcc2)

## ------------------------------------------------------------------------
elcc3 <- calculate_elcc(td, out.table, obj.metric = "LOLH", obj.value = 1.0)
as.data.frame(elcc3)

## ------------------------------------------------------------------------
cv <- capacity_value(td, out.table)
as.data.frame(cv)

## ------------------------------------------------------------------------
dcast(cv, Level + Area + Metric + Objective ~ CVTech, value.var = "CV")
ggplot(cv, aes(x = paste(Level, Area, sep = ", "), weight = CV, fill = CVTech)) +
  geom_bar() +
  labs(x = "Area", y = "Capacity Value (MW)", fill = "Type")

## ------------------------------------------------------------------------
cv.mar <- capacity_value(td, out.table, marginal = TRUE)
as.data.frame(cv.mar)


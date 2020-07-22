
library(tidyverse)
library(purrr)
library(ggplot2)
theme_set(bayesplot::theme_default(base_family = "sans"))

## Binomial model from Chapter 2 of Gelman

ggplot(data.frame(x = 0:1, y = dbinom(0:1, 1, 0.5)), aes(x = x, y = y)) + 
  geom_col() +
  scale_x_discrete(breaks = 0:1, limits = 0:1) +
  labs(x = "y", y = "probability", title = bquote("Binomial distribution with " ~ theta ~ "=0.5, n=1"))

library(tidyverse)
library(ggplot2)
library(knitr)
library(here)
library(ggthemes)
library(lme4)
library(ds4ling)
library(patchwork)

source(here::here("scripts", "libs.R"))

# load data

data_raw <- read_csv(here("data", "raw", "response_latency_data.csv"))

# check structure and get familiar with it

view(data_raw)

# tidy the data

# separate id and language, get columns for word_type and 
# item, filter pseudowords and selecting what interests for this paper

data_tidy <- data_raw |>
  separate(
    col = id,
    into = c("id", "language"),
    sep = "_") |>
  separate (
    col = item,
    into = c("word_type", "word"),
    sep = "_", remove = F) |>
  rename(accuracy = response) |>
  filter(word_type == "emotional" | word_type == "nonemotional") |>
  select(id, language, word_type, item, accuracy, latency)

# save data_tidy in folder
## write.csv has been used in class before, but I couldn't find it in my notes so
## I checked ChatGPT for help

write.csv(data_tidy, "data_tidy.csv", row.names = FALSE)

# Accuracy Model

# fit the model

mod_accuracy <- glmer(
  accuracy ~ language * word_type + (1|id) + (1|item), 
  data = data_tidy, 
  family = binomial(link = "logit"))

# summary of model

summary(mod_accuracy)

## visited https://stats.stackexchange.com/questions/647410/confidence-interval-for-glmm 
## to learn about how to obtain 95% CI with glmm

confint(mod_accuracy, method = "Wald")

## to obtain R squared, I visited https://ecologyforacrowdedplanet.wordpress.com/2013/08/27/r-squared-in-mixed-models-the-easy-way/
## to learn about R squared with glmm

install(MuMIn)
library(MuMIn)
r.squaredGLMM(mod_accuracy)

# write up of output [see manuscript]

# generate plot

plot_accuracy <- data_tidy |>
  ggplot(aes(x = word_type, y = accuracy, color = language)) +
  facet_grid(. ~ language) +
  geom_jitter(height = 0, width = 0.2, alpha = 0.4) +
  stat_summary(
    fun.data = mean_sdl,
    geom = 'pointrange') +
  scale_color_viridis_d(guide = "none", begin = 0.3, end = 0.8) +
  ds4ling::ds4ling_bw_theme() +
  labs(
    title = "Accuracy per Word Type by language",
    x = "Word Type", y = "Accuracy")

# save plot

ggsave(
  filename = "scripts/plots/plot_accuracy.png",
  plot = plot_accuracy
)

# Latency Model

# fit the model

mod_latency <- lmer(
  latency ~ language * word_type + (1|id) + (1|item),
  data = data_tidy)

# summary of model

summary(mod_latency)

## 95% CI with lmer

confint(mod_latency, method = "Wald")

## R squared with lmer

r.squaredGLMM(mod_latency)

# assumptions

plot(mod_latency)
qqnorm(residuals(mod_latency))

# write up of output [see manuscript]

# generate plot

plot_latency <- data_tidy |>
  ggplot(aes(x = word_type, y = latency, color = language)) +
  facet_grid(. ~ language) +
  geom_jitter(height = 0, width = 0.2, alpha = 0.4) +
  scale_color_viridis_d(guide = "none", begin = 0.3, end = 0.8) +
  ds4ling::ds4ling_bw_theme() +
  labs(
    title = "Latency per Word Type by language",
    x = "Word Type", y = "Latency (ms)")


# save plot

ggsave(
  filename = "scripts/plots/plot_latency.png",
  plot = plot_latency
)

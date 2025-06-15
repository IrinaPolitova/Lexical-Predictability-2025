library(tidyverse)
library(ggplot2)
library(lme4)

setwd('C:/Users/irina/Documents/ПИС')

N = 389470868

pent <- read_csv('POSes_pentagram_analysis.csv')
pent <- pent %>% 
  filter(left_empty == 0) %>% 
  mutate(correct_guess_prop = correct_guess / number_of_participants) %>% 
  mutate(correct_pos_prop = correct_pos / number_of_participants) %>% 
  mutate(penta = (penta_count+1) / (penta_gap_count+1)) %>% 
  mutate(tri_1 = tri_1_count / tri_1_gap_count) %>% 
  mutate(tri_2 = tri_2_count / tri_2_gap_count) %>%
  mutate(tri_3 = tri_3_count / tri_3_gap_count) %>% 
  mutate(left = left_new / left_new_gap)


pent %>% 
  ggplot(aes(x = penta, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Pentagram', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")

cor(pent$correct_pos_prop, pent$penta, method = 'pearson') # 0.19

pent %>% 
  ggplot(aes(x = tri_1, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Trigram - left', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")




pent %>% 
  filter(pos == 'v') %>% 
  ggplot(aes(x = tri_1, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Trigram on the left', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")


cor(pent$correct_pos_prop, pent$tri_1, method = 'pearson') # 0.29




pent %>% 
  ggplot(aes(x = left, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'PoS probability in SynTagRus', y = 'Proportion of correct PoS guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")


cor(pent$correct_pos_prop, pent$left, method = 'pearson') # 0.30



pent <- pent %>% 
  filter(with_empty == 0)

pent %>% 
  ggplot(aes(x = left, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Trigram - left', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")


cor(pent$correct_pos_prop, pent$left, method = 'pearson') # 0.30








pent %>% 
  ggplot(aes(x = tri_2, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Trigram - middle', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")

cor(pent$correct_pos_prop, pent$tri_2, method = 'pearson') # 0.23

pent %>% 
  ggplot(aes(x = tri_3, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Trigram - right', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")

cor(pent$correct_pos_prop, pent$tri_3, method = 'pearson') # 0.21













pent %>% 
  ggplot(aes(x = penta, y = correct_guess_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Pentagram', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")

cor(pent$correct_guess_prop, pent$penta, method = 'pearson') # 0.21

pent %>% 
  ggplot(aes(x = tri_1, y = correct_guess_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Trigram - left', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")

cor(pent$correct_guess_prop, pent$tri_1, method = 'pearson') # 0.23

pent %>% 
  ggplot(aes(x = tri_2, y = correct_guess_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Trigram - middle', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")

cor(pent$correct_guess_prop, pent$tri_2, method = 'pearson') # 0.24

pent %>% 
  ggplot(aes(x = tri_3, y = correct_guess_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Trigram - right', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")

cor(pent$correct_guess_prop, pent$tri_3, method = 'pearson') # 0.19











pent_local <- pent %>% 
  filter(predictability_type == 'local')




pent_local %>% 
  ggplot(aes(x = penta, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Pentagram', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")

cor(pent_local$correct_pos_prop, pent_local$penta, method = 'pearson') # 0.17

pent_local %>% 
  ggplot(aes(x = tri_1, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Trigram - left', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")

cor(pent_local$correct_pos_prop, pent_local$tri_1, method = 'pearson') # 0.26

pent_local %>% 
  ggplot(aes(x = tri_2, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Trigram - middle', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")

cor(pent_local$correct_pos_prop, pent_local$tri_2, method = 'pearson') # 0.23

pent_local %>% 
  ggplot(aes(x = tri_3, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Trigram - right', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")

cor(pent_local$correct_pos_prop, pent_local$tri_3, method = 'pearson') # 0.18

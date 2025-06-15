library(tidyverse)
library(ggplot2)
library(lme4)

setwd('C:/Users/irina/Documents/ПИС')

N = 389470868

poses <- read_csv('POSes.csv')
poses <- poses %>% 
  mutate(correct_guesses_prop = correct_guess / number_of_participants) %>% 
  mutate(skipped_prop = skipped / number_of_participants) %>% 
  mutate(frequency_log = log2(frequency + 1)) %>% 
  mutate(number_prop = number_in_text / number_of_gaps) %>% 
  mutate(left_frequency_log = log2(left_bigram + 1)) %>%
  mutate(right_frequency_log = log2(right_bigram + 1)) %>% 
  mutate(sum_frequency_log = log2(sum_bigram + 1)) %>% 
  mutate(max_frequency_log = log2(max_bigram + 1)) %>% 
  mutate(mi_left = log2(left_bigram * N / (left_frequency * token_frequency) + 1)) %>% 
  mutate(mi_right = log2(right_bigram * N / (right_frequency * token_frequency) + 1)) %>% 
  mutate(coll_left = log2(left_bigram / left_frequency + 1)) %>% 
  mutate(coll_right = log2(right_bigram / right_frequency + 1)) %>% 
  mutate(len_word = nchar(word)) %>% 
  mutate(correct_pos_prop = correct_pos / number_of_participants) %>% 
  mutate(correct_sem_prop = correct_sem / number_of_participants)

poses_verbs <- read_csv('POSes_verbs_only.csv')
poses_verbs <- poses_verbs %>% 
  mutate(correct_guesses_prop = correct_guess / number_of_participants) %>% 
  mutate(skipped_prop = skipped / number_of_participants) %>% 
  mutate(frequency_log = log2(frequency + 1)) %>% 
  mutate(number_prop = number_in_text / number_of_gaps) %>% 
  mutate(left_frequency_log = log2(left_bigram + 1)) %>%
  mutate(right_frequency_log = log2(right_bigram + 1)) %>% 
  mutate(sum_frequency_log = log2(sum_bigram + 1)) %>% 
  mutate(max_frequency_log = log2(max_bigram + 1)) %>% 
  mutate(mi_left = log2(left_bigram * N / (left_frequency * token_frequency) + 1)) %>% 
  mutate(mi_right = log2(right_bigram * N / (right_frequency * token_frequency) + 1)) %>% 
  mutate(coll_left = log2(left_bigram / left_frequency + 1)) %>% 
  mutate(coll_right = log2(right_bigram / right_frequency + 1)) %>% 
  mutate(len_word = nchar(word)) %>% 
  mutate(correct_pos_prop = correct_pos / number_of_participants) %>% 
  mutate(correct_sem_prop = correct_sem / number_of_participants) %>% 
  mutate(sem_tag_size_log = log2(sem_tag_size)) %>% 
  mutate(sem_tag_size_2_log = log2(sem_tag_size_2))








# Тексты предсказываются по-разному

poses %>% 
  mutate(text_as_factor = as.factor(text)) %>% 
  ggplot(aes(x = text_as_factor, y = correct_guesses_prop, fill = text_as_factor)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Текст', y = 'Доля угадываний')



# Части речи

poses_summary <- poses %>% 
  group_by(pos) %>% 
  summarize(count = n(),
            mean_correct = mean(correct_guesses_prop))

poses %>% 
  ggplot(aes(x = pos, y = correct_guesses_prop, fill = pos)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Часть речи', y = 'Доля угадываний') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

poses %>% 
  ggplot(aes(x = pos, y = correct_guesses_prop, fill = pos)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Part of speech', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none")

aov_model <- aov(poses$correct_guesses_prop ~ poses$pos) # p-value = 0.000208
summary(aov_model)
TukeyHSD(aov_model)



# Частотность

poses %>% 
  ggplot(aes(x = frequency, y = correct_guesses_prop)) +
  geom_point() +
  theme_classic() +
  labs(x = 'Частотность', y = 'Доля угадываний') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

poses %>% 
  ggplot(aes(x = frequency_log, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Частотность (логарифм)', y = 'Доля угадываний') +
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

poses %>% 
  ggplot(aes(x = frequency_log, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Frequency (logarithm)', y = 'Proportion of correct guesses') +
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

cor(poses$correct_guesses_prop, poses$frequency_log, method = 'pearson')

poses %>% 
  ggplot(aes(x = frequency_log, y = correct_guesses_prop, color = pos)) +
  geom_point() +
  theme_classic() +
  facet_wrap(~pos)



# Тип предсказуемости

types <- poses %>% 
  group_by(predictability_type) %>% 
  rename(type = predictability_type) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop))

types %>% 
  ggplot(aes(x = type, y = count)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = 'Тип предсказуемости') +
  geom_text(aes(label = count), vjust = -0.5)

poses %>% 
  ggplot(aes(x = predictability_type, y = correct_guesses_prop, fill = predictability_type)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Тип предсказуемости', y = 'Доля угадываний') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

poses %>% 
  ggplot(aes(x = predictability_type, y = correct_guesses_prop, fill = predictability_type)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Type of predictive context', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none")

poses_for_t <- poses %>% 
  filter(predictability_type != '-')

t.test(poses_for_t$correct_guesses_prop ~ poses_for_t$predictability_type) # p-value = 0.0007924



# Номер пропуска

poses %>% 
  ggplot(aes(x = number_prop, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  facet_wrap(~text)

poses %>% 
  filter(predictability_type == 'global') %>% 
  ggplot(aes(x = number_prop, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  facet_wrap(~text) +
  labs(x = 'Место расположения пропуска', y = 'Доля угадываний') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 20)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")

poses %>% 
  filter(predictability_type == 'global') %>% 
  ggplot(aes(x = number_prop, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  facet_wrap(~text) +
  labs(x = 'Position in text', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 20)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")




# Синтаксис

deps <- poses %>% 
  group_by(dependency) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop))

deps %>% 
  ggplot(aes(x = dependency, y = count)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = 'Зависимость') +
  geom_text(aes(label = count), vjust = -0.5)

poses %>% 
  ggplot(aes(x = dependency, y = correct_guesses_prop, fill = dependency)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Зависимость', y = 'Доля угадываний') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

deps_pos <- poses %>% 
  group_by(pos, dependency) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop))

deps_pos %>% 
  ggplot(aes(x = dependency, y = count)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = 'Зависимость') +
  geom_text(aes(label = count), vjust = -0.5) +
  facet_wrap(~pos) +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 20))

poses <- poses %>% 
  mutate(is_root = factor(ifelse(dependency == "ROOT", "root", "not root")))

poses %>% 
  filter(pos == 'v') %>% 
  ggplot(aes(x = is_root, y = correct_guesses_prop, fill = is_root)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Зависимость', y = 'Доля угадываний') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

poses %>% 
  filter(pos == 'v') %>% 
  group_by(is_root) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop))

verbs <- poses %>% 
  filter(pos == 'v',
         dependency != 'conj')


verbs_root <- poses %>% 
  filter(pos == 'v',
         dependency == 'ROOT')

verbs_second <- poses %>% 
  filter(pos == 'v',
         dependency == 'acl' |
           dependency == 'advcl' |
           dependency == 'csubj' |
           dependency == 'ccomp' |
           dependency == 'xcomp')

verbs_second %>% 
  summarize(mean(correct_guesses_prop))

t.test(verbs_root$correct_guesses_prop, verbs_second$correct_guesses_prop) # p-value = 0.2579

verbs_root <- verbs_root %>% 
  mutate(verb_type = 'main')

verbs_second <- verbs_second %>% 
  mutate(verb_type = 'dependent')

verbs_all <- rbind(verbs_root, verbs_second)

verbs_all %>% 
  ggplot(aes(x = verb_type, y = correct_guesses_prop, fill = verb_type)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Type of the verb', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none")

t.test(verbs$correct_guesses_prop ~ verbs$is_root) # p-value = 0.4122

poses <- poses %>% 
  mutate(is_conj = factor(ifelse(dependency == "conj", "conj", "not conj")))

poses %>% 
  ggplot(aes(x = is_conj, y = correct_guesses_prop, fill = is_conj)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Зависимость', y = 'Доля угадываний') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))



# Часть речи слова до

poses_before <- poses %>% 
  group_by(pos_before) %>% 
  summarize(n(), mean_guesses = mean(correct_guesses_prop))

poses %>% 
  ggplot(aes(x = pos_before, y = correct_guesses_prop, fill = pos_before)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Часть речи', y = 'Доля угадываний')

poses_local_before <- poses %>% 
  filter(predictability_type == 'local') %>% 
  group_by(pos_before) %>% 
  summarize(n(), mean_guesses = mean(correct_guesses_prop))

poses %>% 
  filter(predictability_type == 'local') %>%
  ggplot(aes(x = pos_before, y = correct_guesses_prop, fill = pos_before)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Часть речи предшествующего слова', y = 'Доля угадываний') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))



# Часть речи слова после

poses_after <- poses %>% 
  group_by(pos_after) %>% 
  summarize(n(), mean_guesses = mean(correct_guesses_prop))

poses %>% 
  ggplot(aes(x = pos_after, y = correct_guesses_prop, fill = pos_after)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Часть речи', y = 'Доля угадываний')

poses_local_after <- poses %>% 
  filter(predictability_type == 'local') %>% 
  group_by(pos_after) %>% 
  summarize(n(), mean_guesses = mean(correct_guesses_prop))

poses %>% 
  filter(predictability_type == 'local') %>%
  ggplot(aes(x = pos_after, y = correct_guesses_prop, fill = pos_after)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Часть речи', y = 'Доля угадываний')



# Семантика

poses <- poses %>% 
  mutate(semantics_lower = tolower(semantics)) 

s_n <- poses %>% 
  filter(pos == 'n') %>% 
  group_by(semantics) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop))

s_n_lower <- poses %>% 
  filter(pos == 'n') %>% 
  group_by(semantics_lower) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop))

s_v <- poses %>% 
  filter(pos == 'v') %>% 
  group_by(semantics) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop))

s_v_lower <- poses %>% 
  filter(pos == 'v') %>% 
  group_by(semantics_lower) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop))

s_adj <- poses %>% 
  filter(pos == 'adj') %>% 
  group_by(semantics) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop))

s_adv <- poses %>% 
  filter(pos == 'adv') %>% 
  group_by(semantics) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop))


s_mesto <- poses %>% 
  filter(pos == 'n',
         semantics_lower == 'место в пространстве')

s_telo <- poses %>% 
  filter(pos == 'n',
         semantics_lower == 'часть тела')

t.test(s_mesto$correct_guesses_prop, s_telo$correct_guesses_prop) # p-value = 0.06371





# is_middle

poses <- poses %>% 
  mutate(new_middle_as_factor = as.factor(is_middle_new))

middles <- poses %>% 
  group_by(is_middle_new) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop))

poses %>% 
  ggplot(aes(x = new_middle_as_factor, y = correct_guesses_prop, fill = new_middle_as_factor)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'В серединке или нет', y = 'Доля угадываний') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))


poses %>% 
  ggplot(aes(x = new_middle_as_factor, y = correct_guesses_prop, fill = new_middle_as_factor)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Presence of head and dependents', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none")

t.test(poses$correct_guesses_prop ~ poses$new_middle_as_factor) # p-value = 0.03986



# Количество зависимых

poses %>% 
  ggplot(aes(x = count_dependents_new, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Количество зависимых', y = 'Доля угадываний') +
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

cor(poses$correct_guesses_prop, poses$count_dependents_new, method = 'pearson') # r = 0.015





# Биграммы

poses %>% 
  ggplot(aes(x = left_frequency_log, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Частотность биграммы (логарифм)', y = 'Доля угадываний') +
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

cor(poses$correct_guesses_prop, poses$left_frequency_log, method = 'spearman')


poses %>% 
  ggplot(aes(x = right_frequency_log, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Частотность биграммы (логарифм)', y = 'Доля угадываний') +
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

cor(poses$correct_guesses_prop, poses$right_frequency_log, method = 'spearman')


poses %>% 
  ggplot(aes(x = sum_frequency_log, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Bigram frequency (logarithm)', y = 'Proportion of correct guesses') +
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

cor(poses$correct_guesses_prop, poses$sum_frequency_log, method = 'spearman') # r = 0.37

cor(poses$correct_guesses_prop, poses$sum_frequency_log, method = 'pearson') # r = 0.4


poses %>% 
  ggplot(aes(x = max_frequency_log, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Частотность биграммы (логарифм)', y = 'Доля угадываний') +
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

cor(poses$correct_guesses_prop, poses$max_frequency_log, method = 'spearman')

poses %>% 
  ggplot(aes(x = coll_left, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Частотность биграммы (логарифм)', y = 'Доля угадываний') +
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

poses_filtered <- poses %>% 
  filter(coll_left != 'NaN')

cor(poses_filtered$correct_guesses_prop, poses_filtered$coll_left, method = 'spearman')


poses %>% 
  ggplot(aes(x = mi_left, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Частотность биграммы (логарифм)', y = 'Доля угадываний') +
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


poses %>% 
  ggplot(aes(x = coll_right, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Частотность биграммы (логарифм)', y = 'Доля угадываний') +
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


poses %>% 
  ggplot(aes(x = mi_right, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Частотность биграммы (логарифм)', y = 'Доля угадываний') +
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


poses <- poses %>% 
  mutate(mi = mi_left + mi_right)

poses %>% 
  ggplot(aes(x = mi, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Частотность биграммы (логарифм)', y = 'Доля угадываний') +
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

poses <- poses %>% 
  mutate(coll = coll_left + coll_right)

poses %>% 
  ggplot(aes(x = coll, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Частотность биграммы (логарифм)', y = 'Доля угадываний') +
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

poses_filtered <- poses %>% 
  filter(coll_left != 'NaN')
cor(poses_filtered$correct_guesses_prop, poses_filtered$coll, method = 'kendal')



# Длина слова

poses %>% 
  ggplot(aes(x = len_word, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Word length', y = 'Proportion of correct guesses') +
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

cor(poses$correct_guesses_prop, poses$len_word, method = 'pearson') # r = -0.32


















sem_1 <- read_csv('sem_1.csv')
sem_2 <- read_csv('sem_2.csv')
sem_3 <- read_csv('sem_3.csv')
sem_4 <- read_csv('sem_4.csv')
sem_5 <- read_csv('sem_5.csv')
sem_6 <- read_csv('sem_6.csv')

sem_1_poses <- sem_1 %>% 
  group_by(Пропуск) %>% 
  filter(correct_pos == 1) %>% 
  summarize(sum(Количество))

sem_2_poses <- sem_2 %>% 
  group_by(Пропуск) %>% 
  filter(correct_pos == 1) %>% 
  summarize(sum(Количество))

sem_3_poses <- sem_3 %>% 
  group_by(Пропуск) %>% 
  filter(correct_pos == 1) %>% 
  summarize(sum(Количество))

sem_4_poses <- sem_4 %>% 
  group_by(Пропуск) %>% 
  filter(correct_pos == 1) %>% 
  summarize(sum(Количество))

sem_5_poses <- sem_5 %>% 
  group_by(Пропуск) %>% 
  filter(correct_pos == 1) %>% 
  summarize(sum(Количество))

sem_6_poses <- sem_6 %>% 
  group_by(Пропуск) %>% 
  filter(correct_pos == 1) %>% 
  summarize(sum(Количество))

sem_1_sems <- sem_1 %>% 
  group_by(Пропуск) %>% 
  filter(correct_sem == 1) %>% 
  summarize(sum(Количество))

sem_2_sems <- sem_2 %>% 
  group_by(Пропуск) %>% 
  filter(correct_sem == 1) %>% 
  summarize(sum(Количество))

sem_3_sems <- sem_3 %>% 
  group_by(Пропуск) %>% 
  filter(correct_sem == 1) %>% 
  summarize(sum(Количество))

sem_4_sems <- sem_4 %>% 
  group_by(Пропуск) %>% 
  filter(correct_sem == 1) %>% 
  summarize(sum(Количество))

sem_5_sems <- sem_5 %>% 
  group_by(Пропуск) %>% 
  filter(correct_sem == 1) %>% 
  summarize(sum(Количество))

sem_6_sems <- sem_6 %>% 
  group_by(Пропуск) %>% 
  filter(correct_sem == 1) %>% 
  summarize(sum(Количество))


poses %>% 
  group_by(pos) %>% 
  summarize(mean(correct_pos_prop))

poses %>% 
  ggplot(aes(x = pos, y = correct_pos_prop, fill = pos)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Part of speech', y = 'Proportion of correct PoS guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none")

aov_model <- aov(poses$correct_pos_prop ~ poses$pos) # p-value = 0.02
summary(aov_model)
TukeyHSD(aov_model) # n-adj p-value = 0.03


poses %>% 
  ggplot(aes(x = len_word, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Длина слова', y = 'Доля угадываний') +
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

cor(poses$correct_pos_prop, poses$len_word, method = 'pearson')

poses %>% 
  ggplot(aes(x = len_word, y = correct_sem_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Длина слова', y = 'Доля угадываний') +
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

cor(poses$correct_sem_prop, poses$len_word, method = 'pearson')


poses %>% 
  group_by(pos) %>% 
  summarize(mean(correct_sem_prop))

poses %>% 
  ggplot(aes(x = pos, y = correct_sem_prop, fill = pos)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Part of speech', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none")

aov_model <- aov(poses$correct_sem_prop ~ poses$pos) # p-value = 0.2
summary(aov_model)
TukeyHSD(aov_model) # nothing



poses %>% 
  ggplot(aes(x = frequency_log, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Frequency (logarithm)', y = 'Proportion of correct guesses') +
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

cor(poses$correct_pos_prop, poses$frequency_log, method = 'pearson') # p-value = 0.13

poses %>% 
  ggplot(aes(x = frequency_log, y = correct_sem_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Frequency (logarithm)', y = 'Proportion of correct guesses') +
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

cor(poses$correct_sem_prop, poses$frequency_log, method = 'pearson') # p-value = 0.12

poses %>% 
  summarize(mean(correct_pos_prop), mean(correct_sem_prop), mean(correct_guesses_prop))


poses %>% 
  filter(predictability_type == 'global') %>% 
  ggplot(aes(x = number_prop, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  facet_wrap(~text) +
  labs(x = 'Position in text', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")


poses %>% 
  ggplot(aes(x = number_prop, y = correct_sem_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  facet_wrap(~text) +
  labs(x = 'Position in text', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 18)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")

poses_3 <- poses %>% 
  filter(text == 3)

cor(poses_3$correct_sem_prop, poses_3$number_prop, method = 'pearson')


poses_4 <- poses %>% 
  filter(text == 4)

cor(poses_4$correct_sem_prop, poses_4$number_prop, method = 'pearson')

poses_3 %>% 
  ggplot(aes(x = number_prop, y = correct_sem_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Position in text', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 20)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")


# Синтаксис

deps <- poses %>% 
  group_by(dependency) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_sem_prop))

deps %>% 
  ggplot(aes(x = dependency, y = count)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = 'Зависимость') +
  geom_text(aes(label = count), vjust = -0.5)

poses %>% 
  ggplot(aes(x = dependency, y = correct_guesses_prop, fill = dependency)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Зависимость', y = 'Доля угадываний') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

deps_pos <- poses %>% 
  group_by(pos, dependency) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop))

deps_pos %>% 
  ggplot(aes(x = dependency, y = count)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = 'Зависимость') +
  geom_text(aes(label = count), vjust = -0.5) +
  facet_wrap(~pos) +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 20))





# Тип предсказуемости

types <- poses %>% 
  group_by(predictability_type) %>% 
  rename(type = predictability_type) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_pos_prop))

poses %>% 
  ggplot(aes(x = predictability_type, y = correct_pos_prop, fill = predictability_type)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Predictability type', y = 'Proportion of correct PoS guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none")

poses_for_t <- poses %>% 
  filter(predictability_type != '-')

t.test(poses_for_t$correct_pos_prop ~ poses_for_t$predictability_type) # p-value = 0.005195


types <- poses %>% 
  group_by(predictability_type) %>% 
  rename(type = predictability_type) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_sem_prop))

poses %>% 
  ggplot(aes(x = predictability_type, y = correct_sem_prop, fill = predictability_type)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Predictability type', y = 'Proportion of correct semantics guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none")

poses_for_t <- poses %>% 
  filter(predictability_type != '-')

t.test(poses_for_t$correct_sem_prop ~ poses_for_t$predictability_type) # 0.001799



# is_middle

poses <- poses %>% 
  mutate(new_middle_as_factor = as.factor(is_middle_new))

middles <- poses %>% 
  group_by(is_middle_new) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop),
            mean_pos = mean(correct_pos_prop),
            mean_sem = mean(correct_sem_prop))

poses %>% 
  ggplot(aes(x = new_middle_as_factor, y = correct_pos_prop, fill = new_middle_as_factor)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Head and dependents both present', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none")

t.test(poses$correct_pos_prop ~ poses$new_middle_as_factor) # p-value = 0.1472
t.test(poses$correct_sem_prop ~ poses$new_middle_as_factor) # p-value = 0.07304


# Количество зависимых

poses %>% 
  ggplot(aes(x = count_dependents_new, y = correct_guesses_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Количество зависимых', y = 'Доля угадываний') +
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

cor(poses$correct_pos_prop, poses$count_dependents_new, method = 'pearson') # r = 0.05
cor(poses$correct_sem_prop, poses$count_dependents_new, method = 'pearson') # r = 0.06
cor(poses$correct_guesses_prop, poses$count_dependents_new, method = 'pearson') # r = 0.02


poses %>% 
  ggplot(aes(x = sum_frequency_log, y = correct_pos_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Частотность биграммы (логарифм)', y = 'Доля угадываний') +
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

cor(poses$correct_pos_prop, poses$sum_frequency_log, method = 'pearson') # r = 0.21

poses %>% 
  ggplot(aes(x = sum_frequency_log, y = correct_sem_prop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = 'Частотность биграммы (логарифм)', y = 'Доля угадываний') +
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

cor(poses$correct_sem_prop, poses$sum_frequency_log, method = 'pearson') # r = 0.24







verbs_root <- poses %>% 
  filter(pos == 'v',
         dependency == 'ROOT')

verbs_second <- poses %>% 
  filter(pos == 'v',
         dependency == 'acl' |
           dependency == 'advcl' |
           dependency == 'csubj' |
           dependency == 'ccomp' |
           dependency == 'xcomp')

verbs_second %>% 
  summarize(mean(correct_guesses_prop))

t.test(verbs_root$correct_pos_prop, verbs_second$correct_pos_prop) # p-value = 0.1742




















poses_verbs %>% 
  ggplot(aes(x = sem_tag_size, y = correct_guesses_prop)) +
  geom_point() +
  theme_classic() +
  labs(x = 'Частотность', y = 'Доля угадываний') +
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

poses_verbs %>% 
  ggplot(aes(x = sem_tag_size_log, y = correct_guesses_prop)) +
  geom_point() +
  theme_classic() +
  labs(x = 'Частотность', y = 'Доля угадываний') +
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



poses_verbs %>% 
  ggplot(aes(x = sem_tag_size, y = correct_sem_prop)) +
  geom_point() +
  theme_classic() +
  labs(x = 'Частотность', y = 'Доля угадываний') +
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


poses_verbs %>% 
  ggplot(aes(x = sem_tag_size, y = correct_pos_prop)) +
  geom_point() +
  theme_classic() +
  labs(x = 'Частотность', y = 'Доля угадываний') +
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


poses_verbs %>% 
  ggplot(aes(x = sem_tag_size_log, y = correct_pos_prop)) +
  geom_point() +
  theme_classic() +
  labs(x = 'Size of the semantic field (logarithm)', y = 'Proportion of correct PoS guesses') +
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


cor(poses_verbs$correct_sem_prop, poses_verbs$sem_tag_size_log, method = 'pearson') # r = 0.23

cor(poses_verbs$correct_guesses_prop, poses_verbs$sem_tag_size_log, method = 'pearson') # r = 0.1

cor(poses_verbs$correct_pos_prop, poses_verbs$sem_tag_size, method = 'pearson') # r = 0.3

cor(poses_verbs$correct_pos_prop, poses_verbs$sem_tag_size_log, method = 'pearson') # r = 0.32




cor(poses_verbs$correct_sem_prop, poses_verbs$sem_tag_size_2, method = 'pearson') # r = 0.05

cor(poses_verbs$correct_guesses_prop, poses_verbs$sem_tag_size_2, method = 'pearson') # r = 0.08

cor(poses_verbs$correct_pos_prop, poses_verbs$sem_tag_size_2, method = 'pearson') # r = -0.07


poses_verbs %>% 
  ggplot(aes(x = sem_tag_size_2, y = correct_guesses_prop)) +
  geom_point() +
  theme_classic() +
  labs(x = 'Частотность', y = 'Доля угадываний') +
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






sem <- poses_verbs %>% 
  group_by(sem) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop),
            mean_pos = mean(correct_pos_prop),
            mean_sem = mean(correct_sem_prop))


sem_for_t <- poses_verbs %>% 
  filter(sem != 3)

sem_for_t <- sem_for_t %>% 
  mutate(sem_descr = ifelse(sem == 1, 'action', 'state'))

poses_verbs <- poses_verbs %>% 
  mutate(sem_as_factor = as.factor(sem))


t.test(sem_for_t$correct_guesses_prop ~ sem_for_t$sem_descr) # p-value = 0.1575


sem_for_t %>% 
  ggplot(aes(x = sem_descr, y = correct_guesses_prop, fill = sem_descr)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Semantics', y = 'Proportion of correct guesses') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none")



t.test(sem_for_t$correct_pos_prop ~ sem_for_t$sem_descr) # p-value = 0.2313




poses_verbs %>% 
  ggplot(aes(x = sem_tag_2, y = correct_guesses_prop, fill = sem_tag_2)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = 'Поле', y = 'Доля угадываний') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))


sem_2 <- poses_verbs %>% 
  group_by(sem_tag_2) %>% 
  summarise(count = n(),
            mean_correct = mean(correct_guesses_prop),
            mean_pos = mean(correct_pos_prop),
            mean_sem = mean(correct_sem_prop))





cor(poses$correct_pos_prop, poses$correct_guesses_prop, method = 'pearson') # r = 0.269748

poses %>% 
  ggplot(aes(x = correct_pos_prop, y = correct_guesses_prop)) +
  geom_point() +
  theme_classic() +
  labs(x = 'Доля угадываний лексемы', y = 'Доля угадываний части речи') +
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


cor(poses$correct_sem_prop, poses$correct_guesses_prop, method = 'pearson') # r = 0.5493143

poses %>% 
  ggplot(aes(x = correct_sem_prop, y = correct_guesses_prop)) +
  geom_point() +
  theme_classic() +
  labs(x = 'Correct semantics guesses', y = 'Correct lexeme guesses') +
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


cor(poses$correct_pos_prop, poses$correct_sem_prop, method = 'pearson') # r = 0.3867881

poses %>% 
  ggplot(aes(x = correct_pos_prop, y = correct_sem_prop)) +
  geom_point() +
  theme_classic() +
  labs(x = 'Доля угадываний части речи', y = 'Доля угадываний семантического поля') +
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

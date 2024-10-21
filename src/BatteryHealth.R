library(tidyverse)
library(broom) 
library(MetBrewer)
library(patchwork)

batt.df = met.df |> filter(Var == 'BattV_Min') |> 
  bind_rows(bb.df |> filter(Var == 'BattV_Min')) |> 
  group_by(Date = as.Date(TIMESTAMP), sitename, Var) |> 
  summarise(batt_V = mean(value))


# Starting battery voltage 
p1 = batt.df |> filter(month(Date) == 12 | month(Date) == 8) |> 
  mutate(month = month(Date)) |> arrange(sitename, month) |> 
  ungroup() |> 
  ggplot() +
  geom_hline(aes(yintercept = 12), linetype = 2) +
  geom_boxplot(aes(x = factor(sitename), y = batt_V, fill = factor(month)), position = "identity") +
  scale_fill_manual(values = c('#bf6747', '#80bd96'), name = 'Month') +
  ylab('Battery Voltage Dec 2023') + xlab('Station') +
  theme_bw(base_size = 10) +
  theme(#legend.title = element_blank(),
        axis.title.x = element_blank())


# Battery declien
p2 = batt.df |> filter(Date >= as.Date('2024-05-01'), Date <= as.Date('2024-09-01')) |> 
  group_by(sitename) |> 
  mutate(battLoss =  batt_V - first(batt_V)) |> 
  ggplot() +
  geom_smooth(aes(x = Date, y = battLoss, col = sitename), method = 'lm', se = FALSE, linewidth = 0.5) +
  # geom_smooth(data = . %>% filter(sitename == 'LHBB'), aes(x = Date, y = battLoss), method = 'lm', color = 'black', linetype = 2) +
  scale_colour_met_d(palette_name = 'VanGogh2') +
  ylab('Battery Voltage') + 
  ylim(-0.8,0.2) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank())

p1/p2
ggsave('Figures/BatteryHealth_2024.png', width = 9, height = 7, dpi = 500)

library(broom)
batt.df |> filter(Date >= as.Date('2024-05-01'), Date <= as.Date('2024-09-01')) |> 
  group_by(sitename) %>%
  mutate(battLoss =  batt_V - first(batt_V)) %>% ungroup() |> 
  nest_by(sitename) %>%
  mutate(mod = list(lm(battLoss ~ Date, data = data))) %>%
  reframe(tidy(mod)) |> filter(term == 'Date') |> 
  arrange(estimate)


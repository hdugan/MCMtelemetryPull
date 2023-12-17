library(googledrive)
library(tidyverse)
library(lubridate)
library(MetBrewer)
library(patchwork)

# Find IDs of recent uploads
drive_find(n_max = 10)

### Remove temp files
removefiles = list(list.files("TempData/", full.names = TRUE, pattern = '.dat'))
do.call(file.remove, removefiles)

##### Download data to temp folder #####
# LFBB_15min.dat
drive_get(id = '1REy9USo6-5Bm_D1zVRIKaVF8bnXQxLlZ')
drive_download(as_id('1REy9USo6-5Bm_D1zVRIKaVF8bnXQxLlZ'), path = 'TempData/LFBB_15min.dat', overwrite = TRUE)

# LHBB_15min.dat
drive_get(id = '1-B-pKHyjdibOCevghndiTYnx-bXoAIWX')
drive_download(as_id('1-B-pKHyjdibOCevghndiTYnx-bXoAIWX'), path = 'TempData/LHBB_15min.dat', overwrite = TRUE)

# ELBBB_15min.dat
drive_get(id = '1PEsP9ctqs1POyRdQ9DLw3H3pKgFeyskP')
drive_download(as_id('1PEsP9ctqs1POyRdQ9DLw3H3pKgFeyskP'), path = 'TempData/ELBBB_15min.dat', overwrite = TRUE)

# WLBBB_15min.dat
drive_get(id = '10wlT2XtOglhaL2zosDCfwcD_PCm8Jds0')
drive_download(as_id('10wlT2XtOglhaL2zosDCfwcD_PCm8Jds0'), path = 'TempData/WLBBB_15min.dat', overwrite = TRUE)


files = list.files(path = 'TempData', full.names = T, pattern = '.dat')
files.short = list.files(path = 'TempData', pattern = '.dat')
sitenames = stringr::str_sub(files.short, 1,4)

bb.long.list = list() # For all sites
for (i in 1:length(sitenames)) {
  # indx = grepl(sitenames[i],files.short)
  file = files[i]
  sitename = sitenames[i]

  # Read in blue box file 
  site = read.csv(file, skip = 0, header = F, nrows = 1, as.is = T) |>
    select(2) |> pull(1)
  headers = read.csv(file, skip = 1, header = F, nrows = 1, as.is = T)
  bb.df = read_csv(file, skip = 3) |> mutate(sitename = sitename)
  colnames(bb.df) = c(headers, 'sitename')
  bb.df = bb.df |> mutate(Year = year(TIMESTAMP))

  # Convert to long 
  bb.df.long = bb.df |> select(-RECORD, -Year, -(Day_of_Year:DecTime_2)) |>
    arrange(TIMESTAMP) |>
    pivot_longer(cols = -c(TIMESTAMP, sitename), names_to = 'Var')

  bb.long.list[[i]] = bb.df.long
}

bb.df = do.call(bind_rows, bb.long.list) |> 
  mutate(sitename = factor(sitename, levels = c('LFBB','LHBB','ELBB','WLBB'))) |> 
  filter(TIMESTAMP >= as.POSIXct('2023-11-24'))

### Remove temp files
removefiles = list(list.files("TempData/", full.names = TRUE, pattern = '.dat'))
do.call(file.remove, removefiles)

### Plot 
p1 = ggplot(bb.df |> filter(Var == 'PTemp_C')) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-24'), Sys.Date()) +
  ylab('Temp (°C)') +
  scale_color_met_d(name = 'Egypt') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank())

p2 = ggplot(bb.df |> filter(Var == 'stage_Avg')) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-24'), Sys.Date()) +
  ylab('Stage (m)') +
  scale_color_met_d(name = 'Egypt') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank())

p3 = ggplot(bb.df |> filter(Var == 'OSat_Dshallow_Avg')) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-24'), Sys.Date()) +
  ylab('DO Shallow (%)') +
  scale_color_met_d(name = 'Egypt') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank())

p4 = ggplot(bb.df |> filter(Var == 'OSat_DOdeep_Avg')) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-24'), Sys.Date()) +
  ylab('DO Shallow (%)') +
  scale_color_met_d(name = 'Egypt') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank())

p5 = ggplot(bb.df |> filter(Var == 'ablation_Avg')) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-24'), Sys.Date()) +
  ylab('Ablation (m)') +
  scale_color_met_d(name = 'Egypt') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank())

p6 = ggplot(bb.df |> filter(Var == 'BattV_Min')) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-24'), Sys.Date()) +
  ylab('Battery (V)') +
  scale_color_met_d(name = 'Egypt') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank())

# Join plots 
p1 + p2 + p5 + p3 + p4 + p6 + plot_layout(guides = 'collect')
# Save figure 
ggsave('Figures/BB_Telemetry.pdf', width = 12, height = 10)

# Just battery plots

p.batt = ggplot(bb.df |> filter(Var == 'BattV_Min')) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-24'), Sys.Date()) +
  ylab('Battery (V)') +
  scale_color_met_d(name = 'Egypt') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~sitename)

# Save figure 
ggsave('Figures/BB_Battery.pdf', width = 12, height = 10)


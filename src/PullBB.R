library(googledrive)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)
source('src/DOconvert.R')

# for googledrive
# https://www.obrien.page/blog/2023/03_10_google_and_github_actions/
# drive_auth(path = 'ignore/mcmtelemetrypull-d1867fa65877.json') # for local
drive_auth(path = Sys.getenv('GDRIVE_PAT')) # for github action

# Find IDs of recent uploads
# drive_find(n_max = 10)

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
sitenames = substr(files.short, start = 1, stop = 4)

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

  # Add in salinity and DO solubility
  topS = top.sal |> filter(location == sitename) |> pull(salinity)
  botS = bot.sal |> filter(location == sitename) |> pull(salinity)
  
  bb.df = 
    bb.df |> 
    mutate(DOsolubilitytop = do(Temp_DOshallow_Avg + 273.15) * fs(Temp_DOshallow_Avg + 273.14, topS)) |> 
    mutate(DOsolubilitybot = do(Temp_DOdeep_Avg + 273.15) * fs(Temp_DOdeep_Avg + 273.14, botS)) |> 
    mutate(DO.actualShallow.mgl = DOsolubilitytop * OSat_Dshallow_Avg/100) |> 
    mutate(DO.actualDeep.mgl = DOsolubilitybot * OSat_DOdeep_Avg/100)
    
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


### Plot major variables
p.bb = bb.df |> filter(Var %in% c('PTemp_C', 'stage_Avg', 'OSat_Dshallow_Avg',
                                  'OSat_DOdeep_Avg', 'ablation_Avg', 'BattV_Min')) |>
  mutate(value = if_else(Var != 'PTemp_C' & value < 1, NA, value)) |>
  filter(TIMESTAMP >= as.POSIXct('2024-11-15')) |> 
  ggplot() +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2024-11-15'), Sys.Date() + 1) +
  # ylab('Temp (°C)') +
  scale_color_manual(values = c("#dd5129", "#0f7ba2", "#43b284", "#fab255")) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~Var, scales = 'free_y')

# Save figure 
# ggsave('Figures/BB_Telemetry.pdf', width = 12, height = 10)
ggsave(plot = p.bb, 'Figures/BB_Telemetry.png', width = 8, height = 5)


### Plot oxygen correct
p.oxygen = bb.df |> filter(Var %in% c(#'OSat_Dshallow_Avg','OSat_DOdeep_Avg', 
  "Conc_mgL_DOshallow_Avg", "Conc_mgL_DOdeep_Avg",
  "DO.actualShallow.mgl", "DO.actualDeep.mgl")) |> 
  filter(value > 1) |> 
  filter(TIMESTAMP >= as.POSIXct('2024-11-15'))|> 
  ggplot() +
  geom_path(aes(x = TIMESTAMP, y = value, color = Var)) +
  xlim(as.POSIXct('2024-11-15'), Sys.Date() + 1) +
  ylab('DO (mg/L)') +
  scale_color_manual(values = c("#dd5129", "#0f7ba2", "red4", "navy")) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~sitename, scales = 'free_y')

# Save figure 
ggsave(plot = p.oxygen, 'Figures/BB_Oxygen.png', width = 12, height = 10)


# Just battery plots
p.battery = ggplot(bb.df |> filter(Var == 'BattV_Min') |> 
                     filter(value > 1) |> 
                     filter(TIMESTAMP >= as.POSIXct('2024-11-15'))) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2024-11-15'), Sys.Date() + 1) +
  ylab('Battery (V)') +
  scale_color_manual(values = c("#dd5129", "#0f7ba2", "#43b284", "#fab255")) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~sitename)

# Save figure 
ggsave(plot = p.battery, 'Figures/BB_Battery.png', width = 12, height = 10)


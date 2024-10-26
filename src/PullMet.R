library(googledrive)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)
library(patchwork)

source('src/met_functions.R')

# Find IDs of recent uploads
# drive_find(n_max = 20)

# for googledrive
# https://www.obrien.page/blog/2023/03_10_google_and_github_actions/
# drive_auth(path = 'ignore/mcmtelemetrypull-d1867fa65877.json') # for local
drive_auth(path = Sys.getenv('GDRIVE_PAT')) # for github action

### Remove temp files
removefiles = list(list.files("TempData/", full.names = TRUE, pattern = '.dat'))
do.call(file.remove, removefiles)

##### Download data to temp folder #####
# BOYM15
drive_download(as_id('129Xy8dP5ghs4Sbk_FOmJdkiE_Zo18bP5'), path = 'TempData/BOYM15.dat', overwrite = TRUE)
# BRHM15
drive_download(as_id('13VKHeWqLe2QN7fYdskBxkPwWrvXy5wYh'), path = 'TempData/BRHM15.dat', overwrite = TRUE)
# CAAM15
drive_download(as_id('15Fe8HHJMkumgu7cYmqM2nmVV-csG_bUD'), path = 'TempData/CAAM15.dat', overwrite = TRUE)
# COHM15
drive_download(as_id('13MdIZKde_XaqExVfjNM-34NZAeMd-WMJ'), path = 'TempData/COHM15.dat', overwrite = TRUE)
# EXEM15
drive_download(as_id('13DD4emyuwF-mhnGPO_pOldFm6Hk34p7C'), path = 'TempData/EXEM15.dat', overwrite = TRUE)
# FLMM15
drive_download(as_id('17cdUcPgkRHweXxvTp4W5P6qPEQAXAxKh'), path = 'TempData/FLMM15.dat', overwrite = TRUE)
# FRSM15
drive_download(as_id('17oWPOd_aZYSPo436r5c7ioalbHKDP2cX'), path = 'TempData/FRSM15.dat', overwrite = TRUE)
# FRLM15
drive_download(as_id('15anL99xr6c6u0KxQJj8CZPAyknDcA4q6'), path = 'TempData/FRLM15.dat', overwrite = TRUE)
# HODM15
drive_download(as_id('15VxBHxdX0bsI-W1LKN6tOxUq_HH7tx-Z'), path = 'TempData/HODM15.dat', overwrite = TRUE)
# HO2M15
drive_download(as_id('1-nf_T9RlSX6G4oxF8ol4x4zdOoMJu7i2'), path = 'TempData/HO2M15.dat', overwrite = TRUE)
# TARM15
drive_download(as_id('17Q-WIIASlNG-LmSmIIZpHu1LWGO5B1VK'), path = 'TempData/TARM15.dat', overwrite = TRUE)
# VAAM15
drive_download(as_id('13VGm6aX6H5gepaWzchw_8eiS_0LBoEzR'), path = 'TempData/VAAM15.dat', overwrite = TRUE)
# VIAM15
drive_download(as_id('15CiY1JvquJdIB7adFH_dg9qe6HU52kJ1'), path = 'TempData/VIAM15.dat', overwrite = TRUE)
# 
# drive_download(as_id(''), path = 'TempData/.dat', overwrite = TRUE)


files = list.files(path = 'TempData', full.names = T, pattern = '.dat')
files.short = list.files(path = 'TempData', pattern = '.dat')
sitenames = stringr::str_sub(files.short, 1,4)

met.long.list = list() # For all sites
for (i in 1:length(sitenames)) {
  # indx = grepl(sitenames[i],files.short)
  file = files[i]
  sitename = sitenames[i]
  
  # Read in met file 
  site = read.csv(file, skip = 0, header = F, nrows = 1, as.is = T) |>
    select(2) |> pull(1)
  headers = read.csv(file, skip = 1, header = F, nrows = 1, as.is = T)
  met.df = read_csv(file, skip = 3) |> mutate(sitename = sitename)
  colnames(met.df) = c(headers, 'sitename')
  met.df = met.df |> mutate(Year = year(TIMESTAMP))
  
  # # Apply rclow to air temperatures
  # met.df = met.df %>%
  #   group_by(1:n()) %>% 
  #   mutate_at(vars(contains('AirT')), ~rclow(.))

  if (!'Day_of_Year' %in% names(met.df)){
    met.df = met.df |> mutate(Day_of_Year = yday(TIMESTAMP))
    met.df = met.df |> mutate(DecTime_2 = NA_complex_)
  }
  
  # Convert to long 
  met.df.long = met.df |> select(-RECORD, -Year, -(Day_of_Year:DecTime_2)) |>
    arrange(TIMESTAMP) |>
    pivot_longer(cols = -c(TIMESTAMP, sitename), names_to = 'Var')
  
  met.long.list[[i]] = met.df.long
}

met.df = do.call(bind_rows, met.long.list) |> 
  filter(TIMESTAMP >= as.POSIXct('2023-11-22'))

### Remove temp files
removefiles = list(list.files("TempData/", full.names = TRUE, pattern = '.dat'))
do.call(file.remove, removefiles)

# get table of variables
table(met.df$Var)

# Just battery plots (all stations)
p.batt = ggplot(met.df |> filter(Var == 'BattV_Min')) +
  geom_hline(aes(yintercept = 12), linetype = 2, linewidth = 0.2) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-24'), Sys.Date() + 1) +
  ylim(9,NA) +
  ylab('Battery (V)') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~sitename, ncol = 3)

# Save figure 
ggsave('Figures/Met_Battery.png', width = 12, height = 10)

# Just pressure plot
p.pressure = ggplot(met.df |> filter(Var == 'Pressure')) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-24'), Sys.Date() + 1) +
  ylab('Pressure (mb)') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) 

# Save figure 
ggsave('Figures/Met_Pressure.png', width = 12, height = 10)

# Plot of sonics 
p.sonic = ggplot(met.df |> filter(Var %in% c('Depth')) |> filter(value > 0)) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-24'), Sys.Date() + 1) +
  ylab('Distance (cm)') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~sitename, scales = 'free_y', ncol = 3)

# Save figure 
ggsave('Figures/Met_Sonic.png', width = 12, height = 10)

# Figure of soil temp
p.soil0 = ggplot(met.df |> filter(Var %in% c('SoilT0cm'))) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  geom_path(data = met.df |> filter(Var %in% c('SoilT10cm')), aes(x = TIMESTAMP, y = value), 
            color = 'black', width = 0.3) +
  xlim(as.POSIXct('2023-11-24'), Sys.Date() + 1) +
  ylab('Soil Temp (°C)') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~sitename, scales = 'free_y', ncol = 3)
# Save figure 
ggsave(plot = p.soil0, 'Figures/Met_SoilTemp0.png', width = 12, height = 10)


# Figure of airtemp + wind speed from met stations 
p.air = ggplot(met.df |> filter(Var == 'AirT3m')) +
  geom_path(data = met.df |> filter(Var == 'WSpd_Avg'), 
            aes(x = TIMESTAMP, y = value), color = 'black', linewidth = 0.3) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-24'), Sys.Date() + 1) +
  ylab('Air Temp (°C) and Wind Speed (m/s)') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~sitename, ncol = 3)

# Save figure 
ggsave(plot = p.air, 'Figures/Met_AirT3m_WSpd.png', width = 12, height = 10)


# Glacier stations
glacier.df = met.df |> 
  filter(sitename %in% c('CAAM','COHM','HODM','TARM'))

met.df = met.df |> 
  filter(!sitename %in% c('CAAM','COHM','HODM','TARM'))

### Plot major variables
p.metground = met.df |> filter(Var %in% c('AirT3m', 'WSpd_Avg', 'SwRadIn',
                            'BattV_Min')) |> 
  ggplot() +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-22'), Sys.Date() + 1) +
  # ylab('Temp (°C)') +
  scale_color_manual(values = c("#bd3106", "#d9700e", "#e9a00e", "#eebe04", "#5b7314",
                                "#c3d6ce", "#89a6bb", "#454b87", "#190730")) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~Var, scales = 'free_y')

# Save figure 
# ggsave('Figures/Met_Ground_Telemetry.pdf', width = 12, height = 10)
ggsave(plot = p.metground, 'Figures/Met_Ground_Telemetry.png', width = 8, height = 5)

### Plot major variables
p.glaciers = glacier.df |> filter(Var %in% c('AirT3m', 'WSpd_Avg', 'SwRadIn',
                                'BattV_Min')) |> 
  ggplot() +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-22'), Sys.Date() + 1) +
  # ylab('Temp (°C)') +
  scale_color_manual(values = c("#bd3106", "#eebe04", "#5b7314",
                                "#454b87")) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~Var, scales = 'free_y')

# Save figure 
# ggsave('Figures/Met_Glacier_Telemetry.pdf', width = 12, height = 10)
ggsave(plot = p.glaciers, 'Figures/Met_Glacier_Telemetry.png', width = 8, height = 5)

## Plot precipitation at Bonney and Hoare
precip = met.df |> filter(sitename %in% c('BOYM', 'HO2M'), Var == 'Precip_TotalNRT')

p.precip = ggplot(precip) +
  geom_point(aes(x = TIMESTAMP, y = value, fill = sitename), shape = 21, stroke = 0.01) +
  ylab('Precip Total NRT (mm)') +
  scale_fill_manual(values = c("#bd3106", 
                               "#454b87")) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank())

# ggsave('Figures/Met_Precip_Telemetry.pdf', width = 6, height = 4)
ggsave(plot = p.precip, 'Figures/Met_Precip_Telemetry.png', width = 8, height = 4)

# Last week at Lake Fryxell 
fryx = met.df |> filter(sitename %in% c('FRLM')) |> 
  filter(Var %in% c('AirT3m', 'WSpd_Avg', 'SwRadIn')) |> 
  pivot_wider(names_from = Var, values_from = value) |> 
  filter(TIMESTAMP >= as.POSIXct(Sys.Date() - 7))

p1 = ggplot(fryx) +
  geom_path(aes(x = TIMESTAMP, y = AirT3m), color = "#bd3106") +
  xlim(as.POSIXct(Sys.Date() - 7), Sys.Date() + 1) +
  ylab('Air Temp (°C)') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) +
  labs(title = 'Last week at Lake Fryxell') 

p2 = ggplot(fryx) +
  geom_path(aes(x = TIMESTAMP, y = SwRadIn), color = "#eebe04") +
  xlim(as.POSIXct(Sys.Date() - 7), Sys.Date() + 1) +
  ylab('Solar Radiation (W/m2)') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) 

p3 = ggplot(fryx) +
  geom_path(aes(x = TIMESTAMP, y = WSpd_Avg), color = "#82a1bd") +
  xlim(as.POSIXct(Sys.Date() - 7), Sys.Date() + 1) +
  ylab('Wind Speed (m/s)') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) 

p.frx = p1 / p2 / p3
ggsave(plot = p.frx, 'Figures/Met_FryxellCurrent.png', width = 6, height = 6, dpi = 500)



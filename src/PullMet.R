library(googledrive)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)

# Find IDs of recent uploads
# drive_find(n_max = 10)

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

# Just battery plots
p.batt = ggplot(met.df |> filter(Var == 'BattV_Min')) +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-24'), Sys.Date() + 1) +
  ylab('Battery (V)') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~sitename, ncol = 3)

# Save figure 
ggsave('Figures/Met_Battery.pdf', width = 12, height = 10)

# Glacier stations
glacier.df = met.df |> 
  filter(sitename %in% c('CAAM','COHM','HODM','TARM'))

met.df = met.df |> 
  filter(!sitename %in% c('CAAM','COHM','HODM','TARM'))

### Plot major variables
met.df |> filter(Var %in% c('AirT3m', 'WSpd_Avg', 'SwRadIn',
                            'BattV_Min')) |> 
  ggplot() +
  geom_path(aes(x = TIMESTAMP, y = value, color = sitename)) +
  xlim(as.POSIXct('2023-11-22'), Sys.Date() + 1) +
  # ylab('Temp (°C)') +
  scale_color_manual(values = c("#bd3106", "#d9700e", "#e9a00e", "#eebe04", "#5b7314",
                                "#c3d6ce", "#89a6bb", "#454b87")) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~Var, scales = 'free_y')

# Save figure 
ggsave('Figures/Met_Ground_Telemetry.pdf', width = 12, height = 10)
ggsave('Figures/Met_Ground_Telemetry.png', width = 8, height = 5)

### Plot major variables
glacier.df |> filter(Var %in% c('AirT3m', 'WSpd_Avg', 'SwRadIn',
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
ggsave('Figures/Met_Glacier_Telemetry.pdf', width = 12, height = 10)
ggsave('Figures/Met_Glacier_Telemetry.png', width = 8, height = 5)

## Plot precipitation at Bonney and Hoare
precip = met.df |> filter(sitename %in% c('BOYM', 'HO2M'), Var == 'Precip_TotalNRT')

ggplot(precip) +
  geom_point(aes(x = TIMESTAMP, y = value, fill = sitename), shape = 21, stroke = 0.01) +
  ylab('Precip Total NRT (mm)') +
  scale_fill_manual(values = c("#bd3106", 
                               "#454b87")) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank())

ggsave('Figures/Met_Precip_Telemetry.pdf', width = 6, height = 4)
ggsave('Figures/Met_Precip_Telemetry.png', width = 8, height = 4)

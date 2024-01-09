library(oce)

# https://water.usgs.gov/admin/memo/QW/qw11.03.pdf
# https://water.usgs.gov/water-resources/memos/documents/WQ.2011.03.pdf
# Benson Karuse equation

# Benson and Krause derived a series of equations from thermodynamic principles that compute
# the solubility of oxygen in water. 
# All three sets of equations apply to a temperature range of 0–40 °C, a salinity range of 0–40‰,
# and a pressure range of 0.5–1.1 atmospheres (380–836 mm Hg). 

# To compute the oxygen solubility in mg/L, their
#baseline DO concentration at zero salinity and one atmosphere 
# tempK = tempC + 273.15
do <- function(temp) {
  exp(-139.34411 + (1.575701e5/temp) - (6.642308e7/(temp^2)) + (1.243800e10/(temp^3)) - (8.621949e11/(temp^4)))
}

#salinity factor
fs <- function(temp, salinity) {
  exp(-salinity * (0.017674 - (10.754/temp) + (2140.7/(temp^2))))
}

#pressure factor
oo <- function(temp) {
  0.000975 - (1.426e-5 * temp) + (6.436e-8 * (temp^2))
}

u <- function(temp) {
  exp(11.8571 - (3840.70/temp) - (216961/(temp^2)))
}
ps <- function(pressure, temp) {
  (pressure - u(temp)) * (1 - (oo(temp) * pressure)) / ((1- u(temp)) * (1-oo(temp)))
}

# ps(pressure = 1, temp = 10+273.15)

########################### Derive lake salinities from CTD casts #######################
# Dissolved oxygen sensors deployed at:
# LF BB 6.67 m
# 6.645965861	1.042193	2.0554

LFSal.top = swSCTp(
  conductivity = 1.04, temperature = 2.05, pressure = NULL,
  conductivityUnit = "mS/cm", eos = getOption("oceEOS", default = "gsw")
)

# Dissolved oxygen sensors deployed at:
# LF BB 8.85 m
# 8.830492502	2.714378	2.6859

LFSal.bot = swSCTp(
  conductivity = 2.71, temperature = 2.68, pressure = NULL,
  conductivityUnit = "mS/cm", eos = getOption("oceEOS", default = "gsw")
)

# Dissolved oxygen sensors deployed at:
# LH BB 6.65 m
# 6.621754936	0.359222	0.993

LHSal.top = swSCTp(
  conductivity = 0.359, temperature = 0.993, pressure = NULL,
  conductivityUnit = "mS/cm", eos = getOption("oceEOS", default = "gsw")
)

# Dissolved oxygen sensors deployed at:
# LH BB 10.10 m
# 10.15171817	0.492027	0.8511

LHSal.bot = swSCTp(
  conductivity = 0.492, temperature = 0.851, pressure = NULL,
  conductivityUnit = "mS/cm", eos = getOption("oceEOS", default = "gsw")
)

# Dissolved oxygen sensors deployed at:
# ELB 15.95 m - 
# 15.95376338	17.617249	4.654

ELBSal.top = swSCTp(
  conductivity = 17.61, temperature = 4.65, pressure = NULL,
  conductivityUnit = "mS/cm", eos = getOption("oceEOS", default = "gsw")
)

# ELB 17.95 m -
# 17.94985271	30.278634	4.5745

ELBSal.bot = swSCTp(
  conductivity = 30.27, temperature = 4.57, pressure = NULL,
  conductivityUnit = "mS/cm", eos = getOption("oceEOS", default = "gsw")
)

# WLB 17.39 m - 
# 17.46486026	46.997003	0.5772
WLBSal.top = swSCTp(
  conductivity = 46.99, temperature = 0.57, pressure = NULL,
  conductivityUnit = "mS/cm", eos = getOption("oceEOS", default = "gsw")
)

# WLB 17.88 m - 
# 17.90411112	54.371638	0.3772
WLBSal.bot = swSCTp(
  conductivity = 54.37, temperature = 0.377, pressure = NULL,
  conductivityUnit = "mS/cm", eos = getOption("oceEOS", default = "gsw")
)

#### Data frame of salinities 
top.sal = data.frame(location = c("ELBB", "LFBB", "LHBB", "WLBB"), 
                     depth = c(15.95, 6.67, 6.65, 17.47),
                     salinity = c(ELBSal.top, LFSal.top, LHSal.top, WLBSal.top))

bot.sal = data.frame(location = c("ELBB", "LFBB", "LHBB", "WLBB"), 
                     depth = c(17.95, 8.85, 10.10, 17.88),
                     salinity = c(ELBSal.bot, LFSal.bot, LHSal.bot, WLBSal.bot))

# fs(10 + 273.14, 35)
# 
# df = data.frame(temp = 0:35) %>% 
#   mutate(Freshwater_1atm = do(temp + 273.15)) %>% 
#   mutate(Freshwater_2atm = Freshwater_1atm * ps(pressure = 2, temp)) %>%
#   mutate(ELBSal.top = Freshwater_1atm * fs(temp + 273.14, ELBSal.top)) |> 
#   mutate(ELBSal.top_2atm = Freshwater_2atm * fs(temp + 273.14, ELBSal.top))
#   # mutate(ELBSal.bot = Freshwater_1atm * fs(temp + 273.14, ELBSal.bot)) |> 
#   # mutate(WLBSal.top = Freshwater_1atm * fs(temp + 273.14, WLBSal.top)) |> 
#   # mutate(WLBSal.bot = Freshwater_1atm * fs(temp + 273.14, WLBSal.bot))
# 
# df2 = pivot_longer(df, cols = -temp)
# 
# ggplot(df2) + geom_path(aes(x = temp, y = value, color = name)) +
#   scale_colour_viridis_d(name = '') +
#   xlab('Water Temp (°C)') + ylab('Oxygen solubility (mg/L)') +
#   theme_bw(base_size = 8) +
#   labs(title = 'Solubility of oxygen in water')

# 
# ##################
# 7 2023-11-24 06:00:00 ELBB     Temp_DOdeep_Avg          3.98
# 8 2023-11-24 06:00:00 ELBB     OSat_DOdeep_Avg        409.  
# 9 2023-11-24 06:00:00 ELBB     Conc_mgL_DOdeep_Avg     54.0 
# 10 2023-11-24 06:00:00 ELBB     Conc_ppm_DOdeep_Avg     54.0 
# 11 2023-11-24 06:00:00 ELBB     Temp_DOshallow_Avg       4.10
# 12 2023-11-24 06:00:00 ELBB     OSat_Dshallow_Avg      530.  
# 13 2023-11-24 06:00:00 ELBB     Conc_mgL_DOshallow_Avg  69.8 
# 14 2023-11-24 06:00:00 ELBB     Conc_ppm_DOshallow_Avg  69.8
# 
# ELB.bot.solubility = do(3.98 + 273.15) * fs(3.98 + 273.14, ELBSal.bot) # 17.95 m 
# ELB.bot.solubility * 409/100 #(actual in mg/L)
# 
# ELB.top.solubility = do(4.10 + 273.15) * fs(4.10 + 273.14, ELBSal.top) # 15.95 m 
# ELB.top.solubility * 530/100 #(actual in mg/L)
# 
# 
# 7 2023-11-24 06:00:00 WLBB     Temp_DOdeep_Avg      -0.269
# 8 2023-11-24 06:00:00 WLBB     OSat_DOdeep_Avg     328.   
# 9 2023-11-24 06:00:00 WLBB     Conc_mgL_DOdeep_Avg  48.7  
# 10 2023-11-24 06:00:00 WLBB     Conc_ppm_DOdeep_Avg  48.7  
# 
# 11 2023-11-24 06:00:00 WLBB     Temp_DOshallow_Avg      -0.443
# 12 2023-11-24 06:00:00 WLBB     OSat_Dshallow_Avg      102.   
# 13 2023-11-24 06:00:00 WLBB     Conc_mgL_DOshallow_Avg  15.1  
# 14 2023-11-24 06:00:00 WLBB     Conc_ppm_DOshallow_Avg  15.1 
# 
# WLB.bot.solubility = do(-0.269 + 273.15) * fs(-0.269 + 273.14, WLBSal.bot) # 17.88 m 
# WLB.bot.solubility * 328/100 #(actual in mg/L)
# 
# WLB.top.solubility = do(-0.443 + 273.15) * fs(-0.443 + 273.14, WLBSal.top) # 17.39 m 
# WLB.top.solubility * 102/100 #(actual in mg/L)
# 

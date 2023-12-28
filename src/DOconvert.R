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



# Convert DO solubility to percent
# Dissolved oxygen sensors deployed at:
# ELB 15.95 m - 
# 15.95472694	18.460161	4.4769
# 15.95376338	17.617249	4.654

ELBSal.top = swSCTp(
  conductivity = 18,
  temperature = 4.5,
  pressure = NULL,
  conductivityUnit = "mS/cm",
  eos = getOption("oceEOS", default = "gsw")
)

# ELB 17.95 m -
# 17.99264851	29.929814	4.5704
# 17.908	29.949	4.514
# 17.90775291	29.582	4.483

ELBSal.bot = swSCTp(
  conductivity = 29.9,
  temperature = 4.5,
  pressure = NULL,
  conductivityUnit = "mS/cm",
  eos = getOption("oceEOS", default = "gsw")
)

# WLB 17.39 m - 
# 17.43325096	47.339193	0.7324
# 17.44999946	49.099081	0.2503
# 17.47	51.32633	0.3073
WLBSal.top = swSCTp(
  conductivity = 50,
  temperature = 0.3,
  pressure = NULL,
  conductivityUnit = "mS/cm",
  eos = getOption("oceEOS", default = "gsw")
)

# WLB 17.88 m - 
# 17.886	56.028133	0.0929
# 17.90758847	55.347987	-0.0614
WLBSal.bot = swSCTp(
  conductivity = 55,
  temperature = 0.07,
  pressure = NULL,
  conductivityUnit = "mS/cm",
  eos = getOption("oceEOS", default = "gsw")
)

top.sal = data.frame(location = c("ELBB", "LFBB", "LHBB", "WLBB"), 
                     depth = c(15.95, NA, NA, 17.47),
                     salinity = c(ELBSal.top, NA, NA, WLBSal.top))

bot.sal = data.frame(location = c("ELBB", "LFBB", "LHBB", "WLBB"), 
                     depth = c(17.95, NA, NA, 17.88),
                     salinity = c(ELBSal.bot, NA, NA, WLBSal.bot))

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

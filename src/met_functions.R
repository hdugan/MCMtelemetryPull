# '********************************************************
# rclow = reverse temperatures to mV and apply clow subroutine to mV values using
# Steinhart-Hart equation
# Temp is the unconverted temperature
# TOffset is the offset betwen individual thermister and factory standard

# Originally coded in VB by TH Nylen, 3-Sep-1999
# '********************************************************

# rclow ####
rclow <- function(Temp, TOffset = 0) {
  
  # Return NA if temp is NA
  if(is.na(Temp)){
    return(NA)
  }
  
  # Define constants 
  c0 = -53.4601
  c1 = 9.08067
  c2 = -0.832569
  c3 = 0.0522829
  c4 = -0.00167234
  c5 = 2.21098E-05
  
  x1 = 0
  x2 = 25
  acc = 1E-06
  maxl = 90
  fmid = (c0 + c1 * x2 + c2 * x2 * x2 + c3 * x2 * x2 * x2 + c4 * x2 * x2 * x2 * x2 + c5 * x2 * x2 * x2 * x2 * x2) - Temp
  f = (c0 + c1 * x1 + c2 * x1 * x1 + c3 * x1 * x1 * x1 + c4 * x1 * x1 * x1 * x1 + c5 * x1 * x1 * x1 * x1 * x1) - Temp
  
  bisec = fmid * f
  
  if (f < 0){
    root2 = x1
    dx = x2 - x1
  } else {
    root2 = x2
    dx = x1 - x2
  }
  
  for(i in 1:maxl) {
    dx = dx * 0.5
    xmid = root2 + dx
    fmid = (c0 + c1 * xmid + c2 * xmid * xmid + c3 * xmid * xmid * xmid + c4 * xmid * xmid * xmid * xmid + c5 * xmid * xmid * xmid * xmid * xmid) - Temp
    
    if (fmid < 0) {
      root2 = xmid
    }
    
    if (abs(dx) < acc) {
      fmid = 0
    } else {
      i = maxl
    }
  }
  
  # convert finally value to mv
  mv = root2 / 4
   
  # convert mv to temp using CSI Polynomial
    a = 0.00084636
    b = 0.00020627
    c = 8.6958E-08

    rs = 1000 * (2000 / mv - 1) - 249000
    lnrs = log(rs)
    result = 1 / (a + b * (lnrs - TOffset) + c * (lnrs - TOffset) ^ 3)
   
    
  if (bisec > 0.1) {
    rclow = NA
  } else {
    rclow = result - 273.16
  }
    
  if (Temp == -6999) {
    rclow = NA
  }
   
  return(rclow) 
}

# rclow.bisec ####
rclow.bisec <- function(Temp, TOffset = 0) {
  
  # Define constants 
  c0 = -53.4601
  c1 = 9.08067
  c2 = -0.832569
  c3 = 0.0522829
  c4 = -0.00167234
  c5 = 2.21098E-05
  
  x1 = 0
  x2 = 25
  acc = 1E-06
  maxl = 90
  fmid = (c0 + c1 * x2 + c2 * x2 * x2 + c3 * x2 * x2 * x2 + c4 * x2 * x2 * x2 * x2 + c5 * x2 * x2 * x2 * x2 * x2) - Temp
  f = (c0 + c1 * x1 + c2 * x1 * x1 + c3 * x1 * x1 * x1 + c4 * x1 * x1 * x1 * x1 + c5 * x1 * x1 * x1 * x1 * x1) - Temp
  
  bisec = fmid * f
  
  return(bisec) 
}

# '********************************************************
#### Flag - Definition (data processing) --> Data manager flag 
# R - Out of range --> Flag as R, except flag as "U" when IceT20cm exceeds 0 degrees and "V" when IceT1m exceeds 0 degrees
# Z - Negative values zeroed out (convert to zero)
# T - Bad value, below zeroing value (value omitted) --> Flag as F
# B - Bad value, -6999 or questionable (value omitted) 
# F - Bad value, raw temp -53C and 32.79C which exceeds the bracketed limit for bisection (value omitted) --> Flag as B
# S - SwRadOut is greater than a % of SwRadIn
# N - Wdir and WDirStD zeroed out because WSpd = 0 (Converted to zero)
# M - Value missing 
# '********************************************************

# Temperature flags
# Max and min temperature ranges typically 15 and -70


# range.flag ####
rclow.flag <- function(Temp, Toffset = 0, Max = 15, Min = -70){
  
  adj.temp = rclow(Temp, TOffset) 
  
  # This can happen when temp is high (> 32) 
  if (is.na(adj.temp)) {
    flag = 'F'
    return(flag)
  }
  
  bisec = rclow.bisec(Temp, TOffset)
  
  if (adj.temp > Max | adj.temp < Min) {
    if (bisec > 0.1) {
      flag = 'F'
    } else {
      flag = 'R'
    }
  } else {
    flag = ''
  }
  
  if (Temp == -6999) {
    flag = 'B'
  }
  return(flag)
}

# Relative humidity correction note: All of the relative humidity (RH) values were corrected for a systematic error in the
# measurement created by an instrument manufacturer error. All RH data with air temperatures below freezing were
# corrected using the vapor pressure over ice (rather than over water which was used initially). The error became quite large
# for very cold temperatures (the correction could grow to around 30%). The polynomials used for the correction is based on
# Lowe (1977).

# =[RH3m]*(6.107799961 + [AirT3m] * (0.4436518521 + [AirT3m] * (0.01428945805 + [AirT3m] * (0.0002650648471 +
# [AirT3m] *(0.000003031240396 + [AirT3m] * (0.00000002034080948 + 0.00000000006136820929 * [AirT3m])))))) /
# (6.109177956 + [AirT3m] * (0.503469897 + [AirT3m] * (0.01886013408 + [AirT3m] * (0.0004176223716 + [AirT3m] *
# (0.00000582472028 + [AirT3m] * (0.00000004838803174 + 0.0000000001838826904 * [AirT3m]))))))
# Relative Humidity values are capped between 0 to 100%. 
# Any values that fall outside of this range are flagged as ‘R’.

# rh.corr ####
rh.corr <- function(rh, airt) {
  rh.adj =rh*(6.107799961 + airt * (0.4436518521 + airt * (0.01428945805 + airt * (0.0002650648471 + airt *(0.000003031240396 + airt * (0.00000002034080948 + 6.136820929E-11 * airt)))))) / (6.109177956 + airt * (0.503469897 + airt * (0.01886013408 + airt * (0.0004176223716 + airt * (0.00000582472028 + airt * (0.00000004838803174 + 0.0000000001838826904 * airt))))))
  return(rh.adj)
}


# rclow.flag ####
range.flag <- function(value, Max, Min) {
  
  if (is.na(value)) {
    flag = 'M'
    return(flag)
  }
  
  if (value > Max | value < Min) {
    flag = 'R'
  } else {
    flag = ''
  }
  
  if (value == -6999 | value == 99999 | value == 6999) {
    flag = 'B'
  }
  
  return(flag)
}

# '********************************************************
# '  r2: improved calculation of longwave radiation
# '      using adjustments for case and dome temperatures
# '
# '      Assumes albedo of 1.0
# '********************************************************/

# LwRad2 ####
LwRad2 <- function(DomeTemp, Thermopile, CaseTemp, Multiplier, Near0_Min) {

  s_b = 5.67E-08  #Stefan-Boltzman constant
  k = 3.5           #NOAA-CMDL recommended offset (sensor-specific) */;

  c1 = 0.0010295    # Eppley recommended 
  c2 = 0.0002391    # Eppley recommended 
  c3 = 1.568E-07    # Eppley recommended 
  
  if (DomeTemp == -6999 | CaseTemp == -6999 | Thermopile == -6999) {
    LwRad2 = -6999
  } else {
    caseT2 = 1 / (c1 + (c2 * (Log(1000 * (1 / CaseTemp - 1)))) + (c3 * (Log(1000 * (1 / CaseTemp - 1))) ^ 3))
    domeT2 = 1 / (c1 + (c2 * (Log(1000 * (1 / DomeTemp - 1)))) + (c3 * (Log(1000 * (1 / DomeTemp - 1))) ^ 3))
    LwRad2 = ((Thermopile) * Multiplier) + (s_b * (caseT2 ^ 4)) + (k * s_b * ((caseT2 ^ 4) - (domeT2 ^ 4)))
  }

  if (LwRad2 < 0 & LwRad2 > Near0_Min) {
    LwRad2 = 0
  } 
  if (LwRad2 != -6999 & LwRad2 <= Near0_Min) {
    LwRad2 = NA
  } 
  return(LwRad2)
  
}

# LwRad2flag ####
LwRad2flag <- function(DomeTemp, Thermopile, CaseTemp, Multiplier, Near0_Min) {
  s_b = 5.67E-08  #Stefan-Boltzman constant
  k = 3.5           #NOAA-CMDL recommended offset (sensor-specific) */;
  
  c1 = 0.0010295    # Eppley recommended 
  c2 = 0.0002391    # Eppley recommended 
  c3 = 1.568E-07    # Eppley recommended 
  
  if (DomeTemp == -6999 | CaseTemp == -6999 | Thermopile == -6999) {
    LwRad2 = -6999
  } else {
    caseT2 = 1 / (c1 + (c2 * (Log(1000 * (1 / CaseTemp - 1)))) + (c3 * (Log(1000 * (1 / CaseTemp - 1))) ^ 3))
    domeT2 = 1 / (c1 + (c2 * (Log(1000 * (1 / DomeTemp - 1)))) + (c3 * (Log(1000 * (1 / DomeTemp - 1))) ^ 3))
    LwRad2 = ((Thermopile) * Multiplier) + (s_b * (caseT2 ^ 4)) + (k * s_b * ((caseT2 ^ 4) - (domeT2 ^ 4)))
  }
        
  if (LwRad2 > -5 & LwRad2 <= 400) {
    LwRad2flag = ''
  } 
  if (LwRad2 > 400) {
    LwRad2flag = 'R'
  }
  if (LwRad2 <= -5 & DomeTemp != -6999 | CaseTemp != -6999 | Thermopile != -699) {
    LwRad2flag = 'T'
  }
  
  return(LwRad2flag)
}


# '********************************************************
# '  RadCorr: post-processing to multiply raw value by post-processing
# '     factor and change small negative values to zero
# '********************************************************/
  
# RadCorr ####

RadCorr <- function(value, Process_Factor, Near0_Min) {
  RadTCorr = value * Process_Factor
  RadCorr = RadTCorr
  
  if (RadTCorr >= Near0_Min & RadTCorr < 0) {
    RadCorr = 0
  }
  if (RadTCorr < Near0_Min) {
    RadCorr = NA
  } 
  if (value == -6999) {
    RadCorr = NA
  } 
  return(RadCorr)
}

# '********************************************************
# '  RadFlag: Flag Max Values, plus flag when change small
# '           negative values to zero and when below Near0
# '           minimal
# '********************************************************/
# '                   Thomas Nylen
# '                   Portland State University
# '                   Septemper 6, 1999

# RadFlag ####
RadFlag <- function(value, Process_Factor, Near0_Min, Max) {
  
  RadFCorr = value * Process_Factor
  
  if(RadFCorr < 0 & RadFCorr >= Near0_Min) {
    RadFlag = 'Z'
  } 
  if(RadFCorr < Near0_Min & value != -6999){
    RadFlag = 'T'
  } 
  if(RadFCorr > Max) {
    RadFlag = 'R'
  } 
  if(RadFCorr >= 0 & RadFCorr <= Max){
    RadFlag = ''
  } 
  if (value == -6999) {
    RadFlag = 'B'
  } 
  
  return(RadFlag)
}

# '********************************************************
# '  SwRadOut:Flag Max Values, plus flag when change small
# '           negative values to zero and when below Near0
# '           minimal. Also flag when SwRadOut is > than
# '           specified percentage of SwRadIn
# '********************************************************
# '                   Thomas Nylen
# '                   Portland State University
# '                   September 6, 1999

SwRadOutFlag <- function(OutRaw_Value, Out_Factor, InRaw_Value, In_Factor, Max) {

    OutRadCorr = OutRaw_Value * Out_Factor
    InRadCorr = InRaw_Value * In_Factor
    
    if(InRadCorr >= 0){ 
        PerCorr = InRadCorr * 0.95
    } else {
        PerCorr = 0
    }
    
    if(InRaw_Value == -6999 & OutRadCorr >= 0 & OutRadCorr <= Max) {  
      SwRadOutFlag = ''
    }
    if(OutRadCorr >= 0 & OutRadCorr <= Max) {  
      SwRadOutFlag = ''
    }
    if(OutRaw_Value >= -5 & OutRaw_Value < 0) {  
      SwRadOutFlag = "Z"
    }
    if(OutRaw_Value < -5 & OutRaw_Value != -6999) {  
      SwRadOutFlag = "T"
    }
    if(OutRadCorr > Max) {  
      SwRadOutFlag = "R"
    }
    if(OutRadCorr > PerCorr & OutRadCorr > 0 & InRaw_Value != -6999 & OutRadCorr <= Max) {  
      SwRadOutFlag = "S"
    }
    if(OutRadCorr > PerCorr & OutRadCorr > 0 & InRaw_Value != -6999 & OutRadCorr > Max) {  
      SwRadOutFlag = "S,R"
    }
    if(OutRaw_Value == -6999) {  
      SwRadOutFlag = "B"
    }
    
    return(SwRadOutFlag)
}


# '********************************************************
# '  WDir: zero out columns which are irrelevant when a
# '       reference Column = 0
# '********************************************************/

# WDir ####
WDir <- function(WDirection, WSpeed) {
  if (WSpeed < 0.05) {
    WDir = NA
  } else {
    WDir = WDirection
  }
  return(WDir)
}

# '/********************************************************
# '  WDirFlag: zero out columns which are irrelevent when a
# '       reference Column = 0
# '********************************************************/

# wdirFlag ####
wdirFlag <- function(WDirection, WSpeed, Max, Min) {
  if (WDirection > Max | Wirection < Min) {
    wdirFlag = 'R'
  } else {
    wdirFlag = ''
  }
  if (WSpeed < 0.05 & WSpeed != -6999) {
    wdirFlag = 'N'
  }
  return(wdirFlag)
}


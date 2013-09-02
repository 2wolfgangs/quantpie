#*****************************************************************
# MOMENTUM FUNCTIONS
# ==================
#*****************************************************************
# For these functions to run effectively, the environment must 
# contain: 
#  - data   (an SIT backtest environment)
#  - prices (xts - daily prices)
# 
# Uses code from https://github.com/systematicinvestor/SIT
#*****************************************************************

require(SIT)
require(TTR)

#*****************************************************************
# Total return momentum 
#*****************************************************************
trMomentum <- function(n.mom) {
  return((prices / mlag(prices, n.mom)) - 1)
}

#*****************************************************************
# Total return less most recent month
#*****************************************************************
trx1Momentum <- function(n.mom) {
  return((mlag(prices, 22) / mlag(prices, n.mom + 22)) - 1)
}  

#*****************************************************************
# SMA differential
#*****************************************************************
SMADifferential <- function(n.mom) {
  smafast <- bt.apply(data, function(x) { SMA(Cl(x), n.mom / slowfastratio) } )
  smaslow <- bt.apply(data, function(x) { SMA(Cl(x), n.mom) } ) 
  return((smafast / smaslow) - 1)
}

#*****************************************************************
# Price to SMA differential
#*****************************************************************
priceToSMADifferential <- function(n.mom) {
  smaslow <- bt.apply(data, function(x) { SMA(Cl(x), n.mom) } ) 
  return((prices / smaslow) - 1)
}

#*****************************************************************
# Instantaneous Slope
# Daily rate of change of SMA 
#*****************************************************************
instantaneousSlope <- function(n.mom) {
  smaslow <- bt.apply(data, function(x) { SMA(Cl(x), n.mom) } )
  return((smaslow / mlag(smaslow, 1) - 1))
}

#*****************************************************************
# Percentile Rank
# See: http://en.wikipedia.org/wiki/Percentile_rank
#*****************************************************************
percentileRank <- function(n.mom) {
  return(bt.apply(data, function(x) { runPercentRank(Cl(x), n.mom) } ))
}

#*****************************************************************
# Z score
# The magnitude that the current price deviates
# from the average price over the period
#*****************************************************************
zScore <- function(n.mom) {
  zmean <- bt.apply(data, function(x) { runMean(Cl(x), n.mom) } )
  zsd <- bt.apply(data, function(x) { runSD(Cl(x), n.mom) } )
  return((prices - zmean) / zsd)
}

#*****************************************************************
# Z distribution 
# A transformation of the z score to a percentile value on the 
# cumulative normal distribution
#*****************************************************************
zDistribution <- function(n.mom) {
  zmean <- bt.apply(data, function(x) { runMean(Cl(x), n.mom) } )
  zsd <- bt.apply(data, function(x) { runSD(Cl(x), n.mom) } )
  return(pnorm((prices - zmean) / zsd))
}


#******************************************************************
# UTILITY FUNCTIONS
# =================
#******************************************************************

#******************************************************************
# Create standardised momentum measure over a range of lookback periods
#******************************************************************
StandardisedMomentum <- function(momentumfunc) {
  stdmom <- 0 
  for (look in momlookback[1:length(momlookback)]) {
    moms <- momentumfunc(look)
    # Standardised momentum
    # abs(asset momentum) / abs(sum of all asset momentums) * sign of asset momentum
    stmomentum <- (abs(moms) / abs(rowSums(moms, na.rm = TRUE))) * sign(moms)
    stdmom <- stdmom + stmomentum
  }  
  # Return standardised momentum amount, normalised to 1 or -1
  return(stdmom / length(momlookback))
}

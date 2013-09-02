require(SIT)
require(TTR)

#*****************************************************************
# MOMENTUM FUNCTIONS
# ==================
#*****************************************************************

#*****************************************************************
# Total return momentum 
#*****************************************************************
trMomentum <- function(n.mom) {
  (prices / mlag(prices, n.mom)) - 1
}

#*****************************************************************
# Total return less most recent month
#*****************************************************************
trx1Momentum <- function(n.mom) {
  (mlag(prices, 22) / mlag(prices, n.mom + 22)) - 1
}  

#*****************************************************************
# SMA differential
#*****************************************************************
SMADifferential <- function(n.mom) {
  smafast <- bt.apply(data, function(x) { SMA(Cl(x), n.mom / slowfastratio) } )
  smaslow <- bt.apply(data, function(x) { SMA(Cl(x), n.mom) } ) 
  (smafast / smaslow) - 1
}

#*****************************************************************
# Price to SMA differential
#*****************************************************************
priceToSMADifferential <- function(n.mom) {
  smaslow <- bt.apply(data, function(x) { SMA(Cl(x), n.mom) } ) 
  (prices / smaslow) - 1
}

#*****************************************************************
# Instantaneous Slope
# Daily rate of change of SMA 
#*****************************************************************
instantaneousSlope <- function(n.mom) {
  smaslow <- bt.apply(data, function(x) { SMA(Cl(x), n.mom) } )
  (smaslow / mlag(smaslow, 1) - 1)
}

#*****************************************************************
# Percentile Rank
# See: http://en.wikipedia.org/wiki/Percentile_rank
#*****************************************************************
percentileRank <- function(n.mom) {
  bt.apply(data, function(x) { runPercentRank(Cl(x), n.mom) } )
}

#*****************************************************************
# Z score
# The magnitude that the current price deviates
# from the average price over the period
#*****************************************************************
zScore <- function(n.mom) {
  zmean <- bt.apply(data, function(x) { runMean(Cl(x), n.mom) } )
  zsd <- bt.apply(data, function(x) { runSD(Cl(x), n.mom) } )
  (prices - zmean) / zsd
}

#*****************************************************************
# Z distribution 
# A transformation of the z score to a percentile value on the 
# cumulative normal distribution
#*****************************************************************
zDistribution <- function(n.mom) {
  zmean <- bt.apply(data, function(x) { runMean(Cl(x), n.mom) } )
  zsd <- bt.apply(data, function(x) { runSD(Cl(x), n.mom) } )
  pnorm((prices - zmean) / zsd)
}


#******************************************************************
# UTILITY FUNCTIONS
# =================
#******************************************************************

#******************************************************************
# Create standardised momentum measure over a range of lookback periods
#******************************************************************
standardisedMomentum <- function(momentumfunc) {
  stdmom <- 0
  for (n.mom in momlookback[1:length(momlookback)]) {
    moms <- momentumfunc(n.mom)
    # Standardised momentum
    # abs(asset momentum) / abs(sum of all asset momentums) * sign of asset momentum
    stmomentum <- (abs(moms) / abs(rowSums(moms, na.rm = TRUE))) * sign(moms)
    stdmom <- stdmom + stmomentum
  }  
  # Return standardised momentum amount, normalised to 1 or -1
  stdmom / length(momlookback)
}


#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')

# Need to get some better data - but for now, let's use this and expand the rest.
tickers = c('SPY','EFA','EWJ','EEM','IYR','RWX','IEF','TLT','DBC','GLD')


data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)                            
bt.prep(data, align='keep.all', dates='2004:12::')

#*****************************************************************
# Code Strategies
#******************************************************************
prices <- data$prices  
n <- ncol(prices)

models <- list()

# find period ends
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]

# Momentum parameters
n.top <- 5       # no of momentum positions to hold     
# Momentum lookback (1,3,6,9,12 months)
momlookback <- c(1*22,3*22,6*22,9*22,12*22) 
# momlookback <- 6 * 22
# n.mom <- 6 * 22
slowfastratio <- 10

#*****************************************************************
# Total return momentum 
#*****************************************************************

totalreturn <- standardisedMomentum(function(x) trMomentum(x))

data$weight[] <- NA
data$weight[period.ends,] <- ntop(totalreturn[period.ends,], n.top)   
models$totalreturn <- bt.run.share(data, clean.signal=F)

#*****************************************************************
# Total return less most recent month
#*****************t************************************************
trx1 <- standardisedMomentum(function(x) trx1Momentum(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(trx1[period.ends,], n.top)   
models$trx1 <- bt.run.share(data, clean.signal=F)

#*****************************************************************
# SMA differential
#*****************************************************************
smadiff <- standardisedMomentum(function(x) SMADifferential(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(smadiff[period.ends,], n.top)  
models$smadiff <- bt.run.share(data, clean.signal=F)

#*****************************************************************
# Price to SMA differential
#*****************************************************************
pricesmadiff <- standardisedMomentum(function(x) priceToSMADifferential(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(pricesmadiff[period.ends,], n.top)  
models$pricesmadiff <- bt.run.share(data, clean.signal=F)

#*****************************************************************
# Instantaneous Slope
# Daily rate of change of SMA 
#*****************************************************************
instslope <- standardisedMomentum(function(x) instantaneousSlope(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(instslope[period.ends,], n.top)  
models$instslope <- bt.run.share(data, clean.signal=F)

#*****************************************************************
# Percentile Rank
# See: http://en.wikipedia.org/wiki/Percentile_rank
#*****************************************************************
percentrank <- standardisedMomentum(function(x) percentileRank(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(percentrank[period.ends,], n.top)  
models$percentrank <- bt.run.share(data, clean.signal=F)

#*****************************************************************
# Z score
# The magnitude that the current price deviates
# from the average price over the period
#*****************************************************************
zscore <- standardisedMomentum(function(x) zScore(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(zscore[period.ends,], n.top)  
models$zscore <- bt.run.share(data, clean.signal=F)

#*****************************************************************
# Z distribution 
# A transformation of the z score to a percentile value on the 
# cumulative normal distribution
#*****************************************************************
zdist <- standardisedMomentum(function(x) zDistribution(x)) 

data$weight[] <- NA
data$weight[period.ends,] <- ntop(zdist[period.ends,], n.top)  
models$zdist <- bt.run.share(data, clean.signal=F)


#*****************************************************************
# Create Report
#******************************************************************    
models = rev(models)

strategy.performance.snapshoot(models, T)



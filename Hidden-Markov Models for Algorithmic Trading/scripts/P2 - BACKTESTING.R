
###########################################################################
# LIBS --------------------------------------------------------------------
###########################################################################

LoadLibraries <- function() {
  require(tidyverse) # general
  require(lubridate) # periods, date, etc...
  require(TTR) # Technical indicators such as ATR, EMA, ADX, VHF, DonchianChannel
  require(mhsmm) # Hidden Markov Models, Semi-HMM
  require(mstudentd) # dmtd() used in mtdist.hsmm()
  require(gamlss) # fitdist()
  require(zoo) # rollapply()
  require(tseries) # rolling mean
  require(PerformanceAnalytics) # indicators 
  require(patchwork) # plots
}

LoadLibraries()

###########################################################################
# DATA --------------------------------------------------------------------
###########################################################################

bt.set.1 <- tail(df.opt.5min,6500) # ~22 trading days 

attr(bt.set.1,"period") <- list(
  from = first(bt.set.1$DateTime),
  to = last(bt.set.1$DateTime),
  bars = nrow(bt.set.1)
)

attr(bt.set.1,"period")

bt.set.1$DateTime <- as.POSIXct(
  paste(bt.set.1$Date, bt.set.1$Time),
  format = "%Y.%m.%d %H:%M"
)

###########################################################################
# INDICATORS --------------------------------------------------------------
###########################################################################

bb <- BBands(HLC = bt.set.1[, c("High","Low","Close")], n = 20, sd = 2)
bt.set.1$BBup <- bb[, "up"]
bt.set.1$BBlow <- bb[, "dn"]
bt.set.1$BBpct <- bb[, "pctB"] # %B helper, very useful since its a continuous metric
bt.set.1$BBmavg <- bb[, "mavg"]

bt.set.1$HMM <- tail(geometric.mdl$K3$yhat, 6500)

bt.set.1$SMA200 <- SMA(bt.set.1$Typical_Price, n = 200)

# In here we compute the zscore of rolling mean of Vol. Tick for the past 14 days. This will be the indicator we use to establish dynamic tp range

rollmean <- rollapply(bt.set.1$Tick.Vol, width = 14, FUN = mean, align = "right", fill = NA)
rollstd <- rollapply(bt.set.1$Tick.Vol, width = 14, FUN = sd, align = "right", fill = NA)
bt.set.1$zTick.Vol <- (bt.set.1$Tick.Vol - rollmean) / rollstd

# Remove NA's
bt.set.1 <- na.omit(bt.set.1)

###########################################################################
# ENTRY & EXIT LOGIC ------------------------------------------------------
###########################################################################

# sl.buffer in pips, if no Stop-Loss is needed just type ex. "99999999999999999"
# comission in USD
# slippage in pips
# pip value in USD

trade.fibo <- function(data, fib.level = 0.618, sl.buffer = 0.1, slippage = 0.1, 
                       comission = 3, lot.size = 1e5, pip.value = 10) {
  pip <- 1e-4 # 0.0001
  sl.buf <- sl.buffer * pip
  slip.val <- slippage * pip
  
  n <- nrow(data)
  in.pos <- F
  side <- NA_character_
  entry.px <- exit.px <- sl.px <- NA_real_
  entry.ix <- NA_integer_
  
  trades <- list()
  
  for (i in seq_len(n)) {
    row <- data[i, ] 
    # Manage open trades
    if (in.pos) {
      if ((side == "long"  && as.numeric(row$Low)  <= sl.px) ||
          (side == "short" && as.numeric(row$High) >= sl.px)) {
        exit.px <- sl.px
        reason  <- "SL"
        
      } else if ((side == "long"  && as.numeric(row$High) >= row$BBmavg) ||
                 (side == "short" && as.numeric(row$Low)  <= row$BBmavg)) {
        exit.px <- row$BBmavg
        reason  <- "TP"
        
      } else next
      
      gross.pips <- if (side == "long") (exit.px - entry.px)/pip
      else                (entry.px - exit.px)/pip
      
      net.pips <- gross.pips - 2 * slippage-comission/pip.value
      net.usd <- net.pips * pip.value
      
      trades[[length(trades) + 1]] <- data.frame(
        entry = entry.px,
        exit = exit.px,
        side = side,
        bars = i - entry.ix,
        reason = reason,
        gross.pips = gross.pips,
        net.pips = net.pips,
        net.usd = net.usd,
        row.names = index(row)
      )
      
      in.pos <- FALSE
    }
    if (!in.pos && !is.na(row$SMA200)) {
      # LONG
      if (as.numeric(row$Close) > row$SMA200 &&
          as.numeric(row$Close) < row$BBlow) {
        crng <- as.numeric(row$High) - as.numeric(row$Low)
        entry.px <- as.numeric(row$High) - fib.level * crng
        sl.px <- as.numeric(row$Low) - sl.buf
        side <- "long"
        in.pos <- T
        entry.ix <- i
        next
      }
      # SHORT
      if (as.numeric(row$Close) < row$SMA200 &&  # requirement - We do not trade if the price closed below the 
          as.numeric(row$Close) > row$BBup) {    # SMA200 because then we are applying mean-rev against the trend so we are going to lose money
        crng <- as.numeric(row$High) - as.numeric(row$Low)
        entry.px <- as.numeric(row$Low) + fib.level * crng
        sl.px <- as.numeric(row$High) + sl.buf
        side <- "short"
        in.pos <- T
        entry.ix <- i
        next 
      }
    }
  }
  if (length(trades)) do.call(rbind, trades) else data.frame()
}

# Trade execution

trades <- trade.fibo(
  data = bt.set.1, fib.level = 0.618, sl.buffer = 100, slippage = 0, comission = 3, lot.size = 1e5, pip.value = 10 
)
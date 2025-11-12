# LATEST FUNCTION VERSION - INCLUDES DYNAMIC RANGE SETUP, HMM, COSTS, MOMENTUM BASED STRATEGY WITH PERFORMANCE METRICS

trade.fibo.dynamic <- function(data, fib.level = 0.618, sl.buffer = 0.1, slippage = 0.1, 
                       comission = 3, lot.size = 1e5, pip.value = 10, state = NULL,
                       tp.bracket = "dynamic") {
  pip <- 1e-4 # 0.0001
  sl.buf <- sl.buffer * pip
  slip.val <- slippage * pip
  
  n <- nrow(data)
  in.pos <- F
  side <- NA_character_
  entry.px <- exit.px <- sl.px <-  NA_real_
  entry.time <- NA
  entry.ix <- NA_integer_
  
  trades <- list()
  
  for (i in seq_len(n)) {
    row <- data[i, ]
    # Skip if not in desired HMM state
    if (!is.null(state) && !(row$HMM %in% state)) next
    
    # Manage open trades
    if (in.pos) {
      if ((side == "long"  && as.numeric(row$Low)  <= sl.px) ||
          (side == "short" && as.numeric(row$High) >= sl.px)) {
        exit.px <- sl.px
        reason  <- "SL"
      } else {
        # Dynamic TP logic
        tp.multiplier <- if(tp.bracket == "dynamic") {
          vol.score <- row$zATR14 + row$zTick.Vol
          if (vol.score < -0.5) 1.0 else if (vol.score > 0.5) 2.0 else 1.5
        } else if (is.numeric(tp.bracket)) {
          tp.bracket
        } else 1.0
        
        # Apply TP level
        if (side == "long" && as.numeric(row$High) >= as.numeric(row$BBmavg) * tp.multiplier) {
          exit.px <- as.numeric(row$BBmavg) * tp.multiplier
          reason <- "TP"
        } else if (side == "short" && as.numeric(row$Low) <= as.numeric(row$BBmavg) * (2 - tp.multiplier)) {
          exit.px <- as.numeric(row$BBmavg) * (2 - tp.multiplier)
          reason <- "TP"
        } else {
          next
        }
      }
      
      gross.pips <- if (side == "long") (exit.px - entry.px)/pip else (entry.px - exit.px)/pip
      net.pips <- gross.pips - 2 * slippage - comission/pip.value
      net.usd <- net.pips * pip.value
      
      trades[[length(trades) + 1]] <- data.frame(
        entry.time = entry.time,
        exit.time = row$DateTime,
        entry = entry.px,
        exit = exit.px,
        side = side,
        bars = i - entry.ix,
        holding.time.min = (i - entry.ix) * 5,
        reason = reason,
        gross.pips = gross.pips,
        net.pips = net.pips,
        net.usd = net.usd,
        win = as.integer(net.usd > 0)
      )
      
      in.pos <- FALSE
    }
    
    if (!in.pos && !is.na(row$SMA200)) {
      crng <- as.numeric(row$High) - as.numeric(row$Low)
      # LONG
      if (as.numeric(row$Close) > row$SMA200 && as.numeric(row$Close) < row$BBlow) {
        entry.px <- as.numeric(row$High) - fib.level * crng
        sl.px <- as.numeric(row$Low) - sl.buf
        side <- "long"
        in.pos <- T
        entry.ix <- i
        entry.time <- row$DateTime
        next
      }
      # SHORT
      if (as.numeric(row$Close) < row$SMA200 && as.numeric(row$Close) > row$BBup) {
        entry.px <- as.numeric(row$Low) + fib.level * crng
        sl.px <- as.numeric(row$High) + sl.buf
        side <- "short"
        in.pos <- T
        entry.ix <- i
        entry.time <- row$DateTime
        next 
      }
    }
  }
  
  if (length(trades)) {
    df <- do.call(rbind, trades)
    df$cumpnl <- cumsum(df$net.usd)
    drawdown <- df$cumpnl - cummax(df$cumpnl)
    mdd <- min(drawdown)
    
    total.trades <- nrow(df)
    wins <- sum(df$win)
    win.rate <- wins / total.trades
    sharpe <- mean(df$net.usd) / sd(df$net.usd)
    
    perf <- list(
      total.trades = total.trades,
      win.rate = round(win.rate, 4),
      avg.pnl = round(mean(df$net.usd), 2),
      sharpe = round(sharpe, 3),
      mdd = round(mdd, 2),
      net.pips = round(sum(df$net.pips), 2),
      net.usd = round(sum(df$net.usd), 2),
      avg.holding.time.min = round(mean(df$holding.time.min), 1)
    )
    
    attr(df, "performance") <- perf
    return(df)
  } else {
    return(data.frame())
  }
}

# TRADE SUMMARY

# NO HMM

trades.mmt <- trade.fibo.dynamic(data = bt.set.1, fib.level = 0.618, sl.buffer = 100, slippage = 0, 
                                 comission = 3, lot.size = 1e5, pip.value = 10, state = NULL,
                                 tp.bracket = "dynamic")

sum(trades.mmt$net.usd)
attr(trades.mmt, "performance")

# OPTIMAL HMM

trades.mmt.hmm <- trade.fibo.dynamic(data = bt.set.1, fib.level = 0.618, sl.buffer = 100, slippage = 0, 
                                    comission = 3, lot.size = 1e5, pip.value = 10, state = 1,
                                    tp.bracket = "dynamic")

sum(trades.mmt.hmm$net.usd)
attr(trades.mmt.hmm, "performance")

# FUNCTION VERSION THAT INCLUDES HMM, STRICT TP BRACKETS, BETTER PERFORMANCE METRICS

trade.fibo <- function(data, fib.level = 0.618, sl.buffer = 0.1, slippage = 0.1, 
                       comission = 3, lot.size = 1e5, pip.value = 10, state = NULL,
                       tp.bracket = 1.0) {
  pip <- 1e-4 # 0.0001
  sl.buf <- sl.buffer * pip
  slip.val <- slippage * pip
  
  n <- nrow(data)
  in.pos <- F
  side <- NA_character_
  entry.px <- exit.px <- sl.px <- tp.px <- NA_real_
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
        
      } else if ((side == "long"  && as.numeric(row$High) >= row$BBmavg) ||
                 (side == "short" && as.numeric(row$Low)  <= row$BBmavg)) {
        exit.px <- tp.px
        reason  <- paste0("TP", tp.bracket)
        
      } else next
      
      gross.pips <- if (side == "long") (exit.px - entry.px)/pip
                    else                (entry.px - exit.px)/pip
      
      net.pips <- gross.pips - 2 * slippage-comission/pip.value
      net.usd <- net.pips * pip.value
      
      trades[[length(trades) + 1]] <- data.frame(
        entry.time = entry.time,
        exit.time = row$DateTime,
        entry = entry.px,
        exit = exit.px,
        side = side,
        bars = i - entry.ix,
        holding.time.min = (i - entry.ix) * 5, # replace int with barsize to standardize
        reason = reason,
        gross.pips = gross.pips,
        net.pips = net.pips,
        net.usd = net.usd,
        win = as.integer(net.usd > 0)
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
        tp.px <- entry.px + tp.bracket * (as.numeric(row$BBmavg) - entry.px)
        side <- "long"
        in.pos <- T
        entry.ix <- i
        entry.time <- row$DateTime
        next
      }
      # SHORT
      if (as.numeric(row$Close) < row$SMA200 &&  # requirement
          as.numeric(row$Close) > row$BBup) {
        crng <- as.numeric(row$High) - as.numeric(row$Low)
        entry.px <- as.numeric(row$Low) + fib.level * crng
        sl.px <- as.numeric(row$High) + sl.buf
        tp.px <- entry.px - tp.bracket * (as.numeric(row$BBmavg) - entry.px)
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

rm(trade.fibo)

# TRADE SUMMARY

# NO HMM
trades.mr <- trade.fibo(
  data = bt.set.1, fib.level = 0.618, sl.buffer = 100, slippage = 0, comission = 3, lot.size = 1e5,
  pip.value = 10, state = NULL, tp.bracket = 2.0
)

sum(trades.mr$net.usd)
attr(trades.mr, "performance")

# OPTIMAL HMM

trades.mr.hmm12 <- trade.fibo(
  data = bt.set.1, fib.level = 0.618, sl.buffer = 100, slippage = 0, comission = 3, lot.size = 1e5,
  pip.value = 10, state = c(1, 2), tp.bracket = 2.0
)

sum(trades.mr.hmm12$net.usd)
attr(trades.mr.hmm12, "performance")


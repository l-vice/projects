# LATEST FUNCTION VERSION - INCLUDES DYNAMIC RANGE SETUP, HMM, COSTS 

trade.fibo <- function(data, fib.level = 0.618, sl.buffer = 0.1, slippage = 0.1, 
                       comission = 3, lot.size = 1e5, pip.value = 10, state = NULL,
                       tp.bracket = "dynamic") {
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
      crng <- as.numeric(row$High) - as.numeric(row$Low)
      # LONG
      if (as.numeric(row$Close) > row$SMA200 &&
          as.numeric(row$Close) < row$BBlow) {
        entry.px <- as.numeric(row$High) - fib.level * crng
        sl.px <- as.numeric(row$Low) - sl.buf
        side <- "long"
        in.pos <- T
        entry.ix <- i
        next
      }
      # SHORT
      if (as.numeric(row$Close) < row$SMA200 &&  # requirement
          as.numeric(row$Close) > row$BBup) {
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


tradesdynamicHMM123 <- trade.fibo(data = bt.set.1, fib.level = 0.618, sl.buffer = 100, slippage = 0, 
                       comission = 3, lot.size = 1e5, pip.value = 10, state = NULL,
                       tp.bracket = "dynamic")

tradesdynamicHMM12 <- trade.fibo(data = bt.set.1, fib.level = 0.618, sl.buffer = 100, slippage = 0, 
                                 comission = 3, lot.size = 1e5, pip.value = 10, state = c(1,2),
                                 tp.bracket = "dynamic")

sum(tradesdynamicHMM12$net.usd)

tradesdynamicHMM2 <- trade.fibo(data = bt.set.1, fib.level = 0.618, sl.buffer = 100, slippage = 0, 
                                comission = 3, lot.size = 1e5, pip.value = 10, state = 2,
                                tp.bracket = "dynamic")

tradesdynamicHMM1 <- trade.fibo(data = bt.set.1, fib.level = 0.618, sl.buffer = 100, slippage = 0, 
                                comission = 3, lot.size = 1e5, pip.value = 10, state = 1,
                                tp.bracket = "dynamic")

sum(tradesdynamicHMM1$net.usd)

sum(tradesdynamicHMM2$net.usd)

sum(tradesfixedbracketHMM$net.usd)

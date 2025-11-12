# FUNCTION VERSION THAT INCLUDES HMM, STRICT TP BRACKETS

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
        tp.px <- entry.px + tp.bracket * (as.numeric(row$BBmavg) - entry.px)
        side <- "long"
        in.pos <- T
        entry.ix <- i
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
        next 
      }
    }
  }
  if (length(trades)) do.call(rbind, trades) else data.frame()
}


tradesfixedbracket <- trade.fibo(
  data = bt.set.1, fib.level = 0.618, sl.buffer = 100, slippage = 0, comission = 3, lot.size = 1e5, 
  pip.value = 10, state = NULL, tp.bracket = 1.5
  )

sum(tradesfixedbracket$net.pips)
sum(tradesfixedbracket$net.usd)


tradesfixedbracketHMM <- trade.fibo(
  data = bt.set.1, fib.level = 0.618, sl.buffer = 100, slippage = 0, comission = 3, lot.size = 1e5, 
  pip.value = 10, state = c(1, 2), tp.bracket = 1.5
)

rm(tradesfixedbracketHMM)
rm(tradesfixedbracketHMM)
sum(tradesfixedbracket$net.usd)
sum(tradesfixedbracketHMM$net.usd) # 3012.754
sum(tradesfixedbracketHMM$net.pips) # 301.2754

tradesfixedbracketHMM23 <- trade.fibo(
  data = bt.set.1, fib.level = 0.618, sl.buffer = 100, slippage = 0, comission = 3, lot.size = 1e5, 
  pip.value = 10, state = c(2, 3), tp.bracket = 1.5
)

sum(tradesfixedbracketHMM23$net.usd)

tradesfixedbracket2HMM12 <- trade.fibo(
  data = bt.set.1, fib.level = 0.618, sl.buffer = 200, slippage = 0, comission = 3, lot.size = 1e5, 
  pip.value = 10, state = c(1, 2), tp.bracket = 2.0
  )


sum(tradesfixedbracket2HMM12$net.usd) # 4105.005 or 410.5 pips


tradesfixedbracket2HMM12.take2 <- trade.fibo(
  data = bt.set.1, fib.level = 0.618, sl.buffer = 200, slippage = 0, comission = 3, lot.size = 1e5, 
  pip.value = 10, state = c(1, 2), tp.bracket = 2.0
)

sum(tradesfixedbracket2HMM12.take2$net.usd)

View(tradesfixedbracket2HMM12)

tradesfixedbracket2HMM12[1,]



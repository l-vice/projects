
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
}

LoadLibraries()

###########################################################################
# DATA --------------------------------------------------------------------
###########################################################################

# 5 minute dataframe
# The data contains around 98k observations it contains 5min time frames from the past ~2 months. Obtained from MetaTrader5.

df.opt.5min <- read.csv(file = "EURUSD_M5_202402161725_202506132355.csv")

N <- nrow(df.opt.5min);p <- ncol(df.opt.5min);print(N);print(p);colnames(df.opt.5min) 
# [1] 98560
# [1] 9
# [1] "Date"     "Time"     "Open"     "High"     "Low"      "Close"    "Tick.Vol" "Volume"   "Spread" 

df.opt.5min$DateTime <- as.POSIXct(
  paste(df.opt.5min$Date, df.opt.5min$Time),
  format = "%Y.%m.%d %H:%M"
)

# Typical Price
# We used this variable instead of the actual price, the purpose is to smooth out rapid changes in the price action.
# Especially since we are dealing with 5min intra-day data. So essentailly a smoothed version of actual price.

# Typical Price = (HIGH + LOW + CLOSE / 3)
df.opt.5min$Typical_Price <- (df.opt.5min$High+df.opt.5min$Low+df.opt.5min$Close)/3 

# Returns
# We take log returns to add some remedy to heavy tails on the right side.
df.opt.5min$Returns <- c(NA,diff(log(df.opt.5min$Close)))

df.opt.5min$zReturns <- scale(df.opt.5min$Returns)

# The purpose of adding the following variables in our models is to basically feed relevant data to our HMM models
# so it can use that information to establish the separations of latent regimes. Every single indicator is used to signal
# trend strenght and direction. 

# ATR
# Average True Range = ATR
ATR <- ATR(df.opt.5min[c("High","Low","Close")], n=14)

df.opt.5min$ATR <- ATR[,"atr"]
df.opt.5min$zATR <- scale(df.opt.5min$ATR)

# EMA 20
# 20-period (1 Candle = 1 Period) Exponential Moving Average
df.opt.5min$EMA20 <- EMA(df.opt.5min$Typical_Price,n = 20)

df.opt.5min$EMA20.1bar <- c(NA,diff(df.opt.5min$EMA20))
df.opt.5min$zEMA20.1bar <- scale(df.opt.5min$EMA20.1bar)

# ADX
# The average Directional Movement Index, the trend strength indicator.

ADX <- ADX(df.opt.5min[,c("High", "Low", "Close")], n = 14)

df.opt.5min$ADX14 <- ADX[,"ADX"]

df.opt.5min$zADX14 <- scale(df.opt.5min$ADX14)

df.opt.5min$DIp <- ADX[,"DIp"]
df.opt.5min$DIn <- ADX[,"DIn"]

# Aroon Difference
# The Aroon Difference indicator also known as the Aroon Oscillator measures the difference between the
# Aroon Up and Aroon Down indicators to identify the strenght and direction of the trend....
# Anyway this indicator is used by the traders to predict and establish the strenght of a trend.
ar <- aroon(df.opt.5min[,c("High","Low")],n=25)
df.opt.5min$Aroon <- ar[,"oscillator"]
df.opt.5min$zAroon <- scale(df.opt.5min$Aroon)

# VHF(28)
# Vertical Horizontal Filter - this indicator does not indicate the direction of the trend but rather the extent to which
# the asset is trending.
df.opt.5min$VHF28 <- VHF(df.opt.5min$Close,n=28)
df.opt.5min$zVHF28 <- scale(df.opt.5min$VHF28)

# Donchian Width(20)
# This indicator is used to identify potential breakout points, confirm trends or periods of consolidation.
# Again it's an indicator used to iden
dc <- DonchianChannel(df.opt.5min[,c("High","Low")],n=20)

df.opt.5min$DonchianWidth20 <- dc[,"high"]-dc[,"low"]

df.opt.5min$zDonchianWidth20 <- scale(df.opt.5min$DonchianWidth20)

# Rem NA's + Cosmetics

df.opt.5min <- na.omit(df.opt.5min)
colnames(df.opt.5min)[colnames(df.opt.5min)=="ATR"] <- "ATR14"
colnames(df.opt.5min)[colnames(df.opt.5min)=="zATR"] <- "zATR14"

# Features

features <- df.opt.5min[, c("zReturns","zATR14","zEMA20.1bar","zADX14",
                               "zAroon","zVHF28","zDonchianWidth20")]

# write.csv(x = features, file = "features.HMM.csv", row.names = F)

###########################################################################
# DISTRIBUTION ------------------------------------------------------------
###########################################################################

x <- as.matrix(features)

# Quick sanity check
par(mfrow = c(2, 4))

for (col in c("zReturns", "zATR14", "zEMA20.1bar", "zADX14", "zAroon", "zVHF28", "zDonchianWidth20")) {
  x <- df.opt.5min[[col]]
  qqnorm(x, main = paste("QQ Plot:", col), col = "blue")
  qqline(x, col = "red", lwd = 2)
}

par(mfrow=c(1,1))

# optim <- fitDist(x,k=2,type="realAll")
# It's not necessary to run this since we won't fit the recommended distribution
# We will go for t-dist since the whole process is straightforward. 
# The real reason i did not go for Box-Cox is time. I wasted a lot of time initially trying to
# "overfit" or find a "perfect" distribution (applying a 4 parameter Generalized T and waiting all night for it to run through
# K = 1,....., 6. models. I almost went for a 5 parameter Gen-T which has a parameter for each tail.... 
# I should have recognized that to remedy this kind of skew i should have combined variable transformations and then
# found a distribution that would be the most appropriate fit. However, you live and you learn.
# At least, I learned my lesson the hard way.

###########################################################################
# PDF ---------------------------------------------------------------------
###########################################################################

mtdist.hsmm <- function(x, j, model) {
  mu    <- model$parms.emission$mu[[j]]
  Sigma <- model$parms.emission$sigma[[j]]
  nu    <- model$parms.emission$nu[[j]]
  p     <- length(mu)
  
  if (is.vector(x)) x <- matrix(x, ncol = p)
  x <- t(x)
  
  if (!isSymmetric(Sigma)) 
    stop("Sigma must be symmetric")
  eig <- eigen(Sigma, symmetric = TRUE)
  lambda <- eig$values
  if (any(lambda < 1e-6 * max(abs(lambda)))) 
    stop("Sigma not positive definite")
  
  invSigma <- solve(Sigma)
  xc <- x-mu
  exponent <- colSums((invSigma %*% xc) * xc)
  
  num <- gamma((nu + p) / 2)
  denom <- gamma(nu / 2) * (nu * pi)^(p / 2) * sqrt(det(Sigma))
  density <- num / denom * (1 + exponent / nu)^(-(nu + p) / 2)
  return(as.numeric(density))
}

attr(mtdist.hsmm, "src") <- "https://rdrr.io/cran/mstudentd/src/R/dmtd.R"

# mstep

mtdist.mstep <- function(x, wt) {
  n <- nrow(x)
  p <- ncol(x)
  J <- ncol(wt)
  
  mu.list <- vector("list", J)
  Sigma.list <- vector("list", J)
  nu.vector <- numeric(J)
  
  for (j in 1:J) {
    wj <- wt[, j]
    wj <- wj / sum(wj)
    mu.j <- colSums(wj * x)
    xc <- sweep(x, 2, mu.j)
    sigma.j <- crossprod(sqrt(wj) * xc)
    nu.j <- 5
    
    mu.list[[j]] <- mu.j
    Sigma.list[[j]] <- sigma.j
    nu.vector[j] <- nu.j
  }
  list(mu = mu.list, sigma = Sigma.list, nu = nu.vector)
}

###########################################################################
# WRAPPER -----------------------------------------------------------------
###########################################################################
 

fit.mvt.markov <- function(obs, K = 3, type = c("poisson", "gamma", "geometric"), shift.val = 5) {
  
  type <- match.arg(type)
  
  # Data
  x  <- as.matrix(obs)
  
  # ISS
  km <- kmeans(x,centers = K, nstart = 10, algorithm = "Lloyd",iter.max = 500)
  s <- km$cluster
  
  # TP
  A <- matrix(0.01, K, K)
  for (t in 2:length(s))
    A[s[t-1], s[t]] <- A[s[t-1], s[t]] + 1
  
  if (type %in% c("poisson", "gamma")) {
    diag(A) <- 0 # Impose 0 on diagonals only if type=c("poisson","gamma") 
  }

  A <- A / rowSums(A) # normalization
  
  # IP
  init <- rep(1/K, K)
  
  ## SD
  rl     <- rle(s)
  shift  <- rep(shift.val, K)
  
  if (type == "poisson") {
    lambda   <- pmax(tapply(rl$lengths, rl$values, mean) - shift[1], 0.1)
    sojourn  <- list(type = "poisson", lambda = as.numeric(lambda), shift = shift)
  } else if (type == "gamma") {
    sojourn  <- list(type = "gamma",
                     shape = rep(3, K),
                     scale = rep(2, K),
                     shift = shift)
  }
  
  ## 5. ED (parms = mu, sigma, df)
  mu.list    <- lapply(1:K, function(j) colMeans(x[s == j, , drop = FALSE]))
  sigma.list <- lapply(1:K, function(j) var(x[s == j, , drop = FALSE]))
  nu.list    <- rep(8, K)
  
  # SPEC
  if (type == "geometric") {
    # HMM SPEC
    model <- hmmspec(
      init = init,
      trans = A,
      parms.emission = list(mu = mu.list, sigma = sigma.list, nu = nu.list),
      dens.emission = mtdist.hsmm
    )
    
    fit <- suppressWarnings(
      hmmfit(x, model, mstep = mtdist.mstep, maxit = 50)
      ) # suppressWarnings() --> To avoid non-list warning
  } else {
    # HSMM SPEC
    model <- hsmmspec(
      init         = init,
      transition   = A,
      parms.emission = list(mu = mu.list, sigma = sigma.list, nu = nu.list),
      sojourn        = sojourn,
      dens.emission  = mtdist.hsmm
    )
    
    fit <-  suppressWarnings(
      hsmmfit(x, model, mstep = mtdist.mstep, maxit = 50)
      ) # suppressWarnings() --> To avoid non-list warning 
  }
  
  # Return
  structure(list(
    fit = fit,
    yhat = fit$yhat,
    logLik = tail(fit$loglik, 1),
    K = K,
    type = paste0("mvt_", type)
  ),
  class = "multivariate_markov_model")
}

###########################################################################
# MODELS ------------------------------------------------------------------
###########################################################################

# Poisson

run.hsmm.tdist.poisson <- function(feature) {
  results <- list()
  for (K in 2:4) {
    cat("Fitting ED: Student-T  | SD: Poisson | Shift = 5 | K =", K, "\n")
    fit <- fit.mvt.markov(feature, K = K, type = "poisson", shift.val = 5)
    results[[paste0("ED: Student-T  | SD: Poisson | Shift = 5 | K =", K)]] <- fit
  }
  return(results)
}

# poisson.mdl <- run.hsmm.tdist.poisson(features)

# Gamma

run.hsmm.tdist.gamma <- function(feature) {
  results <- list()
  for (K in 2:5) {
    cat("Fitting ED: Student-T  | SD: Gamma | Shift = 0 | K =", K, "\n")
    fit <- fit.mvt.markov(feature, K = K, type = "gamma", shift.val = 0)
    results[[paste0("ED: Student-T  | SD: Gamma | Shift = 0 | K =", K)]] <- fit
  }
  return(results)
}

# gamma.mdl <- run.hsmm.tdist.gamma(features)

# Geometric

run.hmm.tdist.geometric <- function(feature) {
  results <- list()
  for (K in 2:5) {
    cat("Fitting ED: Student-T | K =", K, "\n")
    fit <- fit.mvt.markov(feature, K = K, type = "geometric")
    results[[paste0("ED: Student-T | K =", K)]] <- fit
  }
  return(results)
}

# geometric.mdl <- run.hmm.tdist.geometric(features)

# Run & Save

# save(
#   fit.mvt.markov,
#   mtdist.hsmm,
#   mtdist.mstep,
#   run.hsmm.tdist.poisson,
#   run.hsmm.tdist.gamma,
#   run.hmm.tdist.geometric,
#   poisson.mdl,
#   gamma.mdl,
#   geometric.mdl,
#   file = "models_and_functions_multivariate_markov_backup.RData"
# )

# EURUSD Backtest -->
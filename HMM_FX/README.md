# Hidden Semi-Markov Models for FX Regime Detection
## Quantitative Finance Project — CF II: LBK Algorithmic Trading Series

Objective
---------
The goal of this project is to identify hidden volatility regimes in EUR-based currency pairs and analyze how these regimes affect a mean-reversion trading strategy. 
The study combines financial econometrics, machine learning, and algorithmic trading logic to improve decision-making under regime shifts.

Methodology
------------
Model Framework:
- Models: Hidden Markov Models (HMM) and Hidden Semi-Markov Models (HSMM)
- Hidden States: K = 2 to 6
- Emission Distributions: Normal and Student’s t (for fat-tail modeling)
- Sojourn Distributions: Geometric, Poisson, and Gamma
- Implementation: R using the mhsmm package and custom wrapper functions
- Initialization: k-means clustering on log returns
- Model Selection: AIC, BIC, and ICL ranking
- Decoding: Viterbi algorithm and posterior state probabilities

Trading Application:
Decoded regimes are used to filter a mean-reversion trading strategy.

State 1: Low-volatility, stable market conditions — baseline strategy active  
State 2: Moderate, mean-reverting conditions — best performance zone  
State 3: High-volatility, trending conditions — trading disabled  

Backtesting used realistic EUR/USD 5-minute data with market-level costs:
Spread ≈ 0.1–0.2 pips, slippage 0.1 pip per side, commission $3 per lot round trip.

Key Results
------------
- Best models: Student’s t emission with Poisson sojourn distribution, K = 3–4
- HSMMs captured duration effects, but HMMs were sufficient for trading use
- State-based filtering reduced drawdowns and improved Sharpe ratio stability

Tools
-----
Language: R  
Packages: mhsmm, data.table, dplyr, ggplot2  
IDE: RStudio / VS Code  
Platform: Windows 11 (MSYS2 + Git Bash)  
Version Control: Git and GitHub

References
----------
Zucchini et al. (2016). Hidden Markov Models for Time Series: An Introduction Using R  
Maurotti et al. (2023). HSMM Model Selection in Financial Regimes  
Hamilton (1989). A New Approach to the Economic Analysis of Non-stationary Time Series

Author
------
Luka Bojovic  
Email: lukabojovic@gmail.com  
LinkedIn: linkedin.com/in/lukabojovic  
GitHub: github.com/lukabojovic

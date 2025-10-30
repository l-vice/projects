\# Hidden (Semi-)Markov Models for FX Regime Detection  

\*\*Quantitative Finance Project — CF II: LBK Algorithmic Trading Series\*\*



---



\## Objective

Identify \*\*hidden volatility regimes\*\* in EUR-based currency pairs and analyze how those regimes affect a mean-reversion trading strategy.  

The project combines \*\*financial econometrics\*\*, \*\*machine learning\*\*, and \*\*algorithmic trading logic\*\* to improve decision-making under regime shifts.



---



\## Methodology



\### Model Framework

\- \*\*Models:\*\* Hidden Markov Models (HMM) and Hidden Semi-Markov Models (HSMM)  

\- \*\*Hidden States:\*\* K = 2 – 6  

\- \*\*Emission Distributions:\*\* Normal and Student’s t (fat-tail capture)  

\- \*\*Sojourn Distributions:\*\* Geometric, Poisson, Gamma  

\- \*\*Implementation:\*\* R (`mhsmm`, custom wrappers)  

\- \*\*Initialization:\*\* k-means clustering on log returns  

\- \*\*Model Selection:\*\* AIC, BIC, ICL ranking  

\- \*\*Decoding:\*\* Viterbi algorithm + posterior state probabilities  



\### Trading Application

Decoded regimes are used to filter a mean-reversion strategy:

| State | Interpretation | Trading Action |

|:--|:--|:--|

| 1 | Low-volatility, stable | Baseline strategy active |

| 2 | Moderate, mean-reverting | Best performance zone |

| 3 | High-volatility, trending | Trading disabled |



Backtests use realistic EUR/USD 5-minute data with costs:  

spread ≈ 0.1–0.2 pips, slippage 0.1 pip/side, commission $3 per lot round trip.



---



\## Key Results

\- Best-ranked models: \*\*Student’s t + Poisson sojourn\*\*, K = 3 – 4  

\- HSMM captured duration effects; HMM performed comparably for trading use  

\- State-based filtering reduced drawdowns and improved Sharpe ratio stability  



---



\## Tools

\- Language: R  

\- Packages: `mhsmm`, `data.table`, `dplyr`, `ggplot2`  

\- IDE: RStudio / VS Code  

\- Platform: Windows 11 (MSYS2 + Git Bash)  

\- Version Control: Git + GitHub  



---



\## References

\- Zucchini et al. (2016). \*Hidden Markov Models for Time Series: An Introduction Using R.\*  

\- Maurotti et al. (2023). \*HSMM Model Selection in Financial Regimes.\*  

\- Hamilton (1989). \*A New Approach to the Economic Analysis of Non-stationary Time Series.\*



---



\## Author

Luka Bojovic

\[lukabojovic@gmail.com](mailto:lukabojovic@gmail.com)  

\[linkedin.com/in/lukabojovic](https://linkedin.com/in/lukabojovic)  

\[github.com/lukabojovic](https://github.com/lukabojovic)


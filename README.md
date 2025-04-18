# Time-Series-Forecasting-in-R
This project explores time series forecasting using datasets from different domains such as employment, production, wildlife populations, and retail. The analysis was conducted in R using the fpp3 package, which offers a framework for working with tidy time series data.
# Objectives
- Explore key time series characteristics: trend, seasonality, cyclicity, and anomalies
- Apply visual tools for decomposition and autocorrelation analysis
- Perform benchmark forecasting and evaluate model performance
- Conduct residual diagnostics to assess model assumptions
# Tools & Libraries
- Language: R
- Key Library: fpp3
- Other tools: ggplot2, dplyr, tidyverse, tsibble, feasts
# Datasets & Techniques
- Visual diagnostics using: autoplot(), gg_season(), gg_subseries(), gg_lag(), and ACF()
- Data sources include: U.S. employment data, Australian production and retail, Wildlife data (e.g., hare populations), Gasoline consumption and livestock numbers
- Benchmark forecasting using: Naive, Seasonal Naive, Mean, and Drift models
- Performance evaluation with test/train split and residual analysis

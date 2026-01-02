# Time Series Forecasting in R

## Project Overview
This project explores time series forecasting across multiple domains, including employment, production, wildlife populations, retail, and energy consumption. Using the fpp3 framework in R, I conducted visual diagnostics, benchmark forecasting, and residual analysis to better understand trends, seasonality, cyclicity, and anomalies in real-world datasets.

## Technologies
- R (fpp3, tsibble, feasts)
- Data manipulation & visualization: tidyverse, ggplot2, dplyr

## Methods & Data
- **Visual diagnostics:** autoplot(), gg_season(), gg_subseries(), gg_lag(), ACF plots  
- **Datasets:** U.S. employment, Australian production and retail, wildlife populations (hares), gasoline consumption, livestock counts  
- **Benchmark forecasting models:** Naive, Seasonal Naive, Mean, Drift  
- **Evaluation:** Train/test split, residual diagnostics to assess model assumptions

## Key Takeaways
- Explored trends, seasonality, and anomalies across diverse datasets  
- Short-term forecasts were often well captured by Naive models; drift models better captured long-term trends  
- Strong foundation in visual and statistical time series analysis in R

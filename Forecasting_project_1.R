install.packages(c("fpp3", "latex2exp", "knitr"))
#Ex 1
#Can you spot any seasonality, cyclicity and trend?
employment_data <- us_employment %>% filter(Title == "Total Private")
employment_data

#total private employment
#time plot
autoplot(employment_data, Employed) + labs(title = "Gasoline Consumption (US)")

#Seasonality
gg_season(employment_data, Employed) + labs(title = 'Seasonal plot for Total Private Employment')

#Subs plot for investigation of seasonal patterns by year
gg_subseries(employment_data, Employed) + labs(title = "Subseries plot for Total Private Employmenr")+ theme(
  axis.text.x = element_text(size = 10, angle=45),
  strip.text = element_blank()
)

#transformation
employment_data <- employment_data %>%
  mutate(Month = yearmonth(Month)) %>%
  tsibble(index = Month)
#autocorrelation
autocorrelation_plot_1 <- employment_data %>%
  ACF(Employed) %>%
  autoplot() + 
  labs(title = "Autocorrelation Plot for Total Private Employment", x = "Lag", y='acf')

autocorrelation_plot_1

library(feasts)
#Lags (for autocorrel)
gg_lag(employment_data, Employed, geom = 'point') + 
  labs(title = "Lag Plot for Total Private Employment")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8))
#it seems that time series is highly autocorrelated
#no white noise

#Bricks from aus
data('aus_production')

drop_na(bricks_data)

bricks_data <- aus_production %>% select(Quarter, Bricks) %>% as_tsibble(index = Quarter)
bricks_data
#bricks data = tsibble 

#autoplot
bricks_data %>%
  autoplot(Bricks) + labs(title = 'Bricks Production', y = "Bricks produced")

#Seasonal Plot
bricks_data %>%
  gg_season(Bricks) + 
  labs(title = "Subseries Plot for Bricks Production")
# i dont need mutate as its in correct fomat

#subseries
bricks_data %>%
  drop_na(Bricks) %>%
  gg_subseries(Bricks) + labs(title = "Subseries Plot for Bricks Production")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#autocorrel
bricks_data %>%
  ACF(Bricks) %>%
  autoplot() + 
  labs(title = "Autocorrelation Plot for Bricks Production")

#lag plot
bricks_data %>%
  drop_na(Bricks) %>%
  gg_lag(Bricks, geom = 'point') + 
  labs(title = "Lag Plot for Bricks Production")

#there is a pattern in lags so the variation is not due to white noise

#Hare from pelt
data('pelt')
pelt

pelt %>% 
  autoplot(Hare) + 
  labs(title = "Hare Population", y = "Population", x = "Year")

#seasonality (not expected in yearly data- thats good it doeant work)
pelt %>%
  gg_season(Hare) +
  labs(title = "Hare Population", y = "Population", x = "Year")

#subseries
summary(pelt$Hare)
pelt %>%
  gg_subseries(Hare) + 
  labs(title = "Subseries of Hare Population")

pelt %>%
  ACF(Hare) %>%
  autoplot() +
  labs(title = "Autocorrelation for Hare Population", y= "year")

pelt %>%
  gg_lag(Hare, geom = 'point') + 
  labs(title = "Lag Plot for Hare Population", x = "Hare", y = "Year")

#“H02” Cost from PBS
data("PBS")
PBS

pbs_h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  select(Month, Cost) %>%
  group_by(Month) %>%
  summarise(Cost = sum(Cost), .groups = "drop") %>%
  as_tsibble(index = Month, regular = TRUE)
pbs_h02

#autoplot
pbs_h02 %>%
  autoplot(Cost) +
  labs(title = "H02 Cost", y='cost', x='Month')

#seasonality
pbs_h02 %>%
  gg_season(Cost) + 
  labs(title = "Seasonality Plot of H02 COst", y="Cost", x = 'Year')
#subseries
pbs_h02 %>%
  gg_subseries(Cost) +
  labs(title = "Subseries of Plot of H02 Cost",y="cost", x='year') +
  theme(strip.text = element_blank())
#it calculates the autocorrel for all 4 types

#autocorrel for all types at once
pbs_h02_sum <- pbs_h02 %>%
  index_by(Month) %>%
  summarise(Total_Costs = sum(Cost))

#autocorrel single
pbs_h02_sum %>%
  ACF(Total_Costs) %>%
  autoplot() +
  labs(title = "Autocorrelation Plot for Total H02 Costs")

#gg lag
pbs_h02_sum %>% 
  gg_lag(Total_Costs, geom = 'point') +
  labs(title = "Lag Plot for Total Costs of H02", y="Total Costs", x= "Lagged total cost")

#and us_gasoline
data('us_gasoline')
us_gasoline

#autoplot
us_gasoline %>%
  autoplot(Barrels) + 
  labs(title = "US Gasoline Barrels", y = "Barrels", x = "Week")

#seasonality
us_gasoline %>% 
  gg_season(Barrels) + 
  labs(title = "Seasonality Pllot of US Gasoline Barrels")



#subseries
us_gasoline %>%
  gg_subseries(Barrels) +
  labs(title = "Subseries Plot of US Gasoline Barrels", y="Barrels", x = "Week")

#autocorrel
us_gasoline %>%
  ACF(Barrels) %>%
  autoplot() + 
  labs(title = "Autocorrelation Plot for US Gasoline Barrels")

#lags
us_gasoline %>%
  gg_lag(Barrels, geom = "point") +
  labs(title = "Lag Plot of US Gasoline Barrels", y = "Barrels", x = "Week")+
  theme(
    legend.text = element_text(size = 6),  # Adjust this value as needed
    legend.title = element_text(size = 7)  # Adjust this value as needed
  )


#Ex2
library(dbplyr)
library(tsibble)
data('aus_livestock')
aus_livestock
#filter year from month column 2.1
aus_livestock_filtered <- aus_livestock %>%
  filter(year(Month) >= 1990 & year(Month) <= 1995)
aus_livestock_filtered

#autoplot 2.2
library(ggplot2)
autoplot(aus_livestock_filtered, Count) +
  ggtitle("Pigs Slauthered in Vicroria 1990-1995") +
  ylab("Number")
#plot
acf(aus_livestock_filtered$Count, main= "Acf of pig slauters")

#2.3 longer data set will give more reliable ACF

#ex 3
#a)
#i did autoplots as it is more suited for fpp data
library(fpp3)
data('global_economy')
global_economy

#extract
us_gdp <- global_economy %>% filter(Country == "United States") %>% select(GDP)

#it showwed. a bit of exponential growth
autoplot(us_gdp, GDP) +
  ggtitle("US GDP") +
  ylab("GDP") +
  xlab("Year") #the graph is increasing (i will try log transformation)

#log transform= didnt work still non linear
autoplot(us_gdp, log(GDP)) + 
  ggtitle("Log transformation of GDP data")+
  ylab("Log of GDP")+
  xlab("Year")
#square root transform= it stabilized data and it seems approx linear now= i used mutation inside 
autoplot(us_gdp, sqrt(GDP)) + 
  ggtitle("Sqrt transformation of GDP data")+
  ylab("sqr of GDP")+
  xlab("Year")



#lambda optimum

lambda <- us_gdp %>%
  features(GDP, features = guerrero) %>%
  pull(lambda_guerrero)

#box cox also provides approx streight line
us_gdp <- us_gdp %>%
  mutate(BoxCox_GDP = box_cox(GDP, lambda)) 

autoplot(us_gdp, BoxCox_GDP) +
  ggtitle("Box-Cox transformation of gdp US data")+
  ylab("transformed GDP")+
  xlab("Year")

#b)
bulls_data <- aus_livestock %>%
  filter(Animal == "Bulls, bullocks and steers")

#autoplot of bulls from different territories
autoplot(bulls_data, Count) + 
  ggtitle("Counts of Bulls Bullocks and Steers") +
  ylab('Numbers')+
  xlab("Year")

#i need only victoria territory
#filter for victoria
bulls_victoria <- aus_livestock %>%
  filter(Animal == "Bulls, bullocks and steers" & State == "Victoria")


#many fluctuations
autoplot(bulls_victoria, Count)+
  ggtitle("Counts of Bulls, Bullocks and Steer from Victoria") +
  ylab("Number")+
  xlab("year")
#log and sq root doenst work, box cox didnt work


#box cox
lambda <- bulls_victoria %>%
  features(Count, features = guerrero) %>%
  pull(lambda_guerrero)

bulls_victoria <- bulls_victoria %>%
  mutate(box_cox_count = box_cox(Count, lambda))

#plot box cox
autoplot(bulls_victoria, box_cox_count) +
  ggtitle("Box-Cox transformed Count")+
  ylab("Box-Cox Counts")+
  xlab("Month")

autoplot(bulls_victoria, log(Count))+
  ggtitle("Log tranfsormation of Bulls, Bullocks and Steer for Victoria") +
  ylab("Number")+
  xlab("Month")

autoplot(bulls_victoria, sqrt(Count))+
  ggtitle("Autoplot of Bulls, Bullocks and Steer for Victoria") +
  ylab("Number")+
  xlab("Month")


#3.3 Gas from aus data
#autoplot
autoplot(aus_production, Gas) +
  ggtitle("Gas Prod. in Australia")+
  ylab('Gas Prod') +
  xlab("Year")

#log transformation doesnt work, sq doensnt work, box cox doesnt work
library(fpp3)
gas_data <- aus_production %>% select(Gas)
lambda <- gas_data %>%
  features(Gas, features = guerrero)

autoplot(aus_production, Gas) +
  ggtitle("Autoplot Gas Prouction in Australia")+
  ylab('Gas Prod') +
  xlab("Quarter")

lambda_val <- lambda %>% pull(lambda_guerrero)
lambda_val

gas_data <- gas_data %>%
  mutate(BoxCox_gas = box_cox(Gas, lambda))
autoplot(gas_data, BoxCox_gas) +
  ggtitle("Box cox transformed gas")+
  ylab("Box cox gas")+
  xlab("year")

#log
autoplot(aus_production, log(Gas)) +
  ggtitle("Log transformed Gas Prouction in Australia")+
  ylab('Gas Prod') +
  xlab("Year")

#square
autoplot(aus_production, sqrt(Gas)) +
  ggtitle("squae Gas Prod. in Australia")+
  ylab('Gas Prod') +
  xlab("Year")
#cube
autoplot(aus_production, signif(Gas)) +
  ggtitle("Gas Prod. in Australia")+
  ylab('Gas Prod') +
  xlab("Year")

#question 4
#4.1
library(fpp3)
aus_retail


takeaway_data <- aus_retail %>%
  filter(Industry == "Takeaway food services" & State == "Australian Capital Territory") %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

#autoplot of takeway data for australia
autoplot(takeaway_data, Turnover) +
  ggtitle("Australian Takeaway data autoplot")+
  xlab("Year")+
  ylab("turnover")
#increasing trend and some flustuations


#4.1

#extract yesr from month col
year_last <-max(year(takeaway_data$Month))
# define train
train <- takeaway_data %>%
  filter(year(Month) <= year_last - 4)
train
#test
test <- takeaway_data %>%
  filter(year(Month) > year_last -4)
test
#start and end of both
train %>% summarise(Start = min(Month), End = max(Month))
test %>% summarise(Start = min(Month), End = max(Month))

attr(train, "key") #data is grouped by industry and state
#test contains data from las 4 years and train from max year - 4 years

#4.2
#decomposition to know which model is appropriate (on training set)
#the data in train is grouped so to decompose we need to ungrpup it

#robust = true = less sensitive to outliers, on train data
library(fable)
decomp_STL <- train %>%
  model(decomp = STL(Turnover ~ season(window="periodic"), robust = TRUE))

#extract decomp components
decomp_components <- decomp_STL %>%
  components()
#stl plot (we have season and trend)
autoplot(decomp_components) +
  ggtitle("STL decomposition of Austrailia Takeaway") +
  ylab("Turnover Componens")+
  xlab("Year")
#seats
library(seasonal)
#start in april 82
train_ts <-ts(train$Turnover, start = c(1982, 4), frequency = 12)#convert to ts
seats <- seas(train_ts)
summary(seats)

autoplot(seats)+
  ggtitle("SEATS decomp")

#X11- leave x11="" as it tell function to apply seas adjustment
x_11 <- seas(train_ts, x11="")
summary(x_11)

autoplot(x_11) + 
  ggtitle("X-11 decomp")
#i should use additive stl

#now forecast
#mean model is not appropriate as it as it does not acc for trend and season (assums data is constant)
#Drift does not account for season( only for trend)
#Naive is for not trend and seas
#seas naive (appropriate-works well with seasonality)


#Drift on train set (but it not incorporate seasonality)
drift <- train %>%
  model(drift = RW(Turnover ~ drift()))

#forecast for test set (4 years)
drift_forecast <- drift %>%
  forecast(h= nrow(test))
#extract
drift_mean <- drift_forecast %>% pull(.mean)

#Seasonal naive 
s_naive <- train %>%
  model(snaive = SNAIVE(Turnover ~ lag('year')))
#forecast
s_naive_forecast <- s_naive %>%
  forecast(h=nrow(test))
#extract
s_naive_mean <- s_naive_forecast%>% pull(.mean)

s_naive_mean
s_naive_forecast
#benchmark method- Naive
naive <- train %>%
  model(naive = NAIVE(Turnover))

naive_forecast <- naive %>%
  forecast(h =nrow(test))

naive.mean <- naive_forecast %>% pull(.mean)

#extract actual and test
test_val <- test %>% pull(Turnover)
train_val <-train %>% pull(Turnover)

#4.3
# drift model accuracy
accuracy_drift <- accuracy(drift_forecast, test)

# accuracy s naive
accuracy_s_naive <- accuracy(s_naive_forecast, test)

#naive accuracy
accuracy_naive <- accuracy(naive_forecast, test)
#Accuracy results
accuracy_results <- data.frame(
  Method = c("Drift", "S.Naive", "Naive"),
  MAPE = c(accuracy_drift$MAPE, accuracy_s_naive$MAPE, accuracy_naive$MAPE),
  RMSE = c(accuracy_drift$RMSE, accuracy_s_naive$RMSE, accuracy_naive$RMSE),
  MAE = c(accuracy_drift$MAE, accuracy_s_naive$MAE, accuracy_naive$MAE),
  ME = c(accuracy_drift$ME, accuracy_s_naive$ME, accuracy_naive$ME),
  MPE = c(accuracy_drift$MPE, accuracy_s_naive$MPE, accuracy_naive$MPE))

accuracy_results
#MAPE
mape_drift <- accuracy_drift %>% select(MAPE) %>% pull()
mape_s_naive <- accuracy_s_naive %>% select(MAPE) %>%pull()
mape_naive <- accuracy_naive %>% select(MAPE) %>% pull()

rmse_drift <-accuracy_drift %>% select(RMSE) %>% pull()
rmse_s_naive <- accuracy_s_naive %>% select(RMSE) %>% pull()
rmse_naive <- accuracy_naive %>% select(RMSE) %>% pull()

mae_drift <- accuracy_drift %>% select(MAE) %>% pull()
mae_s_naive <- accuracy_s_naive %>% select(MAE) %>% pull()
mae_naive <-accuracy_naive %>% select(MAE) %>% pull()

mase_drift <- accuracy_drift  %>% select(MASE) %>% pull()
mase_s_naive <- accuracy_s_naive %>% select(MASE) %>% pull()
mase_naive <- accuracy_naive %>% select(MASE) %>% pull()

me_drift <- accuracy_drift %>% select(ME) %>% pull()
me_s_naive <- accuracy_s_naive %>% select(ME) %>% pull()
me_naive <- accuracy_naive %>% select(ME) %>% pull()


smape_drift <- accuracy_drift$ME
smape_s_naive <- accuracy_s_naive %>% select(sMAPE) %>% pull()
s_mape_naive <- accuracy_naive %>% select(sMAPE) %>% pull()


#accuracy_drift1 <- accuracy(drift_forecast, test)

drift_actual <- test$Turnover
drift_predicted <- drift_forecast$.fitted

#df for mape
accuracy_results <- data.frame(
  Method = c("Drift", "S.Naive", "Naive"),
  MAPE = c(mape_drift, mape_s_naive, mape_naive),
  RMSE = c(rmse_drift, rmse_s_naive, rmse_naive),
  MAE = c(mae_drift, mae_s_naive, mae_naive),
  MASE = c(mase_drift, mase_s_naive, mase_naive),
  ME = c(me_drift, me_s_naive, me_naive))

accuracy_results

#MPE RMSSE
accuracy()

#lower mape= better forecast
#add more measures


#residuals + white noise
#get resuduals
drift_fitted <- drift_fitted %>% pull(.fitted)


#residual calculation
train$Turnover <-as.numeric(train$Turnover)
residuals_drift <- train$Turnover - drift_fitted
residuals_drift <- na.omit(residuals_drift)

#it contains all inniv resid and fitted
augment(drift)

#tresiduals it gives innov resid
#we want residuals to look like white noise (look at the line correlosthj)
drift1 <- train %>%
  model(drift=RW(Turnover ~ drift()))
drift1 %>%  
  gg_tsresiduals()

#testljung-box test (chose seasonal data L!!!) white noise test
test_residuals <-drift1 %>%
  augment() %>%
  features(.innov, ljung_box, lag=10)

test_residuals

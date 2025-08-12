# Load necessary libraries
library(fpp3)
library(forecast)
library(ggplot2)
library(tsibble)
library(tseries)
library(lubridate)

# Load the data
gdp_data <- read.csv("us_gdp.csv", header = TRUE)

# Convert the 'quarter' column to a quarterly Date type
gdp_data$quarter <- yearquarter(gdp_data$quarter)

# Convert to a tsibble for time series analysis
gdp_ts <- as_tsibble(gdp_data, index = quarter)

# Split data into training and validation sets
split_point <- round(nrow(gdp_ts) * 0.8)
training_set <- gdp_ts[1:split_point, ]
validation_set <- gdp_ts[(split_point + 1):nrow(gdp_ts), ]


# Time series plot
autoplot(training_set) + xlab("Quarter") + ylab("GDP") + ggtitle("Time Series Plot for US GDP")

# Seasonal plot
gg_season(training_set) + xlab("Quarter") + ylab("GDP") + ggtitle("Seasonal Plot for US GDP")

# Sub-series plot
gg_subseries(training_set) + xlab("Quarter") + ylab("GDP") + ggtitle("Sub-series Plot for US GDP")

#Fit a linear regression model considering trend and seasonality
model_trend_season <- training_set %>%
  model(TSLM(gdp ~ trend() + season()))

forecast_trend_season <- forecast(model_trend_season, h = nrow(training_set))

model_trend_season %>%
  augment() %>%
  ggplot(aes(x = quarter, y = gdp)) +
  geom_line(aes(y = gdp, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "red", Fitted = "black")) +
  labs(y = "GDP of USA",
       title = "Time series regression model with trend and season for USA's GDP") +
  guides(colour = guide_legend(title = "Legend"))

# Forecasting with the TSLM model
forecast_data <- forecast(model_trend_season, new_data = validation_set)

# Plot the forecast and prediction intervals
forecast_data %>%
  autoplot() +
  labs(title = "Forecast of USA's GDP",
       x = "Quarter",
       y = "GDP")

forecast_gdp <- model_trend_season %>%
  forecast(h = 4 * 5)

forecast_gdp %>%
  autoplot(gdp_ts) +
  labs(title = "USA GDP",
       x = "Quarter",
       y = "GDP")

training_set %>% autoplot(gdp)

training_set %>% gg_season(gdp, period ="year")

training_set %>% gg_subseries(gdp)

#Plot manual ARIMA
training_set %>% features(gdp, unitroot_nsdiffs)

training_set %>% gg_tsdisplay(difference(gdp, 2), plot_type='partial')

ARIMA1.fit <- training_set %>% model(ARIMA(gdp ~ pdq(2,0,2) + PDQ(0,0,0)))
report(ARIMA1.fit) #Model is NULL - hence can be discarded

ARIMA2.fit <- training_set %>% model(ARIMA(gdp ~ pdq(1,0,2) + PDQ(0,0,0)))
report(ARIMA2.fit) #AIC=1014.1   AICc=1014.7   BIC=1036.97 -> ma1 = -0.1397

ARIMA3.fit <- training_set %>% model(ARIMA(gdp ~ pdq(2,0,1) + PDQ(0,0,0)))
report(ARIMA3.fit) #AIC=1014.04   AICc=1014.65   BIC=1036.88 -> ma1 = -0.3252 -> consider ARIMA2

ARIMA4.fit <- training_set %>% model(ARIMA(gdp ~ pdq(1,0,1) + PDQ(0,0,0)))
report(ARIMA4.fit) #AIC=1015.55   AICc=1016.33   BIC=1041.69 -> ma1 =  0.6514

ARIMA5.fit <- training_set %>% model(ARIMA(gdp ~ pdq(1,0,0) + PDQ(0,0,0)))
report(ARIMA5.fit) #Model is NULL - hence can be discarded

# ARIMA3 with the MA1 term has the lowest AIC
augment(ARIMA3.fit) %>% gg_tsdisplay(.resid, plot_type='partial')
#Still a spike at lag 15 in the PACF

ARIMA3.fit %>%
  augment() %>%
  ggplot(aes(x = quarter, y = gdp)) +
  geom_line(aes(y = gdp, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "red", Fitted = "black")) +
  labs(y = "USA GDP",
       title = "Manual Arima model for USA's GDP") +
  guides(colour = guide_legend(title = "Legend"))

augment(ARIMA3.fit) %>%
  features(.innov, ljung_box, lag=22) #Significant

#plot for Manual arima
forecast_arima_manual_model <- forecast(ARIMA3.fit, new_data = validation_set)

forecast_arima_manual_model %>%
  autoplot(gdp_ts) +
  labs(title = "Manual Arima Model forecast of USA's GDP",
       x = "Quarter",
       y = "GDP")

forecast_arima_manual_model %>%
  autoplot(gdp_ts) +
  labs(title = "Manual Arima Model forecast of USA's GDP",
       x = "Quarter",
       y = "Value")

arima.model.fit <- training_set %>% 
  model(arima_auto = ARIMA(gdp))

report(arima.model.fit)

arima.model.fit %>%
  augment() %>%
  ggplot(aes(x = quarter, y = gdp)) +
  geom_line(aes(y = gdp, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "red", Fitted = "black")) +
  labs(y = "Value",
       title = "Auto Arima model of USA's GDP") +
  guides(colour = guide_legend(title = "Legend"))


{r}
#plot for auto arima
forecast_arima_auto_model <- forecast(arima.model.fit, new_data = validation_set)

forecast_arima_auto_model %>%
  autoplot(gdp_ts) + 
  labs(title = "Auto ARIMA Forecast of USA's GDP",
       x = "Quarter",
       y = "Value")

ets_manual1 <- training_set %>% model(ETS(gdp ~ error("A") + trend("A") + season("A")))
report(ets_manual1)

ets_manual2 <- training_set %>% model(ETS(gdp ~ error("M") + trend("Ad") + season("M")))
report(ets_manual2)

ets_manual3 <- training_set %>% model(ETS(gdp ~ error("M") + trend("A") + season("A")))
report(ets_manual3)

ets_manual4 <- training_set %>% model(ETS(gdp ~ error("M") + trend("Ad") + season("A")))
report(ets_manual4)

ets_manual5 <- training_set %>% model(ETS(gdp ~ error("A") + trend("Ad") + season("A")))
report(ets_manual5)

ets_manual6 <- training_set %>% model(ETS(gdp ~ error("M") + trend("Ad") + season("M")))
report(ets_manual6)

ets_manual7 <- training_set %>% model(ETS(gdp ~ error("A") + trend("Ad") + season("M")))
report(ets_manual7)

ets_manual8 <- training_set %>% model(ETS(gdp ~ error("M") + trend("A") + season("A")))
report(ets_manual8)

ets_manual1 %>%
  augment() %>%
  ggplot(aes(x = quarter, y = gdp)) +
  geom_line(aes(y = gdp, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "red", Fitted = "black")) +
  labs(y = "GDP",
       title = "Manual ETS model of USA's GDP") +
  guides(colour = guide_legend(title = "Legend"))

ets_manual1 %>%
  forecast(h = 20) %>%
  autoplot(gdp_ts)


ses <- training_set %>% model(ETS(gdp ~ error("A") + trend("N") + season("N")))
report(ses)
ses.pred <- forecast(ses, h = 20)
ses.pred %>% autoplot(gdp_ts)
ses.pred %>% fabletools::accuracy(gdp_ts)


ets.auto.model.fit <- training_set %>%
  model(ets_auto = ETS(gdp))
report(ets.auto.model.fit)

ets.auto.model.fit %>%
  augment() %>%
  ggplot(aes(x = quarter, y = gdp)) +
  geom_line(aes(y = gdp, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "red", Fitted = "black")) +
  labs(y = "GDP",
       title = "Auto ETS model of USA's GDP") +
  guides(colour = guide_legend(title = "Legend"))

ets.auto.model.fit %>%
  forecast(h = 20) %>%
  autoplot(gdp_ts)

# Let's look at the predictions
ARIMA3.fit %>% forecast(h = 20) %>% autoplot(validation_set)

# We can also stack models
all.models.fit <- gdp_ts %>%
  model(arima_manual = ARIMA(gdp ~ pdq(2,0,1) + PDQ(0,0,0)),
        arima_auto = ARIMA(gdp),
        model_trend_season = TSLM(gdp ~ trend() + I(trend()^2) + season()),
        ets_manual1 = ETS(gdp ~ error("A") + trend("M") + season("A")),
        ets_manual2 = ETS(gdp ~ error("A") + trend("N") + season("A")),
        ets_auto = ETS(gdp))

all.models.pred <- all.models.fit %>% forecast(h = 20)


# How accurate are the models in cross-validation?
all.models.pred %>% fabletools::accuracy(gdp_ts)

# Our winning forecast
all.models.pred %>% 
  filter(.model == "model_trend_season") %>%
  autoplot(gdp_ts)

# Operational forecast (best model fit to all the data)
best.model.fit <- gdp_ts %>% 
  model(TSLM(gdp ~ trend() + I(trend()^2) + season()))

best.model.pred <- best.model.fit %>% forecast(h = 20)

best.model.pred %>% autoplot(gdp_ts)

#fit our standard regression, auto-ETS, and auto-ARIMA models - averages of the models
usgdpdata <- training_set %>%
  model(
    reg = TSLM(gdp ~ trend() + I(trend()^2) + season()),
    ets = ETS(gdp),
    arima = ARIMA(gdp),
  ) %>%
  mutate(combination = (reg + ets + arima) / 3)

# How well does each model fit the training data?
augment(usgdpdata)
View(augment(usgdpdata))

# And compare the training data to the fit of each model
augment(usgdpdata) %>% 
  filter(.model == "reg") %>%
  ggplot(aes(x = quarter)) +
  geom_line(aes(y = gdp, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Actual vs. Fitted Turnover Values for the Regression Model"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

augment(usgdpdata) %>%
  filter(.model == "ets") %>%
  ggplot(aes(x = quarter)) +
  geom_line(aes(y = gdp, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Actual vs. Fitted Turnover Values for the ETS Model"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

augment(usgdpdata) %>%
  filter(.model == "arima") %>%
  ggplot(aes(x = quarter)) +
  geom_line(aes(y = gdp, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Actual vs. Fitted Turnover Values for the ARIMA Model"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

augment(usgdpdata) %>%
  filter(.model == "combination") %>%
  ggplot(aes(x = quarter)) +
  geom_line(aes(y = gdp, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Actual vs. Fitted Turnover Values for the Ensemble Model"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

# Generate forecasts
usgdp_fc <- usgdpdata %>%
  forecast(h = "5 years")

# What does the regression model forecast look like?
usgdp_fc %>%
  filter(.model == "reg") %>%
  autoplot(validation_set) +
  labs(y = "$ billion",
       title = "USA GDP FORECAST - Regression Model")

# What does the auto-ETS model forecast look like?
usgdp_fc %>%
  filter(.model == "ets") %>%
  autoplot(validation_set) +
  labs(y = "$ billion",
       title = "USA GDP FORECAST - ETS Model")

# What does the auto-ARIMA model forecast look like?
usgdp_fc %>%
  filter(.model == "arima") %>%
  autoplot(validation_set) +
  labs(y = "$ billion",
       title = "USA GDP FORECAST - ARIMA Model")

# What does the ensemble forecast look like? Note that lack of prediction intervals!
usgdp_fc %>%
  filter(.model == "combination") %>%
  autoplot(validation_set) +
  labs(y = "$ billion",
       title = "USA GDP FORECAST - Ensemble Model")

# Which forecast is most accurate? - 
all.models.pred %>% fabletools::accuracy(gdp_ts)

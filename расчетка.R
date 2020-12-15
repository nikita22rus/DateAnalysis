library("fGarch")
library("quantmod")
library("forecast")
library("fracdiff")

# 1 Данные 200 наблюдений + сезонность
X <- PassUSA$Pass
plot(X, type = "l")

# 2 Проверяем уровень унтегрирования
plot(acf(PassUSA$Pass,
         plot = FALSE),
     type = "l") # автокорреляционная функция для AR(1) процесса
spectrum(PassUSA$Pass,
         method = "ar") # спектрограмма
# Расширенный тест Дики-Фуллера
# ndiffs(X, test = "adf")
# Тест KPSS
ndiffs(X, test = "kpss") # X ~ I(1) не стационарен d=1
# Тест Филлипса-Ферона
# ndiffs(X, test = "pp") 

# 3
main_X <- X
t <- 1:NROW(main_X)
t2 <- t^2
fit_t <- lm(main_X~t)
summary(fit_t)

fit_t2 <- lm(main_X~t2)
summary(fit_t2) # меньше Adj. R-sq

fit_tt2 <- lm(main_X~t+t2)
summary(fit_tt2) # чуть больше

fit_exp <- lm(log(main_X)~t)
plot(fit_exp$residuals)
summary(fit_exp) # чуть больше

# 4 Интервальный прогноз на 10 шагов вперед
main_fit <- fit_t2
new_t <- 201:211
new_t2 <- new_t^2
new <- data.frame(new_t2)
names(new) <- c("t2")
prediction <- forecast(main_fit, new)
autoplot(prediction,
         main = "Prediction +10")

# 5 
ndiffs(main_fit$residuals, test = "kpss") # ~I(0)
plot(acf(main_fit$residuals,
         plot = FALSE),
     type = "l")
spectrum(main_fit$residuals,
         method = "ar")

# 6
arima_fit <- auto.arima(main_X,
                        ic = "aic",
                        test = "pp")
summary(arima_fit)
autoplot(forecast(arima_fit, 10), main = "Prediction +10")

# 7
ndiffs(X)
#r <- diff(x)
#r <- tail(r,69)

fitarima1 <- arima(log(X), order=c(1, 1, 0))
fitarima <- auto.arima(X)
summary(fitarima)
fitgarch <- garchFit(~garch(1,1), data=fitarima$residuals)
summary(fitgarch)
ndiffs(r) 
fit_1 <- garchFit(~garch(1, 0), data=x)
summary(fit_1)

forecast <- predict(fit, 10)
plot(forecast$standardDeviation)

forecast <- predict(fitgarch, 10)
plot(forecast$standardDeviation)
     




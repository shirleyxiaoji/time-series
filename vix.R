df = read.csv("VIX.csv", head = T)

par(mfrow = c(2,2))

df1 = df[, c(1,5)]

plot(df1, ylim = c(10,40), main = "DAILY VIX PLOT", col = "black")
lines(df1)

df2 = df1
df2$Close = log(df1$Close)
colnames(df2) = c("Date","Log Close")
plot(df2,ylim = c(2.3, 3.5), main = "DAILY LOG VIX")
lines(df2)

#############nonliear###############
qqnorm(df1$Close)
qqline(df1$Close)

qqnorm(df2$`Log Close`)
qqline(df2$`Log Close`)

############linear##############
acf(df1$Close)
pacf(df1$Close)
fit = ar(df1$Close)

acf(df2$`Log Close`)

############WEEKLY##########
ts = read.csv("WEEKLY VIX.csv")


par(mfrow = c(2,2))

ts1 = ts[, c(1,5)]

plot(ts1, lty = 1,ylim = c(10,40), main = "WEEKLY VIX PLOT", col = "red")
lines(ts1)

ts2 = ts1
ts2$Close = log(ts1$Close)
colnames(ts2) = c("Date","Log Weekly Close")
plot(ts2,ylim = c(2.3,3.5), main = "LOG WEEKLY VIX")
lines(ts2)

qqnorm(ts1$Close)
qqline(ts1$Close)

qqnorm(ts2$`Log Weekly Close`)
qqline(ts2$`Log Weekly Close`)

acf(ts1$Close)
acf(ts2$`Log Weekly Close`)

pacf(ts1$Close)
pacf(ts2$`Log Weekly Close`)

ar(ts2$`Log Weekly Close`)
ar(ts1$Close)

arima(ts1$Close)
arima(ts2$`Log Weekly Close`)


####################trend###########

par(mfrow = c(2,2))

df3 = df1[nrow(df1):1,]
df3 = ts(df3$Close, frequency=250, start= c(2012,2,2))
dfdc = decompose(df3)
plot(dfdc)

plot(dfdc$seasonal)
dfadjusted = df3 - dc$seasonal
plot(dfadjusted, main = "Without seasonal trends")






ts3 =ts1[nrow(ts1):1,]
ts4 =ts2[nrow(ts2):1,]


ts3 = ts(ts3$Close, frequency=250, start= c(2012,2,1))
tsdc = decompose(ts3)
plot(tsdc)

tsadjusted = ts3 - tsdc$seasonal
plot(tsadjusted, main = "Without seasonal trends")


decompose(ts2$`Log Weekly Close`)
plot(tsdc$seasonal)


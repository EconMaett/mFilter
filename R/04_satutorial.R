# Tutorial - Decomposing Time Series Data ----
# by Kevin Kotzé
# Available at: https://kevinkotze.github.io/ts-5-tut/

# Note that this course contains the following tutorials:
# 1. The R programming language: https://kevinkotze.github.io/ts-1-tut/
# 2. ARIMA models: https://kevinkotze.github.io/ts-2-tut/
# 3. Forecasting: https://kevinkotze.github.io/ts-3-tut/
# 4. State-Space Modelling: https://kevinkotze.github.io/ts-4-tut/
# 5. Decomposing time series: https://kevinkotze.github.io/ts-5-tut/
# 6. Dieckey-Fuller Test: https://kevinkotze.github.io/ts-6-tut/
# 7. VAR models: https://kevinkotze.github.io/ts-7-tut/
# 8. Structural VAR models: https://kevinkotze.github.io/ts-8-tut/
# 9. Bayesian VAR models: https://kevinkotze.github.io/ts-9-tut/
# 10. Cointegration: https://kevinkotze.github.io/ts-10-tut/


# 1 Decomposing South African Output ----

devtools::install_github("KevinKotze/tsm")

# install.packages("mFilter")
library(tsm)
library(mFilter)

## 1.1 Load the data ----
dat <- sarb_quarter$KBP6006D
dat.tmp <- log(na.omit(dat))
gdp <- ts(dat.tmp, start = c(1960, 2), frequency = 4)

plot(gdp)


## 1.2 De-trend the data with a linear filter ----
lin.mod <- lm(gdp ~ time(gdp))
lin.trend <- lin.mod$fitted.values  # fitted values pertain to time trend
linear <- ts(lin.trend, start = c(1960, 1), frequency = 4)  # create a time series variable for trend
lin.cycle <- gdp - linear  # cycle is the difference between the data and linear trend

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)  # plot two graphs side by side and squash margins
plot.ts(gdp, ylab = "")  # first plot time series
lines(linear, col = "red")  # include lines over the plot
legend("topleft", legend = c("data", "trend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(lin.cycle, ylab = "")  # second plot for cycle
legend("topright", legend = c("cycle    "), lty = 1, col = c("black"), 
       bty = "n")


## 1.3 De-trend the data with the Hodrick-Prescott filter ----
hp.decom <- hpfilter(gdp, freq = 1600, type = "lambda")

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(gdp, ylab = "")  # plot time series
lines(hp.decom$trend, col = "red")  # include HP trend
legend("topleft", legend = c("data", "HPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(hp.decom$cycle, ylab = "")  # plot cycle
legend("topleft", legend = c("HPcycle"), lty = 1, col = c("black"), 
       bty = "n")


## 1.4 De-trend data with he Baxter-King filter ----
bp.decom <- bkfilter(gdp, pl = 6, pu = 32)

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(gdp, ylab = "")
lines(bp.decom$trend, col = "red")
legend("topleft", legend = c("data", "BPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(bp.decom$cycle, ylab = "")
legend("topleft", legend = c("BPcycle"), lty = 1, col = c("black"), 
       bty = "n")


## 1.5 De-trend the data with the Christiano-Fitzgerald filter ----
cf.decom <- cffilter(gdp, pl = 6, pu = 32, root = TRUE)

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(gdp, ylab = "")
lines(cf.decom$trend, col = "red")
legend("topleft", legend = c("data", "CFtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(cf.decom$cycle, ylab = "")
legend("topleft", legend = c("CFcycle"), lty = 1, col = c("black"), 
       bty = "n")


## 1.6 De-trend the data with the Beveridge-Nelson decomposition ----
bn.decomp <- bnd(gdp, nlag = 8)  # apply the BN decomposition that creates dataframe

bn.trend <- ts(bn.decomp[, 1], start = c(1960, 1), frequency = 4)  # first column contains trend
bn.cycle <- ts(bn.decomp[, 2], start = c(1960, 1), frequency = 4)  # second column contains cycle

par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(gdp, ylab = "")
lines(bn.trend, col = "red")
legend("topleft", legend = c("data", "BNtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(bn.cycle, ylab = "")
legend("topleft", legend = c("BNcycle"), lty = 1, col = c("black"), 
       bty = "n")


comb <- ts.union(lin.cycle, hp.decom$cycle, bp.decom$cycle, 
                 cf.decom$cycle, bn.cycle)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 2, 1), cex = 0.8)
plot.ts(comb, ylab = "", plot.type = "single", col = c("blue", 
                                                       "red", "darkgrey", "sienna", "darkgreen"))
legend("topleft", legend = c("linear", "hp-filter", "bp-filter", 
                             "cf-filter", "bn-decomp"), lty = 1, col = c("blue", 
                                                                         "red", "darkgrey", "sienna", "darkgreen"), bty = "n")


# 2 Spectral decompositions ----
library(tsm)
library(TSA)
library(mFilter)

no.obs <- 100
t <- seq(1, no.obs, 1)
w <- c(6/no.obs, 30/no.obs, 40/no.obs)

x1 <- 2 * cos(2 * pi * t * w[1]) + 3 * sin(2 * pi * t * 
                                             w[1])  # frequency 6 cycles in no.obs points
x2 <- 4 * cos(2 * pi * t * w[2]) + 5 * sin(2 * pi * t * 
                                             w[2])  # frequency 10 cycles in no.obs points
x3 <- 6 * cos(2 * pi * t * w[3]) + 7 * sin(2 * pi * t * 
                                             w[3])  # frequency 40 cycles in no.obs points
y <- x1 + x2 + x3


par(mfrow = c(2, 2), mar = c(2.2, 2.2, 2, 1), cex = 0.8)
plot(x1, type = "l", main = "x1")
plot(x2, type = "l", main = "x2")
plot(x3, type = "l", main = "x3")
plot(y, type = "l", main = "y")


periodogram(x1, main = "x1", col = "red")


periodogram(x2, main = "x2", col = "red")


periodogram(x3, main = "x3", col = "red")


periodogram(y, main = "y", col = "red")


ybp <- cffilter(y, pl = 16, pu = 20)
par(mfrow = c(1, 1))
periodogram(ybp$cycle, col = "red")


par(mfrow = c(1, 2), mar = c(2.2, 2.2, 2, 1), cex = 0.8)
plot(x1, type = "l", lty = 1)
lines(ybp$cycle, lty = 3, lwd = 3, col = "red")
legend("topleft", legend = c("X1", "Filtered     "), lty = c(1, 
                                                             3), col = c("black", "red"), bty = "n")

plot(y, type = "l", lty = 1)
lines(ybp$cycle, lty = 3, lwd = 3, col = "red")
legend("topleft", legend = c("Actual", "Filtered     "), 
       lty = c(1, 3), col = c("black", "red"), bty = "n")


## 2.1 Spectral decompositions on the South African business cycle ----

source("T5_decomp.R")

periodogram(lin.cycle, main = "Linear", col = "red")

periodogram(hp.decom$cycle, main = "Hodrick-Prescott", col = "red")

periodogram(bp.decom$cycle[13:(length(bp.decom$cycle) - 
                                 12)], main = "Band-Pass", col = "red")
periodogram(cf.decom$cycle, main = "Christiano-Fitzgerald", 
            col = "red")

periodogram(bn.cycle, main = "Beveridge-Nelson", col = "red")


# 3 Wavelet decompositions -----

library(tsm)
library(waveslim)

dat <- sarb_month$KBP7170N
cpi <- ts(na.omit(dat), start = c(2002, 1), frequency = 12)
inf.yoy <- diff(cpi, lag = 12)/cpi[-1 * (length(cpi) - 11):length(cpi)]

plot(inf.yoy)


inf.d4 <- modwt(inf.yoy, "d4", n.levels = 3)
names(inf.d4) <- c("w1", "w2", "w3", "v3")
inf.d4 <- phase.shift(inf.d4, "d4")


par(mfrow = c(5, 1))
plot.ts(inf.yoy, axes = FALSE, ylab = "actual", main = "")
for (i in 1:4) {
  plot.ts(inf.d4[[i]], axes = FALSE, ylab = names(inf.d4)[i])
}
axis(side = 1, at = c(seq(1, (length(inf.yoy)), by = 48)), 
     labels = c("2002", "2006", "2010", "2014"))


inf.tren <- ts(inf.d4$v3, start = c(2003, 1), frequency = 12)

plot.ts(inf.yoy, ylab = "inf")
lines(inf.tren, col = "red")


inf.tmp <- inf.tren + inf.d4$w3
inf.tren2 <- ts(inf.tmp, start = c(2003, 1), frequency = 12)

plot.ts(inf.yoy, ylab = "inf")
lines(inf.tren2, col = "red")


# 4 Correlation between the cyclical components of related economics variables ----

# We can now make use of the library
library(tsm)
library(mFilter)

# set datapath and get data
dat_tmp <- read.csv(file = "~/git/tsm/ts-5-tut/tut/stylizedData.csv")
dat <- dat_tmp[, 2:8]

n.obs <- dim(dat)[1]
n.var <- dim(dat)[2]

yd <- dat[5:n.obs, ] - dat[1:(n.obs - 4), ]  # store output

yc_li <- matrix(rep(0, n.obs * n.var), ncol = n.var)
yc_hp <- matrix(rep(0, n.obs * n.var), ncol = n.var)
yc_bp <- matrix(rep(0, n.obs * n.var), ncol = n.var)
yc_bn <- matrix(rep(0, n.obs * n.var), ncol = n.var)


for (i in 1:n.var) {
  
  # Detrend data with linear filter
  lin.mod <- lm(dat[, i] ~ time(dat[, i]))
  lin.trend <- lin.mod$fitted.values
  lin.cycle <- dat[, i] - lin.trend
  yc_li[, i] <- lin.cycle
  
  # Detrend data with HP filter
  hp.decom <- hpfilter(dat[, i], freq = 1600, type = "lambda")
  yc_hp[, i] <- hp.decom$cycle
  
  # Detrend data with Band-Pass filter
  bp.decom <- bkfilter(dat[, i], pl = 6, pu = 32)
  yc_bp[, i] <- bp.decom$cycle
  
  # Beveridge-Nelson decomposition
  bn.decomp <- bnd(dat[, i], nlag = 8)
  yc_bn[, i] <- bn.decomp[, 2]
}


ynames <- c("GDP", "CONS", "EXP", "IMP", "PROD", "INVEST", 
            "EMP")
filterNameList <- c("li", "hp", "bp", "bn", "yd")

leadAndLag <- seq(-4, 4, 1)

maxLeadLag <- max(leadAndLag)
corrStylizedFacts <- matrix(rep(NaN, (n.var * n.var * (4 + 
                                                         1) * length(leadAndLag))), ncol = length(leadAndLag))

cnt <- 0
ynamesLong <- matrix(rep(NaN, (n.var * n.var * (4 + 1))), 
                     ncol = 1)

for (i in 1:n.var) {
  for (j in 1:n.var) {
    c_li <- leadlag(yc_li[, i], yc_li[, j], maxLeadLag)
    c_hp <- leadlag(yc_hp[, i], yc_hp[, j], maxLeadLag)
    c_bp <- leadlag(yc_bp[4:(n.obs - 3), i], yc_bp[4:(n.obs - 
                                                        3), j], maxLeadLag)
    c_bn <- leadlag(yc_bn[, i], yc_bn[, j], maxLeadLag)
    c_yd <- leadlag(yd[, i], yd[, j], maxLeadLag)
    
    corrStylizedFacts[(1 + cnt):(cnt + 5), ] <- rbind(t(c_li), 
                                                      t(c_hp), t(c_bp), t(c_bn), t(c_yd))
    
    for (k in 1:5) {
      ynames.tmp <- c(ynames[i], "_", ynames[j], "_", 
                      filterNameList[k])
      ynamesLong[(cnt + k), 1] <- paste(ynames.tmp, 
                                        collapse = "")
    }
    cnt <- cnt + 5
  }
}



# linear trend

op <- par(mfrow = c(1, 3))
idx <- which(ynamesLong == "GDP_PROD_li")
barplot(corrStylizedFacts[idx, ], ylim = c(-1, 1), col = "red", 
        border = NA, names.arg = leadAndLag)
box()
abline(h = 0, col = "darkgray")
title(main = "GDP_PROD_li")

idx <- which(ynamesLong == "GDP_EMP_li")
barplot(corrStylizedFacts[idx, ], ylim = c(-1, 1), col = "red", 
        border = NA, names.arg = leadAndLag)
box()
abline(h = 0, col = "darkgray")
title(main = "GDP_EMP_li")

idx <- which(ynamesLong == "GDP_IMP_li")
barplot(corrStylizedFacts[idx, ], ylim = c(-1, 1), col = "red", 
        border = NA, names.arg = leadAndLag)
box()
abline(h = 0, col = "darkgray")
title(main = "GDP_IMP_li")


par(op)

# hp filter

op <- par(mfrow = c(1, 3))
idx <- which(ynamesLong == "GDP_PROD_hp")
barplot(corrStylizedFacts[idx, ], ylim = c(-1, 1), col = "red", 
        border = NA, names.arg = leadAndLag)
box()
abline(h = 0, col = "darkgray")
title(main = "GDP_PROD_hp")

idx <- which(ynamesLong == "GDP_EMP_hp")
barplot(corrStylizedFacts[idx, ], ylim = c(-1, 1), col = "red", 
        border = NA, names.arg = leadAndLag)
box()
abline(h = 0, col = "darkgray")
title(main = "GDP_EMP_hp")

idx <- which(ynamesLong == "GDP_IMP_hp")
barplot(corrStylizedFacts[idx, ], ylim = c(-1, 1), col = "red", 
        border = NA, names.arg = leadAndLag)
box()
abline(h = 0, col = "darkgray")
title(main = "GDP_IMP_hp")


par(op)

# beveridge nelson decomposition

op <- par(mfrow = c(1, 3))
idx <- which(ynamesLong == "GDP_PROD_bn")
barplot(corrStylizedFacts[idx, ], ylim = c(-1, 1), col = "red", 
        border = NA, names.arg = leadAndLag)
box()
abline(h = 0, col = "darkgray")
title(main = "GDP_PROD_bn")

idx <- which(ynamesLong == "GDP_EMP_bn")
barplot(corrStylizedFacts[idx, ], ylim = c(-1, 1), col = "red", 
        border = NA, names.arg = leadAndLag)
box()
abline(h = 0, col = "darkgray")
title(main = "GDP_EMP_bn")

idx <- which(ynamesLong == "GDP_IMP_bn")
barplot(corrStylizedFacts[idx, ], ylim = c(-1, 1), col = "red", 
        border = NA, names.arg = leadAndLag)
box()
abline(h = 0, col = "darkgray")
title(main = "GDP_IMP_bn")


par(op)

# END
# END

# mFilter examples -----

# Check out the examples provided in
help("mFilter")

# Decomposition of a time series into trend and cyclical components 
# using various filters


## Description ----
# mFilter is a generic function for filtering time series data. 

# The function invokes particular filters which depend on 
# the filter type specified via its argument `filter`. 

# The filters implemented in the package mFilter package are useful 
# for smoothing, and estimating tend and cyclical components. 

# Some of these filters are commonly used in economics and finance 
# for estimating cyclical component of time series.

# The `mFilter` package currently applies only to time series 
# objects of class `ts`.

# However a default method is available and should work 
# for any `base::numeric()` or `base::vector()` object.


## Usage ----

# The generic `S3` method is `mFilter(x, ...)`.

# The default `S3` method for `stats::ts()` objects is
# `mFilter(x, filter = c("HP", "BK", "CF", "BW", "TR"), ...)`.


## Arguments ----

# The argument `x` is a time series object of class `stats::ts()`.

# The `filter` argument specifies the filter type:
# - "HP": Hodrick-Prescott
# - "BK": Baxter-King
# - "CF": Christiano-Fitzgerald
# - "BW": Butterworth
# - "TR": trigonometric regrression

# Additionall arguments passed to the ellipsis parameter (`...`)
# are passed to the relevant filter functions
# - `hpfilter()`
# - `bkfilter()`
# - `cffilter()`
# - `bwfilter()`
# - `trfilter()`


# The return object is of class "mFilter".

# Use the `summary()` method to print a summary of the results,
# and the `plot()` method to plot the original series, trend, and
# cyclical components.

# The `print()` method displays the estimation results.

# The generic accessor functions `fitted()` and `residuals()`
# extract the estimated trend and cyclical components of the "mFilter" object.

# The return object of class "mFilter" contains at least the following 
# elements:
# - `cycle`: Estimated cyclical (irregular) component of the series.
# - `trend`: Estimated trend (smooth) component of the series.
# - `fmatrix`: The filter matrix applied to the original series.
# - `method`: The method, if available, for the filter type applied.
# - `type`: The filter type applied to the series.
# - `call`: Call to the function-
# - `title`: The title for displaying results.
# - `xname`: Name of the series passed to `mFilter()` for filtering.
# - `x`: The original time series (or drift-adjusted, if `drift = TRUE`)

# The following elements exist depending on the applied filter:
# - `nfix`: Length or order of the fixed length filters.
# - `pl`: Minimum period of oscillation of desired component (`2 <= pl`).
# - `pu`: Maximum period of oscillation of desired component (`2 <= pl < pu < Inf`).
# - `lambda`: Lambda (smoothness) parameter for the HP filter.
# - `root`: Whether the time series has a unit root, either TRUE or FALSE (default).
# - `thetha`: MA coefficients for time sereis model, used in "CF" fitler.


## Run examples ----

library(mFilter)

data(unemp)

### Hodrick-Prescott filter
unemp_hp <- mFilter(unemp, filter = "HP")

print(unemp_hp)
# Title: Hodrick-Prescott Filter
# Call: hpfilter(x = x, freq = ag$freq, type = ag$type, drift = ag$drift)
# Method: hpfitler
# Filter Type: lambda
# Series: unemp

# YYYY QX unemp Trend Cycle

summary(unemp_hp)
# In-sample error measures ME, MSE, MAE, MPE, MAPE are reported.

residuals(unemp_hp)

fitted(unemp_hp)

plot(unemp_hp)
png(filename = "figures/03_us-unemp-hpfilter.png", height = 600, width = 800)
dev.off()

# Plot the trend
plot(fitted(unemp_hp))

plot(residuals(unemp_hp))


### Other filters ----

unemp_bk <- mFilter(unemp, filter = "BK")
unemp_cf <- mFilter(unemp, filter = "CF")
unemp_bw <- mFilter(unemp, filter = "BW")
unemp_tr <- mFilter(unemp, filter = "TR")

# Plot the default trend output of the available filters
par(mfrow = c(2, 1), mar = c(3, 3, 2, 1), cex = 0.8)
plot(
  unemp,
  main = "Unemployment Series & Estimated Trends",
  col = 1,
  ylab = ""
)
lines(unemp_hp$trend, col = 2)
lines(unemp_bk$trend, col = 3)
lines(unemp_cf$trend, col = 4)
lines(unemp_bw$trend, col = 5)
lines(unemp_tr$trend, col = 6)

legend(
  "topleft",
  legend = c("series", "HP", "BK", "CF", "BW", "TR"),
  col = 1:6,
  lty = rep(1, 6),
  ncol = 2
)

# Plot the default cycle output of the different filters
plot(
  unemp_hp$cycle,
  main = "Estimated Cyclical Component",
  ylim = c(-2, 2.5),
  col = 2,
  ylab = ""
)
lines(unemp_bk$cycle, col = 3)
lines(unemp_cf$cycle, col = 4)
lines(unemp_bw$cycle, col = 5)
lines(unemp_tr$cycle, col = 6)

png(filename = "figures/04_us-unemp-filters.png", height = 600, width = 800)
dev.off()


### Different filter specifications ----

unemp_cf1 <- mFilter(unemp, filter = "CF", drift = TRUE, root = TRUE)

unemp_cf2 <- mFilter(unemp, filter = "CF", pl = 8, pu = 40, drift = TRUE, root = TRUE)

unemp_cf3 <- mFilter(unemp, filter = "CF", pl = 2, pu = 60, drift = TRUE, root = TRUE)

unemp_cf4 <- mFilter(unemp, filter = "CF", pl = 2, pu = 40, dift = TRUE, root = TRUE, theta = c(0.1, 0.4))


# Plot the estimated trend and cycle components for the different
# specifications of the Christiano-Fitzgerald fitler

## legend("topleft",legend=c("HP","BK","CF","BW","TR"),
## col=2:6,lty=rep(1,5),ncol=2)

par(mfrow = c(2, 1), mar = c(3, 3, 2, 1), cex = 0.8)

plot(
  unemp,
  main = "Christiano-Fitzgerald filter of unemployment: Trend\n root = TRUE, drift = TRUE",
  col = 1,
  ylab = ""
)
lines(unemp_cf1$trend, col = 2)
lines(unemp_cf2$trend, col = 3)
lines(unemp_cf3$trend, col = 4)
lines(unemp_cf4$trend, col = 5)
legend(
  "topleft",
  legend = c(
    "series", 
    "pl = 2, pu = 32", 
    "pl = 8, pu = 40", 
    "pl = 2, pu = 60", 
    "pl = 2, pu = 40", 
    "theta = 0.1, 0.4"
    ),
  col = 1:5,
  lty = rep(1, 5),
  ncol = 1
)

plot(
  unemp_cf1$cycle,
  main = "Christiano-Fitzgerald filter of unemployment: Cycle \n root=TRUE,drift=TRUE",
  col = 2, ylab = "", ylim = range(unemp_cf3$cycle)
  )
lines(unemp_cf2$cycle, col = 3)
lines(unemp_cf3$cycle, col = 4)
lines(unemp_cf4$cycle, col = 5)

png(filename = "figures/05_us-unemp-cffilter.png", height = 600, width = 800)
dev.off()

# END
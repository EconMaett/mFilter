# mFilter - miscellaneous time series filters ----

## Installation ----

# The package is available on CRAN
# install.packages("mFilter")
library(mFilter)


## Example ----

# This is a basic example which shows you how to do 
# Butterworth filtering

# Load the quarterly unemployment series for the US
# that is shipped with the `mFilter` package
data("unemp")
class(unemp)
# "ts"

### Basic BW Flter ----
unemp_bw <- bwfilter(unemp)
plot(unemp_bw)

png(filename = "figures/01_us-unemp-bwfilter.png", height = 800, width = 400)
dev.off()

### BW Filter arguments ----

# Try out the arguments of the `mFilter::bwfilter()` function

# `drift` in time series
unemp_bw1 <- bwfilter(unemp, drift = TRUE)


# `freq` is the cut-off frequency for the BW filter.
# default is `trunc(2.5 * frequency(x))`
trunc(2.5 * frequency(unemp))
# 10


# lower cut-off frequency and drift
unemp_bw2 <- bwfilter(unemp, freq = 8, drift = TRUE)


# Set higher order of the BW filter and drift
# Default is 2.
unemp_bw3 <- bwfilter(unemp, freq = 10, nfix = 3, drift = TRUE)

# Try even higher order
unemp_bw4 <- bwfilter(unemp, freq = 10, nfix = 4, drift = TRUE)

# Plot the results of the different BW filters
par(mfrow = c(2, 1), mar = c(3, 3, 2, 1), cex = 0.8)

plot(
  unemp_bw1$x, 
  main = "Butterworth filter of unemployment: Trend, drift = TRUE", 
  col = 1, ylab = ""
  )
lines(unemp_bw1$trend, col = 2)
lines(unemp_bw2$trend, col = 3)
lines(unemp_bw3$trend, col = 4)
lines(unemp_bw4$trend, col = 5)
legend(
  "topleft", 
  legend = c(
    "series", 
    "freq = 10, nfix = 2", 
    "freq = 8, nfix = 2", 
    "freq = 10, nfix = 3", 
    "freq = 10, nfix = 4"
    ),
  col = 1:5,
  lty = rep(1, 5),
  ncol = 1
  )

png(filename = "figures/02_us-unemp-bwfilter-arguments.png", height = 400, width = 800)
dev.off()

# END
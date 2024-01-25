# Long Run vs Short Run Decompositions in R ----

# The HP filter vs the Hamilton filter
# By Carlos Mendez
# Website: https://carlos-mendez.org/
# Available on RPubs: https://rpubs.com/quarcs-lab/long-run-filters

# Citation
# Mendez C. (2020). Long Run vs Short Run Decompositions in R: 
# The HP filter vs the Hamilton filter. R Studio/RPubs. 
# Available at <https://rpubs.com/quarcs-lab/long-run-filters>.


## 1. Set parameters ----

# Name of the series
seriesName <- "RGDPNAIDA666NRUG"

# Examples for other series:

# - Total GDP of Japan: "JPNRGDPEXP"
# - GDP per capita of Japan: "RGDPCHJPA625NUPN"
# - GPD per capita of Bolivia: "NYGDPPCAPKDBOL"
# - Total GDP of Bolivia: "RGDPNABOA666NRUG"
# - Total GDP of Indonesia: "RGDPNAIDA666NRUG"


## 2. Load the libraries ----
{
  library(mFilter)
  library(quantmod)
  library(tidyverse)
  library(neverhpfilter)
  library(xts)
  library(dygraphs)
}

# Change the presentation of decimal numbers to 4 
# and avoid scientific notation.
# options(prompt = "R> ", digits = 3, scipen = 999)


### 3. Import data ----
seriesName <- quantmod::getSymbols(
  Symbols = seriesName, 
  src = "FRED", 
  auto.assign = FALSE
)

head(seriesName)
is.xts(seriesName)
# TRUE

periodicity(seriesName)
# Yearly periodicity from 1960-01-01 to 2019-01-01 


## 4. Transform the data ----

# Take the logarithm of the series

seriesName <- log(seriesName)


## 5. Plot the evolution of the variable ----

dygraph(data = seriesName) |> 
  dyRangeSelector()


## 6. Apply the HP filter ----

seriesName_filtered_HP <- mFilter::hpfilter(seriesName, freq = 6.25)


### 6.1 Plot the HP filter ----

#### 6.1.1 Long-run trend ----

# Create matrix of actual, trend, and cycle values

actual  <- seriesName_filtered_HP[["x"]]
trendHP <- seriesName_filtered_HP[["trend"]]
cycleHP <- actual - trendHP

colnames(actual)  <- c("actual")
colnames(trendHP) <- c("trendHP")
colnames(cycleHP) <- c("cycleHP")

actual_and_trend <- cbind(actual, trendHP)

dygraph(actual_and_trend[ , 1:2]) |> 
  dyRangeSelector()


#### 6.1.2 Short-run fluctuations ----

dygraph(cycleHP) |> 
  dyRangeSelector()


## 7. Apply the Hamilton filter ----
seriesName_filtered_Hamilton <- neverhpfilter::yth_filter(
  x = seriesName, h = 2, p = 4, 
  output = c("x", "trend", "cycle")
)


### 7.1 Plot the Hamilton filter ----

#### 7.1.1 Long-run trend -----

# Rename the columns
colnames(seriesName_filtered_Hamilton) <- c("actual", "trendHamilton", "cycleHamilton")

dygraph(seriesName_filtered_Hamilton[ , 1:2]) |> 
  dyRangeSelector()


#### 7.1.2 Short-run fluctuations ----

dygraph(seriesName_filtered_Hamilton[ , 3]) |> 
  dyRangeSelector()


## 8. Run it in the cloud ----

# Tip: Copy and paste this link in another tab
# of your browser:
# URL: https://rstudio.cloud/project/25043

# END
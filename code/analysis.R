# In using R in VS code
# 1. Open R terminal
#Press shift + enter to run one LINE of code
#Press ctrl + shift + enter to run all code in the file
#View a data frame by typing View(dataframe)

################################# Setup ##################################

##### Set working directory
rm(list=ls(all=TRUE))
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

##### Load Libraries
library(tidyverse)
library(readxl)
library(tidyquant)
library(collapse)
library(zoo)

#source functions
source("funcs.R")


############################# Download data ##############################

#read csv from data folder
metadata <- read.csv("../data/metadata.csv")

#find first and last date from ESG_RATING_DATE
metadata %>%
    summarise(
        first_date = min(esg_rating_date),
         last_date = max(esg_rating_date)
    )
#download data from yahoo finance
first_date <- "2019/02/26"
last_date <- "2023/03/08"
tickers <- metadata$yahoo_ticker

all_time_series <- get_data(tickers, last_date, first_date)
#The following Time Series were not downloaded
#[1] NA
#[1] NA
#[1] "DE000A3E5D64.SG"

#drop columns containing NA or DE000A3E5D64.SG from metadata as there
#is no time series for those
metadata <- metadata %>% 
  filter(yahoo_ticker != "DE000A3E5D64.SG") %>%
  drop_na(yahoo_ticker)

#check if only 2 NAs and DE000A3E5D64.SG were dropped
assertthat::are_equal(nrow(metadata), 251)

#save data to "stockdata.csv" in data folder
#write.csv(all_time_series, "../data/stockdata.csv", row.names = FALSE)




######################## Clean stock time series #########################

#read csv from data folder
metadata <- read.csv("../data/metadata.csv")
stockdata <- read.csv("../data/stockdata.csv")

#some values in stockdata were wrongfully written as "null" (character)
#these corrupt values are replaced here with NA
stockdata <- stockdata %>%
  replace(. == "null", NA)

#fill NAs by filling-in the previous value
stockdata_clean <- apply(
  stockdata,
  2,
  FUN = function(data) {zoo::na.locf(data, na.rm = FALSE, fromLast =FALSE)}
)

#some time series have leading NAs. This is because the stock-price-series were
#not available for the 2019/02/26, the initial downloading date. 
#Here, the first date where the price is not NA for each column is identified
metadata["data_available_from"] <- as.Date(NA)
for (ticker in metadata$yahoo_ticker) {
  
  #build stockdata colname from metadata ticker
  colname_stockdata_clean <- paste0(ticker, "_adj_close")

  #get the time series for the current ticker
  time_series <- stockdata_clean[,c("Date", colname_stockdata_clean)]

  #compute the starting date of the time series
  begin_date <- time_series[sum(is.na(time_series)) + 1, "Date"]
  
  if (identical(begin_date, character(0))) {
    begin_date <- as.Date("2019/02/26")
  }
  
  #insert the starting date into metadata
  metadata[which(metadata$yahoo_ticker == ticker), "data_available_from"] <- begin_date

}

# Identify all tickers, where there is not enough data for the analysis 
# (because the are not at least 180 days of stock price data available, 
# before the esg_rating_date)
# The following stocks were dropped: EXO.AS, P911.DE
which(as.Date(metadata$esg_rating_date)-180 < metadata$data_available_from)
metadata <- metadata %>%
  filter(!yahoo_ticker %in% c("EXO.AS", "P911.DE"))
stockdata_clean <- stockdata_clean[, -c(77, 105)]

#check that the deletion of EXO.AS & P911.DE worked for both
#metadata & stockdata_clean
assertthat::are_equal(nrow(metadata), 249)
assertthat::are_equal(ncol(stockdata_clean), 250)
assertthat::are_equal("EXO.AS_adj_close" %in% colnames(stockdata_clean), FALSE)
assertthat::are_equal("P911.DE_adj_close" %in% colnames(stockdata_clean), FALSE)

#fill leading NAs by filling-in the first value available
stockdata_clean <- apply(
  stockdata_clean,
  2,
  FUN = function(data) {zoo::na.locf(data, na.rm = FALSE, fromLast =TRUE)}
)

# extract the date vector which can be used later as none of the rows are
# dropped anywhere
date_vector <- stockdata_clean[, "Date"]

#set the Date column as rowindex
rownames(stockdata_clean) <- stockdata_clean[, "Date"]
stockdata_clean <- stockdata_clean[, -1]



#check if there are now no NAs in the data anymore.
assertthat::are_equal(sum(is.na(stockdata_clean)), 0)

# Standardize data by dividing each value of a given column by the
# first value of the column
stockdata_standardized <- apply(
  stockdata_clean[,1:ncol(stockdata_clean)],
  2,
  FUN = function(data){return(as.numeric(data) / as.numeric(data[1]))}
)

#check if the standardized and non-standardized series are equivalent
assertthat::are_equal(ncol(stockdata_clean), ncol(stockdata_standardized)) #the first column is the Date column (not present in stockdata_standardized)
assertthat::are_equal(nrow(stockdata_clean), nrow(stockdata_standardized))
assertthat::are_equal(colnames(stockdata_clean), colnames(stockdata_standardized)) #the first column is the Date column (not present in stockdata_standardized)




############ Eliminate systematic factors in the time series #############

# Compute log prices of the standardized time series.
log_prices <- log(stockdata_standardized)
returns <- apply(log_prices, 2, diff)

#check log-returns and standardized series are equivalent
assertthat::are_equal(nrow(stockdata_standardized), nrow(returns)+1) #the first value has no difference
assertthat::are_equal(ncol(stockdata_standardized), ncol(returns))
assertthat::are_equal(colnames(stockdata_standardized), colnames(returns))

#compute covariance matrix
cov_matr <- cov(returns)

#check if matrix is symmetric anc contains all the columns
assertthat::are_equal(nrow(cov_matr), ncol(cov_matr))
assertthat::are_equal(nrow(cov_matr), ncol(returns))
assertthat::are_equal(sum(is.na(cov_matr)), 0)


#-------- compute benchmark for covariance matrix and for log-prices ---------#

# diagonal matrix with the dimensions of the covariance matrix but with one more column (not row!) for the benchmark
identity <- diag(nrow = nrow(cov_matr), ncol = ncol(cov_matr)+1) 
colnames(identity) <- c(colnames(cov_matr), "benchmark")
rownames(identity) <- row.names(cov_matr)

#fill the column "benchmark" with individual contributions of the stocks to the benchmark
#Note: Here we compute an equal-weighted benchmark. The weight of each stock is thus
#equivalent --> 1/249, since we have 249 stocks
identity[, "benchmark"] <- rep(1/nrow(cov_matr), nrow(cov_matr))

#compute the covariance of each stock with the benchmark using hadamard product
#also compute benchmark variance
#all can be done in one step I^T * SIGMA * I
cov_matr_bm <- t(identity) %*% cov_matr %*% identity

#compute benchmark time series on log prices
log_prices_bm <- log_prices %*% identity

#the benchmark log price series 
ts.plot(log_prices_bm[,"benchmark"]);grid()

#---- estimate beta and calculate it out of the cov_matr & returns series ----#

# calculate beta according to b = cov(R_stock, R_benchmark) / var(R_benchmark)
# https://www.investopedia.com/terms/b/beta.asp
# the right term here is market beta: Market beta: This refers to the beta of
# an asset or portfolio relative to the overall market, which is typically represented
# by a benchmark index such as the S&P 500.
betas <- cov_matr_bm[1:nrow(cov_matr_bm)-1, "benchmark"] / cov_matr_bm["benchmark", "benchmark"]

#check if the beta was computed correctly
assertthat::are_equal(as.numeric(betas %*% rep(1/nrow(cov_matr), nrow(cov_matr))), 1) #the benchmark beta must be 1
assertthat::are_equal(as.numeric(round(lm(diff(log_prices_bm[,"ASML.AS_adj_close"]) ~ diff(log_prices_bm[,"benchmark"]))$coefficients[2],5)), 
                      as.numeric(round(betas["ASML.AS_adj_close"],5))) #check that beta estimated through regression is approximately equivalent to the directly calculated beta
assertthat::are_equal(length(betas), ncol(cov_matr)) #there should not be a beta for the benchmark in the betas vector

#plot the beta for ASML.AS_adj_close (3.2350428)

beta_bayer <- ggplot(mapping = aes(
  x = diff(log_prices_bm[,"BAYN.DE_adj_close"]),
  y = diff(log_prices_bm[,"benchmark"]) ) ) +
  geom_point(size = 0.40) +
  geom_smooth(se = FALSE, smooth=TRUE, method = "lm", col = "red", size = 0.5) +
  xlab("Bayer AG Log-Returns") +
  ylab("Benchmark Log-Returns") +
  geom_text(aes(x = -0.13, y = -0.07), 
            label = latex2exp::TeX("$\\hat{\\beta}$ ≈ $0.94$"), 
            col = "red") +
  theme_bw()

beta_bayer
#ggsave("../plots/bayer_beta.png", beta_bayer, width = 1920, height = 1080, units = "px", dpi=300)

beta_ASML <- ggplot(mapping = aes(x = diff(log_prices_bm[,"ASML.AS_adj_close"]),
                                  y = diff(log_prices_bm[,"benchmark"]) ) ) +
  geom_point(size = 0.40) +
  geom_smooth(se = FALSE, smooth=TRUE, method = "lm", col = "red", size = 0.5) +
  xlab("ASML Holding N.V. Log-Returns") +
  ylab("Benchmark Log-Returns") +
  geom_text(aes(x = -0.118, y = -0.063), 
            label = latex2exp::TeX("$\\hat{\\beta}$ ≈ $1.21$"), 
            col = "red") +
  theme_bw()

beta_ASML
#ggsave("../plots/ASML_beta.png", beta_ASML, width = 1920, height = 1080, units = "px", dpi=300)


#summary statistics for the beta coefficient
cbind(
  min = min(betas),
  max = max(betas),
  mean = mean(betas),
  median = median(betas),
  sd = sd(betas)
)

#------------ compute excess prices and excess covariance matrix -------------#

# Why do we compute excess covariance? --> We have denoised our data from the 
# Beta, now we want to split the REMAINING systematic variance into systematic
# factors and stock specific factors (alpha). Notice later, the PCA is computed
# on the excess covariance matrix. 

#make identity matrix + one more row and insert the negative betas into the last row.
#the negative betas because we want to subtract (remove) the market factor (beta) from the original values
identity <- diag(nrow = nrow(cov_matr)+1, ncol=nrow(cov_matr))
rownames(identity) <- c(rownames(cov_matr), "betas")
colnames(identity) <- rownames(cov_matr)
identity["betas",] <- -betas

#beta-adjusted covariance matrix (excess returns / residuals)
cov_matr_residuals <- t(identity) %*% cov_matr_bm %*% identity

assertthat::are_equal(nrow(cov_matr_residuals), ncol(cov_matr_residuals)) #symmetry
assertthat::are_equal(dim(cov_matr_residuals), dim(cov_matr)) #the benchmark should now be removed
assertthat::are_equal(names(cov_matr), names(cov_matr_residuals)) #only the original tickers should be in the adjusted covariance matrix

#residual log returns: for each stock at each timepoint:
#the value of the benchmark at this time point is multiplied by the stocks
#beta, the result is subtracted from the log-price of the stock at the given date. e.g.
#2022-01-01-> NESTLE_stockprice_at_2022-01-01 - NESTLE_beta * benchmark_at_2022-01-01
residual_log_prices <- log_prices_bm %*% identity

assertthat::assert_that(isFALSE(any(residual_log_prices[1,]!=0))) #as we deal with standardized series, the first value of any stock must be 0
assertthat::are_equal(names(residual_log_prices), names(log_prices)) #check that no stock was lost
assertthat::are_equal(dim(residual_log_prices), dim(log_prices)) #there should not be a column added or removed from the log-prices matrix

#----------- remove further systematic factors from excess returns -----------#

#standardize covariance matrix --> correlation Matrix
corr_matr_residuals <- cov2cor(cov_matr_residuals)

assertthat::are_equal(dim(corr_matr_residuals), dim(cov_matr_residuals)) #the covariance and the correlation matrix should have the same length
assertthat::assert_that(isFALSE(any(corr_matr_residuals > 1))) #none of the values should be larger than 1
assertthat::assert_that(isFALSE(any(corr_matr_residuals < -1))) #the none of the values should be smaller than -1
assertthat::are_equal(t(corr_matr_residuals), corr_matr_residuals) #the correlation matrix must be symmetric

#standardize log-return series 
#Note: This must be done since we want the covariance matrix to be of equivalent scale as the log_prices
#Further note that we could do: corr_matr_residuals <- t(identity) %*% cov_matr_residuals %*% identity to get the 
#instead of cov2corr
identity_scaled <- diag(1 / sqrt(diag(cov_matr_residuals))) #take the inverse of the square root of all variances of the residuals.
rownames(identity_scaled) <- rownames(cov_matr)
colnames(identity_scaled) <- colnames(cov_matr)

# for each stock at each timepoint:
# the value of the log price series is multiplied with its scale factor
scaled_residual_log_prizes <- residual_log_prices %*% identity_scaled

assertthat::are_equal(dim(scaled_residual_log_prizes), dim(scaled_residual_log_prizes)) #there should not be a column added or removed from the log-prices matrix
assertthat::are_equal(colnames(scaled_residual_log_prizes), colnames(scaled_residual_log_prizes)) # no time series should be added or removed
assertthat::are_equal(rownames(scaled_residual_log_prizes), rownames(scaled_residual_log_prizes)) # no time series have the same data

#plot the series to inspect the y-label
ts.plot(scaled_residual_log_prizes)
ts.plot(residual_log_prices)

#-------- eigenvalue decomposition on the excess covariance matrix ----------# 

# eigenvalue decomposition on the RESIDUAL CORRELATION MATRIX
# here it becomes evident, why the Covariance Matrix was scaled before:
# Principal component analysis only works with scaled data!
principal_component_analysis <- eigen(corr_matr_residuals)

#plot eigenvalues
eigenvalues_residuals <- ggplot(mapping = aes(
  x = principal_component_analysis$values
)) + 
  geom_histogram(bins = 100, col = "black") + 
  geom_vline(xintercept = 5, col = "red") + 
  xlab("Eigenvalues of the Residual Correlation Matrix") +
  ylab("Number of Eigenvalues") +
  theme_bw()

eigenvalues_residuals # Note: we only want the eigenvalues that are on the right of the red line (larger than 1)

#ggsave("../plots/eigenvalues_residual_corr_mat.png", eigenvalues_residuals, width = 1920, height = 1080, units = "px", dpi=300)

# number of principal components according to the kaiser kriterion
sum(principal_component_analysis$values >= 1) # = 66

# we want a bit more of a conservative measure and therefore choose 16 principal components.
n_components = 5

#amount of variance loaded onto the first 5 eigenvectors (0.2278548)
sum(sort(principal_component_analysis$values, decreasing = TRUE)[1:n_components]) / sum(principal_component_analysis$values)
# | n_components | cumulated variance |
# | 5            | 0.2278548          |
# | 10           | 0.3089488          |
# | 20           | 0.4128858          |
# | 30           | 0.4871777          |
# | 40           | 0.5475375          |
# | 50           | 0.5993433          |
# | 60           | 0.644625           |

# Make a vector with the names for our eigenvectors.
# e.g. ["PC_1", "PC_2", ...]
principal_component_names <- paste0("PC_", seq_len(n_components))

# get the relevant eigenvectors from the principal_component_analysis-object
# and rename them with the above defined names!
relevant_eigenvectors_scaled <- principal_component_analysis$vectors[,1:n_components]
colnames(relevant_eigenvectors_scaled) <- principal_component_names
rownames(relevant_eigenvectors_scaled) <- rownames(cov_matr)
# NOTE: notice that the principal components were computed on the scaled covariance matrix
# and not on the price- or return series! Therefore the interpretation of each
# row of the eigenvector is the "exposure" of the stock to the principal component.

# from raw ticker to component
# As the covariance matrix and the price series had to be scaled to compute the PCA,
# the Eigenvectors now have to be scaled back to the orignial scale of the covariance and price series
relevant_eigenvectors <- identity %*% identity_scaled %*% relevant_eigenvectors_scaled

# add the (not scaled) the contribution (weights) of each stock to the benchmark to the matrix.
# In our case, each stock has the same weight (0.004016064) (equal weighted benchmark).
# INTERPRETATION: The values of the eigenvector can similarly be interpreted as the weight,
# of each stock on the factor! (here of course the stocks are not equal weighted.)
factor_weights = cbind(
  "benchmark" = append(rep(1/nrow(cov_matr), nrow(cov_matr)), 0), 
  relevant_eigenvectors
)


#--------- Compute Factor Covariance Matrix and Factor Performance -----------# 

#factor performance = performance time series of each factor
#factor covariance = covariance matrix between different factors
factor_cov_matr <- t(factor_weights) %*% cov_matr_bm %*% factor_weights
factor_prices <- log_prices_bm %*% factor_weights

# check if the covariance/correlation between the factors 
# are 0. This should be ensured because principal components are orthogonal.
assertthat::are_equal( all(c(diag(round(cov2cor(factor_cov_matr),4))) == 1), TRUE )

# price series of the benchmark
ts.plot(factor_prices[, "benchmark"]); grid()

#see how each of the components has performed over time
factor_performance_plot_data <- as.data.frame(
  t(t(factor_prices) / sqrt(diag(factor_cov_matr)) * 0.01)
) %>% gather() %>%
  mutate( time = rep( as.Date(date_vector), 6 ) ) %>%
  mutate( key = recode(
    key,
    benchmark = "Benchmark",
    PC_1 = "Latent Factor 1",
    PC_2 = "Latent Factor 2",
    PC_3 = "Latent Factor 3",
    PC_4 = "Latent Factor 4",
    PC_5 = "Latent Factor 5"
    )
  ) %>%
  rename(Factor = key)

factor_performance <- ggplot(mapping = aes(
  x = time,  
  y = value),
  data = factor_performance_plot_data
  ) +
  geom_line(aes(colour=Factor)) +
  xlab("Time") +
  ylab("Log-Price per Factor") +
  scale_color_brewer(palette="Paired") +
  theme_bw()

factor_performance

#ggsave("../plots/factor_performance.png", factor_performance, width = 1920, height = 1080, units = "px", dpi=300)


#------------------- compute stock specific factor beta ----------------------#

#### Prepare matrices

#Identity matrix where the factor performances are added as columns to the right
identity <- diag(nrow = nrow(cov_matr) + 1, ncol = nrow(cov_matr) + 1 + n_components)
colnames(identity) <- c(colnames(cov_matr), "benchmark", principal_component_names)
rownames(identity) <- c(rownames(cov_matr), "benchmark")

# "insert the factor weights into the identity matrix
identity[, c("benchmark", principal_component_names)] <- factor_weights

# "insert" the original covariance matrix into the Covariance matrix with the components
cov_matr_bm_components <- t(identity) %*% cov_matr_bm %*% identity
log_prices_bm_components <- cbind(log_prices, factor_prices)

assertthat::are_equal(log_prices_bm_components, log_prices_bm %*% identity)

### compute stock specific factor sensitivity
# in oppose to the market beta/market sensitivity/market risk, which only refers
# to the beta of a stock relative to the market/index, factor sensitivity/factor
# beta refers to the beta of a stock relative to all systematic factors( principal
# components)
#covariance of each factor with the original log_prices * solve(factor_cov_matr)
# the first part is the covariance of each share with each factor.
factor_beta <- cov_matr_bm_components[rownames(cov_matr), c("benchmark", principal_component_names)] %*% solve(cov_matr_bm_components[c("benchmark", principal_component_names), c("benchmark", principal_component_names)])
factor_beta <- t(factor_beta)

# the calculation was correct if the original market beta vector is equivalent
# to the benchmark vector in the factor beta matrix
assertthat::are_equal(solve(factor_cov_matr), solve(cov_matr_bm_components[c("benchmark", principal_component_names), c("benchmark", principal_component_names)]))
assertthat::are_equal(betas, factor_beta["benchmark",])
assertthat::are_equal(log_prices_bm[,"benchmark"], factor_prices[,"benchmark"])
assertthat::are_equal(betas, factor_beta[1,])


#--------------------- compute stock specific  alpha -------------------------#

identity <- diag(nrow = ncol(cov_matr) + 1 + n_components, ncol = nrow(cov_matr))
colnames(identity) <- c(rownames(cov_matr))
rownames(identity) <- c(colnames(cov_matr), "benchmark", principal_component_names)

# fill the the benchmark and PC-rows of the identity matrix with the negative 
# factor beta (intuitively we want to substract the factor performance from 
# the price series)
identity[c("benchmark", principal_component_names),] <- -factor_beta

# compute the denoised residual covariance matrix (that is, the covariance matrix 
# for the alpha)
cov_matr_denoised <- t(identity) %*% cov_matr_bm_components %*% identity

# compute log price series only containing the stock specific alpha
log_prices_alpha <- log_prices_bm_components %*% identity

#the covariance matrix must be equal to the covariance matrix of the log RETURN series
assertthat::are_equal(cov(apply(log_prices_alpha, 2, diff)), cov_matr_denoised)

# compute factor performance log price series FOR AN INDIVIDUAL SHARE
log_prices_factors <- factor_prices %*% factor_beta

#------------------------ Plot Alpha of Bayer and ASML -----------------------#

########## Bayer


# wrangle data such that it is in plottable form
alpha_bayer_plot_data <- as.data.frame(cbind(
  "Bayer AG Log-Price" = log_prices[, "BAYN.DE_adj_close"],
  "Bayer AG Performance \n due to Systematic Factors" = log_prices_factors[, "BAYN.DE_adj_close"],
  "Bayer AG alpha" = log_prices_alpha[, "BAYN.DE_adj_close"]
  )) %>%
  gather() %>%
  mutate("Time" = rep( as.Date(date_vector), 3) ) %>%
  rename("Time Series" = "key")

#plot the alpha of the bayer share
alpha_bayer <- ggplot(
  mapping = aes(
    x = Time,
    y = value
  ),
  data = alpha_bayer_plot_data) +
  geom_line(aes(colour = `Time Series`)) +
  xlab("Time") +
  ylab("Log-Price") +
  scale_color_brewer(palette="Paired") +
  theme_bw() +
  theme(legend.position = 'bottom', 
        legend.title=element_blank())

alpha_bayer

#ggsave("../plots/bayer_alpha.png", alpha_bayer, width = 1920, height = 1080, units = "px", dpi=300)


########## ASML

# wrangle data such that it is in plottable form
alpha_ASML_plot_data <- as.data.frame(cbind(
  "ASML Holding N.V. Log-Price" = log_prices[, "ASML.AS_adj_close"],
  "ASML Holding N.V. Performance \n due to Systematic Factors" = log_prices_factors[, "ASML.AS_adj_close"],
  "ASML Holding N.V. alpha" = log_prices_alpha[, "ASML.AS_adj_close"]
)) %>%
  gather() %>%
  mutate("Time" = rep( as.Date(date_vector), 3) ) %>%
  rename("Time Series" = "key")

#plot the alpha of the ASML share
alpha_ASML <- ggplot(
  mapping = aes(
    x = Time,
    y = value
  ),
  data = alpha_ASML_plot_data) +
  geom_line(aes(colour = `Time Series`)) + 
  xlab("Time") +
  ylab("Log-Price") +
  scale_color_brewer(palette="Paired") +
  theme_bw() +
  theme(legend.position = 'bottom', 
        legend.title=element_blank())

alpha_ASML

#ggsave("../plots/asml_alpha.png", alpha_ASML, width = 1920, height = 1080, units = "px", dpi=300)


#--- find the 10 best and worst performing stocks according to their alpha ---#

# compute the stock specific variance of the alpha of each equity
alpha_variance_per_equity <- diag(cov_matr_denoised)

# compute stock specificthe mean return on the alpha log prices
mean_alpha_log_return <- apply(log_prices_alpha, 2, diff)
mean_alpha_log_return <- apply(mean_alpha_log_return, 2, mean)

# the names must be exactly equal because we divide stock specific return by stock specific risk!
assertthat::are_equal(all(names(diag(cov_matr_denoised)) == names(mean_alpha_log_return)), TRUE)

# risk adjusted mean returns per stock.
# Notice that this is the Sharpe-Ratio. Since we compute this metric with alpha,
# which is basically a better market adjustment than in sharpes R_stock - R_market,
# this can be interpreted similar to the sharpe ratio
risk_adjusted_mean_log_alpha_return <- mean_alpha_log_return / diag(cov_matr_denoised)

# find the 10 best and worst performing stocks according to the sharpe ratio
bottom_10 <- names(sort(risk_adjusted_mean_log_alpha_return))[1:10]
top_10 <- names(sort(risk_adjusted_mean_log_alpha_return, decreasing = TRUE))[1:10]

#Display the 10 worst performing shares (according to their alpha)

alpha_worst_performer_plot_data <- as.data.frame(log_prices_alpha[, bottom_10]) %>%
  gather() %>%
  mutate(Time = rep( as.Date(date_vector), 10) ) %>%
  mutate( key = recode(
    key,
    FRE.DE_adj_close = "Fresenius SE & Co. KGaA",
    GBLB.BR_adj_close = "Groupe Bruxelles Lambert SA",
    PHIA.AS_adj_close = "Koninklijke Philips N.V.",
    BAS.DE_adj_close = "BASF SE",
    PROX.BR_adj_close = "Proximus PLC",
    ADS.DE_adj_close = "adidas AG",
    FME.DE_adj_close = "Fresenius Medical Care AG & \nCo. KGaA",
    CON.DE_adj_close = "Continental Aktiengesellschaft",
    HEN.DE_adj_close = "Henkel AG & Co. KGaA",
    HEIO.AS_adj_close = "Heineken Holding N.V."
  )
  ) %>%
  rename("Stock" = key)

alpha_10_worst_performers <- ggplot(
  mapping = aes(
    x = Time,
    y = value
  ),
  data = alpha_worst_performer_plot_data) +
  geom_line(aes(colour = Stock)) +
  xlab("Time") +
  ylab("Log-Price") +
  scale_color_brewer(palette="Paired") +
  theme_bw() +
  theme(legend.title=element_blank())

alpha_10_worst_performers

#ggsave("../plots/alpha_worst_10.png", alpha_10_worst_performers, width = 1920, height = 1080, units = "px", dpi=300)


#Display the 10 best performing shares (according to their alpha)

alpha_best_performer_plot_data <- as.data.frame(log_prices_alpha[, top_10]) %>%
  gather() %>%
  mutate(Time = rep( as.Date(date_vector), 10) ) %>%
  mutate( key = recode(
    key,
    MC.PA_adj_close = "LVMH Moët Hennessy - Louis \nVuitton,Société Européenne",
    DTE.DE_adj_close = "Deutsche Telekom AG",
    RMS.PA_adj_close = "Hermès International Société\nen commandite par actions",
    ELISA.HE_adj_close = "Elisa Oyj",
    BNP.PA_adj_close = "BNP Paribas SA",
    CS.PA_adj_close = "AXA SA",
    ISP.MI_adj_close = "Intesa Sanpaolo S.p.A.",
    NDA.FI.HE_adj_close = "Nordea Bank Abp",
    WDP.BR_adj_close = "Warehouses De Pauw NV/SA",
    DIE.BR_adj_close = "D'Ieteren Group SA" 
  )) %>%
  rename("Stock" = key)

alpha_10_best_performers <- ggplot(
  mapping = aes(
    x = Time,
    y = value
  ),
  data = alpha_best_performer_plot_data) +
  geom_line(aes(colour = Stock)) +
  xlab("Time") +
  ylab("Log-Price") +
  scale_color_brewer(palette="Paired") +
  theme_bw() +
  theme(legend.title=element_blank())

alpha_10_best_performers

#ggsave("../plots/alpha_best_10.png", alpha_10_best_performers, width = 1920, height = 1080, units = "px", dpi=300)

#------------------------------ Trend Estimation -----------------------------#

assertthat::are_equal(length(date_vector), nrow(log_prices_alpha))

#create matrix for the results
results <- matrix(nrow=nrow(metadata), ncol = 7)
colnames(results) <- c("equity", "category", "mean_after", "mean_before", "after_minus_before", "controversy", "CD")

#add the date vector to the alpha
log_prices_alpha_date <- cbind(log_prices_alpha,"Date" = date_vector)

for (index in 1:nrow(metadata)) {
  
  # load relevant row from metadata
  row <- metadata[index,]
  
  # save trend_category and trend_shift_date into separate variables
  trend_category <- row$esg_rating_trend
  trend_shift_date <- as.Date(row$esg_rating_date)
  
  
  colname_log_prices_alpha_date <- paste0(row$yahoo_ticker, "_adj_close")
  ts <- log_prices_alpha_date[,c(colname_log_prices_alpha_date, "Date")]
  
  # identify the start- and end-date as the date that is 180 days before / after
  # the date of the esg-trend-shift
  from_date <- as.Date(trend_shift_date)-180
  to_date <- as.Date(trend_shift_date)+180
  
  # it might be that the start/end date is not in the data, as the data holds
  # only values of days on which the stock exchange is open
  while( (from_date %in% as.Date(ts[, "Date"])) == FALSE ) {
    from_date <- from_date + 1
  }
  while( (to_date %in% as.Date(ts[, "Date"])) == FALSE ) {
    to_date <- to_date - 1
  }
  
  # identify log-prices AFTER the esg-rating-trend-shift
  ts_after_shift <- which((ts[,"Date"] > trend_shift_date) & (ts[,"Date"] <= to_date))
  ts_after_shift <- as.numeric(ts[ts_after_shift, colname_log_prices_alpha_date])
  
  # identify log-prices BEFORE the esg-rating-trend-shift
  ts_before_shift <- which((ts[,"Date"] < trend_shift_date) & (ts[,"Date"] >= from_date))
  ts_before_shift <- as.numeric(ts[ts_before_shift, colname_log_prices_alpha_date])
  
  #compute mean return
  mean_after <- mean(diff(ts_after_shift, lag = 1))
  mean_before <- mean(diff(ts_before_shift, lag = 1))
  results[index,1] <- row$yahoo_ticker
  results[index,2] <- trend_category
  results[index,3] <- mean_after
  results[index,4] <- mean_before
  results[index,5] <- mean_after - mean_before
  results[index,6] <- row$controversy_flag
  results[index,7] <- row$GICS_cd
  
  print(paste("Stock", row$yahoo_ticker, "finished"))

}

#-------------------------------- Plot Results -------------------------------#

results <- as.data.frame(results)

blinded_results <- results %>%
  select(-c(controversy, CD, category))

#write.csv(blinded_results, "../data/blinded_results.csv")

return_diff <- ggplot(
  mapping = aes(
    x = as.factor(results$category),
    y = as.numeric(results$after_minus_before)
  ),
) +
  geom_boxplot(aes(colour=results$category), show.legend = FALSE) +
  xlab("ESG Rating Trend") +
  ylab("Difference in Mean Return \nBefore vs. After ESG Rating Adjustment") +
  scale_color_brewer(palette="Paired") +
  theme_bw()

return_diff
#ggsave("../plots/boxplots_return_differences_Cyclical.png", return_diff, width = 1920, height = 1080, units = "px", dpi=300)


# Mean Return After ESG Rating Adjustment minus Mean Return Before ESG Rating Adjustment
#median grösser 0: nach den rating shift war die performance im mittel besser als zuvor
# median kleiner als 0: nach dem rating shift war die performance schlechter als zuvor

# Descriptive Statistics per Category
results %>% 
  group_by(category) %>%
  summarise("Mean" = mean(as.numeric(after_minus_before)),
            "Median" = median(as.numeric(after_minus_before)),
            "Standard Deviation" = sd(as.numeric(after_minus_before)),
            "Min" = min(as.numeric(after_minus_before)),
            "Max" = max(as.numeric(after_minus_before)),
            "N" = n()
            )

# Descriptive Statistics per Category and controversy_flag
results %>% 
  group_by(controversy, category) %>%
  summarise("Mean" = mean(as.numeric(after_minus_before)),
            "Median" = median(as.numeric(after_minus_before)),
            "Standard Deviation" = sd(as.numeric(after_minus_before)),
            "Min" = min(as.numeric(after_minus_before)),
            "Max" = max(as.numeric(after_minus_before)),
            "N" = n()
  )

# Descriptive Statistics per Category and Cyclical/Devensive
results %>% 
  group_by(CD, category) %>%
  summarise("Mean" = mean(as.numeric(after_minus_before)),
            "Median" = median(as.numeric(after_minus_before)),
            "Standard Deviation" = sd(as.numeric(after_minus_before)),
            "Min" = min(as.numeric(after_minus_before)),
            "Max" = max(as.numeric(after_minus_before)),
            "N" = n()
  )

# Shares that got an ESG Rating improvement of two categories
metadata[which(metadata$esg_rating_trend == 2), ]
# Bayer Aktiengesellschaft (BAYN.DE) https://www.bayer.com/en/sustainability/esgratings https://www.msci.com/our-solutions/esg-investing/esg-ratings-climate-search-tool/issuer/bayer-aktiengesellschaft/IID000000002123944
# Banco Santander, S.A. (SAN.MC)
# Porsche Automobil Holding SE (PAH3.DE) https://newsroom.porsche.com/en/company/annual-sustainability-report-2021/environment-social-and-governance/governance-and-transparency.html
# Mediobanca Banca di Credito Finanziario S.p.A. (MB.MI)
# BAWAG Group AG (BG.VI)
# SCOR SE (SCR.PA)

#

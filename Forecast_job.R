

################################### SET OF LIBRARY USED##############################################################

library(readxl) # To read a excel data

library(DBI)
library(RSQLite) # For SQL database manipulation 

library(tsibble)
library(tidyverse) # For data manipulation
library(tsibble)
library(dplyr)

library(corrplot) # to implement correlation matrix

library(FactoMineR) # to make principal component analysis (PCA)
library(factoextra)

library(KFAS) # to make a State Space Model 
library(mFilter)
library(prophet)




#####################################################################################################################
#                                   *******IMPORT*******                                                            #
#                                   **SQL DATABASE CREATION**                                                       #
#####################################################################################################################



Database <- read_excel("C:/Users/a535948/Desktop/SPP forecast project directory/Data/DatabaseV2.xlsx")


# Initialize a temporary in DATA database 
connection <- dbConnect(SQLite(), "DATA")

# Copy a data.frame into it
# dbWriteTable(connection, "Database", Database, overwrite = TRUE)

# Connection
table <- tbl(connection, "Database")




## Call of visualization function
source('C:/Users/a535948/Desktop/SPP forecast project directory/Forecast_job/Visualization_job.R')



VizCtry(c('Algeria', 'Argentina', 'Australia', 'Austria', 'Belarus',
          'Belgium', 'Bosnia and Herzegovina', 'Botswana', 'Brazil',
          'Bulgaria', 'Canada', 'Chile', 'China', 'China Domestic',
          'China External', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark',
          'Estonia', 'Finland', 'France', 'French Guyana'), range=1995:2024, segment = "Heavy Duty", Indicator = 'SALES_TMI')

VizInd(country = "US",  segment = "Heavy Duty", Indicators  = c('SALES_TMI','CSP_TOT_USD'), range=1990:2024)

















#####################################################################################################################
#                                   ******* US, CANADA and MEXICO *******                                           #
#                                                 **CASE**                                                          #
#####################################################################################################################






### Data preprocessing

US_data <- Database %>% filter(COUNTRY %in% c('US', 'Canada', 'Mexico'), YEAR %in% 1995:2060,  SEGMENT !='Light Duty') %>%
           select(-c(PRICE_FUEL_WI, RS_IDX, IP_MAN, GVA_MANUF_REAL, GVA_AGRF_REAL,
                  ENG_CCPI))
                                                                                    

US_data$CSP_OTH_USD <- US_data$CSP_TOT_USD - US_data$CSP_HOUS_USD - US_data$CS_NPTS_USD - US_data$CS_NPTS_USD-US_data$

#### Case 1 : Heavy duty


HUS_data <- US_data %>% filter(SEGMENT=='Heavy Duty' & YEAR %in% 2000:2024)  %>% 
            select(!c(SALES_TMF))

HUS_data1 <- HUS_data %>% select(7:38)

View(HUS_data1)



correlation_matrix <- cor(HUS_data1)
correlation_matrix1 <- cor(HUS_data1, method = "spearman")
hchart(correlation_matrix)
hchart(correlation_matrix1)





rownames(correlation_matrix) <- labels
length(colnames(correlation_matrix)) <- labels
## Modélisation

Ht <- matrix(NA)


cy=bkfilter(HUS_data2$GDP_CPE_USD)

c= cy$cycle


decompose(HUS_data2$SALES_TMI)




us <- table %>% filter(COUNTRY == 'US', SEGMENT == 'Heavy Duty', YEAR %in% 1990:2036) 

us <- us %>% collect()



us1 <- us %>% select(YEAR, GDP_CPE_USD, SALES_TMI) %>% mutate(across(c(2,3), log))


us2 <- us1 %>% as_tsibble(index = YEAR)

# Introducing Dummy
shock_years <- c(2008, 2009, 2014, 2020, 2022)
us2$dummy <- ifelse(us2$YEAR %in% shock_years, 1, 0)

# Modeling step (us case)-----------------------------------------------------------------------------------------------------------------------------------------------------------------------



# GDP is the principal exogeneous variable for this step



exp(12.491)


# Case 1 : Arima model family


# packages (installez si besoin)

library(forecast); library(prophet); library(dplyr); library(highcharter)

# --------------------------
# Données : adapter les noms de colonnes
# us2 doit contenir : year, y, x1, x2
# --------------------------
# Exemple : us2 <- data.frame(year = 1990:2020, y = rnorm(31), x1 = rnorm(31), x2 = rnorm(31))

start_year <- min(us2$YEAR)
y_ts <- ts(us2$SALES_TMI, start = start_year, frequency = 1)

# Prophet dataframe (ds / y) + inclure x1,x2

df_prophet <- us2 %>%
  mutate(ds = as.Date(paste0(YEAR, "-01-01"))) %>%
  select(ds, SALES_TMI, GDP_CPE_USD)

# horizon
h <- 12
n <- nrow(us2)
train_n <- n - h

# Train / test split
train_ts <- window(y_ts, end = 2024)


train_xreg <- as.matrix(us2[1:35, c("GDP_CPE_USD")])   # matrix for ARIMA
future_xreg <- as.matrix(us2[35:47, c("GDP_CPE_USD")]) # if you have future covariates

# If you don't have future covariates, decide un scénario, example: repeat last obs
if(nrow(future_xreg) < h){
  last_x <- as.numeric(tail(us2[, c("x1","x2")], 1))
  future_xreg <- matrix(rep(last_x, h), nrow = h, byrow = TRUE)
}

# --------------------------
# 1) ARIMA with xreg
# --------------------------
fit_arima <- auto.arima(train_ts, xreg = train_xreg, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
fc_arima <- forecast(fit_arima, xreg = future_xreg, h = h, level = c(80,95))

# prepare arima forecast dataframe
# extraire l'axe temporel du forecast
years_fc <- time(fc_arima$mean)   # vecteur des années correspondantes
arima_fc_df <- data.frame(
  ds = as.Date(paste0(round(years_fc), "-01-01")), # transforme en Date
  mean = as.numeric(fc_arima$mean),
  lower = as.numeric(fc_arima$lower[,2]),
  upper = as.numeric(fc_arima$upper[,2])
)




# --------------------------
# 4) highcharter interactive plots (combined)
# --------------------------
# Observed full series
obs_df <- df_prophet %>% select(ds, y)

# Combined HC: observed + ARIMA fc + Prophet fc
hc_arima <- hchart(us2, "line", hcaes(x = as.Date(paste0(YEAR, "-01-01")), y = SALES_TMI), 
                   name = "Observé") %>%
  hc_add_series(arima_fc_df, "line", hcaes(x = ds, y = mean), 
                name = "Prévision ARIMA") %>%
  hc_add_series(arima_fc_df, "arearange", 
                hcaes(x = ds, low = lower, high = upper),
                name = "IC 95%", color = hex_to_rgba("#87CEEB", 0.3)) %>%
  hc_title(text = "Prévisions ARIMA (annuel)") %>%
  hc_xAxis(title = list(text = "Année")) %>%
  hc_yAxis(title = list(text = "Valeur"))





#-- Case 2 : State Space Model with AR(2) cycle spec---------------------------------------------------------------------------------------------------------------------------------



Zt <- matrix(c(1,1,0), 1, 3)
Ht <- matrix(NA) 

Tt <- array(0, dim = c(3, 3))
Tt[1, 1] = 1 
Tt[2, 2] = 1
Tt[2, 3] = -1
Tt[3, 2] = 1 

Rt <- diag(3)

Qt <- matrix(0, 3, 3)
Qt[1,1] <- 0.5      # variance mu
Qt[2,2] <- 0.5      # variance cycle
Qt[3,3] <- 0        # c_{t-1}




a1 <- matrix(c(0,0,0), 3, 1)

P1  <- diag(0, 3, 3)
P1inf <- diag(c(1, 1, 0))

y <- us2[['SALES_TMI']][1:35]
x <- us2[['GDP_CPE_USD']][1:35]

model <- SSModel(
  y ~ -1 + 
    SSMcustom(
      Z = Zt, 
      T = Tt, 
      R = Rt, 
      Q = Qt, 
      a1 = a1, 
      P1 = P1,
      P1inf = P1inf, 
      state_names = c("mu","c","c*")
    ) +
    SSMregression(~ x, Q = 0), 
  H = Ht
)







updatefn <- function(pars, model) {
  # pars : vecteur de paramètres à estimer
  # Ordre des pars : log(sigma2_mu), log(sigma2_cycle), phi1, phi2, log(H)
  
  # Extraire les matrices
  Tt <- model$T
  Qt <- model$Q
  Ht <- model$H
  
  # Variances (log transformées pour garantir positivité)
  Qt[1,1,] <- exp(pars[1])   # sigma2_mu
  Qt[2,2,] <- exp(pars[2])   # sigma2_cycle
  Ht[]     <- exp(pars[5])   # variance observation
  
  # AR(2) coefficients
  Tt[2,2,] <- pars[3]  # phi1
  Tt[2,3,] <- pars[4]  # phi2
  
  # Mettre à jour le modèle
  model$T <- Tt
  model$Q <- Qt
  model$H <- Ht
  
  return(model)
}




var_y = var(y, na.rm=TRUE)

init_par <- c(log(var_y/10), log(var_y/10), 1, -1, log(var_y/20))  # inits pour log-variances et phi

fit <- fitSSM(model, inits = init_par, updatefn = updatefn, method = "BFGS")

fit$optim.out$par


out_gaussian <- KFS(fit$model)







###### prediction

x_new <- us2[['GDP_CPE_USD']][36:47]





# --- Paramètres estimés depuis fit ---

pars_hat <- fit_gaussian$optim.out$par
sigma_mu2     <- exp(pars_hat[1])
sigma_cycle2  <- exp(pars_hat[2])
phi1_hat      <- pars_hat[3]
phi2_hat      <- pars_hat[4]
sigma_eps2    <- exp(pars_hat[5])



# --- Nouvelles exogènes ---

x_new <- us2[['GDP_CPE_USD']][36:47]
n_future <- length(x_new)



# --- Construire T pour la période future ---
T_future <- Tt

T_future[2, 2] <- phi1_hat
T_future[2, 3] <- phi2_hat



# --- R et Q pour le futur ---
R_future <- Rt
Q_future <- Qt
Q_future[1,1] <- sigma_mu2
Q_future[2,2] <- sigma_cycle2


# --- H pour le futur ---

H_future <- Ht
H_future[1,1] <- sigma_eps2

# --- a1 et P1 pour le futur ---
a1_future <- a1
P1_future <- P1
P1inf_future <- P1inf

# --- Construire le SSModel futur ---
y_future <- rep(NA, n_future)

model_future <- SSModel(
  y_future ~ -1 +
    SSMcustom(
      Z = Zt,
      T = T_future,
      R = R_future,
      Q = Q_future,
      a1 = a1_future,
      P1 = P1_future,
      P1inf = P1inf_future,
      state_names = c("mu","c","c_lag")
    ) +
    SSMregression(~ x_new, Q = 0),  # inclure l'exogène
  H = H_future
)



forecast <- predict(fit$model, newdata = model_future,
                    interval = "prediction", level = 0.95)




# --- Transformer le forecast en dataframe ---
forecast_df <- data.frame(
  ds    = as.Date(paste0((us2 %>% filter(YEAR >=2025))$YEAR, "-01-01")),  # si tes années réelles sont 36:47
  mean  = as.numeric(forecast[, "fit"]),
  lower = as.numeric(forecast[, "lwr"]),
  upper = as.numeric(forecast[, "upr"])
)

# --- Séries historiques ---

historical_df_clean <- data.frame(
  ds    = as.Date(paste0(us2$YEAR, "-01-01")),
  mean  = us2$SALES_TMI,
  lower = NA,
  upper = NA
) %>% filter(ds <="2024-01-01")





# ----- Fusion passé + futur ------

combined_df <- bind_rows(
  historical_df_clean,  # déjà ds, mean, lower, upper
  forecast_df     # ds, mean, lower, upper
)


#


combined_df$ds <- as.Date(combined_df$ds)


# --- Graphique Highcharter ---
hc <- highchart() %>%
  # Série historique
  hc_add_series(combined_df%>% filter(ds <="2024-01-01"), "line",
                hcaes(x = ds, y = mean),
                name = "Observed", color = "#000000") %>%
  # Prévision moyenne
  hc_add_series(combined_df%>% filter(ds >"2024-01-01"), "line",
                hcaes(x = ds, y = mean),
                name = "Forecast", color = "#0072B2") %>%
  # IC 95%   
  hc_add_series(combined_df, "arearange",
                hcaes(x = ds, low = lower, high = upper),
                name = "IC 95%", color = hex_to_rgba("#87CEEB", 0.3)) %>%
  hc_title(text = "Prévisions Dynamiques AR(2) + Trend + Regressors") %>%
  hc_xAxis(type = "datetime", title = list(text = "Année")) %>%
  hc_yAxis(title = list(text = "SALES"))

hc


#---Case 2 : State Space Model with Harvey(1989) cycle spec---------------------------------------------------------------------------------------------------------------------------------



Zt <- matrix(c(1,1,0), 1, 3)
Ht <- matrix(NA) 

Tt = diag(3) 

Rt <- diag(3)
                       
Qt <- diag(1e-2, 3)


a1 <- matrix(c(0,0,0), 3, 1)

P1 <- diag(c(0, 1e-1, 1e-1))
P1inf <- diag(c(1, 0, 0))

y <- us2[['SALES_TMI']][1:35]
x <- us2[['GDP_CPE_USD']][1:35]
dummy <- us2[['dummy']][1:35]

y_lag1 <- lag(y, 1)
x_lag1 <- lag(x, 1)

valid <- !is.na(y_lag1)
y <- y[valid]
x <- x[valid]
y_lag1 <- y_lag1[valid]
x_lag1 <- x_lag1[valid]
dummy <- dummy[valid]






model <- SSModel(
  y ~ -1 + 
    SSMcustom(
      Z = Zt, 
      T = Tt, 
      R = Rt, 
      Q = Qt, 
      a1 = a1, 
      P1 = P1,
      P1inf = P1inf, 
      state_names = c("mu","u","u1")
    ) +
    SSMregression(~  dummy , Q = 0), 
  H = Ht
)




inv_logit <- function(z) 1 / (1 + exp(-z))

updatefn <- function(pars, model) {
  # pars : vecteur de paramètres à estimer
  # Ordre des pars : log(sigma2_mu), log(sigma2_cycle), phi1, phi2, log(H)
  
  # Extraire les matrices
  
  Tt <- model$T
  Qt <- model$Q
  Ht <- model$H
  
  # Variances (log transformées pour garantir positivité)
  Qt[1,1,] <- exp(pars[1])   # sigma2_mu
  Qt[2,2,] <- exp(pars[2])  # sigma2_cycle
  Qt[3,3,] <- exp(pars[3])   # sigma2_cycle*
  Ht[]     <- exp(pars[4])   # variance observation
  a_lambda<- pars[5]
  a_rho   <- pars[6]
  
  # mapping stable
  lambda <- pi * plogis(a_lambda)   # in (0, pi)
  rho    <- plogis(a_rho)
  
  # Cycle coefficients
  Tt[2,2,] <- Tt[3,3,] <- rho * cos(lambda)
  Tt[2,3,] <- rho * sin(lambda)
  Tt[3,2,] <- -Tt[2,3,]
  
  # Mettre à jour le modèle
  
  model$T <- Tt
  model$Q <- Qt
  model$H <- Ht
  
  return(model)
}


2*pi/(pi*plogis(-1.097064))



var_y = var(y, na.rm=TRUE)

init_par <- c(log(var_y/10), log(var_y/10), log(var_y/10), log(var_y/10),  qlogis(0.3), qlogis(0.9))  # inits pour log-variances et phi

fit <- fitSSM(model, inits = init_par, updatefn = updatefn, method = "BFGS")

fit$optim.out$par
plogis(1.122605)

out_gaussian <- KFS(fit$model, smoothing = "state")



checkresiduals(out_gaussian)
out_gaussian$model


###### prediction

x_new <- us2[['GDP_CPE_USD']][36:47]





# --- Paramètres estimés depuis fit ---

pars_hat <- fit$optim.out$par

sigma_mu <- exp(pars_hat[1])
sigma_u  <- exp(pars_hat[2])
sigma_u1 <- pars_hat[3]
sigma_eps <- exp(pars_hat[4])
rho <- pi * plogis(pars_hat[5])
lambda <- plogis(pars_hat[6])


# --- Nouvelles exogènes ---

x_new <- us2[['GDP_CPE_USD']][36:47]
dummy_new <- us2[['dummy']][36:47]
n_future <- length(x_new)



# --- Construire T pour la période future ---

T_future <- Tt


T_future[2,2] <- T_future[3,3] <- rho * cos(lambda)
T_future[2,3] <- rho * sin(lambda)
T_future[3,2] <- -T_future[2,3]

# --- R et Q pour le futur ---
R_future <- Rt
Q_future <- Qt

Q_future[1,1] <- sigma_mu
Q_future[2,2] <- sigma_u
Q_future[3,3] <- sigma_u1

# --- H pour le futur ---

H_future <- Ht
H_future[1,1] <- sigma_eps

# --- a1 et P1 pour le futur ---
a1_future <- a1
P1_future <- P1
P1inf_future <- P1inf

# --- Construire le SSModel futur ---
y_future <- rep(NA, n_future)

model_future <- SSModel(
  y_future ~ -1 +
    SSMcustom(
      Z = Zt,
      T = T_future,
      R = R_future,
      Q = Q_future,
      a1 = a1_future,
      P1 = P1_future,
      P1inf = P1inf_future,
      state_names = c("mu","u","u1")
    ) +
    SSMregression(~ x_new+dummy_new  , Q = 0),  # inclure l'exogène
  H = H_future
)



forecast <- predict(fit$model, newdata = model_future,
                    interval = "prediction", level = 0.95)




# --- Transformer le forecast en dataframe ---
forecast_df <- data.frame(
  ds    = as.Date(paste0((us2 %>% filter(YEAR >=2025))$YEAR, "-01-01")),  # si tes années réelles sont 36:47
  mean  = as.numeric(forecast[, "fit"]),
  lower = as.numeric(forecast[, "lwr"]),
  upper = as.numeric(forecast[, "upr"])
)




forecast[, "upr"]-forecast[, "lwr"]



# --- Séries historiques ---

historical_df_clean <- data.frame(
  ds    = as.Date(paste0(us2$YEAR, "-01-01")),
  mean  = us2$SALES_TMI,
  lower = NA,
  upper = NA
) %>% filter(ds <="2024-01-01")





# ----- Fusion passé + futur ------

combined_df <- bind_rows(
  historical_df_clean,  # déjà ds, mean, lower, upper
  forecast_df     # ds, mean, lower, upper
)




combined_df$ds <- as.Date(combined_df$ds)


# --- Graphique Highcharter ---

hc <- highchart() %>%
  # Série historique
  hc_add_series(combined_df%>% filter(ds <="2024-01-01"), "line",
                hcaes(x = ds, y = round(exp(mean))),
                name = "Observed", color = "#000000") %>%
  # Prévision moyenne
  hc_add_series(combined_df%>% filter(ds >"2024-01-01"), "line",
                hcaes(x = ds, y =  round(exp(mean))),
                name = "Forecast", color = "#0072B2") %>%
  # IC 95%   
  hc_add_series(combined_df, "arearange",
                hcaes(x = ds, low = round(exp(lower)), high = round(exp(upper))),
                name = "IC 95%", color = hex_to_rgba("#87CEEB", 0.3)) %>%
  hc_title(text = "Prévisions Dynamiques Harvey + Trend + Regressors") %>%
  hc_xAxis(type = "datetime", title = list(text = "Année")) %>%
  hc_yAxis(title = list(text = "SALES"))

hc













# us case with prophet-----------------------------------------------------------------------------------------------------------------------------------------------------------------



library(prophet)
library(dplyr)

# Série historique

historical_df <- data.frame(
  ds = as.Date(paste0(1990:(1990+length(y)-1), "-01-01")),
  y  = y,
  GDP = x  # exogène
)




m <- prophet(
  yearly.seasonality = FALSE,  # on ajoute une saison personnalisée
  daily.seasonality = FALSE,
  weekly.seasonality = FALSE
)

# Ajouter exogène
m <- add_regressor(m, 'GDP')

# Ajouter composante saisonnière courte (par exemple période 4 ans)
m <- add_seasonality(
  m, 
  name = 'short_cycle', 
  period = 8,      # périodicité en années
  fourier.order = 3  # nombre de coefficients pour approx. de Fourier
)

# Ajuster le modèle
m <- fit.prophet(m, historical_df)




# Futur 10 ans
future_df <- make_future_dataframe(m, periods = 12, freq = 'year')

# Ajouter exogène futur
future_df <- future_df %>%
  left_join(
    data.frame(ds = future_df$ds[(length(y)+1):47],
               GDP = x_new),
    by = "ds"
  ) %>% filter(ds >= "2025-01-01")

forecast <- predict(m, future_df)




library(highcharter)

# Nombre d'années historiques et futur
n_hist <- length(y)
n_future <- length(x_new)

# Préparer dataframe historique
historical_df2 <- data.frame(
  ds    = as.Date(paste0(1990:(1990+n_hist-1), "-01-01")),
  mean  = y,
  lower = NA,
  upper = NA
)

# Préparer dataframe futur

forecast_df <- data.frame(
  ds    = as.Date(paste0((1990+n_hist):(1990+n_hist+n_future-1), "-01-01")),
  mean  = as.numeric(forecast$yhat[1:12]),
  lower = as.numeric(forecast$yhat_lower[1:12]),
  upper = as.numeric(forecast$yhat_upper[1:12])
)

# Combiner
combined_df <- bind_rows(historical_df2, forecast_df)


highchart() %>%
  hc_add_series(combined_df, "line", hcaes(x=ds, y=mean), name="Prévision") %>%
  hc_add_series(combined_df, "arearange",
                hcaes(x=ds, low=lower, high=upper),
                name="IC 95%", color="lightblue", fillOpacity=0.3) %>%
  hc_xAxis(type="datetime") %>%
  hc_yAxis(title=list(text="Ventes")) %>%
  hc_title(text="Prévision ventes avec Prophet + saison courte + GDP")







########### Automatisation ##############################################################################################################################################



### supply feature

supply<- c('COAL_IDX',
            'GAS_IDX',
            'OIL_IDX',
            'AGRI_WLD',
            'ALU_USD',
            'COM_IDX',
            'ENG_CCPI',
            'COP_USD',
            'ENG_WLD',
            'GAS_WLD',
            'LITH_C_USD',
            'LITH_H_USD',
            'OIL_WPI_BRENT',
            'PRICE_FUEL_WI',
            'IR_CB',
            'IR_LEND',
            'INV_TFI_USD',
            'CS_IDX')


### demand feature

demand <- c('CSP_HOUS_USD',
            'CS_NPTS_USD',
            'CS_PTRC_USD',
            'CSP_TOT_USD',
            'CS_TSVP_USD',
            'CSP_VEH_USD',
            'RS_IDX',
            'EXP_USD',
            'IMP_GS_CPE_USD',
            'GDP_IND_REAL',
            'GVA_AGRF_REAL',
            'GDP_CPE_USD',
            'GVA_CONS_REAL',
            'GVA_MANUF_REAL',
            'GVA_SERV_REAL',
            'IP_MAN',
            'EMP_TOT',
            'GDP_PC_REAL',
            'POP_TOT',
            'RE_CPI',
            'URB_POP_PCT',
            'IP_IDX')

Identificator <- c("OXLO_CODE", "COUNTRY", "ISO3_CODE", "SEGMENT", "YEAR")


shock_years <- c(2008, 2009, 2014, 2015, 2020, 2021, 2022)


### Saving feature name

Var_labels <- list('COAL_IDX'= 'Domestic price of coal index',
                   'GAS_IDX'= 'Domestic price of gas index',
                   'OIL_IDX'= 'Domestic price of oil index',
                   'AGRI_WLD'= 'World agriculture raw materials price',
                   'ALU_USD'= 'World aluminum price, US$',
                   'COM_IDX'= 'World commodity index price, non-fuel',
                   'ENG_CCPI'= 'Energy component of CPI',
                   'COP_USD'= 'World copper price, US$',
                   'ENG_WLD'= 'World Energy Prices: Oil, Gas and Coal',
                   'GAS_WLD'= 'World gas price',
                   'LITH_C_USD'= 'World lithium carbonate price, US$',
                   'LITH_H_USD'= 'World lithium hydroxide price, US$',
                   'OIL_WPI_BRENT'= 'World oil price index, Brent crude spot',
                   'PRICE_FUEL_WI'= 'Prices, fuels, wholesale index',
                   'IR_CB'= 'Interest rate, central bank policy',
                   'IR_LEND'= 'Interest rate, lending',
                   'INV_TFI_USD'= 'Total fixed investment, constant prices and exchange rate, US$',
                   'CS_IDX'= 'Consumer price index',
                   'CSP_HOUS_USD'= 'Consumer spending, real, US$ - Housing electricity, gas and other fuels' ,
                   'CS_NPTS_USD'= 'Consumer spending, real, US$ - Non-personal transport services',
                   'CS_PTRC_USD'= 'Consumer spending, real, US$ - Personal transport running costs',
                   'CSP_TOT_USD'= 'Consumer spending, real, US$ - Total consumer spending',
                   'CS_TSVP_USD'= 'Consumer spending, real, US$ - Transport services and vehicle purchases - Total',
                   'CSP_VEH_USD'= 'Consumer spending, real, US$ - Vehicle purchases',
                   'RS_IDX'= 'Retail sales, volume index',
                   'EXP_USD'= 'Exports, goods & services, constant prices and exchange rate, US$',
                   'IMP_GS_CPE_USD'= 'Imports, goods & services, constant prices and exchange rate, US$',
                   'GDP_IND_REAL'= 'GDP, industry, real',
                   'GVA_AGRF_REAL'= 'Gross value added in agriculture and forestry, real',
                   'GDP_CPE_USD'= 'GDP, constant prices and exchange rate, US$',
                   'GVA_CONS_REAL'= 'Gross value added in construction, real',
                   'GVA_MANUF_REAL'= 'Gross value added in manufacturing, real',
                   'GVA_SERV_REAL'= 'Gross value added in services, real',
                   'IP_MAN'= 'Industrial production index, Manufacturing',
                   'EMP_TOT'= 'Employment, total',
                   'GDP_PC_REAL'= 'GDP per capita, real, US$, constant prices',
                   'POP_TOT'= 'Population, total',
                   'RE_CPI'= 'Real Earnings (relative to CPI)',
                   'URB_POP_PCT'= 'Urban population (% of total)',
                   'IP_IDX'= 'Industrial production index'
                   )





#### Build job database function



 Build_job_data <- function(Area, segment, data = table) {
  
                      data_filtered <- data %>% filter(COUNTRY == Area, SEGMENT==segment, YEAR <=2040) %>% collect()
                      
                      #  Fill missing values in SALES_TMF column with the corresponding values from SALES_TMI column
                      
                      data_filtered <- data_filtered %>% mutate(SALES_TMF = if_else( is.na(SALES_TMF), SALES_TMI, SALES_TMF))
                      
                      # Reduce data  to a corresponding window of non-null value of SALES_TMF column
                      
                      min_year <- min(data_filtered$YEAR[!is.na(data_filtered$SALES_TMF)])                
                      data_filtered <- data_filtered %>% filter(YEAR >= min_year)
                      
                      # Select non-null columns or feature
                      
                      
                      criteria = ~(sum(is.na(.))/length(.))*100 <= 30 ## Select the feature with contains less of 30 percents NA value
                      
                      col <- colnames(data_filtered %>% filter(YEAR <=2024) %>% select(where(criteria), SALES_TMF))
                      
                      data_filtered <- data_filtered %>% select(col) %>% arrange(COUNTRY, SEGMENT, YEAR)
                      
                      min_year1 <- data_filtered %>% filter(if_all(-Identificator, ~ !is.na(.))) %>% slice(1) %>% pull(YEAR)
                      
                      data_filtered <- data_filtered %>% filter(YEAR >= min_year1)
                      
                      data_filtered$SHOCK <- ifelse(data_filtered$YEAR %in% shock_years, 1, 0) # Add schok feature
                      
                      data_filtered <- data_filtered[, c(Identificator, intersect(colnames(data_filtered), c(supply, demand)), 'SHOCK', 'SALES_TMF')]
                      
                      data_filtered <- data_filtered %>% as_tsibble(index = YEAR)
                      
                      # Feature log-transfomation
                      
                      vars_to_exclude = c(Identificator, 'INF_CPI_YOY')
                      data_filtered <- data_filtered %>%
                        mutate(
                          across(
                            .cols = -any_of(vars_to_exclude),
                            .fns = ~ifelse(.x <= 0, 0, log(.x))
                          )
                        )
                      
                      # Apply PCA to extract Component feature
                      PCA.result = Add_Pcomponent(data_filtered)
                      
                      data_filtered = PCA.result$dataframe_adjusted
                      
                      return(list(data = data_filtered, PCA.summary = PCA.result))
                      
                  
                   }






#### PCA function

Add_Pcomponent <- function (dataframe){
                  
                  # Selecting variables
                  Country_supply <- intersect(colnames(dataframe), supply)
                  Country_demand <- intersect(colnames(dataframe), demand)
                  Country_whole <- c(Country_supply, Country_demand)
                  
                  # New dataframe according 
                  df_supply <- dataframe[,Country_supply]
                  df_demand <- dataframe[, Country_demand]
                  df <- dataframe[, Country_whole]
                  
                  # Perform PCA
                  pca_supply <- PCA(df_supply, scale.unit = TRUE,  graph = FALSE)
                  pca_demand <- PCA(df_demand, scale.unit = TRUE,  graph = FALSE) 
                  pca <- PCA(df, scale.unit = TRUE,  graph = FALSE) 
                  
                  # Graph result 
                  PCA_supplyPlot <- fviz_pca_var(pca_supply, col.var="contrib",
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                               repel = TRUE, # Avoid text overlapping
                               title = 'Principal Component Analysis for the supply side'
                               
                                    )
                  
                  PCA_demandPlot <- fviz_pca_var(pca_demand, col.var="contrib",
                                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                               repel = TRUE, # Avoid text overlapping
                                               title = 'Principal Component Analysis for the the demand side'
                                                
                                    )
                  
                  
                  PCA_T <- fviz_pca_var(pca, col.var="contrib",
                                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                        repel = TRUE, # Avoid text overlapping
                                        title = 'Principal Component Analysis for the the demand side'
                           )
                  
                  PCA_supplyVarContrib <- fviz_contrib(pca_supply, choice = "var", axes = 1, top = 10)
                  PCA_demandVarContrib <- fviz_contrib(pca_demand, choice = "var", axes = 1, top = 10) 
                  PCAVarContrib <- fviz_contrib(pca, choice = "var", axes = 1, top = 10) 
                  
                  Supply_factors <- as_tibble(pca_supply$ind$coord[, 1:5])
                  Demand_factors <- as_tibble(pca_demand$ind$coord[, 1:5])
                         factors <- as_tibble(pca$ind$coord[, 1:5])
                  
                  
                  Supply_factors <- Supply_factors %>%
                    rename_with(~ paste0("Supply_", .))
                  
                  Demand_factors <- Demand_factors %>%
                    rename_with(~ paste0("Demand_", .))
                  
                  factors <- factors %>%
                    rename_with(~ paste0("Macro_", .))
                  
                  
                  dataframe_adjusted <- bind_cols(dataframe, Supply_factors, Demand_factors, factors)
                  
                  
                  return(list(dataframe_adjusted = dataframe_adjusted, 
                              plot = list(supply=list(var=PCA_supplyPlot, contrib=PCA_supplyVarContrib),
                                          demand=list(var=PCA_demandPlot, contrib=PCA_demandVarContrib),
                                          macro=list(var=PCA_T, contrib=PCAVarContrib))))
                  
                  }




#### Build State Space Model function



Build_model <- function(Area, segment,  selected_feature = 'GDP_CPE_USD'){
  
      
                  out <- Build_job_data(Area, segment)
                  data <- out$data
                  data <- data[data$YEAR<=2024,]
                  y <- data$SALES_TMF
                  formula <- reformulate(c(selected_feature))
        
                  
                  Zt <- matrix(c(1,1,0), 1, 3)
                  Ht <- matrix(NA) 
                  
                  Tt = diag(3) 
                  
                  Rt <- diag(3)
                  
                  Qt <- diag(1e-2, 3)
                  
                  
                  a1 <- matrix(c(0,0,0), 3, 1)
                  
                  P1 <- diag(c(0, 1e-1, 1e-1))
                  P1inf <- diag(c(1, 0, 0))
                  
                  
                  model <- SSModel(
                    y ~ -1 + 
                      SSMcustom(
                        Z = Zt, 
                        T = Tt, 
                        R = Rt, 
                        Q = Qt, 
                        a1 = a1, 
                        P1 = P1,
                        P1inf = P1inf, 
                        state_names = c("mu","u","u*")
                      ) +
                      SSMregression(formula, data= data,  Q = 0), 
                    H = Ht
                            )
  
                  return(list(model=model, Formula=formula,  Matrice_system = list(Zt=Zt, Ht=Ht, Tt=Tt, Rt=Rt, Qt=Qt, a1=a1, P1=P1, P1inf=P1inf)))
                  
                }




#### Update function for state space model

updatefn <- function(pars, model) {
  # pars : parameter's vector 
  # pars order : log(sigma2_mu), log(sigma2_cycle), log(sigma2_cycle*), log(H), a_lambda, a_rho
  
  # matrix extraction
  
  Tt <- model$T
  Qt <- model$Q
  Ht <- model$H
  
  # exponential-transformation of Variance
  Qt[1,1,] <- exp(pars[1])   # sigma2_mu
  Qt[2,2,] <- exp(pars[2])  # sigma2_cycle
  Qt[3,3,] <- exp(pars[3])   # sigma2_cycle*
  Ht[]     <- exp(pars[4])   # variance observation
  a_lambda<- pars[5]
  a_rho   <- pars[6]
  
  # mapping stable
  lambda <- pi*plogis(a_lambda)  # in (0, pi)
  rho <- 0.8 + 0.19*plogis(a_rho) # in (0.8, 0.99)

  
  # Cycle coefficients
  Tt[2,2,] <- Tt[3,3,] <- rho * cos(lambda)
  Tt[2,3,] <- rho * sin(lambda)
  Tt[3,2,] <- -Tt[2,3,]
  
# Updating model
  
  model$T <- Tt
  model$Q <- Qt
  model$H <- Ht
  
  return(model)
  
}




#### Build running State Space Model (SSM) function


Run_SSM <- function(Area, segment,  selected_feature = 'GDP_CPE_USD') {
  
  
            # Training step
  
            result <- Build_model(Area, segment,  selected_feature)
            
            model <- result$model
            
            var_y <- mad(model$y, constant = 1)
            
            sigma_eta_init <- sqrt(0.3 * var_y)    # trend
            sigma_u_init   <- sqrt(0.25 * 0.5 * var_y)  # cycle c_t
            sigma_u_star_init <- sqrt(0.75 * 0.5 * var_y) # cycle c*_t
            sigma_eps_init <- sqrt(0.2 * var_y)    # observation
            
            
            init_par <- c(log(sigma_eta_init),
                          log(sigma_u_init),
                          log(sigma_u_star_init),
                          log(sigma_eps_init),  0, 0)  # inits pour log-variances, rho and lambda
            
            fit <- fitSSM(model, inits = init_par, control = list(maxit = 2000), updatefn = updatefn, method = "BFGS")
            
            out_gaussian <- KFS(fit$model, smoothing = "state")
            
            return(list(result=result, fit=fit, out_gaussian = out_gaussian)) 
            
           }




#### Make a visualization of forecast


Make_forecast <- function(Area, segment,  selected_feature = 'GDP_CPE_USD'){
  
                  # Train model  
                  result <- Run_SSM(Area, segment,  selected_feature)
                  
                  
                  # Estimated parameters recovery
                  pars_hat <- result$fit$optim.out$par
                  
                  sigma_mu <- exp(pars_hat[1])
                  sigma_u  <- exp(pars_hat[2])
                  sigma_u1 <- pars_hat[3]
                  sigma_eps <- exp(pars_hat[4])
                  lambda <- pi*plogis(pars_hat[5])
                  a_rho <-  pars_hat[6]
                  rho <- 0.8 + 0.19*plogis(a_rho)
                  
                  
                  # New value of regressors
                  out <- Build_job_data(Area, segment)
                  data <- out$data
                  new_data <- data[data$YEAR>=2025,]
  
    
                  n_future <- length(new_data$YEAR)
                  
                  
                  # Build matrice system for the future time period
                  
                  T_future <- result$result$Matrice_system$Tt
                  T_future[2,2] <- T_future[3,3] <- rho * cos(lambda)
                  T_future[2,3] <- rho * sin(lambda)
                  T_future[3,2] <- -T_future[2,3]
                  
                  # --- R and Q for the future ---
                  R_future <- result$result$Matrice_system$Rt
                  Q_future <- result$result$Matrice_system$Qt
                  
                  Q_future[1,1] <- sigma_mu
                  Q_future[2,2] <- sigma_u
                  Q_future[3,3] <- sigma_u1
                  
                  # --- H for the future ---
                  
                  H_future <- result$result$Matrice_system$Ht
                  H_future[1,1] <- sigma_eps
                  
                  # --- a1 and P1 for the future ---
                  a1_future <- result$result$Matrice_system$a1
                  P1_future <- result$result$Matrice_system$P1
                  P1inf_future <- result$result$Matrice_system$P1inf
                  
                  #--- Future spec which describe the dynamic of the future
                  formula <- reformulate(c(selected_feature))
                  y_future <- rep(NA, n_future)
                  
                  model_future <- SSModel(
                    y_future ~ -1 +
                      SSMcustom(
                        Z = Zt,
                        T = T_future,
                        R = R_future,
                        Q = Q_future,
                        a1 = a1_future,
                        P1 = P1_future,
                        P1inf = P1inf_future,
                        state_names = c("mu","u","u1")
                      ) +
                      SSMregression(formula, data = new_data, Q = 0),  
                    H = H_future
                                  )
                  
                  # Make a forcast
                  forecast <- predict(result$fit$model, newdata = model_future, interval = "prediction", level = 0.95)
                  
                  return(list(forecast_data = forecast, Fit_result = result))
                  
                                   
                    }



#### Output recovery : This section have the purpose to build a function that provide the forecast visualization and forecast table data




Forecast_output <- function(Area, segment,  selected_feature = 'GDP_CPE_USD'){
  
  
                    out <- Make_forecast(Area, segment,  selected_feature)
                    out1 <- Build_job_data(Area, segment)
                    data <- out1$data
                    
                    # --- Transforming the forecast into dataframe ---
                    forecast_df <- data.frame(
                      ds    = as.Date(paste0((data %>% filter(YEAR >=2025))$YEAR, "-01-01")), 
                      mean  = as.numeric(out$forecast_data[, "fit"]),
                      lower = as.numeric(out$forecast_data[, "lwr"]),
                      upper = as.numeric(out$forecast_data[, "upr"])
                      
                                   )
                    
                    # --- Historical series --- 
                    historical_df_clean <- data.frame(
                      ds    = as.Date(paste0(data$YEAR, "-01-01")),
                      mean  = data$SALES_TMF,
                      lower = NA,
                      upper = NA
                    ) %>% filter(ds <="2024-01-01")
                    
                    
                    
                    # ----- Combining past and future data ------
                    
                    combined_df <- bind_rows(
                      historical_df_clean,  
                      forecast_df     
                    )
                    combined_df$ds <- as.Date(combined_df$ds) # To force data format
                    
                    
                    # --- Highcharter Graph  ---
                    
                    hc <- highchart() %>%
                      # historical data
                      hc_add_series(combined_df%>% filter(ds <="2024-01-01"), "line",
                                    hcaes(x = ds, y = round(exp(mean))),
                                    name = "Observed", color = "#000000") %>%
                      # Forecast
                      hc_add_series(combined_df%>% filter(ds >"2024-01-01"), "line",
                                    hcaes(x = ds, y =  round(exp(mean))),
                                    name = "Forecast", color = "#0072B2") %>%
                      # Confidence Interval with level=95%   
                      hc_add_series(combined_df, "arearange",
                                    hcaes(x = ds, low = round(exp(lower)), high = round(exp(upper))),
                                    name = "IC 95%", color = hex_to_rgba("#87CEEB", 0.3)) %>%
                      hc_title(text = "Forecast") %>%
                      hc_xAxis(type = "datetime", title = list(text = "Année")) %>%
                      hc_yAxis(title = list(text = "SALES"))
                    
                   return(list(Graph= hc, Table = combined_df))
                    
  
                   }













































?sapply


#### Back-casting and Model efficiency optimization

?replace



correlation_matrix <- cor(US[1:35, setdiff(colnames(US),c(Identificator,'SHOCK'))])
correlation_matrix1 <- cor(HUS_data1, method = "spearman")

colnames(correlation_matrix)


correlation_matrix[2]


hchart(correlation_matrix)
hchart(correlation_matrix1)


unique(Database$COUNTRY)

#### Test of automation

colnames(us)


us <- Build_job_data('US', 'Heavy Duty')

us$Macro_Dim.2
COV <- colnames(Build_job_data('Algeria', 'Heavy Duty'))

COV  <-setdiff(COV, Identificator)

COV <- setdiff(COV, "SALES_TMF")

p = Forecast_output('US', 'Heavy Duty', selected_feature = c('GDP_CPE_USD', 'SHOCK'))

p2 = Forecast_output( "US" , 'Heavy Duty', selected_feature = c('Macro_Dim.1','Macro_Dim.2','SHOCK'))


p2$Graph
 

o <- Make_forecast('Canada', 'Heavy Duty', selected_feature = c('GDP_CPE_USD',  'SHOCK'))



t=Add_Pcomponent(china[,6:25])


o$Fit_result$fit$optim.out

checkresiduals(o$Fit_result$out_gaussian)












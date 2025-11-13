

################################### SET OF LIBRARY USED ##############################################################

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



Database <- read_excel("Confidential")


# Initialize a temporary in DATA database 
connection <- dbConnect(SQLite(), "DATA")

# Copy a data.frame into it
# dbWriteTable(connection, "Database", Database, overwrite = TRUE)

# Connection
table <- tbl(connection, "Database")




## Call of visualization function
source('Confidential/Visualization_job.R')











########### Automatisation ##############################################################################################################################################



### supply feature

supply<- c("Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
           "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential")


### demand feature

demand <- c("Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
           "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential")

Identificator <- c("Confidential", "COUNTRY", "Confidential", "Confidential", "YEAR")


shock_years <- c(2008, 2009, 2014, 2015, 2020, 2021, 2022)


### Saving feature name

Var_labels <- list("Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
           "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
            "Confidential",
                   .
                   .
                   .
            "Confidential")





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
                      
                    
                      #min_year1 <- data_filtered %>% filter(if_all(-Identificator, ~ !is.na(.))) %>% slice(1) %>% pull(YEAR)
                      
                      
                      #data_filtered <- data_filtered %>% filter(YEAR >= min_year1)
                      
                      data_filtered$SHOCK <- ifelse(data_filtered$YEAR %in% shock_years, 1, 0) # Add schok feature
                      
                      data_filtered <- data_filtered[, c(Identificator, intersect(colnames(data_filtered), c(supply, demand)), "Confidential", "Confidential")]
                      
                      data_filtered <- data_filtered %>% as_tsibble(index = YEAR)
                      
                      # Feature log-transfomation
                      
                      vars_to_exclude = c(Identificator, "Confidential")
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



Build_model <- function(Area, segment,  selected_feature = "Confidential", data = table){
  
      
                  out <- Build_job_data(Area, segment, data)
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




#### Update function for State Space Model (SSM)

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


Run_SSM <- function(Area, segment,  selected_feature ="Confidential", data = table) {
  
  
            # Training step
  
            result <- Build_model(Area, segment,  selected_feature, data = table)
            
            model <- result$model
            
            var_y <- mad(model$y, constant = 1)
            
            sigma_eta_init <- sqrt(0.2 * var_y)    # trend
            sigma_u_init   <- sqrt(0.25 * 0.5 * var_y)  # cycle c_t
            sigma_u_star_init <- sqrt(0.75 * 0.5 * var_y) # cycle c*_t
            sigma_eps_init <- sqrt(0.3 * var_y)    # observation
            
            
            init_par <- c(log(sigma_eta_init),
                          log(sigma_u_init),
                          log(sigma_u_star_init),
                          log(sigma_eps_init),  0, 0)  # inits pour log-variances, rho and lambda
            
            fit <- fitSSM(model, inits = init_par, control = list(maxit = 2000), updatefn = updatefn, method = "BFGS")
            
            out_gaussian <- KFS(fit$model, smoothing = c("state", "signal"))
            
            return(list(result=result, fit=fit, out_gaussian = out_gaussian)) 
            
           }




#### Make a visualization of forecast


Make_forecast <- function(Area, segment,  selected_feature ="Confidential", data = table){
  
  # Train model  
  result <- Run_SSM(Area, segment,  selected_feature, data = table)

  
  # Estimated parameters recovery
  pars_hat <- result$fit$optim.out$par
  
  sigma_mu <- exp(pars_hat[1])
  sigma_u  <- exp(pars_hat[2])
  sigma_u1 <- exp(pars_hat[3])
  sigma_eps <- exp(pars_hat[4])
  lambda <- pi*plogis(pars_hat[5])
  a_rho <-  pars_hat[6]
  rho <- 0.8 + 0.19*plogis(a_rho)
  
  
  # New value of regressors
  out <- Build_job_data(Area, segment, data = table)
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
  
  kfs <- result$out_gaussian
  
  state_names <- c("mu","u","u*")
  state_idx <- which(colnames(kfs$alphahat) %in% state_names)
  
  a1_final <- as.numeric(kfs$alphahat[nrow(kfs$alphahat), state_idx])
  a1_final <- matrix(a1_final, ncol = 1)
  P1_full  <- as.matrix(kfs$V[,, nrow(kfs$alphahat)])
  P1_final <- P1_full[state_idx, state_idx]
  
  a1_future <- a1_final                                              #result$result$Matrice_system$a1
  P1_future <- P1_final                                              #result$result$Matrice_system$P1
  P1inf_future <- result$result$Matrice_system$P1inf
  
  #--- Future spec which describe the dynamic of the future
  formula <- reformulate(c(selected_feature))
  y_future <- rep(NA, n_future)
  
  model_future <- SSModel(
    y_future ~ -1 +
      SSMcustom(
        Z = result$result$Matrice_system$Zt,
        T = T_future,
        R = R_future,
        Q = Q_future,
        a1 = a1_future,
        P1 = P1_future,
        P1inf = P1inf_future,
        state_names = c("mu","u","u*")
      ) +
      SSMregression(formula, data = new_data, Q = 0),  
    H = H_future
  )
  
  
  # Make a forecast
  
  forecast <- predict(result$fit$model, newdata = model_future, filtered = TRUE, interval = "prediction", level = 0.95)

  # Make a backcast
  
  # backcast <- predict(result$fit$model, newdata = model_back, interval = "prediction", filtered = TRUE, level = 0.95)
  
  
  
  return(list(forecast_data = forecast,  Fit_result = result))
  
  
} 




#### Backcasting : This section aim to assess the ability of our model to reproduce the past data




Make_backcast <- function(Area, segment,  selected_feature = "Confidential", data = table){


  # Train model  
  result <- Run_SSM(Area, segment,  selected_feature, data = table)
  
  
  # Estimated parameters recovery
  pars_hat <- result$fit$optim.out$par
  
  sigma_mu <- exp(pars_hat[1])
  sigma_u  <- exp(pars_hat[2])
  sigma_u1 <- exp(pars_hat[3])
  sigma_eps <- exp(pars_hat[4])
  lambda <- pi*plogis(pars_hat[5])
  a_rho <-  pars_hat[6]
  rho <- 0.8 + 0.19*plogis(a_rho)
  
  
  
  out <- Build_job_data(Area, segment, data = table)
  data <- out$data
  new_data <- data[data$YEAR>=2025,]
  old_data <- data[data$YEAR<2025,]
  y <- old_data$SALES_TMF
  
  
  
  # --- Historical series --- 
  historical_df_clean <- data.frame(
    ds    = as.Date(paste0(data$YEAR, "-01-01")),
    mean  = data$SALES_TMF,
    lower = NA,
    upper = NA
  ) %>% filter(ds <="2024-01-01")
  
  
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
  
  
  n_back <- length(data$YEAR)-n_future
  y_back <- rep(NA, n_back)
  back_data <- data[data$YEAR<2025,]
  formula <- reformulate(c(selected_feature))
  
  model_back <- SSModel(
    y_back ~ -1 +
      SSMcustom(
        Z = result$result$Matrice_system$Zt,
        T = T_future,
        R = R_future,
        Q = Q_future,
        a1 = a1_future,
        P1 = P1_future,
        P1inf = P1inf_future,
        state_names = c("mu","u","u1")
      ) +
      SSMregression(formula, data = back_data, Q = 0),  
    H = H_future
  )
  
  
  #sim_back <- simulateSSM(model_back, nsim = 5000, type = "obs", conditional = TRUE)
  backcast <- predict(result$fit$model, interval = "confidence", filtered = FALSE, level = 0.95)
  
  backcast_df <- data.frame(
    
    ds    = as.Date(paste0((data %>% filter(YEAR <2025))$YEAR, "-01-01")), 
    mean  = as.numeric(backcast[, "fit"]),
    lower = as.numeric(backcast[, "lwr"]),
    upper = as.numeric(backcast[, "upr"])
    
  ) 
  
  # Compute score 
  
  score <- calc_errors(backcast_df$mean, historical_df_clean$mean)
  
  scoreT <- data.frame(
    
    MAE = score$MAE,
    ADJ_mae = score$ADJ_mae,
    RMSE = score$RMSE,
    'R-squared' = score$`R²`
  )
  
  # --- Highcharter Graph  ---
  
  hc <- highchart() %>%
    # historical data
    hc_add_series(historical_df_clean%>% filter(ds <="2024-01-01"), "line",
                  hcaes(x = ds, y = round(exp(mean))),
                  name = "Observed", color = "#000000") %>%
    # Forecast
    hc_add_series(backcast_df%>% filter(ds <="2024-01-01"), "line",
                  hcaes(x = ds, y = round(exp(mean))),
                  name = "Backcast", color = "red") %>%
    # Confidence Interval with level=95%   
    hc_add_series(backcast_df, "arearange",
                  hcaes(x = ds, low = round(exp(lower)), high = round(exp(upper))),
                  name = "IC 95%", color = hex_to_rgba("#FFC0CB", 0.3)) %>%
    hc_title(text = "Backcast") %>%
    hc_xAxis(type = "datetime", title = list(text = "Year")) %>%
    hc_yAxis(title = list(text = "SALES"))
  
   return(list(Graph=hc, Score=scoreT))
  
}



testo$forecast_data





#### Output recovery : This section have the purpose to build a function that provide the forecast visualization and forecast table data




Forecast_output <- function(Area, segment,  selected_feature = "Confidential", data = table){
  
  
                    out <- Make_forecast(Area, segment,  selected_feature, data = table)
                    out1 <- Build_job_data(Area, segment, data = table)
                    data <- out1$data
                    
                    # --- Transforming the forecast and backcast into dataframe ---
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
                    combined_df1 <- combined_df
                    combined_df1$mean <- round(exp(combined_df1$mean))
                    combined_df1$lower <- round(exp(combined_df1$lower))
                    combined_df1$upper <- round(exp(combined_df1$upper))
                    
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
                      hc_xAxis(type = "datetime", title = list(text = "Year")) %>%
                      hc_yAxis(title = list(text = "SALES"))
                    
                    
                    
                    
                   return(list(Graph = hc,  Table = combined_df, Table1 = combined_df1))
                    
  
                   }








# Function for compute  MAE et RMSE

calc_errors <- function(y_true, y_pred) {
  
  if(length(y_true) != length(y_pred)) {
    stop("Vectors y_true and y_pred must have the same length.")
  }
  
  mae  <- mean(abs(y_true - y_pred))
  rmse <- sqrt(mean((y_true - y_pred)^2))
  mape <- mean(abs((y_true - y_pred)/y_true)) * 100
  r2 <- paste0(round((1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2))*100), '%')
  adj_mae <- paste0(round((1 - (mae / mean(abs(y_true))))*100), '%')
  
  return(list(MAE = mae, ADJ_mae = adj_mae, RMSE = rmse, 'R²'= r2))
  
}








































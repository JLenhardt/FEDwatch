#R          Total number of rounds 
#T          Total observations used in round r
#pi_star    True inflation rate objective 
#tpi        Tendency of inflation
#u_star_cb  Nairu according to central bank, assumed equal to objective
#u_star     Unemployment rate at full capacity utilization
#psi        Autocorrelation  of inflation 
#rho        Autocorrelation of unemployment series 
#gamma      Slope of unemployment on future inflation rate/inflation prediction

library(dplyr)
set.seed(260)

sim <- function(R, T, pi_star, tpi, u_star_cb, u_star, psi, rho, gamma) {
  #Initialize
   unem <- cir_unem <- pi <-cir_pi <-constant <- pi_SQ_coef <- pi_coef <- unem_coef <- pi_star_est <-
     cor_lin <- cor_SQ <- bias_unem <- constant_2 <- pi_SQ_coef_2 <- pi_coef_2 <- cir_pi_SQ_coef  <- cir_pi_coef <- unem_gap_2_coef <- cir_unem_gap_coef <- numeric()
  
  
  
  for (rd in 1:R) {
  #t1
  unem[1] = u_star + rnorm(1, mean = 0, sd = 0.25*u_star)
  cir_unem[1] = u_star + rnorm(1, mean = 0, sd = 0.25*u_star) 
  pi[1] = tpi + rnorm(1, mean = 0, sd = 1*tpi)
  cir_pi[1] = tpi + rnorm(1, mean = tpi, sd = 1*tpi)
  
  for (t in 2:T) {
    
    # Unemployment depend on trend unemployment, autocorrelation, net interest effects 
    unem[t] <-  (1-rho)*u_star + rho*unem[t - 1]  + rnorm(1, mean = 0, sd = 0.25*u_star)
    
    # cir unemployment prediction depends on trend unemployment, autocorrelation of unemployment
    cir_unem[t] = (1-rho)*u_star_cb + rho*unem[t] + rnorm(1, mean = 0, sd = 0.25*u_star) 
    
    # inflation depend on the inflation trend, persistence of inflation and previous period unemployment
    pi[t] <-  (1-psi)*tpi + psi*pi[t - 1] + gamma*(unem[t-1]-u_star) + rnorm(1, mean = 0, sd = 1*tpi)
    
    # cir inflation prediction depends on trend inflation, persistence of inflation and previous unemployment
    cir_pi[t] <-   (1-psi)*tpi + psi*pi[t] + gamma*(unem[t]-u_star_cb) + rnorm(1, mean = 0, sd = 1*tpi)
    
  }
    
    # central bank loss series
    
    L = 1*(unem-u_star_cb)^2 + 1*(pi-pi_star)^2 + 1*(cir_pi-pi_star)^2 + 1*(cir_unem-u_star_cb)^2  + rnorm(T, mean = 0, sd = 2)
    
    #Regression variables 
    pi_SQ = pi^2
    unem_gap_SQ = (unem-u_star_cb)^2
    cir_unem_gap_SQ = (cir_unem-u_star_cb)^2
    cir_pi_SQ  = cir_pi^2

    ###########################################
    reg1 = lm(L~pi_SQ + pi + unem_gap_SQ )
    reg2 = lm(L~pi_SQ + pi + cir_pi_SQ + cir_pi+ unem_gap_SQ + cir_unem_gap_SQ )
    
    #extracting estimates for round t, reg1
    constant =  rbind(as.double(coef(reg1)[1]), constant)
    pi_SQ_coef = rbind(as.double(coef(reg1)[2]), pi_SQ_coef)
    pi_coef = rbind(as.double(coef(reg1)[3]), pi_coef)
    unem_coef = rbind(as.double(coef(reg1)[4]), unem_coef)
    
    # u bias
    cor_lin =  rbind(cor( (unem-u_star_cb),(cir_pi-pi_star)), cor_lin)
    cor_SQ   =  rbind(cor( (unem-u_star_cb)^2,(cir_pi-pi_star)^2 ), cor_SQ)
    bias_unem = rbind(as.double(coef(reg1)[4]-1), bias_unem)
    
    #pi star reg 1
    pi_star_est = rbind(as.double((-coef(reg1)[3]) / (2*coef(reg1)[2])), pi_star_est)
    
    #extracting estimates for reg2
    constant_2 =  rbind(as.double(coef(reg2)[1]), constant_2)
    pi_SQ_coef_2 = rbind(as.double(coef(reg2)[2]),  pi_SQ_coef_2)
    pi_coef_2 = rbind(as.double(coef(reg2)[3]), pi_coef_2)
    cir_pi_SQ_coef = rbind(as.double(coef(reg2)[4]),cir_pi_SQ_coef)
    cir_pi_coef = rbind(as.double(coef(reg2)[5]),cir_pi_coef)
    unem_gap_2_coef = rbind(as.double(coef(reg2)[6]),unem_gap_2_coef)
    cir_unem_gap_coef = rbind(as.double(coef(reg2)[7]), cir_unem_gap_coef)
    
  }
  bias_d  = sign(bias_unem) == sign(cor_SQ) # Is the overestimation/underestimation of beta_4 in line with the correlation of the squared endogenous and the squared omitted variable ?  
  sim_df = data.frame(constant, pi_SQ_coef, pi_coef, unem_coef,pi_star_est, cor_lin, cor_SQ, bias_unem, bias_d, constant_2, pi_SQ_coef_2, pi_coef_2, cir_pi_SQ_coef, cir_pi_coef, unem_gap_2_coef, cir_unem_gap_coef,  row.names = NULL)
  return(sim_df)
  
}

# baseline scenario as suggested by Shapiro and Wilson


sim0_df <- sim(R = 2000,        #Rounds
              T = 200,          #length of Timespan
              u_star_cb = 4,    #nairu estimate of the central bank
              u_star = 4,       #true nairu
              pi_star = 2,      #central bank inflation goal
              tpi = 2,          #expected value of inflation
              rho = 0.8,        #persistence in unemployment series 
              psi = 0.4,        #persistence of inflation
              gamma = -0.4 )    #phillips curve slope

  sim0_df$scenario = "base"

  

# scenario no persistence 
  
  sim1_df <- sim(R = 2000,        #Rounds
                T = 200,          #length of Timespan
                u_star_cb = 4,    #nairu estimate of the central bank
                u_star = 4,       #true nairu
                pi_star = 2,      #central bank inflation goal
                tpi = 2,          #expected value of inflation
                rho = 0,          #persistence in unemployment series 
                psi = 0.4,        #persistence of inflation
                gamma = 0 )       #phillips curve slope  
  
  sim1_df$scenario = "no-autoco-no-pc" 

# scenario failure to reach inflation target  
   
  sim2_df <- sim(R = 2000,        #Rounds
               T = 200,           #length of Timespan
               u_star_cb = 4,     #nairu estimate of the central bank
               u_star = 4,        #true nairu
               pi_star = 2,       #central bank inflation goal
               tpi = 3,           #expected value of inflation
               rho = 0,           #persistence in unemployment series 
               psi = 0.4,         #persistence of inflation
               gamma = -0.4 )        #phillips curve slope  

  sim2_df$scenario = "inf_trgt_fail" 



sim_df_c1 = bind_rows(sim0_df, sim1_df)  

  
  library(ggplot2)
    ggplot(sim_df_c1, aes(x = unem_coef,fill=scenario))+geom_density(alpha = 0.5)+ 
    geom_vline(xintercept = 1)
    
    
sim_df_c2 = bind_rows(sim0_df, sim2_df)  
    
    library(ggplot2)
    ggplot(sim_df_c2, aes(x = pi_star_est,fill=scenario))+geom_density(alpha = 0.5)+ 
      geom_vline(xintercept = 2)
    
#remove(sim0_df, sim1_df, sim2_df)    

# save datasets 
    saveRDS(sim_df_c1, "comparison1.rds")
    saveRDS(sim_df_c2, "comparison2.rds")
  
midas_list <- function(hor){
  if (hor==0){
# U-MIDAS Monthly prob and monthly factor
    midas.Pp.Pf.fit <- midas_r(GDP.q~trend+mls(GDP.q,1,1)+
                                 mls(prob2.m,3:5,3)+
                                 mls(factor.m,3:5,3),
                               start = NULL)
    
    # Quarterly factor and monthly prob ----->
    midas.Pp.Lf.fit <- midas_r(GDP.q~trend+mls(GDP.q,1,1)+mls(prob1.m,3:5,3)+
                                 mls(prob2.m,3:5,3)
                               # +mls(factor.q,0,1)
                               +mls(factor.q,1,1),
                               start = NULL)
    # Quarterly factor and monthly prob <-----
    
    # Monthly factor and Quarterly prob
    midas.Lp.Pf.fit <- midas_r(GDP.q~trend+mls(GDP.q,1,1)+mls(factor.m,3:5,3)+
                                 # mls(prob1.q,0,1)+mls(prob2.q,0,1)+
                                 mls(prob1.q,1,1)+mls(prob2.q,1,1),
                               start = NULL)
    
    # <--------------------
    
    # Quarterly factor and quarterly prob  -------->
    midas.Lp.Lf.fit <- midas_r(GDP.q~trend+mls(GDP.q,1,1)
                               +mls(factor.q,1,1)
                               +mls(prob1.q,1,1)
                               +mls(prob2.q,1,1),
                               start = NULL)
    
    # Monthly factor
    midas.Np.Pf.fit <- midas_r(GDP.q~trend+mls(GDP.q,1,1)
                               +mls(factor.m,3:5,3),
                               start = NULL)
    
    ############################################
    #    Two Regime 
    #    Begin      
    ###########################################
    
    
    # Regime 2 U-MIDAS Monthly prob and monthly factor Regime 2
    midas.Pp.Pf.fit2 <- midas_r(GDP.q~trend+mls(GDP.q,1,1)+
                                  mls(prob.m,3:5,3)+
                                  mls(factor.m,3:5,3),
                                start = NULL)
    
    # Regime 2 Quarterly factor and monthly prob ----->
    midas.Pp.Lf.fit2 <- midas_r(GDP.q~trend+mls(GDP.q,1,1)
                                +mls(prob.m,3:5,3)
                                +mls(factor.q,1,1),
                                start = NULL)
    # Regime 2 Quarterly factor and monthly prob <-----
    
    # Regime 2 Monthly factor and Quarterly prob ----->
    midas.Lp.Pf.fit2 <- midas_r(GDP.q~trend+mls(GDP.q,1,1)+mls(factor.m,3:5,3)
                                +mls(prob.q,1,1),
                                start = NULL)
    
    # <--------------------
    
    # Regime 2 Quarterly factor and quarterly prob  -------->
    midas.Lp.Lf.fit2 <- midas_r(GDP.q~trend+mls(GDP.q,1,1)
                                +mls(factor.q,1,1)
                                +mls(prob.q,1,1),
                                start = NULL)
    
    
    ############################################
    #    Two Regime 
    #    End     
    ###########################################
    # Quarterly factor <-------
    f.fit <- midas_r(GDP.q~trend+mls(GDP.q,1,1)+mls(factor.q,1,1),
                     start = NULL)
  }
  ##### HORIZON BIGGER THAN 0 -- FORCAST-----
  if (hor>0){
    # U-MIDAS Monthly prob and monthly factor
    midas.Pp.Pf.fit <- midas_r(GDP.q~trend+mls(GDP.q,1,1)+
                                 mls(prob1.m,0:2,3)+
                                 mls(prob2.m,0:2,3)+
                                 mls(factor.m,0:2,3),
                               start = NULL)
    
    # Quarterly factor and monthly prob ----->
    midas.Pp.Lf.fit <- midas_r(GDP.q~trend+mls(GDP.q,1,1)+mls(prob1.m,0:2,3)+
                                 mls(prob2.m,0:2,3)
                               # +mls(factor.q,0,1)
                               +mls(factor.q,0,1),
                               start = NULL)
    # Quarterly factor and monthly prob <-----
    
    # Monthly factor and Quarterly prob
    midas.Lp.Pf.fit <- midas_r(GDP.q~trend+mls(GDP.q,1,1)+mls(factor.m,0:2,3)
                                 +mls(prob1.q,0,1)+mls(prob2.q,0,1),
                               start = NULL)
    
    # <--------------------
    
    # Quarterly factor and quarterly prob  -------->
    midas.Lp.Lf.fit <- midas_r(GDP.q~trend+mls(GDP.q,1,1)
                               +mls(factor.q,0,1)
                               +mls(prob1.q,0,1)
                               +mls(prob2.q,0,1),
                               start = NULL)
    
    # Monthly factor
    midas.Np.Pf.fit <- midas_r(GDP.q~trend+mls(GDP.q,1,1)
                               +mls(factor.m,0:2,3),
                               start = NULL)
    
    ############################################
    #    Two Regime 
    #    Begin      
    ###########################################
    
    
    # Regime 2 U-MIDAS Monthly prob and monthly factor Regime 2
    midas.Pp.Pf.fit2 <- midas_r(GDP.q~trend+mls(GDP.q,1,1)+
                                  mls(prob.m,0:2,3)+
                                  mls(factor.m,0:2,3),
                                start = NULL)
    
    # Regime 2 Quarterly factor and monthly prob ----->
    midas.Pp.Lf.fit2 <- midas_r(GDP.q~trend+mls(GDP.q,1,1)
                                +mls(prob.m,0:2,3)
                                +mls(factor.q,0,1),
                                start = NULL)
    # Regime 2 Quarterly factor and monthly prob <-----
    
    # Regime 2 Monthly factor and Quarterly prob ----->
    midas.Lp.Pf.fit2 <- midas_r(GDP.q~trend+mls(GDP.q,1,1)+mls(factor.m,0:2,3)
                                +mls(prob.q,0,1),
                                start = NULL)
    
    # <--------------------
    
    # Regime 2 Quarterly factor and quarterly prob  -------->
    midas.Lp.Lf.fit2 <- midas_r(GDP.q~trend+mls(GDP.q,1,1)
                                +mls(factor.q,0,1)
                                +mls(prob.q,0,1),
                                start = NULL)
    
    
    ############################################
    #    Two Regime 
    #    End     
    ###########################################
    # Quarterly factor <-------
    f.fit <- midas_r(GDP.q~trend+mls(GDP.q,1,1)+mls(factor.q,0,1),
                     start = NULL)
  }
  ### Baseline model
ar1.fit <- midas_r(GDP.q~trend+mls(GDP.q,1,1),
                   start = NULL)

ret_list <- list(midas.Pp.Pf.fit, 
                 midas.Pp.Lf.fit,
                 midas.Lp.Pf.fit,
                 midas.Lp.Lf.fit,
                 midas.Pp.Pf.fit2, 
                 midas.Pp.Lf.fit2,
                 midas.Lp.Pf.fit2,
                 midas.Lp.Lf.fit2,
                 midas.Np.Pf.fit,
                 f.fit,
                 ar1.fit)
return(ret_list)
}
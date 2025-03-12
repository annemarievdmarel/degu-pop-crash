
#####################COVAR MODELS###############################################
################################################################################
# Degu CMR COVAR models
################################################################################

#NOTE: you can only use the variables that you see in ddl (plus automaticammy created
# covars like time. Be VERY careful with the spelling or the models won't run!)
head(pradel.ddl$Phi)
head(pradel.ddl$f)
head(pradel.ddl$p)
str(pradel.ddl$Phi)

#note: rain_sum_lag2 etc are SCALED covariate values
#covars <- read.csv("data/covariates_select_scaled_v10_avg.csv") 
# to check whether all the names are the same

################################################################################
# Degu models with climatic covariates
################################################################################

degu.models.WITH.covars = function() {

  ##############################################################################
  # Top 5 models for p based on the above models
  ##############################################################################
  p.year2 = list(formula = ~year2)
  p.year2.plus.sex = list(formula = ~year2 + sex)
  p.time.plus.sex = list(formula = ~time+sex)
  p.year2.times.season2.plus.sex = list(formula =~year2 * season2 + sex)
  p.time.plus.sex = list(formula = ~time+sex)
  
  ############################# phi: models ####################################
  
  # Ignore trapping session - not a biological meaningful variables
  # See top non covar model
  #Only consider additive effect of sex?
  # Trapping session is not biological covar - exclude
  # Phi(year*season2) has too many inestimable params - exclude
  
  #Top no covar Phi models. To be used as reference
  Phi.year.plus.season.plus.sex = list(formula = ~year2+season2+sex)
  Phi.year.plus.season.times.sex = list(formula = ~year2+season2*sex)
  
  #Singular effect of covars, and interaction with season2 and year, with and without
  # additive effect of sex
  
  
  ## temp_cv2
  Phi.temp_cv2 = list(formula = ~temp_cv2)
  Phi.temp_cv2.times.season = list(formula = ~temp_cv2*season2)
  Phi.temp_cv2.plus.season = list(formula = ~temp_cv2+season2)
  Phi.temp_cv2.times.season.plus.sex = list(formula = ~temp_cv2*season2+sex)
  Phi.temp_cv2.plus.season.plus.sex = list(formula = ~temp_cv2+season2+sex)
  
  # and additive effect of year
  Phi.temp_cv2.plus.year = list(formula = ~temp_cv2+year2)
  Phi.temp_cv2.times.season.plus.year = list(formula = ~temp_cv2*season2+year2)
  Phi.temp_cv2.plus.season.plus.year = list(formula = ~temp_cv2+season2+year2)
  Phi.temp_cv2.times.season.plus.sex.plus.year = list(formula = ~temp_cv2*season2+sex+year2)
  Phi.temp_cv2.plus.season.plus.sex.plus.year = list(formula = ~temp_cv2+season2+sex+year2)
  
  
  ## temp_max2  (exclude as R>0.5, if we choose vif<3, we can include it)
  #Phi.temp_max2 = list(formula = ~temp_max2)
  #Phi.temp_max2.times.season = list(formula = ~temp_max2*season2)
  #Phi.temp_max2.plus.season = list(formula = ~temp_max2+season2)
  #Phi.temp_max2.times.season.plus.sex = list(formula = ~temp_max2*season2+sex)
  #Phi.temp_max2.plus.season.plus.sex = list(formula = ~temp_max2+season2+sex)
  
  # and additive effect of year
  #Phi.temp_max2.plus.year = list(formula = ~temp_max2+year2)
  #Phi.temp_max2.times.season.plus.year = list(formula = ~temp_max2*season2+year2)
  #Phi.temp_max2.plus.season.plus.year = list(formula = ~temp_max2+season2+year2)
  #Phi.temp_max2.times.season.plus.sex.plus.year = list(formula = ~temp_max2*season2+sex+year2)
  #Phi.temp_max2.plus.season.plus.sex.plus.year = list(formula = ~temp_max2+season2+sex+year2)
  
  ## rain_sum2
  Phi.rain_sum2 = list(formula = ~rain_sum2)
  Phi.rain_sum2.times.season = list(formula = ~rain_sum2*season2)
  Phi.rain_sum2.plus.season = list(formula = ~rain_sum2+season2)
  Phi.rain_sum2.times.season.plus.sex = list(formula = ~rain_sum2*season2+sex)
  Phi.rain_sum2.plus.season.plus.sex = list(formula = ~rain_sum2+season2+sex)
  
  # and additive effect of year
  Phi.rain_sum2.plus.year = list(formula = ~rain_sum2+year2)
  Phi.rain_sum2.times.season.plus.year = list(formula = ~rain_sum2*season2+year2)
  Phi.rain_sum2.plus.season.plus.year = list(formula = ~rain_sum2+season2+year2)
  Phi.rain_sum2.times.season.plus.sex.plus.year = list(formula = ~rain_sum2*season2+sex+year2)
  Phi.rain_sum2.plus.season.plus.sex.plus.year = list(formula = ~rain_sum2+season2+sex+year2)
  
  
  ## rain_cv2
  Phi.rain_cv2 = list(formula = ~rain_cv2)
  Phi.rain_cv2.times.season = list(formula = ~rain_cv2*season2)
  Phi.rain_cv2.plus.season = list(formula = ~rain_cv2+season2)
  Phi.rain_cv2.times.season.plus.sex = list(formula = ~rain_cv2*season2+sex)
  Phi.rain_cv2.plus.season.plus.sex = list(formula = ~rain_cv2+season2+sex)
  
  # and additive effect of year
  Phi.rain_cv2.plus.year = list(formula = ~rain_cv2+year2)
  Phi.rain_cv2.times.season.plus.year = list(formula = ~rain_cv2*season2+year2)
  Phi.rain_cv2.plus.season.plus.year = list(formula = ~rain_cv2+season2+year2)
  Phi.rain_cv2.times.season.plus.sex.plus.year = list(formula = ~rain_cv2*season2+sex+year2)
  Phi.rain_cv2.plus.season.plus.sex.plus.year = list(formula = ~rain_cv2+season2+sex+year2)
  
  ## rain_sum_lag2
  Phi.rain_sum_lag2 = list(formula = ~rain_sum_lag2)
  Phi.rain_sum_lag2.times.season = list(formula = ~rain_sum_lag2*season2)
  Phi.rain_sum_lag2.plus.season = list(formula = ~rain_sum_lag2+season2)
  Phi.rain_sum_lag2.times.season.plus.sex = list(formula = ~rain_sum_lag2*season2+sex)
  Phi.rain_sum_lag2.plus.season.plus.sex = list(formula = ~rain_sum_lag2+season2+sex)
  
  #...and additive effect of year
  Phi.rain_sum_lag2.plus.year = list(formula = ~rain_sum_lag2+year2)
  Phi.rain_sum_lag2.times.season.plus.year = list(formula = ~rain_sum_lag2*season2+year2)
  Phi.rain_sum_lag2.plus.season.plus.year = list(formula = ~rain_sum_lag2+season2+year2)
  Phi.rain_sum_lag2.times.season.plus.sex.plus.year = list(formula = ~rain_sum_lag2*season2+sex+year2)
  Phi.rain_sum_lag2.plus.season.plus.sex.plus.year = list(formula = ~rain_sum_lag2+season2+sex+year2)
  
  ## rain_cv_lag2  
  Phi.rain_cv_lag2 = list(formula = ~rain_cv_lag2)
  Phi.rain_cv_lag2.times.season = list(formula = ~rain_cv_lag2*season2)
  Phi.rain_cv_lag2.plus.season = list(formula = ~rain_cv_lag2+season2)
  Phi.rain_cv_lag2.times.season.plus.sex = list(formula = ~rain_cv_lag2*season2+sex)
  Phi.rain_cv_lag2.plus.season.plus.sex = list(formula = ~rain_cv_lag2+season2+sex)
  
  #...and additive effect of year
  Phi.rain_cv_lag2.plus.year = list(formula = ~rain_cv_lag2+year2)
  Phi.rain_cv_lag2.times.season.plus.year = list(formula = ~rain_cv_lag2*season2+year2)
  Phi.rain_cv_lag2.plus.season.plus.year = list(formula = ~rain_cv_lag2+season2+year2)
  Phi.rain_cv_lag2.times.season.plus.sex.plus.year = list(formula = ~rain_cv_lag2*season2+sex+year2)
  Phi.rain_cv_lag2.plus.season.plus.sex.plus.year = list(formula = ~rain_cv_lag2+season2+sex+year2)
  
  
  ## phi gpp_avg2 
  Phi.gpp_avg2 = list(formula = ~gpp_avg2)
  Phi.gpp_avg2.times.season = list(formula = ~gpp_avg2*season2)
  Phi.gpp_avg2.plus.season = list(formula = ~gpp_avg2+season2)
  Phi.gpp_avg2.times.season.plus.sex = list(formula = ~gpp_avg2*season2+sex)
  Phi.gpp_avg2.plus.season.plus.sex = list(formula = ~gpp_avg2+season2+sex)
  
  #...and additive effect of year
  Phi.gpp_avg2.plus.year = list(formula = ~gpp_avg2+year2)
  Phi.gpp_avg2.times.season.plus.year = list(formula = ~gpp_avg2*season2+year2)
  Phi.gpp_avg2.plus.season.plus.year = list(formula = ~gpp_avg2+season2+year2)
  Phi.gpp_avg2.times.season.plus.sex.plus.year = list(formula = ~gpp_avg2*season2+sex+year2)
  Phi.gpp_avg2.plus.season.plus.sex.plus.year = list(formula = ~gpp_avg2+season2+sex+year2)
  
  
  ## oni_avg2 
  Phi.oni_avg2 = list(formula = ~oni_avg2)
  Phi.oni_avg2.times.season = list(formula = ~oni_avg2*season2)
  Phi.oni_avg2.plus.season = list(formula = ~oni_avg2+season2)
  Phi.oni_avg2.times.season.plus.sex = list(formula = ~oni_avg2*season2+sex)
  Phi.oni_avg2.plus.season.plus.sex = list(formula = ~oni_avg2+season2+sex)
  
  #...and additive effect of year
  Phi.oni_avg2.plus.year = list(formula = ~oni_avg2+year2)
  Phi.oni_avg2.times.season.plus.year = list(formula = ~oni_avg2*season2+year2)
  Phi.oni_avg2.plus.season.plus.year = list(formula = ~oni_avg2+season2+year2)
  Phi.oni_avg2.times.season.plus.sex.plus.year = list(formula = ~oni_avg2*season2+sex+year2)
  Phi.oni_avg2.plus.season.plus.sex.plus.year = list(formula = ~oni_avg2+season2+sex+year2)
  ######################## f: models ###########################################
  
  # f = recruitment; Annual and seasonal variation has a great effect on recruitment
  #  TOP NO COVAR F MODELS
  
  #f(~year2 * season2 + sex)  -> f(~year2 + season2 * sex)  ?
  #f(~year2 * season2)
  #Only consider additive effect of sex
  ##############################################################################
  f.year.times.season = list(formula = ~year2*season2)
  f.year.times.season.plus.sex = list(formula = ~year2*season2+sex)  # should this not be ->
  f.year.plus.season.plus.sex = list(formula = ~year2+season2+sex)
  
  #Covar models (logic as in the Phi models)
  #  Hypo (among others): effect on recruitment varies between seasons, and is
  #but only additive effect of years
  
  
  ## temp_cv2  
  f.temp_cv2 = list(formula = ~temp_cv2)
  f.temp_cv2.times.season = list(formula = ~temp_cv2*season2)
  f.temp_cv2.plus.season = list(formula = ~temp_cv2+season2)
  f.temp_cv2.times.season.plus.sex = list(formula = ~temp_cv2*season2+sex)
  f.temp_cv2.plus.season.plus.sex = list(formula = ~temp_cv2+season2+sex)
  
  #...and additive effect of year
  f.temp_cv2.plus.year = list(formula = ~temp_cv2+year2)
  f.temp_cv2.times.season.plus.year = list(formula = ~temp_cv2*season2+year2)
  f.temp_cv2.plus.season.plus.year = list(formula = ~temp_cv2+season2+year2)
  f.temp_cv2.times.season.plus.sex.plus.year = list(formula = ~temp_cv2*season2+sex+year2)
  f.temp_cv2.plus.season.plus.sex.plus.year = list(formula = ~temp_cv2+season2+sex+year2)
  
  ## temp_max2
  # f.temp_max2 = list(formula = ~temp_max2)
  # f.temp_max2.times.season = list(formula = ~temp_max2*season2)
  # f.temp_max2.plus.season = list(formula = ~temp_max2+season2)
  # f.temp_max2.times.season.plus.sex = list(formula = ~temp_max2*season2+sex)
  # f.temp_max2.plus.season.plus.sex = list(formula = ~temp_max2+season2+sex)
  # 
  # #...and additive effect of year
  # f.temp_max2.plus.year = list(formula = ~temp_max2+year2)
  # f.temp_max2.times.season.plus.year = list(formula = ~temp_max2*season2+year2)
  # f.temp_max2.plus.season.plus.year = list(formula = ~temp_max2+season2+year2)
  # f.temp_max2.times.season.plus.sex.plus.year = list(formula = ~temp_max2*season2+sex+year2)
  # f.temp_max2.plus.season.plus.sex.plus.year = list(formula = ~temp_max2+season2+sex+year2)
  
  ## rain_sum2  
  f.rain_sum2 = list(formula = ~rain_sum2)
  f.rain_sum2.times.season = list(formula = ~rain_sum2*season2)
  f.rain_sum2.plus.season = list(formula = ~rain_sum2+season2)
  f.rain_sum2.times.season.plus.sex = list(formula = ~rain_sum2*season2+sex)
  f.rain_sum2.plus.season.plus.sex = list(formula = ~rain_sum2+season2+sex)
  
  #...and additive effect of year
  f.rain_sum2.plus.year = list(formula = ~rain_sum2+year2)
  f.rain_sum2.times.season.plus.year = list(formula = ~rain_sum2*season2+year2)
  f.rain_sum2.plus.season.plus.year = list(formula = ~rain_sum2+season2+year2)
  f.rain_sum2.times.season.plus.sex.plus.year = list(formula = ~rain_sum2*season2+sex+year2)
  f.rain_sum2.plus.season.plus.sex.plus.year = list(formula = ~rain_sum2+season2+sex+year2)    
  
  
  ## rain_cv2  
  f.rain_cv2 = list(formula = ~rain_cv2)
  f.rain_cv2.times.season = list(formula = ~rain_cv2*season2)
  f.rain_cv2.plus.season = list(formula = ~rain_cv2+season2)
  f.rain_cv2.times.season.plus.sex = list(formula = ~rain_cv2*season2+sex)
  f.rain_cv2.plus.season.plus.sex = list(formula = ~rain_cv2+season2+sex)
  
  #...and additive effect of year
  f.rain_cv2.plus.year = list(formula = ~rain_cv2+year2)
  f.rain_cv2.times.season.plus.year = list(formula = ~rain_cv2*season2+year2)
  f.rain_cv2.plus.season.plus.year = list(formula = ~rain_cv2+season2+year2)
  f.rain_cv2.times.season.plus.sex.plus.year = list(formula = ~rain_cv2*season2+sex+year2)
  f.rain_cv2.plus.season.plus.sex.plus.year = list(formula = ~rain_cv2+season2+sex+year2)
  
  
  ## rain_sum_lag2  
  f.rain_sum_lag2 = list(formula = ~rain_sum_lag2)
  f.rain_sum_lag2.times.season = list(formula = ~rain_sum_lag2*season2)
  f.rain_sum_lag2.plus.season = list(formula = ~rain_sum_lag2+season2)
  f.rain_sum_lag2.times.season.plus.sex = list(formula = ~rain_sum_lag2*season2+sex)
  f.rain_sum_lag2.plus.season.plus.sex = list(formula = ~rain_sum_lag2+season2+sex)
  
  #...and additive effect of year
  f.rain_sum_lag2.plus.year = list(formula = ~rain_sum_lag2+year2)
  f.rain_sum_lag2.times.season.plus.year = list(formula = ~rain_sum_lag2*season2+year2)
  f.rain_sum_lag2.plus.season.plus.year = list(formula = ~rain_sum_lag2+season2+year2)
  f.rain_sum_lag2.times.season.plus.sex.plus.year = list(formula = ~rain_sum_lag2*season2+sex+year2)
  f.rain_sum_lag2.plus.season.plus.sex.plus.year = list(formula = ~rain_sum_lag2+season2+sex+year2)
  
  
  ## rain_cv_lag2  
  f.rain_cv_lag2 = list(formula = ~rain_cv_lag2)
  f.rain_cv_lag2.times.season = list(formula = ~rain_cv_lag2*season2)
  f.rain_cv_lag2.plus.season = list(formula = ~rain_cv_lag2+season2)
  f.rain_cv_lag2.times.season.plus.sex = list(formula = ~rain_cv_lag2*season2+sex)
  f.rain_cv_lag2.plus.season.plus.sex = list(formula = ~rain_cv_lag2+season2+sex)
  
  #...and additive effect of year
  f.rain_cv_lag2.plus.year = list(formula = ~rain_cv_lag2+year2)
  f.rain_cv_lag2.times.season.plus.year = list(formula = ~rain_cv_lag2*season2+year2)
  f.rain_cv_lag2.plus.season.plus.year = list(formula = ~rain_cv_lag2+season2+year2)
  f.rain_cv_lag2.times.season.plus.sex.plus.year = list(formula = ~rain_cv_lag2*season2+sex+year2)
  f.rain_cv_lag2.plus.season.plus.sex.plus.year = list(formula = ~rain_cv_lag2+season2+sex+year2)
  
  
  ## gpp_avg2  
  f.gpp_avg2 = list(formula = ~gpp_avg2)
  f.gpp_avg2.times.season = list(formula = ~gpp_avg2*season2)
  f.gpp_avg2.plus.season = list(formula = ~gpp_avg2+season2)
  f.gpp_avg2.times.season.plus.sex = list(formula = ~gpp_avg2*season2+sex)
  f.gpp_avg2.plus.season.plus.sex = list(formula = ~gpp_avg2+season2+sex)
  
  #...and additive effect of year
  f.gpp_avg2.plus.year = list(formula = ~gpp_avg2+year2)
  f.gpp_avg2.times.season.plus.year = list(formula = ~gpp_avg2*season2+year2)
  f.gpp_avg2.plus.season.plus.year = list(formula = ~gpp_avg2+season2+year2)
  f.gpp_avg2.times.season.plus.sex.plus.year = list(formula = ~gpp_avg2*season2+sex+year2)
  f.gpp_avg2.plus.season.plus.sex.plus.year = list(formula = ~gpp_avg2+season2+sex+year2)
  
  ## oni_avg2  
  f.oni_avg2 = list(formula = ~oni_avg2)
  f.oni_avg2.times.season = list(formula = ~oni_avg2*season2)
  f.oni_avg2.plus.season = list(formula = ~oni_avg2+season2)
  f.oni_avg2.times.season.plus.sex = list(formula = ~oni_avg2*season2+sex)
  f.oni_avg2.plus.season.plus.sex = list(formula = ~oni_avg2+season2+sex)
  
  #...and additive effect of year
  f.oni_avg2.plus.year = list(formula = ~oni_avg2+year2)
  f.oni_avg2.times.season.plus.year = list(formula = ~oni_avg2*season2+year2)
  f.oni_avg2.plus.season.plus.year = list(formula = ~oni_avg2+season2+year2)
  f.oni_avg2.times.season.plus.sex.plus.year = list(formula = ~oni_avg2*season2+sex+year2)
  f.oni_avg2.plus.season.plus.sex.plus.year = list(formula = ~oni_avg2+season2+sex+year2)
  ########################### create model list ##################################  
  # Create model list
  degu.cmr.cml = create.model.list("Pradrec")
  
  model.list = mark.wrapper(
    degu.cmr.cml,
    data = pradel.process,
    ddl = pradel.ddl,
    #output = F,
    silent = T,
    #initial = pradel.test1,
    #delete = TRUE,
    realvcv = FALSE
  )
  
  return(model.list)
  
}

##################### run analysis #############################################
#Run the analysis, and connect/display results

degu.results_COVAR = degu.models.WITH.covars()

end.time = Sys.time()
time.taken2 = end.time-begin.time
time.taken2

degu.results_COVAR

save(degu.results_COVAR, file = "degu_results_COVAR.RData")

write.csv(
  degu.results_COVAR$model.table,
  "degu_model_table_with_covars_20240321.csv",
  row.names = TRUE
)

xx = as.numeric(rownames(degu.results_COVAR$model.table))[1] #Find the top model

degu.model.table_COVAR = degu.results_COVAR$model.table
round(degu.results_COVAR[[xx]]$results$real[1:4], 3)
degu.results_COVAR[[xx]]$results$beta
degu.results_COVAR[[xx]]$results$AICc

degu.model.table_COVAR

#Second model
mm = as.numeric(rownames(degu.results_COVAR$model.table))[2] 
round(degu.results_COVAR[[mm]]$results$real[1:4], 3)
degu.results_COVAR[[mm]]$results$beta

degu.results_COVAR

###############################################################################
#*****************************************************************************
#*
#*

############################################################
# Covariate models but NO YEAR INCLUDED for f and Phi
#ONLY additive effect of sex
###################################################################################
degu.models.WITH.covars.NO_YR = function() {
  
  ##############################################################################
  # Top 5 models for p based on the above models
  ##############################################################################
  p.year2 = list(formula = ~year2)
  #p.year2.plus.sex = list(formula = ~year2 + sex)
  p.time = list(formula = ~time)
  p.time.plus.sex = list(formula = ~time+sex)
  #p.year2.times.season2.plus.sex = list(formula =~year2 * season2 + sex)
  #p.time.plus.sex = list(formula = ~time+sex)
  
  ############################# phi: models ####################################
  
  # Ignore trapping session - not a biological meaningful variables
  # See top non covar model
  #Only consider additive effect of sex?
  # Trapping session is not biological covar - exclude
  # Phi(year*season2) has too many inestimable params - exclude
  
  #Top no covar Phi models. To be used as reference
  #Phi.year.plus.season.plus.sex = list(formula = ~year2+season2+sex)
  #Phi.year.plus.season.times.sex = list(formula = ~year2+season2*sex)
  
  #Singular effect of covars, and interaction with season2 and year, with and without
  # additive effect of sex
  
  
  ## temp_cv2
  Phi.temp_cv2 = list(formula = ~temp_cv2)
  Phi.temp_cv2.times.season = list(formula = ~temp_cv2*season2)
  Phi.temp_cv2.plus.season = list(formula = ~temp_cv2+season2)
  Phi.temp_cv2.times.season.plus.sex = list(formula = ~temp_cv2*season2+sex)
  Phi.temp_cv2.plus.season.plus.sex = list(formula = ~temp_cv2+season2+sex)
  
  # and additive effect of year
  # Phi.temp_cv2.plus.year = list(formula = ~temp_cv2+year2)
  # Phi.temp_cv2.times.season.plus.year = list(formula = ~temp_cv2*season2+year2)
  # Phi.temp_cv2.plus.season.plus.year = list(formula = ~temp_cv2+season2+year2)
  # Phi.temp_cv2.times.season.plus.sex.plus.year = list(formula = ~temp_cv2*season2+sex+year2)
  # Phi.temp_cv2.plus.season.plus.sex.plus.year = list(formula = ~temp_cv2+season2+sex+year2)
  
  
  ## temp_max2  (exclude as R>0.5, if we choose vif<3, we can include it)
  #Phi.temp_max2 = list(formula = ~temp_max2)
  #Phi.temp_max2.times.season = list(formula = ~temp_max2*season2)
  #Phi.temp_max2.plus.season = list(formula = ~temp_max2+season2)
  #Phi.temp_max2.times.season.plus.sex = list(formula = ~temp_max2*season2+sex)
  #Phi.temp_max2.plus.season.plus.sex = list(formula = ~temp_max2+season2+sex)
  
  # and additive effect of year
  #Phi.temp_max2.plus.year = list(formula = ~temp_max2+year2)
  #Phi.temp_max2.times.season.plus.year = list(formula = ~temp_max2*season2+year2)
  #Phi.temp_max2.plus.season.plus.year = list(formula = ~temp_max2+season2+year2)
  #Phi.temp_max2.times.season.plus.sex.plus.year = list(formula = ~temp_max2*season2+sex+year2)
  #Phi.temp_max2.plus.season.plus.sex.plus.year = list(formula = ~temp_max2+season2+sex+year2)
  
  ## rain_sum2
  Phi.rain_sum2 = list(formula = ~rain_sum2)
  Phi.rain_sum2.times.season = list(formula = ~rain_sum2*season2)
  Phi.rain_sum2.plus.season = list(formula = ~rain_sum2+season2)
  Phi.rain_sum2.times.season.plus.sex = list(formula = ~rain_sum2*season2+sex)
  Phi.rain_sum2.plus.season.plus.sex = list(formula = ~rain_sum2+season2+sex)
  
  # and additive effect of year
  # Phi.rain_sum2.plus.year = list(formula = ~rain_sum2+year2)
  # Phi.rain_sum2.times.season.plus.year = list(formula = ~rain_sum2*season2+year2)
  # Phi.rain_sum2.plus.season.plus.year = list(formula = ~rain_sum2+season2+year2)
  # Phi.rain_sum2.times.season.plus.sex.plus.year = list(formula = ~rain_sum2*season2+sex+year2)
  # Phi.rain_sum2.plus.season.plus.sex.plus.year = list(formula = ~rain_sum2+season2+sex+year2)
  # 
  
  ## rain_cv2
  Phi.rain_cv2 = list(formula = ~rain_cv2)
  Phi.rain_cv2.times.season = list(formula = ~rain_cv2*season2)
  Phi.rain_cv2.plus.season = list(formula = ~rain_cv2+season2)
  Phi.rain_cv2.times.season.plus.sex = list(formula = ~rain_cv2*season2+sex)
  Phi.rain_cv2.plus.season.plus.sex = list(formula = ~rain_cv2+season2+sex)
  
  # and additive effect of year
  # Phi.rain_cv2.plus.year = list(formula = ~rain_cv2+year2)
  # Phi.rain_cv2.times.season.plus.year = list(formula = ~rain_cv2*season2+year2)
  # Phi.rain_cv2.plus.season.plus.year = list(formula = ~rain_cv2+season2+year2)
  # Phi.rain_cv2.times.season.plus.sex.plus.year = list(formula = ~rain_cv2*season2+sex+year2)
  # Phi.rain_cv2.plus.season.plus.sex.plus.year = list(formula = ~rain_cv2+season2+sex+year2)
  # 
  ## rain_sum_lag2
  Phi.rain_sum_lag2 = list(formula = ~rain_sum_lag2)
  Phi.rain_sum_lag2.times.season = list(formula = ~rain_sum_lag2*season2)
  Phi.rain_sum_lag2.plus.season = list(formula = ~rain_sum_lag2+season2)
  Phi.rain_sum_lag2.times.season.plus.sex = list(formula = ~rain_sum_lag2*season2+sex)
  Phi.rain_sum_lag2.plus.season.plus.sex = list(formula = ~rain_sum_lag2+season2+sex)
  
  #...and additive effect of year
  # Phi.rain_sum_lag2.plus.year = list(formula = ~rain_sum_lag2+year2)
  # Phi.rain_sum_lag2.times.season.plus.year = list(formula = ~rain_sum_lag2*season2+year2)
  # Phi.rain_sum_lag2.plus.season.plus.year = list(formula = ~rain_sum_lag2+season2+year2)
  # Phi.rain_sum_lag2.times.season.plus.sex.plus.year = list(formula = ~rain_sum_lag2*season2+sex+year2)
  # Phi.rain_sum_lag2.plus.season.plus.sex.plus.year = list(formula = ~rain_sum_lag2+season2+sex+year2)
  # 
  ## rain_cv_lag2  
  Phi.rain_cv_lag2 = list(formula = ~rain_cv_lag2)
  Phi.rain_cv_lag2.times.season = list(formula = ~rain_cv_lag2*season2)
  Phi.rain_cv_lag2.plus.season = list(formula = ~rain_cv_lag2+season2)
  Phi.rain_cv_lag2.times.season.plus.sex = list(formula = ~rain_cv_lag2*season2+sex)
  Phi.rain_cv_lag2.plus.season.plus.sex = list(formula = ~rain_cv_lag2+season2+sex)
  
  #...and additive effect of year
  # Phi.rain_cv_lag2.plus.year = list(formula = ~rain_cv_lag2+year2)
  # Phi.rain_cv_lag2.times.season.plus.year = list(formula = ~rain_cv_lag2*season2+year2)
  # Phi.rain_cv_lag2.plus.season.plus.year = list(formula = ~rain_cv_lag2+season2+year2)
  # Phi.rain_cv_lag2.times.season.plus.sex.plus.year = list(formula = ~rain_cv_lag2*season2+sex+year2)
  # Phi.rain_cv_lag2.plus.season.plus.sex.plus.year = list(formula = ~rain_cv_lag2+season2+sex+year2)
  # 
  
  ## phi gpp_avg2 
  Phi.gpp_avg2 = list(formula = ~gpp_avg2)
  Phi.gpp_avg2.times.season = list(formula = ~gpp_avg2*season2)
  Phi.gpp_avg2.plus.season = list(formula = ~gpp_avg2+season2)
  Phi.gpp_avg2.times.season.plus.sex = list(formula = ~gpp_avg2*season2+sex)
  Phi.gpp_avg2.plus.season.plus.sex = list(formula = ~gpp_avg2+season2+sex)
  
  #...and additive effect of year
  # Phi.gpp_avg2.plus.year = list(formula = ~gpp_avg2+year2)
  # Phi.gpp_avg2.times.season.plus.year = list(formula = ~gpp_avg2*season2+year2)
  # Phi.gpp_avg2.plus.season.plus.year = list(formula = ~gpp_avg2+season2+year2)
  # Phi.gpp_avg2.times.season.plus.sex.plus.year = list(formula = ~gpp_avg2*season2+sex+year2)
  # Phi.gpp_avg2.plus.season.plus.sex.plus.year = list(formula = ~gpp_avg2+season2+sex+year2)
  # 
  
  ## oni_avg2 
  Phi.oni_avg2 = list(formula = ~oni_avg2)
  Phi.oni_avg2.times.season = list(formula = ~oni_avg2*season2)
  Phi.oni_avg2.plus.season = list(formula = ~oni_avg2+season2)
  Phi.oni_avg2.times.season.plus.sex = list(formula = ~oni_avg2*season2+sex)
  Phi.oni_avg2.plus.season.plus.sex = list(formula = ~oni_avg2+season2+sex)
  
  #...and additive effect of year
  # Phi.oni_avg2.plus.year = list(formula = ~oni_avg2+year2)
  # Phi.oni_avg2.times.season.plus.year = list(formula = ~oni_avg2*season2+year2)
  # Phi.oni_avg2.plus.season.plus.year = list(formula = ~oni_avg2+season2+year2)
  # Phi.oni_avg2.times.season.plus.sex.plus.year = list(formula = ~oni_avg2*season2+sex+year2)
  # Phi.oni_avg2.plus.season.plus.sex.plus.year = list(formula = ~oni_avg2+season2+sex+year2)
  ######################## f: models ###########################################
  
  # f = recruitment; Annual and seasonal variation has a great effect on recruitment
  #  TOP NO COVAR F MODELS
  
  #f(~year2 * season2 + sex)  -> f(~year2 + season2 * sex)  ?
  #f(~year2 * season2)
  #Only consider additive effect of sex
  ##############################################################################
  #f.year.times.season = list(formula = ~year2*season2)
  # f.year.times.season.plus.sex = list(formula = ~year2*season2+sex)  # should this not be ->
  # f.year.plus.season.plus.sex = list(formula = ~year2+season2+sex)
  
  #Covar models (logic as in the Phi models)
  #  Hypo (among others): effect on recruitment varies between seasons, and is
  #but only additive effect of years
  
  
  ## temp_cv2  
  f.temp_cv2 = list(formula = ~temp_cv2)
  f.temp_cv2.times.season = list(formula = ~temp_cv2*season2)
  f.temp_cv2.plus.season = list(formula = ~temp_cv2+season2)
  #f.temp_cv2.times.season.plus.sex = list(formula = ~temp_cv2*season2+sex)
  #f.temp_cv2.plus.season.plus.sex = list(formula = ~temp_cv2+season2+sex)
  
  #...and additive effect of year
  # f.temp_cv2.plus.year = list(formula = ~temp_cv2+year2)
  # f.temp_cv2.times.season.plus.year = list(formula = ~temp_cv2*season2+year2)
  # f.temp_cv2.plus.season.plus.year = list(formula = ~temp_cv2+season2+year2)
  # f.temp_cv2.times.season.plus.sex.plus.year = list(formula = ~temp_cv2*season2+sex+year2)
  # f.temp_cv2.plus.season.plus.sex.plus.year = list(formula = ~temp_cv2+season2+sex+year2)
  # 
  ## temp_max2
  # f.temp_max2 = list(formula = ~temp_max2)
  # f.temp_max2.times.season = list(formula = ~temp_max2*season2)
  # f.temp_max2.plus.season = list(formula = ~temp_max2+season2)
  # f.temp_max2.times.season.plus.sex = list(formula = ~temp_max2*season2+sex)
  # f.temp_max2.plus.season.plus.sex = list(formula = ~temp_max2+season2+sex)
  # 
  # #...and additive effect of year
  # f.temp_max2.plus.year = list(formula = ~temp_max2+year2)
  # f.temp_max2.times.season.plus.year = list(formula = ~temp_max2*season2+year2)
  # f.temp_max2.plus.season.plus.year = list(formula = ~temp_max2+season2+year2)
  # f.temp_max2.times.season.plus.sex.plus.year = list(formula = ~temp_max2*season2+sex+year2)
  # f.temp_max2.plus.season.plus.sex.plus.year = list(formula = ~temp_max2+season2+sex+year2)
  
  ## rain_sum2  
  f.rain_sum2 = list(formula = ~rain_sum2)
  f.rain_sum2.times.season = list(formula = ~rain_sum2*season2)
  f.rain_sum2.plus.season = list(formula = ~rain_sum2+season2)
  f.rain_sum2.times.season.plus.sex = list(formula = ~rain_sum2*season2+sex)
  f.rain_sum2.plus.season.plus.sex = list(formula = ~rain_sum2+season2+sex)
  
  #...and additive effect of year
  # f.rain_sum2.plus.year = list(formula = ~rain_sum2+year2)
  # f.rain_sum2.times.season.plus.year = list(formula = ~rain_sum2*season2+year2)
  # f.rain_sum2.plus.season.plus.year = list(formula = ~rain_sum2+season2+year2)
  # f.rain_sum2.times.season.plus.sex.plus.year = list(formula = ~rain_sum2*season2+sex+year2)
  # f.rain_sum2.plus.season.plus.sex.plus.year = list(formula = ~rain_sum2+season2+sex+year2)    
  # 
  
  ## rain_cv2  
  f.rain_cv2 = list(formula = ~rain_cv2)
  f.rain_cv2.times.season = list(formula = ~rain_cv2*season2)
  f.rain_cv2.plus.season = list(formula = ~rain_cv2+season2)
  f.rain_cv2.times.season.plus.sex = list(formula = ~rain_cv2*season2+sex)
  f.rain_cv2.plus.season.plus.sex = list(formula = ~rain_cv2+season2+sex)
  
  #...and additive effect of year
  # f.rain_cv2.plus.year = list(formula = ~rain_cv2+year2)
  # f.rain_cv2.times.season.plus.year = list(formula = ~rain_cv2*season2+year2)
  # f.rain_cv2.plus.season.plus.year = list(formula = ~rain_cv2+season2+year2)
  # f.rain_cv2.times.season.plus.sex.plus.year = list(formula = ~rain_cv2*season2+sex+year2)
  # f.rain_cv2.plus.season.plus.sex.plus.year = list(formula = ~rain_cv2+season2+sex+year2)
  # 
  
  ## rain_sum_lag2  
  f.rain_sum_lag2 = list(formula = ~rain_sum_lag2)
  f.rain_sum_lag2.times.season = list(formula = ~rain_sum_lag2*season2)
  f.rain_sum_lag2.plus.season = list(formula = ~rain_sum_lag2+season2)
  f.rain_sum_lag2.times.season.plus.sex = list(formula = ~rain_sum_lag2*season2+sex)
  f.rain_sum_lag2.plus.season.plus.sex = list(formula = ~rain_sum_lag2+season2+sex)
  
  #...and additive effect of year
  # f.rain_sum_lag2.plus.year = list(formula = ~rain_sum_lag2+year2)
  # f.rain_sum_lag2.times.season.plus.year = list(formula = ~rain_sum_lag2*season2+year2)
  # f.rain_sum_lag2.plus.season.plus.year = list(formula = ~rain_sum_lag2+season2+year2)
  # f.rain_sum_lag2.times.season.plus.sex.plus.year = list(formula = ~rain_sum_lag2*season2+sex+year2)
  # f.rain_sum_lag2.plus.season.plus.sex.plus.year = list(formula = ~rain_sum_lag2+season2+sex+year2)
  # 
  
  ## rain_cv_lag2  
  f.rain_cv_lag2 = list(formula = ~rain_cv_lag2)
  f.rain_cv_lag2.times.season = list(formula = ~rain_cv_lag2*season2)
  f.rain_cv_lag2.plus.season = list(formula = ~rain_cv_lag2+season2)
  f.rain_cv_lag2.times.season.plus.sex = list(formula = ~rain_cv_lag2*season2+sex)
  f.rain_cv_lag2.plus.season.plus.sex = list(formula = ~rain_cv_lag2+season2+sex)
  
  #...and additive effect of year
  # f.rain_cv_lag2.plus.year = list(formula = ~rain_cv_lag2+year2)
  # f.rain_cv_lag2.times.season.plus.year = list(formula = ~rain_cv_lag2*season2+year2)
  # f.rain_cv_lag2.plus.season.plus.year = list(formula = ~rain_cv_lag2+season2+year2)
  # f.rain_cv_lag2.times.season.plus.sex.plus.year = list(formula = ~rain_cv_lag2*season2+sex+year2)
  # f.rain_cv_lag2.plus.season.plus.sex.plus.year = list(formula = ~rain_cv_lag2+season2+sex+year2)
  # 
  # 
  ## gpp_avg2  
  f.gpp_avg2 = list(formula = ~gpp_avg2)
  f.gpp_avg2.times.season = list(formula = ~gpp_avg2*season2)
  f.gpp_avg2.plus.season = list(formula = ~gpp_avg2+season2)
  f.gpp_avg2.times.season.plus.sex = list(formula = ~gpp_avg2*season2+sex)
  f.gpp_avg2.plus.season.plus.sex = list(formula = ~gpp_avg2+season2+sex)
  
  #...and additive effect of year
  # f.gpp_avg2.plus.year = list(formula = ~gpp_avg2+year2)
  # f.gpp_avg2.times.season.plus.year = list(formula = ~gpp_avg2*season2+year2)
  # f.gpp_avg2.plus.season.plus.year = list(formula = ~gpp_avg2+season2+year2)
  # f.gpp_avg2.times.season.plus.sex.plus.year = list(formula = ~gpp_avg2*season2+sex+year2)
  # f.gpp_avg2.plus.season.plus.sex.plus.year = list(formula = ~gpp_avg2+season2+sex+year2)
  # 
  ## oni_avg2  
  f.oni_avg2 = list(formula = ~oni_avg2)
  f.oni_avg2.times.season = list(formula = ~oni_avg2*season2)
  f.oni_avg2.plus.season = list(formula = ~oni_avg2+season2)
  f.oni_avg2.times.season.plus.sex = list(formula = ~oni_avg2*season2+sex)
  f.oni_avg2.plus.season.plus.sex = list(formula = ~oni_avg2+season2+sex)
  
  #...and additive effect of year
  # f.oni_avg2.plus.year = list(formula = ~oni_avg2+year2)
  # f.oni_avg2.times.season.plus.year = list(formula = ~oni_avg2*season2+year2)
  # f.oni_avg2.plus.season.plus.year = list(formula = ~oni_avg2+season2+year2)
  # f.oni_avg2.times.season.plus.sex.plus.year = list(formula = ~oni_avg2*season2+sex+year2)
  # f.oni_avg2.plus.season.plus.sex.plus.year = list(formula = ~oni_avg2+season2+sex+year2)
  ########################### create model list ##################################  
  # Create model list
  degu.cmr.cml = create.model.list("Pradrec")
  
  model.list = mark.wrapper(
    degu.cmr.cml,
    data = pradel.process,
    ddl = pradel.ddl,
    #output = F,
    silent = T,
    #initial = pradel.test1,
    #delete = TRUE,
    realvcv = FALSE
  )
  
  return(model.list)
  
}

##################### run analysis #############################################
#Run the analysis, and connect/display results

degu.results_COVAR.NO_YR.NO_YR = degu.models.WITH.covars.NO_YR()

end.time = Sys.time()
time.taken2 = end.time-begin.time
time.taken2

degu.results_COVAR.NO_YR

save(degu.results_COVAR.NO_YR.NO_YR, file = "degu_new_covar_model_results_20241209.RData")

write.csv(
  degu.results_COVAR.NO_YR.NO_YR$model.table,
  "degu_model_table_with_covars_NO_YR20241208.csv",
  row.names = TRUE
)

degu.results_COVAR.NO_YR.NO_YR$model.table

zz = as.numeric(rownames(degu.results_COVAR.NO_YR.NO_YR$model.table))[1] #Find the top model

degu.results_COVAR.NO_YR.NO_YR[[zz]]$results$beta
degu.results_COVAR.NO_YR.NO_YR[[zz]]$results$real

degu.results_COVAR.NO_YR.NO_YR[[648]]$results$beta
degu.results_COVAR.NO_YR.NO_YR[[648]]$results$real



#save.image("degu_new_covar_model_results_20241209.RData")


##
cleanup(ask = F)

save.image("degu_covar_models_20250222.RData")

#rm(list=ls())
# files = list.files(file.names = T, pattern = "tmp")
# files = list.files(file.names = T, pattern = "inp")
# files = list.files(file.names = T, pattern = "vcv")
#files = list.files(file.names = T, pattern = "res")
#files = list.files(file.names = T, pattern = "out")
file.remove(files)


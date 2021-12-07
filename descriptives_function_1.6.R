
library('data.table')      # data manipulation
library('dplyr')           # data manipulation

library('ggplot2')         # visualization
library('shiny')           # interactive visualization
library('DT')              # interactive tables

library("broom.mixed")     # data wrangling
library("lme4")            # estimating random effects

library('parallel')        # set the number of usable cores
library('future.apply')    # parallelize apply() functions

# ensure this script returns the same results on each run
set.seed(42) #the answer to life, the universe, and everything

#compute number of cores in cluster for parallelization
workers <- detectCores()-1
future::plan(multisession, workers = workers)


### function to create a named list of data frames
makeNamedList <- function(...){
  structure(list(...), names = as.list(substitute(list(...)))[-1L])
}


### run a null model for each dependent variable
run_mods_f <- function(df,
  
  #each value becomes a dependent variable in the model                           
  DVs = c("ACHIEVEMENT", "BENEVOLENCE", "CONFORMITY", "HEDONISM", 
          "POWER", "SECURITY", "SELF", "STIMULATION", 
          "TRADITION",  "UNIVERSALISM")
  ){
  
  
  #run the same model on each dependent variable
  results <- future_lapply(DVs, function(dv, data=df){
      
    #run model to estimate intercepts
    mod <- lmer(get(dv) ~ (1|participant_ID) + (1|item_ID), data=df) 
      
      })
}
# mods <- run_mods_f(responses_dt)



### extract random intercepts from each model
get_random_intercepts_dt <- function(
  
  #list of models
  mods,
  
  #each value becomes a dependent variable in the model                           
  DVs = c("ACHIEVEMENT", "BENEVOLENCE", "CONFORMITY", "HEDONISM", 
          "POWER", "SECURITY", "SELF", "STIMULATION", 
          "TRADITION",  "UNIVERSALISM")
  ){
  
  mods <- setNames(mods, DVs)
  
  random_intercepts <- lapply(mods, function(mod){
      
      # coax into a tibble, and then a data.table
      mod <- mod  %>% tidy() %>% setDT() 
        
      #pivot to wide so columns are the DVs from above
      mod <- dcast(mod, .~ group, value.var='estimate') 
        
      #pick columns to keep, add a column to indicate the dv
      mod <- mod[,.(Residual, item_ID, participant_ID)]
  }
    )
  
}
# random_intercepts <- get_random_intercepts_dt(mods)


### extract random confidence intervals
get_random_confints_dt <- function(mods,
  
  #each value becomes a dependent variable in the model                           
  DVs = c("ACHIEVEMENT", "BENEVOLENCE", "CONFORMITY", "HEDONISM", 
          "POWER", "SECURITY", "SELF", "STIMULATION", 
          "TRADITION",  "UNIVERSALISM")
  ){

   
   mod_confints <- future_lapply(mods, function(x){
     as.data.frame(confint(x, oldNames=FALSE))
})
      
   mod_confint_dt <- lapply(mod_confints, function(x){

     #coax to data table, and select specific cells
     setDT(x, keep.rownames= "estimate")[1:2, 1:3] %>%

     #convert to wide format
     dcast(., ...~estimate, value.var=c("2.5 %", "97.5 %")) %>%

     #delete useless column
     .[, .:= NULL] %>%

     #rename columns
     setnames(., 
       old = c(
        "2.5 %_sd_(Intercept)|item_ID", 
        "2.5 %_sd_(Intercept)|participant_ID", 
        "97.5 %_sd_(Intercept)|item_ID", 
        "97.5 %_sd_(Intercept)|participant_ID"), 
       new = c(
         "item_ID_lower", 
         "participant_ID_lower", 
         "item_ID_higher", 
         "participant_ID_higher"))
     }) 
      
     #add names to list representing each dependent variable 
     mod_confint_dt <- setNames(mod_confint_dt, DVs)
} 
# random_confints <- get_random_confints_dt(mods)


### gather random effects for each model into a single data frame
list_to_dt <- function(dt_list){
  
  #create empty dataframe
  colnames <- names(dt_list)
  
  #convert list to data table
  results_dt <-rbindlist(dt_list)
  
  #add column representing dependent variable
  results_dt[, model := colnames]
}
#t <- list_to_dt(random_confints)
#s <- list_to_dt(random_intercepts)


###put previous functions together
assemble_random_effects_dt <- function(dt){

  #run models
  mods               <- run_mods_f(dt)
    
  #get a list of one-row vectors for random intercept and residual
  list_of_intercepts <- get_random_intercepts_dt(mods)
  
  #get a list of confidence intervals for random intercepts
  list_of_confints   <- get_random_confints_dt(mods)
  
  #create data table from list of random intercepts
  intercepts_dt      <- list_to_dt(list_of_intercepts)
  
  #create data table for confidence intervals
  confints_dt        <- list_to_dt(list_of_confints)
  
  #merge the two data tables, using the DV as the key
  results_dt         <- intercepts_dt[confints_dt, on=.(model)]
  
  #order columns
  setcolorder(results_dt, c("model", "item_ID", "item_ID_lower", "item_ID_higher", "participant_ID", "participant_ID_lower", "participant_ID_higher", "Residual"))
} 

#random_effects_dt <- assemble_random_effects_dt(responses_dt)

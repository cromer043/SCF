#21/12/23
#Carl Romer
#Data: SCF
#https://www.federalreserve.gov/econres/scfindex.htm
#This code comes from
#https://guilhermejacob.github.io/context/1.6-survey-of-consumer-finances-scf.html#survey-of-consumer-finances-scf
#For users who want to estimate only simple statistics such as sums, means and medians ignoring the effects of imputation error on the standard errors of these estimates, it will probably be sufficient to divide the weights by 5.  Software to compute means and medians and their associated standard errors with respect to imputation and sampling error is provided in the section on sampling error later in this codebook.

#Setup
###########################################
#Setup
###########################################
#Setup

library(tidyverse)
library(haven)
library(stringr)
library(survey)
library(mitools)
library(convey)

setwd("C:/Users/csromer/OneDrive - National Bankers Association/Blogs/2024/Survey of Consumer Finances")


#########################################
#Functions
#####################################

build_table <- function(overall_table,
                        table_by_income,
                        table_by_race,
                        table_by_racecat){
  output_table <- tibble( 
    year = c(
      2022,2022,2022,2022,2022,2022,
      2022,2022,2022,2022,2022,2022,
      2022,2022,2022,2022,2022,2022,
      2022,2022,2022,2022,2022,2022,
      2022,2022,2022,2022,2022,2022,
      2022,2022,2022,2022,2022,2022
    ),
    race = c(
      "Overall",
      "Overall",
      "Overall",
      "Overall",
      "Overall",
      "Overall",
      "White",
      "White",
      "White",
      "White",
      "White",
      "White",
      "Black",
      "Black",
      "Black",
      "Black",
      "Black",
      "Black",
      "Hispanic",
      "Hispanic",
      "Hispanic",
      "Hispanic",
      "Hispanic",
      "Hispanic",
      "Asian",
      "Asian",
      "Asian",
      "Asian",
      "Asian",
      "Asian",
      "Other",
      "Other",
      "Other",
      "Other",
      "Other",
      "Other"
    ),
    income = c(
      "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
      "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
      "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
      "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
      "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
      "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
    ),
    percent = c(
      #overall
      as.numeric(overall_table[1]),
      table_by_income[[1]][[1]], #by income
      table_by_income[[1]][[2]],
      table_by_income[[1]][[3]],
      table_by_income[[1]][[4]],
      table_by_income[[1]][[5]],
      
      #white
      table_by_race[[1]][[1]],
      table_by_racecat[[1]][[1]],
      table_by_racecat[[1]][[2]],
      table_by_racecat[[1]][[3]],
      table_by_racecat[[1]][[4]],
      table_by_racecat[[1]][[5]],
      
      #Black
      table_by_race[[1]][[2]],
      table_by_racecat[[1]][[6]],
      table_by_racecat[[1]][[7]],
      table_by_racecat[[1]][[8]],
      table_by_racecat[[1]][[9]],
      table_by_racecat[[1]][[10]],
      
      #Hispanic
      table_by_race[[1]][[3]],
      table_by_racecat[[1]][[11]],
      table_by_racecat[[1]][[12]],
      table_by_racecat[[1]][[13]],
      table_by_racecat[[1]][[14]],
      table_by_racecat[[1]][[15]],
      
      #Asian
      table_by_race[[1]][[4]],
      NA,
      NA,
      NA,
      NA,
      NA,
      
      #Other
      table_by_race[[1]][[5]],
      table_by_racecat[[1]][[16]],
      table_by_racecat[[1]][[17]],
      table_by_racecat[[1]][[18]],
      table_by_racecat[[1]][[19]],
      table_by_racecat[[1]][[20]]
    ),
    
    low = c(
      confint(homeowners_percent)[1], #Overall
      confint(table_by_income)[1], #By Income
      confint(table_by_income)[2],
      confint(table_by_income)[3],
      confint(table_by_income)[4],
      confint(table_by_income)[5],
      
      confint(table_by_race)[1], #White
      confint(table_by_racecat)[1],
      confint(table_by_racecat)[2],
      confint(table_by_racecat)[3],
      confint(table_by_racecat)[4],
      confint(table_by_racecat)[5],
      
      confint(table_by_race)[2], #Black
      confint(table_by_racecat)[6],
      confint(table_by_racecat)[7],
      confint(table_by_racecat)[8],
      confint(table_by_racecat)[9],
      confint(table_by_racecat)[10],
      
      confint(table_by_race)[3], #Hispanic
      confint(table_by_racecat)[11],
      confint(table_by_racecat)[12],
      confint(table_by_racecat)[13],
      confint(table_by_racecat)[14],
      confint(table_by_racecat)[15],
      
      confint(table_by_race)[4], #Asian
      NA,
      NA,
      NA,
      NA,
      NA,
      
      confint(table_by_race)[5], #Other
      confint(table_by_racecat)[16],
      confint(table_by_racecat)[17],
      confint(table_by_racecat)[18],
      confint(table_by_racecat)[19],
      confint(table_by_racecat)[20]
      
      
    ),
    high = c(
      confint(homeowners_percent)[2], #Overall
      confint(table_by_income)[6], #By income
      confint(table_by_income)[7],
      confint(table_by_income)[8],
      confint(table_by_income)[9],
      confint(table_by_income)[10],
      
      confint(table_by_race)[6], #White
      confint(table_by_racecat)[21],
      confint(table_by_racecat)[22],
      confint(table_by_racecat)[23],
      confint(table_by_racecat)[24],
      confint(table_by_racecat)[25],
      
      confint(table_by_race)[7], #Black
      confint(table_by_racecat)[26],
      confint(table_by_racecat)[27],
      confint(table_by_racecat)[28],
      confint(table_by_racecat)[29],
      confint(table_by_racecat)[30],
      
      confint(table_by_race)[8], #Hispanic
      confint(table_by_racecat)[31],
      confint(table_by_racecat)[32],
      confint(table_by_racecat)[33],
      confint(table_by_racecat)[34],
      confint(table_by_racecat)[35],
      
      confint(table_by_race)[9], #Asian
      NA,
      NA,
      NA,
      NA,
      NA,
      
      confint(table_by_race)[10], #Other
      confint(table_by_racecat)[36],
      confint(table_by_racecat)[37],
      confint(table_by_racecat)[38],
      confint(table_by_racecat)[39],
      confint(table_by_racecat)[40]
    )
    
  )
  return(output_table)
}

#2022
###########################################
#2022
###########################################
#2022

scf_MIcombine <-
  function (results,
            variances,
            call = sys.call(),
            df.complete = Inf,
            ...) {
    m <- length(results)
    oldcall <- attr(results, "call")
    if (missing(variances)) {
      variances <- suppressWarnings(lapply(results, vcov))
      results <- lapply(results, coef)
    }
    vbar <- variances[[1]]
    cbar <- results[[1]]
    for (i in 2:m) {
      cbar <- cbar + results[[i]]
      # MODIFICATION:
      # vbar <- vbar + variances[[i]]
    }
    cbar <- cbar / m
    # MODIFICATION:
    # vbar <- vbar/m
    evar <- var(do.call("rbind", results))
    r <- (1 + 1 / m) * evar / vbar
    df <- (m - 1) * (1 + 1 / r) ^ 2
    if (is.matrix(df))
      df <- diag(df)
    if (is.finite(df.complete)) {
      dfobs <- ((df.complete + 1) / (df.complete + 3)) * df.complete *
        vbar / (vbar + evar)
      if (is.matrix(dfobs))
        dfobs <- diag(dfobs)
      df <- 1 / (1 / dfobs + 1 / df)
    }
    if (is.matrix(r))
      r <- diag(r)
    rval <- list(
      coefficients = cbar,
      variance = vbar + evar *
        (m + 1) / m,
      call = c(oldcall, call),
      nimp = m,
      df = df,
      missinfo = (r + 2 / (df + 3)) / (r + 1)
    )
    class(rval) <- "MIresult"
    rval
  }
#Define a function to download and import each stata file:
  

scf_dta_import <-
  function(this_url) {
    this_tf <- tempfile()
    
    download.file(this_url , this_tf , mode = 'wb')
    
    this_tbl <- read_dta(this_tf)
    
    this_df <- data.frame(this_tbl)
    
    file.remove(this_tf)
    
    names(this_df) <- tolower(names(this_df))
    
    this_df
  }
#Download and import the full, summary extract, and replicate weights tables for 2022:
  scf_df <-
  scf_dta_import("https://www.federalreserve.gov/econres/files/scf2022s.zip")

ext_df <-
  scf_dta_import("https://www.federalreserve.gov/econres/files/scfp2022s.zip")

scf_rw_df <-
  scf_dta_import("https://www.federalreserve.gov/econres/files/scf2022rw1s.zip")



#Confirm both the full public data and the summary extract contain five records per family:
  
  stopifnot(nrow(scf_df) == nrow(scf_rw_df) * 5)
stopifnot(nrow(scf_df) == nrow(ext_df))
#Confirm only the primary economic unit and the five implicate identifiers overlap:
  
  stopifnot(all(sort(intersect(
    names(scf_df) , names(ext_df)
  )) == c('y1' , 'yy1')))
stopifnot(all(sort(intersect(
  names(scf_df) , names(scf_rw_df)
)) == c('y1' , 'yy1')))
stopifnot(all(sort(intersect(
  names(ext_df) , names(scf_rw_df)
)) == c('y1' , 'yy1')))
#Remove the implicate identifier from the replicate weights table, add a column of fives for weighting:
scf_rw_df[, 'y1'] <- NULL

scf_df[, 'five'] <- 5



#Construct a multiply-imputed, complex sample survey design:
  
  #Break the main table into five different implicates based on the final character of the column y1:
  

s1_df <- scf_df[str_sub(scf_df[, 'y1'] ,-1 ,-1) == 1 ,]
s2_df <- scf_df[str_sub(scf_df[, 'y1'] ,-1 ,-1) == 2 ,]
s3_df <- scf_df[str_sub(scf_df[, 'y1'] ,-1 ,-1) == 3 ,]
s4_df <- scf_df[str_sub(scf_df[, 'y1'] ,-1 ,-1) == 4 ,]
s5_df <- scf_df[str_sub(scf_df[, 'y1'] ,-1 ,-1) == 5 ,]
#Combine these into a single list, then merge each implicate with the summary extract:
  
  scf_imp <- list(s1_df , s2_df , s3_df , s4_df , s5_df)

scf_list <- lapply(scf_imp , merge , ext_df)
#Replace all missing values in the replicate weights table with zeroes, multiply the replicate weights by the multiplication factor, then only keep the unique identifier and the final (combined) replicate weights:
  
  scf_rw_df[is.na(scf_rw_df)] <- 0

scf_rw_df[, paste0('wgt' , 1:999)] <-
  scf_rw_df[, paste0('wt1b' , 1:999)] * scf_rw_df[, paste0('mm' , 1:999)]

scf_rw_df <- scf_rw_df[, c('yy1' , paste0('wgt' , 1:999))]

#Sort both the five implicates and also the replicate weights table by the unique identifier:
  
  scf_list <-
  lapply(scf_list , function(w)
    w[order(w[, 'yy1']) ,])

scf_rw_df <- scf_rw_df[order(scf_rw_df[, 'yy1']) ,]
#Define the design:

scf_design <-
  svrepdesign(
    weights = ~ wgt ,
    repweights = scf_rw_df[,-1] ,
    data = imputationList(scf_list) ,
    scale = 1 ,
    rscales = rep(1 / 998 , 999) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )
#Run the convey_prep() function on the full design:
scf_design$designs <- lapply(scf_design$designs , convey_prep)

#Add new columns to the data set:
  
  scf_design <-
  update(
    scf_design ,
    
    hhsex = factor(
      hhsex ,
      levels = 1:2 ,
      labels = c("male" , "female")
    ) ,
    race = factor(
      race ,
      levels = 1:5 ,
      labels = c("white" , "black", "hispanic", "asian", "other")
    ),
    inccat = case_when(inccat == 5 |
                         inccat == 6 ~ 5,
                       T ~ inccat),
    inccat = factor(
      inccat ,
      levels = 1:5 ,
      labels = c("0-20%" , "20-40%", "40-60%", "60-80%", "80-100%")
    ),
    racecat = case_when(
                             race== "white" &
                               inccat == "0-20%" ~ 1,
                             race== "white" &
                               inccat == "20-40%" ~ 2,
                             race== "white" &
                               inccat == "40-60%" ~ 3,
                             race== "white" &
                               inccat == "60-80%" ~ 4,
                             race== "white" &
                               inccat == "80-100%" ~ 5,
                             
                             race== "black" &
                               inccat == "0-20%" ~ 6,
                             race== "black" &
                               inccat == "20-40%" ~ 7,
                             race== "black" &
                               inccat == "40-60%" ~ 8,
                             race== "black" &
                               inccat == "60-80%" ~ 9,
                             race== "black" &
                               inccat == "80-100%" ~ 10,
                             
                             race== "hispanic" &
                               inccat == "0-20%" ~ 11,
                             race== "hispanic" &
                               inccat == "20-40%" ~ 12,
                             race== "hispanic" &
                               inccat == "40-60%" ~ 13,
                             race== "hispanic" &
                               inccat == "60-80%" ~ 14,
                             race== "hispanic" &
                               inccat == "80-100%" ~ 15,
                             
                             race== "asian" &
                               inccat == "0-20%" ~ 16,
                             race== "asian" &
                               inccat == "20-40%" ~ 17,
                             race== "asian" &
                               inccat == "40-60%" ~ 18,
                             race== "asian" &
                               inccat == "60-80%" ~ 19,
                             race== "asian" &
                               inccat == "80-100%" ~ 20,
                             
                             race== "other" &
                               inccat == "0-20%" ~ 16,
                             race== "other" &
                               inccat == "20-40%" ~ 17,
                             race== "other" &
                               inccat == "40-60%" ~ 18,
                             race== "other" &
                               inccat == "60-80%" ~ 19,
                             race== "other" &
                               inccat == "80-100%" ~ 20,
                             T ~ 0),
    racecat = 
      factor(
        racecat,
        levels = 1:20 ,
        labels = c("white: 0-20%" , "white: 20-40%", "white: 40-60%", "white: 60-80%", "white: 80-100%",
                   "black: 0-20%" , "black: 20-40%", "black: 40-60%", "black: 60-80%", "black: 80-100%",
                   "hispanic: 0-20%" , "hispanic: 20-40%", "hispanic: 40-60%", "hispanic: 60-80%", "hispanic: 80-100%",
                  # "asian: 0-20%" , "asian: 20-40%", "asian: 40-60%", "asian: 60-80%", "asian: 80-100%",
                   "other: 0-20%" , "other: 20-40%", "other: 40-60%", "other: 60-80%", "other: 80-100%")
      ),
    married = as.numeric(married == 1) ,
    
    white = case_when(race == "white" ~ 1, 
                      T ~ 0),
    black = case_when(race == "black" ~ 1,
                      T ~ 0),
    hispanic = case_when(race == "hispanic" ~ 1,
                         T ~ 0),
    asian = case_when(race == "asian" ~ 1,
                      T ~ 0),
    other = case_when(race == "other" ~ 1,
                      T ~ 0),
    `0-20%` = case_when(inccat =="0-20%" ~ 1,
                        T ~ 0),
    `20-40%` = case_when(inccat =="20-40%" ~ 1,
                         T ~ 0),
    `40-60%` = case_when(inccat =="40-60%" ~ 1,
                         T ~ 0),
    `60-80%` = case_when(inccat =="60-80%" ~ 1,
                         T ~ 0),
    `80-100%` = case_when(inccat =="80-100%" ~ 1,
                         T ~ 0),
    
    `White: 0-20%` = case_when(inccat =="0-20%" & race == "white" ~ 1,
                               T ~ 0),
    `White: 20-40%` = case_when(inccat =="20-40%" & race == "white" ~ 1,
                                T ~ 0),
    `White: 40-60%` = case_when(inccat =="40-60%" & race == "white" ~ 1,
                                T ~ 0),
    `White: 60-80%` = case_when(inccat =="60-80%" & race == "white" ~ 1,
                                T ~ 0),
    `White: 80-100%` = case_when(inccat =="80-100%" & race == "white" ~ 1,
                                T ~ 0),
    
    `Black: 0-20%` = case_when(inccat =="0-20%" & race == "black" ~ 1,
                               T ~ 0),
    `Black: 20-40%` = case_when(inccat =="20-40%" & race == "black" ~ 1,
                                T ~ 0),
    `Black: 40-60%` = case_when(inccat =="40-60%" & race == "black" ~ 1,
                                T ~ 0),
    `Black: 60-80%` = case_when(inccat =="60-80%" & race == "black" ~ 1,
                                T ~ 0),
    `Black: 80-100%` = case_when(inccat =="80-100%" & race == "black" ~ 1,
                                T ~ 0),
    
    `Hispanic: 0-20%` = case_when(inccat =="0-20%" & race == "hispanic" ~ 1,
                                  T ~ 0),
    `Hispanic: 20-40%` = case_when(inccat =="20-40%" & race == "hispanic" ~ 1,
                                   T ~ 0),
    `Hispanic: 40-60%` = case_when(inccat =="40-60%" & race == "hispanic" ~ 1,
                                   T ~ 0),
    `Hispanic: 60-80%` = case_when(inccat =="60-80%" & race == "hispanic" ~ 1,
                                   T ~ 0),
    `Hispanic: 80-100%` = case_when(inccat =="80-100%" & race == "hispanic" ~ 1,
                                   T ~ 0),
    
    `Asian: 0-20%` = case_when(inccat =="0-20%" & race == "asian" ~ 1,
                               T ~ 0),
    `Asian: 20-40%` = case_when(inccat =="20-40%" & race == "asian" ~ 1,
                                T ~ 0),
    `Asian: 40-60%` = case_when(inccat =="40-60%" & race == "asian" ~ 1,
                                T ~ 0),
    `Asian: 60-80%` = case_when(inccat =="60-80%" & race == "asian" ~ 1,
                                T ~ 0),
    `Asian: 80-100%` = case_when(inccat =="80-100%" & race == "asian" ~ 1,
                                T ~ 0),
    
    `Other: 0-20%` = case_when(inccat =="0-20%" & race == "other" ~ 1,
                               T ~ 0),
    `Other: 20-40%` = case_when(inccat =="20-40%" & race == "other" ~ 1,
                                T ~ 0),
    `Other: 40-60%` = case_when(inccat =="40-60%" & race == "other" ~ 1,
                                T ~ 0),
    `Other: 60-80%` = case_when(inccat =="60-80%" & race == "other" ~ 1,
                                T ~ 0),
    `Other: 80-100%` = case_when(inccat =="80-100%" & race == "other" ~ 1,
                                T ~ 0),
    
    housecl = case_when(
      housecl == 2 ~ 0,
      T ~ housecl
      ),
     homeownerinc = case_when(
      housecl == 1 & inccat == "0-20%" ~ 1,
      housecl == 1 & inccat == "20-40%" ~ 2,
      housecl == 1 & inccat == "40-60%" ~ 3,
      housecl == 1 & inccat == "60-80%" ~ 4,
      housecl == 1 & inccat == "80-100%" ~ 5,
      
      housecl == 0 & inccat == "0-20%" ~ 6,
      housecl == 0 & inccat == "20-40%" ~ 7,
      housecl == 0 & inccat == "40-60%" ~ 8,
      housecl == 0 & inccat == "60-80%" ~ 9,
      housecl == 0 & inccat == "80-100%" ~ 10,
    ),
    homeownerinc = factor(
      homeownerinc,
      levels = 1:10,
      labels = c(
        "Homeowner: 0-20%",
        "Homeowner: 20-40%",
        "Homeowner: 40-60%",
        "Homeowner: 60-80%",
        "Homeowner: 80-100%",
        
        "Not a homeowner: 0-20%",
        "Not a homeowner: 20-40%",
        "Not a homeowner: 40-60%",
        "Not a homeowner: 60-80%",
        "Not a homeowner: 80-100%"
      )
    ),
    
    homeownerrace = case_when(
      housecl == 1 & race == "white" ~ 1,
      housecl == 1 & race == "black" ~ 2,
      housecl == 1 & race == "hispanic" ~ 3,
      housecl == 1 & race == "asian" ~ 4,
      housecl == 1 & race == "other" ~ 5,
      
      housecl == 0 & race == "white" ~ 6,
      housecl == 0 & race == "black" ~ 7,
      housecl == 0 & race == "hispanic" ~ 8,
      housecl == 0 & race == "asian" ~ 9,
      housecl == 0 & race == "other" ~ 10
    ),
    homeownerrace = factor(
      homeownerrace,
      levels = 1:10,
      labels = c(
        "Homeowner: White",
        "Homeowner: Black",
        "Homeowner: Hispanic",
        "Homeowner: Asian",
        "Homeowner: Other",

        "Not a homeowner: White",
        "Not a homeowner: Black",
        "Not a homeowner: Hispanic",
        "Not a homeowner: Asian",
        "Not a homeowner: Other"
      )
    ),
    
    
    
    homeownerracecat = case_when(
      housecl == 1 &
        race== "white" &
        inccat == "0-20%" ~ 1,
      housecl == 1 &
        race== "white" &
        inccat == "20-40%" ~ 2,
      housecl == 1 &
        race== "white" &
        inccat == "40-60%" ~ 3,
      housecl == 1 &
        race== "white" &
        inccat == "60-80%" ~ 4,
      housecl == 1 &
        race== "white" &
        inccat == "80-100%" ~ 5,
      
      housecl == 1 &
        race== "black" &
        inccat == "0-20%" ~ 6,
      housecl == 1 &
        race== "black" &
        inccat == "20-40%" ~ 7,
      housecl == 1 &
        race== "black" &
        inccat == "40-60%" ~ 8,
      housecl == 1 &
        race== "black" &
        inccat == "60-80%" ~ 9,
      housecl == 1 &
        race== "black" &
        inccat == "80-100%" ~ 10,
      
      housecl == 1 &  
        race== "hispanic" &
        inccat == "0-20%" ~ 11,
      housecl == 1 &
        race== "hispanic" &
        inccat == "20-40%" ~ 12,
      housecl == 1 &
        race== "hispanic" &
        inccat == "40-60%" ~ 13,
      housecl == 1 &
        race== "hispanic" &
        inccat == "60-80%" ~ 14,
      housecl == 1 &
        race== "hispanic" &
        inccat == "80-100%" ~ 15,
      
      housecl == 1 &
        race== "asian" &
        inccat == "0-20%" ~ 16,
      housecl == 1 &
        race== "asian" &
        inccat == "20-40%" ~ 17,
      housecl == 1 &
        race== "asian" &
        inccat == "40-60%" ~ 18,
      housecl == 1 &
        race== "asian" &
        inccat == "60-80%" ~ 19,
      housecl == 1 &
        race== "asian" &
        inccat == "80-100%" ~ 20,
      
      housecl == 1 &
        race== "other" &
        inccat == "0-20%" ~ 16,
      housecl == 1 &
        race== "other" &
        inccat == "20-40%" ~ 17,
      housecl == 1 &
        race== "other" &
        inccat == "40-60%" ~ 18,
      housecl == 1 &
        race== "other" &
        inccat == "60-80%" ~ 19,
      housecl == 1 &
        race== "other" &
        inccat == "80-100%" ~ 20,
      
      housecl == 0 &
        race== "white" &
        inccat == "0-20%" ~ 21,
      housecl == 0 &
        race== "white" &
        inccat == "20-40%" ~ 22,
      housecl == 0 &
        race== "white" &
        inccat == "40-60%" ~ 23,
      housecl == 0 &
        race== "white" &
        inccat == "60-80%" ~ 24,
      housecl == 0 &
        race== "white" &
        inccat == "80-100%" ~ 25,
      
      housecl == 0 &
        race== "black" &
        inccat == "0-20%" ~ 26,
      housecl == 0 &
        race== "black" &
        inccat == "20-40%" ~ 27,
      housecl == 0 &
        race== "black" &
        inccat == "40-60%" ~ 28,
      housecl == 0 &
        race== "black" &
        inccat == "60-80%" ~ 29,
      housecl == 0 &
        race== "black" &
        inccat == "80-100%" ~ 30,
      
      housecl == 0 &  
        race== "hispanic" &
        inccat == "0-20%" ~ 31,
      housecl == 0 &
        race== "hispanic" &
        inccat == "20-40%" ~ 32,
      housecl == 0 &
        race== "hispanic" &
        inccat == "40-60%" ~ 33,
      housecl == 0 &
        race== "hispanic" &
        inccat == "60-80%" ~ 34,
      housecl == 0 &
        race== "hispanic" &
        inccat == "80-100%" ~ 35,
      
      housecl == 0 &
        race== "asian" &
        inccat == "0-20%" ~ 36,
      housecl == 0 &
        race== "asian" &
        inccat == "20-40%" ~ 37,
      housecl == 0 &
        race== "asian" &
        inccat == "40-60%" ~ 38,
      housecl == 0 &
        race== "asian" &
        inccat == "60-80%" ~ 39,
      housecl == 0 &
        race== "asian" &
        inccat == "80-100%" ~ 40,
      
      housecl == 0 &
        race== "other" &
        inccat == "0-20%" ~ 36,
      housecl == 0 &
        race== "other" &
        inccat == "20-40%" ~ 37,
      housecl == 0 &
        race== "other" &
        inccat == "40-60%" ~ 38,
      housecl == 0 &
        race== "other" &
        inccat == "60-80%" ~ 39,
      housecl == 0 &
        race== "other" &
        inccat == "80-100%" ~ 40,
      
      T ~ 0),
    homeownerracecat = 
      factor(
        homeownerracecat,
        levels = 1:40 ,
        labels = c("white homeowner: 0-20%" , "white homeowner: 20-40%", "white homeowner: 40-60%", "white homeowner: 60-80%", "white homeowner: 80-100%", 
                   "black homeowner: 0-20%" , "black homeowner: 20-40%", "black homeowner: 40-60%", "black homeowner: 60-80%", "black homeowner: 80-100%", 
                   "hispanic homeowner: 0-20%" , "hispanic homeowner: 20-40%", "hispanic homeowner: 40-60%", "hispanic homeowner: 60-80%", "hispanic homeowner: 80-100%", 
                   # "asian: 0-20%" , "asian: 20-40%", "asian: 40-60%", "asian: 60-80%", "asian: 80-100%",
                   "other homeowner: 0-20%" , "other homeowner: 20-40%", "other homeowner: 40-60%", "other homeowner: 60-80%", "other homeowner: 80-100%", 
                   
                   "white not a homeowner: 0-20%" , "white not a homeowner: 20-40%", "white not a homeowner: 40-60%", "white not a homeowner: 60-80%", "white not a homeowner: 80-100%", 
                   "black not a homeowner: 0-20%" , "black not a homeowner: 20-40%", "black not a homeowner: 40-60%", "black not a homeowner: 60-80%", "black not a homeowner: 80-100%",
                   "hispanic not a homeowner: 0-20%" , "hispanic not a homeowner: 20-40%", "hispanic not a homeowner: 40-60%", "hispanic not a homeowner: 60-80%", "hispanic not a homeowner: 80-100%", 
                   # "asian: 0-20%" , "asian: 20-40%", "asian: 40-60%", "asian: 60-80%", "asian: 80-100%",
                   "other not a homeowner: 0-20%" , "other not a homeowner: 20-40%", "other not a homeowner: 40-60%", "other not a homeowner: 60-80%", "other not a homeowner: 80-100%")
      ),
    edcl =
      factor(
        edcl ,
        levels = 1:4 ,
        labels =
          c(
            "less than high school" ,
            "high school or GED" ,
            "some college" ,
            "college degree"
          )
      )
    
  )



  #2019
  ###########################################
  #2019
  ###########################################
  #2019
  
  #Download and import the full, summary extract, and replicate weights tables for 2019:
  scf_df2019 <-
    scf_dta_import("https://www.federalreserve.gov/econres/files/scf2019s.zip")
  
  ext_df2019 <-
    scf_dta_import("https://www.federalreserve.gov/econres/files/scfp2019s.zip")
  
  scf_rw_df2019 <-
    scf_dta_import("https://www.federalreserve.gov/econres/files/scf2019rw1s.zip")
  #Remove the implicate identifier from the replicate weights table, add a column of fives for weighting:
  scf_rw_df2019[, 'y1'] <- NULL
  
  scf_df2019[, 'five'] <- 5
  
  #Break the main table into five different implicates based on the final character of the column y1:
  
  s1_df2019 <- scf_df2019[str_sub(scf_df2019[, 'y1'] ,-1 ,-1) == 1 ,]
  s2_df2019 <- scf_df2019[str_sub(scf_df2019[, 'y1'] ,-1 ,-1) == 2 ,]
  s3_df2019 <- scf_df2019[str_sub(scf_df2019[, 'y1'] ,-1 ,-1) == 3 ,]
  s4_df2019 <- scf_df2019[str_sub(scf_df2019[, 'y1'] ,-1 ,-1) == 4 ,]
  s5_df2019 <- scf_df2019[str_sub(scf_df2019[, 'y1'] ,-1 ,-1) == 5 ,]
  #Combine these into a single list, then merge each implicate with the summary extract:
  
  scf_imp2019 <- list(s1_df2019 , s2_df2019 , s3_df2019 , s4_df2019 , s5_df2019)
  
  scf_list2019 <- lapply(scf_imp2019 , merge , ext_df2019)
  
  #Replace all missing values in the replicate weights table with zeroes, multiply the replicate weights by the multiplication factor, then only keep the unique identifier and the final (combined) replicate weights:
  
  scf_rw_df2019[is.na(scf_rw_df2019)] <- 0
  
  scf_rw_df2019[, paste0('wgt' , 1:999)] <-
    scf_rw_df2019[, paste0('wt1b' , 1:999)] * scf_rw_df2019[, paste0('mm' , 1:999)]
  
  scf_rw_df2019 <- scf_rw_df2019[, c('yy1' , paste0('wgt' , 1:999))]
  
  #Sort both the five implicates and also the replicate weights table by the unique identifier:
  
  scf_list2019 <-
    lapply(scf_list2019 , function(w)
      w[order(w[, 'yy1']) ,])
  
  scf_rw_df2019 <- scf_rw_df2019[order(scf_rw_df2019[, 'yy1']) ,]
  
  
  #Sort both the five implicates and also the replicate weights table by the unique identifier:
  
  scf_list2019 <-
    lapply(scf_list2019 , function(w)
      w[order(w[, 'yy1']) ,])
  
  scf_rw_df2019 <- scf_rw_df2019[order(scf_rw_df2019[, 'yy1']) ,]
  
  #Define the design:

  scf_design2019 <-
    svrepdesign(
      weights = ~ wgt ,
      repweights = scf_rw_df2019[,-1] ,
      data = imputationList(scf_list2019) ,
      scale = 1 ,
      rscales = rep(1 / 998 , 999) ,
      mse = FALSE ,
      type = "other" ,
      combined.weights = TRUE
    )
  
  #Run the convey_prep() function on the full design:
  scf_design2019$designs <- lapply(scf_design2019$designs , convey_prep)
  
  
  #Add new columns to the data set:
  
  scf_design2019 <-
    update(
      scf_design2019 ,
      
      hhsex = factor(
        hhsex ,
        levels = 1:2 ,
        labels = c("male" , "female")
      ) ,
      race = factor(
        race ,
        levels = 1:5 ,
        labels = c("white" , "black", "hispanic", "asian", "other")
      ),
      inccat = case_when(inccat == 5 |
                           inccat == 6 ~ 5,
                         T ~ inccat),
      inccat = factor(
        inccat ,
        levels = 1:5 ,
        labels = c("0-20%" , "20-40%", "40-60%", "60-80%", "80-100%")
      ),
      racecat = case_when(
        race== "white" &
          inccat == "0-20%" ~ 1,
        race== "white" &
          inccat == "20-40%" ~ 2,
        race== "white" &
          inccat == "40-60%" ~ 3,
        race== "white" &
          inccat == "60-80%" ~ 4,
        race== "white" &
          inccat == "80-100%" ~ 5,
        
        race== "black" &
          inccat == "0-20%" ~ 6,
        race== "black" &
          inccat == "20-40%" ~ 7,
        race== "black" &
          inccat == "40-60%" ~ 8,
        race== "black" &
          inccat == "60-80%" ~ 9,
        race== "black" &
          inccat == "80-100%" ~ 10,
        
        race== "hispanic" &
          inccat == "0-20%" ~ 11,
        race== "hispanic" &
          inccat == "20-40%" ~ 12,
        race== "hispanic" &
          inccat == "40-60%" ~ 13,
        race== "hispanic" &
          inccat == "60-80%" ~ 14,
        race== "hispanic" &
          inccat == "80-100%" ~ 15,
        
        race== "asian" &
          inccat == "0-20%" ~ 16,
        race== "asian" &
          inccat == "20-40%" ~ 17,
        race== "asian" &
          inccat == "40-60%" ~ 18,
        race== "asian" &
          inccat == "60-80%" ~ 19,
        race== "asian" &
          inccat == "80-100%" ~ 20,
        
        race== "other" &
          inccat == "0-20%" ~ 16,
        race== "other" &
          inccat == "20-40%" ~ 17,
        race== "other" &
          inccat == "40-60%" ~ 18,
        race== "other" &
          inccat == "60-80%" ~ 19,
        race== "other" &
          inccat == "80-100%" ~ 20,
        T ~ 0),
      racecat = 
        factor(
          racecat,
          levels = 1:20 ,
          labels = c("white: 0-20%" , "white: 20-40%", "white: 40-60%", "white: 60-80%", "white: 80-100%",
                     "black: 0-20%" , "black: 20-40%", "black: 40-60%", "black: 60-80%", "black: 80-100%",
                     "hispanic: 0-20%" , "hispanic: 20-40%", "hispanic: 40-60%", "hispanic: 60-80%", "hispanic: 80-100%",
                     # "asian: 0-20%" , "asian: 20-40%", "asian: 40-60%", "asian: 60-80%", "asian: 80-100%",
                     "other: 0-20%" , "other: 20-40%", "other: 40-60%", "other: 60-80%", "other: 80-100%")
        ),
      married = as.numeric(married == 1) ,
      
      white = case_when(race == "white" ~ 1, 
                        T ~ 0),
      black = case_when(race == "black" ~ 1,
                        T ~ 0),
      hispanic = case_when(race == "hispanic" ~ 1,
                           T ~ 0),
      asian = case_when(race == "asian" ~ 1,
                        T ~ 0),
      other = case_when(race == "other" ~ 1,
                        T ~ 0),
      `0-20%` = case_when(inccat =="0-20%" ~ 1,
                          T ~ 0),
      `20-40%` = case_when(inccat =="20-40%" ~ 1,
                           T ~ 0),
      `40-60%` = case_when(inccat =="40-60%" ~ 1,
                           T ~ 0),
      `60-80%` = case_when(inccat =="60-80%" ~ 1,
                           T ~ 0),
      `80-100%` = case_when(inccat =="80-100%" ~ 1,
                            T ~ 0),
      
      `White: 0-20%` = case_when(inccat =="0-20%" & race == "white" ~ 1,
                                 T ~ 0),
      `White: 20-40%` = case_when(inccat =="20-40%" & race == "white" ~ 1,
                                  T ~ 0),
      `White: 40-60%` = case_when(inccat =="40-60%" & race == "white" ~ 1,
                                  T ~ 0),
      `White: 60-80%` = case_when(inccat =="60-80%" & race == "white" ~ 1,
                                  T ~ 0),
      `White: 80-100%` = case_when(inccat =="80-100%" & race == "white" ~ 1,
                                   T ~ 0),
      
      `Black: 0-20%` = case_when(inccat =="0-20%" & race == "black" ~ 1,
                                 T ~ 0),
      `Black: 20-40%` = case_when(inccat =="20-40%" & race == "black" ~ 1,
                                  T ~ 0),
      `Black: 40-60%` = case_when(inccat =="40-60%" & race == "black" ~ 1,
                                  T ~ 0),
      `Black: 60-80%` = case_when(inccat =="60-80%" & race == "black" ~ 1,
                                  T ~ 0),
      `Black: 80-100%` = case_when(inccat =="80-100%" & race == "black" ~ 1,
                                   T ~ 0),
      
      `Hispanic: 0-20%` = case_when(inccat =="0-20%" & race == "hispanic" ~ 1,
                                    T ~ 0),
      `Hispanic: 20-40%` = case_when(inccat =="20-40%" & race == "hispanic" ~ 1,
                                     T ~ 0),
      `Hispanic: 40-60%` = case_when(inccat =="40-60%" & race == "hispanic" ~ 1,
                                     T ~ 0),
      `Hispanic: 60-80%` = case_when(inccat =="60-80%" & race == "hispanic" ~ 1,
                                     T ~ 0),
      `Hispanic: 80-100%` = case_when(inccat =="80-100%" & race == "hispanic" ~ 1,
                                      T ~ 0),
      
      `Asian: 0-20%` = case_when(inccat =="0-20%" & race == "asian" ~ 1,
                                 T ~ 0),
      `Asian: 20-40%` = case_when(inccat =="20-40%" & race == "asian" ~ 1,
                                  T ~ 0),
      `Asian: 40-60%` = case_when(inccat =="40-60%" & race == "asian" ~ 1,
                                  T ~ 0),
      `Asian: 60-80%` = case_when(inccat =="60-80%" & race == "asian" ~ 1,
                                  T ~ 0),
      `Asian: 80-100%` = case_when(inccat =="80-100%" & race == "asian" ~ 1,
                                   T ~ 0),
      
      `Other: 0-20%` = case_when(inccat =="0-20%" & race == "other" ~ 1,
                                 T ~ 0),
      `Other: 20-40%` = case_when(inccat =="20-40%" & race == "other" ~ 1,
                                  T ~ 0),
      `Other: 40-60%` = case_when(inccat =="40-60%" & race == "other" ~ 1,
                                  T ~ 0),
      `Other: 60-80%` = case_when(inccat =="60-80%" & race == "other" ~ 1,
                                  T ~ 0),
      `Other: 80-100%` = case_when(inccat =="80-100%" & race == "other" ~ 1,
                                   T ~ 0),
      
      housecl = case_when(
        housecl == 2 ~ 0,
        T ~ housecl
      ),
      homeownerinc = case_when(
        housecl == 1 & inccat == "0-20%" ~ 1,
        housecl == 1 & inccat == "20-40%" ~ 2,
        housecl == 1 & inccat == "40-60%" ~ 3,
        housecl == 1 & inccat == "60-80%" ~ 4,
        housecl == 1 & inccat == "80-100%" ~ 5,
        
        housecl == 0 & inccat == "0-20%" ~ 6,
        housecl == 0 & inccat == "20-40%" ~ 7,
        housecl == 0 & inccat == "40-60%" ~ 8,
        housecl == 0 & inccat == "60-80%" ~ 9,
        housecl == 0 & inccat == "80-100%" ~ 10,
      ),
      homeownerinc = factor(
        homeownerinc,
        levels = 1:10,
        labels = c(
          "Homeowner: 0-20%",
          "Homeowner: 20-40%",
          "Homeowner: 40-60%",
          "Homeowner: 60-80%",
          "Homeowner: 80-100%",
          
          "Not a homeowner: 0-20%",
          "Not a homeowner: 20-40%",
          "Not a homeowner: 40-60%",
          "Not a homeowner: 60-80%",
          "Not a homeowner: 80-100%"
        )
      ),
      
      homeownerrace = case_when(
        housecl == 1 & race == "white" ~ 1,
        housecl == 1 & race == "black" ~ 2,
        housecl == 1 & race == "hispanic" ~ 3,
        housecl == 1 & race == "asian" ~ 4,
        housecl == 1 & race == "other" ~ 5,
        
        housecl == 0 & race == "white" ~ 6,
        housecl == 0 & race == "black" ~ 7,
        housecl == 0 & race == "hispanic" ~ 8,
        housecl == 0 & race == "asian" ~ 9,
        housecl == 0 & race == "other" ~ 10
      ),
      homeownerrace = factor(
        homeownerrace,
        levels = 1:10,
        labels = c(
          "Homeowner: White",
          "Homeowner: Black",
          "Homeowner: Hispanic",
          "Homeowner: Asian",
          "Homeowner: Other",
          
          "Not a homeowner: White",
          "Not a homeowner: Black",
          "Not a homeowner: Hispanic",
          "Not a homeowner: Asian",
          "Not a homeowner: Other"
        )
      ),
      
      
      
      homeownerracecat = case_when(
        housecl == 1 &
          race== "white" &
          inccat == "0-20%" ~ 1,
        housecl == 1 &
          race== "white" &
          inccat == "20-40%" ~ 2,
        housecl == 1 &
          race== "white" &
          inccat == "40-60%" ~ 3,
        housecl == 1 &
          race== "white" &
          inccat == "60-80%" ~ 4,
        housecl == 1 &
          race== "white" &
          inccat == "80-100%" ~ 5,
        
        housecl == 1 &
          race== "black" &
          inccat == "0-20%" ~ 6,
        housecl == 1 &
          race== "black" &
          inccat == "20-40%" ~ 7,
        housecl == 1 &
          race== "black" &
          inccat == "40-60%" ~ 8,
        housecl == 1 &
          race== "black" &
          inccat == "60-80%" ~ 9,
        housecl == 1 &
          race== "black" &
          inccat == "80-100%" ~ 10,
        
        housecl == 1 &  
          race== "hispanic" &
          inccat == "0-20%" ~ 11,
        housecl == 1 &
          race== "hispanic" &
          inccat == "20-40%" ~ 12,
        housecl == 1 &
          race== "hispanic" &
          inccat == "40-60%" ~ 13,
        housecl == 1 &
          race== "hispanic" &
          inccat == "60-80%" ~ 14,
        housecl == 1 &
          race== "hispanic" &
          inccat == "80-100%" ~ 15,
        
        housecl == 1 &
          race== "asian" &
          inccat == "0-20%" ~ 16,
        housecl == 1 &
          race== "asian" &
          inccat == "20-40%" ~ 17,
        housecl == 1 &
          race== "asian" &
          inccat == "40-60%" ~ 18,
        housecl == 1 &
          race== "asian" &
          inccat == "60-80%" ~ 19,
        housecl == 1 &
          race== "asian" &
          inccat == "80-100%" ~ 20,
        
        housecl == 1 &
          race== "other" &
          inccat == "0-20%" ~ 16,
        housecl == 1 &
          race== "other" &
          inccat == "20-40%" ~ 17,
        housecl == 1 &
          race== "other" &
          inccat == "40-60%" ~ 18,
        housecl == 1 &
          race== "other" &
          inccat == "60-80%" ~ 19,
        housecl == 1 &
          race== "other" &
          inccat == "80-100%" ~ 20,
        
        housecl == 0 &
          race== "white" &
          inccat == "0-20%" ~ 21,
        housecl == 0 &
          race== "white" &
          inccat == "20-40%" ~ 22,
        housecl == 0 &
          race== "white" &
          inccat == "40-60%" ~ 23,
        housecl == 0 &
          race== "white" &
          inccat == "60-80%" ~ 24,
        housecl == 0 &
          race== "white" &
          inccat == "80-100%" ~ 25,
        
        housecl == 0 &
          race== "black" &
          inccat == "0-20%" ~ 26,
        housecl == 0 &
          race== "black" &
          inccat == "20-40%" ~ 27,
        housecl == 0 &
          race== "black" &
          inccat == "40-60%" ~ 28,
        housecl == 0 &
          race== "black" &
          inccat == "60-80%" ~ 29,
        housecl == 0 &
          race== "black" &
          inccat == "80-100%" ~ 30,
        
        housecl == 0 &  
          race== "hispanic" &
          inccat == "0-20%" ~ 31,
        housecl == 0 &
          race== "hispanic" &
          inccat == "20-40%" ~ 32,
        housecl == 0 &
          race== "hispanic" &
          inccat == "40-60%" ~ 33,
        housecl == 0 &
          race== "hispanic" &
          inccat == "60-80%" ~ 34,
        housecl == 0 &
          race== "hispanic" &
          inccat == "80-100%" ~ 35,
        
        housecl == 0 &
          race== "asian" &
          inccat == "0-20%" ~ 36,
        housecl == 0 &
          race== "asian" &
          inccat == "20-40%" ~ 37,
        housecl == 0 &
          race== "asian" &
          inccat == "40-60%" ~ 38,
        housecl == 0 &
          race== "asian" &
          inccat == "60-80%" ~ 39,
        housecl == 0 &
          race== "asian" &
          inccat == "80-100%" ~ 40,
        
        housecl == 0 &
          race== "other" &
          inccat == "0-20%" ~ 36,
        housecl == 0 &
          race== "other" &
          inccat == "20-40%" ~ 37,
        housecl == 0 &
          race== "other" &
          inccat == "40-60%" ~ 38,
        housecl == 0 &
          race== "other" &
          inccat == "60-80%" ~ 39,
        housecl == 0 &
          race== "other" &
          inccat == "80-100%" ~ 40,
        
        T ~ 0),
      homeownerracecat = 
        factor(
          homeownerracecat,
          levels = 1:40 ,
          labels = c("white homeowner: 0-20%" , "white homeowner: 20-40%", "white homeowner: 40-60%", "white homeowner: 60-80%", "white homeowner: 80-100%", 
                     "black homeowner: 0-20%" , "black homeowner: 20-40%", "black homeowner: 40-60%", "black homeowner: 60-80%", "black homeowner: 80-100%", 
                     "hispanic homeowner: 0-20%" , "hispanic homeowner: 20-40%", "hispanic homeowner: 40-60%", "hispanic homeowner: 60-80%", "hispanic homeowner: 80-100%", 
                     # "asian: 0-20%" , "asian: 20-40%", "asian: 40-60%", "asian: 60-80%", "asian: 80-100%",
                     "other homeowner: 0-20%" , "other homeowner: 20-40%", "other homeowner: 40-60%", "other homeowner: 60-80%", "other homeowner: 80-100%", 
                     
                     "white not a homeowner: 0-20%" , "white not a homeowner: 20-40%", "white not a homeowner: 40-60%", "white not a homeowner: 60-80%", "white not a homeowner: 80-100%", 
                     "black not a homeowner: 0-20%" , "black not a homeowner: 20-40%", "black not a homeowner: 40-60%", "black not a homeowner: 60-80%", "black not a homeowner: 80-100%",
                     "hispanic not a homeowner: 0-20%" , "hispanic not a homeowner: 20-40%", "hispanic not a homeowner: 40-60%", "hispanic not a homeowner: 60-80%", "hispanic not a homeowner: 80-100%", 
                     # "asian: 0-20%" , "asian: 20-40%", "asian: 40-60%", "asian: 60-80%", "asian: 80-100%",
                     "other not a homeowner: 0-20%" , "other not a homeowner: 20-40%", "other not a homeowner: 40-60%", "other not a homeowner: 60-80%", "other not a homeowner: 80-100%")
        ),
      
      edcl =
        factor(
          edcl ,
          levels = 1:4 ,
          labels =
            c(
              "less than high school" ,
              "high school or GED" ,
              "some college" ,
              "college degree"
            )
        )
      
    )
  
  ###########################################
  #My analysis
  ###########################################
  
  
  ###########################################
  #My analysis: Population
  ###########################################
  
  #Calculate number of observations
  scf_MIcombine(with(scf_design , svyby(~ five , ~ five , unwtd.count)))
  scf_MIcombine(with(scf_design2019 , svyby(~ five , ~ five , unwtd.count)))
  
  #Calculate household population
  
household_pop <- scf_MIcombine(with(scf_design , svytotal(~ five)))
household_pop2019 <- scf_MIcombine(with(scf_design2019 , svytotal(~ five))) 

#Calculate household population by income


household_pop_by_income <- scf_MIcombine(with(scf_design ,
                                              svyby(~ inccat , ~ five , svymean)))

household_pop_by_income2019 <- scf_MIcombine(with(scf_design2019 ,
                                              svyby(~ inccat , ~ five , svymean)))

#Calculate household percentage by race


household_pop_by_race <- scf_MIcombine(with(scf_design ,
                                              svyby(~ race , ~ five , svymean)))

household_pop_by_race2019 <- scf_MIcombine(with(scf_design2019 ,
                                                  svyby(~ race , ~ five , svymean)))

#Calculate household percentage by race and income


white_percentage_by_incomerace <- scf_MIcombine(with(scf_design ,
                                                svyby(~ inccat , ~ white , svymean)))

white_percentage_by_incomerace2019 <- scf_MIcombine(with(scf_design2019 ,
                                                         svyby(~ inccat , ~ white , svymean)))

black_percentage_by_incomerace <- scf_MIcombine(with(scf_design ,
                                                         svyby(~ inccat , ~ black , svymean)))
black_percentage_by_incomerace2019 <- scf_MIcombine(with(scf_design2019 ,
                                                         svyby(~ inccat , ~ black , svymean)))

hispanic_percentage_by_incomerace <- scf_MIcombine(with(scf_design ,
                                                            svyby(~ inccat , ~ hispanic , svymean)))
hispanic_percentage_by_incomerace2019 <- scf_MIcombine(with(scf_design2019 ,
                                                         svyby(~ inccat , ~ hispanic , svymean)))

asian_percentage_by_incomerace <- scf_MIcombine(with(scf_design ,
                                                         svyby(~ inccat , ~ asian , svymean)))
asian_percentage_by_incomerace2019 <- scf_MIcombine(with(scf_design2019 ,
                                                         svyby(~ inccat , ~ asian , svymean)))

other_percentage_by_incomerace <- scf_MIcombine(with(scf_design ,
                                                         svyby(~ inccat , ~ other , svymean)))
other_percentage_by_incomerace2019 <- scf_MIcombine(with(scf_design2019 ,
                                                         svyby(~ inccat , ~ other , svymean)))

#put it into a data frame
household_population <- tibble( 
  year = c(
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019
  ),
  race = c(
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other"
  ),
  income = c(
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
  ),
  population = c(
    household_pop[[1]][1], #Overall
    household_pop_by_income[[1]][[1]], #By income
    household_pop_by_income[[1]][[2]],
    household_pop_by_income[[1]][[3]],
    household_pop_by_income[[1]][[4]],
    household_pop_by_income[[1]][[5]],
    
    household_pop_by_race[[1]][[1]], #White
    white_percentage_by_incomerace[[1]][[2]],
    white_percentage_by_incomerace[[1]][[4]],
    white_percentage_by_incomerace[[1]][[6]],
    white_percentage_by_incomerace[[1]][[8]],
    white_percentage_by_incomerace[[1]][[10]],
    
    household_pop_by_race[[1]][[2]], #Black
    black_percentage_by_incomerace[[1]][[2]],
    black_percentage_by_incomerace[[1]][[4]],
    black_percentage_by_incomerace[[1]][[6]],
    black_percentage_by_incomerace[[1]][[8]],
    black_percentage_by_incomerace[[1]][[10]],
    
    household_pop_by_race[[1]][[3]], #Hispanic
    hispanic_percentage_by_incomerace[[1]][[2]],
    hispanic_percentage_by_incomerace[[1]][[4]],
    hispanic_percentage_by_incomerace[[1]][[6]],
    hispanic_percentage_by_incomerace[[1]][[8]],
    hispanic_percentage_by_incomerace[[1]][[10]],
    
    household_pop_by_race[[1]][[4]],#Asian
    asian_percentage_by_incomerace[[1]][[2]],
    asian_percentage_by_incomerace[[1]][[4]],
    asian_percentage_by_incomerace[[1]][[6]],
    asian_percentage_by_incomerace[[1]][[8]],
    asian_percentage_by_incomerace[[1]][[10]],
    
    household_pop_by_race[[1]][[5]], #Other
    other_percentage_by_incomerace[[1]][[2]],
    other_percentage_by_incomerace[[1]][[4]],
    other_percentage_by_incomerace[[1]][[6]],
    other_percentage_by_incomerace[[1]][[8]],
    other_percentage_by_incomerace[[1]][[10]],
    
    household_pop2019[[1]][[1]], #Overall 2019
    household_pop_by_income2019[[1]][[1]], #By income
    household_pop_by_income2019[[1]][[2]],
    household_pop_by_income2019[[1]][[3]],
    household_pop_by_income2019[[1]][[4]],
    household_pop_by_income2019[[1]][[5]],
    
    household_pop_by_race2019[[1]][[1]], #White
    white_percentage_by_incomerace2019[[1]][[2]],
    white_percentage_by_incomerace2019[[1]][[4]],
    white_percentage_by_incomerace2019[[1]][[6]],
    white_percentage_by_incomerace2019[[1]][[8]],
    white_percentage_by_incomerace2019[[1]][[10]],
    
    household_pop_by_race2019[[1]][[2]],#Black
    black_percentage_by_incomerace2019[[1]][[2]],
    black_percentage_by_incomerace2019[[1]][[4]],
    black_percentage_by_incomerace2019[[1]][[6]],
    black_percentage_by_incomerace2019[[1]][[8]],
    black_percentage_by_incomerace2019[[1]][[10]],
    
    household_pop_by_race2019[[1]][[3]],#Hispanic
    hispanic_percentage_by_incomerace2019[[1]][[2]],
    hispanic_percentage_by_incomerace2019[[1]][[4]],
    hispanic_percentage_by_incomerace2019[[1]][[6]],
    hispanic_percentage_by_incomerace2019[[1]][[8]],
    hispanic_percentage_by_incomerace2019[[1]][[10]],
    
    NA, #Asian not present in data
    NA,
    NA,
    NA,
    NA,
    NA,
    
    household_pop_by_race2019[[1]][[5]], #Other
    other_percentage_by_incomerace2019[[1]][[2]],
    other_percentage_by_incomerace2019[[1]][[4]],
    other_percentage_by_incomerace2019[[1]][[6]],
    other_percentage_by_incomerace2019[[1]][[8]],
    other_percentage_by_incomerace2019[[1]][[10]]
    
  ),
  
  low = c(
    confint(household_pop)[1], #Overall
    confint(household_pop_by_income)[1], #By Income
    confint(household_pop_by_income)[2],
    confint(household_pop_by_income)[3],
    confint(household_pop_by_income)[4],
    confint(household_pop_by_income)[5],
    
    confint(household_pop_by_race)[1], #White
    confint(white_percentage_by_incomerace)[2],
    confint(white_percentage_by_incomerace)[4],
    confint(white_percentage_by_incomerace)[6],
    confint(white_percentage_by_incomerace)[8],
    confint(white_percentage_by_incomerace)[10],
    
    confint(household_pop_by_race)[2], #Black
    confint(black_percentage_by_incomerace)[2],
    confint(black_percentage_by_incomerace)[4],
    confint(black_percentage_by_incomerace)[6],
    confint(black_percentage_by_incomerace)[8],
    confint(black_percentage_by_incomerace)[10],
    
    confint(household_pop_by_race)[3], #Hispanic
    confint(hispanic_percentage_by_incomerace)[2],
    confint(hispanic_percentage_by_incomerace)[4],
    confint(hispanic_percentage_by_incomerace)[6],
    confint(hispanic_percentage_by_incomerace)[8],
    confint(hispanic_percentage_by_incomerace)[10],
    
    confint(household_pop_by_race)[4], #Asian
    confint(asian_percentage_by_incomerace)[2],
    confint(asian_percentage_by_incomerace)[4],
    confint(asian_percentage_by_incomerace)[6],
    confint(asian_percentage_by_incomerace)[8],
    confint(asian_percentage_by_incomerace)[10],
    
    confint(household_pop_by_race)[5], #Other
    confint(other_percentage_by_incomerace)[2],
    confint(other_percentage_by_incomerace)[4],
    confint(other_percentage_by_incomerace)[6],
    confint(other_percentage_by_incomerace)[8],
    confint(other_percentage_by_incomerace)[10],
    
    #2019
    confint(household_pop2019)[1], #Overall
    confint(household_pop_by_income2019)[1], #By Income
    confint(household_pop_by_income2019)[2],
    confint(household_pop_by_income2019)[3],
    confint(household_pop_by_income2019)[4],
    confint(household_pop_by_income2019)[5],
    
    confint(household_pop_by_race2019)[1], #White
    confint(white_percentage_by_incomerace2019)[2],
    confint(white_percentage_by_incomerace2019)[4],
    confint(white_percentage_by_incomerace2019)[6],
    confint(white_percentage_by_incomerace2019)[8],
    confint(white_percentage_by_incomerace2019)[10],
    
    confint(household_pop_by_race2019)[2], #Black
    confint(black_percentage_by_incomerace2019)[2],
    confint(black_percentage_by_incomerace2019)[4],
    confint(black_percentage_by_incomerace2019)[6],
    confint(black_percentage_by_incomerace2019)[8],
    confint(black_percentage_by_incomerace2019)[10],
    
    confint(household_pop_by_race2019)[3], #Hispanic
    confint(hispanic_percentage_by_incomerace2019)[2],
    confint(hispanic_percentage_by_incomerace2019)[4],
    confint(hispanic_percentage_by_incomerace2019)[6],
    confint(hispanic_percentage_by_incomerace2019)[8],
    confint(hispanic_percentage_by_incomerace2019)[10],
    
    NA, #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(household_pop_by_race2019)[5], #Other
    confint(other_percentage_by_incomerace2019)[2],
    confint(other_percentage_by_incomerace2019)[4],
    confint(other_percentage_by_incomerace2019)[6],
    confint(other_percentage_by_incomerace2019)[8],
    confint(other_percentage_by_incomerace2019)[10]
    
    
  ),
  high = c(
    confint(household_pop)[2], #Overall
    confint(household_pop_by_income)[6], #By Income
    confint(household_pop_by_income)[7],
    confint(household_pop_by_income)[8],
    confint(household_pop_by_income)[9],
    confint(household_pop_by_income)[10],
    
    
    confint(household_pop_by_race)[6], #White
    confint(white_percentage_by_incomerace)[12],
    confint(white_percentage_by_incomerace)[14],
    confint(white_percentage_by_incomerace)[16],
    confint(white_percentage_by_incomerace)[18],
    confint(white_percentage_by_incomerace)[20],
    
    confint(household_pop_by_race)[7], #Black
    confint(black_percentage_by_incomerace)[12],
    confint(black_percentage_by_incomerace)[14],
    confint(black_percentage_by_incomerace)[16],
    confint(black_percentage_by_incomerace)[18],
    confint(black_percentage_by_incomerace)[20],
    
    confint(household_pop_by_race)[8], #Hispanic
    confint(hispanic_percentage_by_incomerace)[12],
    confint(hispanic_percentage_by_incomerace)[14],
    confint(hispanic_percentage_by_incomerace)[16],
    confint(hispanic_percentage_by_incomerace)[18],
    confint(hispanic_percentage_by_incomerace)[20],
    
    confint(household_pop_by_race)[9], #Asian
    confint(asian_percentage_by_incomerace)[12],
    confint(asian_percentage_by_incomerace)[14],
    confint(asian_percentage_by_incomerace)[16],
    confint(asian_percentage_by_incomerace)[18],
    confint(asian_percentage_by_incomerace)[20],
    
    confint(household_pop_by_race)[10], #Other
    confint(other_percentage_by_incomerace)[12],
    confint(other_percentage_by_incomerace)[14],
    confint(other_percentage_by_incomerace)[16],
    confint(other_percentage_by_incomerace)[18],
    confint(other_percentage_by_incomerace)[20],
    
    #2019
    confint(household_pop2019)[2], #Overall
    confint(household_pop_by_income2019)[6], #By Income
    confint(household_pop_by_income2019)[7],
    confint(household_pop_by_income2019)[8],
    confint(household_pop_by_income2019)[9],
    confint(household_pop_by_income2019)[10],
    
    
    confint(household_pop_by_race2019)[6], #White
    confint(white_percentage_by_incomerace2019)[12],
    confint(white_percentage_by_incomerace2019)[14],
    confint(white_percentage_by_incomerace2019)[16],
    confint(white_percentage_by_incomerace2019)[18],
    confint(white_percentage_by_incomerace2019)[20],
    
    confint(household_pop_by_race2019)[7], #Black
    confint(black_percentage_by_incomerace2019)[12],
    confint(black_percentage_by_incomerace2019)[14],
    confint(black_percentage_by_incomerace2019)[16],
    confint(black_percentage_by_incomerace2019)[18],
    confint(black_percentage_by_incomerace2019)[20],
    
    confint(household_pop_by_race2019)[8], #Hispanic
    confint(hispanic_percentage_by_incomerace2019)[12],
    confint(hispanic_percentage_by_incomerace2019)[14],
    confint(hispanic_percentage_by_incomerace2019)[16],
    confint(hispanic_percentage_by_incomerace2019)[18],
    confint(hispanic_percentage_by_incomerace2019)[20],
    
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(household_pop_by_race2019)[10], #Other
    confint(other_percentage_by_incomerace2019)[12],
    confint(other_percentage_by_incomerace2019)[14],
    confint(other_percentage_by_incomerace2019)[16],
    confint(other_percentage_by_incomerace2019)[18],
    confint(other_percentage_by_incomerace2019)[20]
  )
  
)

write_csv(
  household_population,
  "2. Data Output/household pop.csv"
)

###########################################
#My analysis:Home ownership
###########################################

#Calculate Homeownership population

homeowners_percent <- scf_MIcombine(with(scf_design , svymean(~ housecl)))
homeowners_percent2019 <- scf_MIcombine(with(scf_design2019 , svymean(~ housecl)))


homeowners_income_percent <- scf_MIcombine(with(scf_design ,
                                     svyby(~ housecl , ~ inccat , svymean)))
homeowners_income_percent2019 <- scf_MIcombine(with(scf_design2019 ,
                                           svyby(~ housecl , ~ inccat , svymean)))

homeowners_race_percent <- scf_MIcombine(with(scf_design ,
                                         svyby(~ housecl , ~ race , svymean)))
homeowners_race_percent2019 <- scf_MIcombine(with(scf_design2019 ,
                                              svyby(~ housecl , ~ race , svymean)))

homeowners_race_income_percent <- scf_MIcombine(with(scf_design ,
                                              svyby(~ housecl , ~ racecat , svymean)))
homeowners_race_income_percent2019 <- scf_MIcombine(with(scf_design2019 ,
                                                  svyby(~ housecl , ~ racecat , svymean)))


homeownership <- tibble( 
  year = c(
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019
  ),
  race = c(
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other"
  ),
  income = c(
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
  ),
  percent = c(
    #overall
    as.numeric(homeowners_percent[1]),
    homeowners_income_percent[[1]][[1]], #by income
    homeowners_income_percent[[1]][[2]],
    homeowners_income_percent[[1]][[3]],
    homeowners_income_percent[[1]][[4]],
    homeowners_income_percent[[1]][[5]],
    
    #white
    homeowners_race_percent[[1]][[1]],
    homeowners_race_income_percent[[1]][[1]],
    homeowners_race_income_percent[[1]][[2]],
    homeowners_race_income_percent[[1]][[3]],
    homeowners_race_income_percent[[1]][[4]],
    homeowners_race_income_percent[[1]][[5]],
    
    #Black
    homeowners_race_percent[[1]][[2]],
    homeowners_race_income_percent[[1]][[6]],
    homeowners_race_income_percent[[1]][[7]],
    homeowners_race_income_percent[[1]][[8]],
    homeowners_race_income_percent[[1]][[9]],
    homeowners_race_income_percent[[1]][[10]],
    
    #Hispanic
    homeowners_race_percent[[1]][[3]],
    homeowners_race_income_percent[[1]][[11]],
    homeowners_race_income_percent[[1]][[12]],
    homeowners_race_income_percent[[1]][[13]],
    homeowners_race_income_percent[[1]][[14]],
    homeowners_race_income_percent[[1]][[15]],
    
    #Asian
    homeowners_race_percent[[1]][[4]],
    NA,
    NA,
    NA,
    NA,
    NA,
    
    #Other
    homeowners_race_percent[[1]][[5]],
    homeowners_race_income_percent[[1]][[16]],
    homeowners_race_income_percent[[1]][[17]],
    homeowners_race_income_percent[[1]][[18]],
    homeowners_race_income_percent[[1]][[19]],
    homeowners_race_income_percent[[1]][[20]],
    
    #2019
    #overall
    as.numeric(homeowners_percent2019[[1]]),
    homeowners_income_percent2019[[1]][[1]], #by income
    homeowners_income_percent2019[[1]][[2]],
    homeowners_income_percent2019[[1]][[3]],
    homeowners_income_percent2019[[1]][[4]],
    homeowners_income_percent2019[[1]][[5]],
    
    #white
    homeowners_race_percent2019[[1]][[1]],
    homeowners_race_income_percent2019[[1]][[1]],
    homeowners_race_income_percent2019[[1]][[2]],
    homeowners_race_income_percent2019[[1]][[3]],
    homeowners_race_income_percent2019[[1]][[4]],
    homeowners_race_income_percent2019[[1]][[5]],
    
    #Black
    homeowners_race_percent2019[[1]][[2]],
    homeowners_race_income_percent2019[[1]][[6]],
    homeowners_race_income_percent2019[[1]][[7]],
    homeowners_race_income_percent2019[[1]][[8]],
    homeowners_race_income_percent2019[[1]][[9]],
    homeowners_race_income_percent2019[[1]][[10]],
    
    #Hispanic
    homeowners_race_percent2019[[1]][[3]],
    homeowners_race_income_percent2019[[1]][[11]],
    homeowners_race_income_percent2019[[1]][[12]],
    homeowners_race_income_percent2019[[1]][[13]],
    homeowners_race_income_percent2019[[1]][[14]],
    homeowners_race_income_percent2019[[1]][[15]],

    #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    
    #Other
    homeowners_race_percent2019[[1]][[4]],
    homeowners_race_income_percent2019[[1]][[16]],
    homeowners_race_income_percent2019[[1]][[17]],
    homeowners_race_income_percent2019[[1]][[18]],
    homeowners_race_income_percent2019[[1]][[19]],
    homeowners_race_income_percent2019[[1]][[20]]
  ),
  
  low = c(
    confint(homeowners_percent)[1], #Overall
    confint(homeowners_income_percent)[1], #By Income
    confint(homeowners_income_percent)[2],
    confint(homeowners_income_percent)[3],
    confint(homeowners_income_percent)[4],
    confint(homeowners_income_percent)[5],
    
    confint(homeowners_race_percent)[1], #White
    confint(homeowners_race_income_percent)[1],
    confint(homeowners_race_income_percent)[2],
    confint(homeowners_race_income_percent)[3],
    confint(homeowners_race_income_percent)[4],
    confint(homeowners_race_income_percent)[5],
    
    confint(homeowners_race_percent)[2], #Black
    confint(homeowners_race_income_percent)[6],
    confint(homeowners_race_income_percent)[7],
    confint(homeowners_race_income_percent)[8],
    confint(homeowners_race_income_percent)[9],
    confint(homeowners_race_income_percent)[10],
    
    confint(homeowners_race_percent)[3], #Hispanic
    confint(homeowners_race_income_percent)[11],
    confint(homeowners_race_income_percent)[12],
    confint(homeowners_race_income_percent)[13],
    confint(homeowners_race_income_percent)[14],
    confint(homeowners_race_income_percent)[15],
    
    confint(homeowners_race_percent)[4], #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(homeowners_race_percent)[5], #Other
    confint(homeowners_race_income_percent)[16],
    confint(homeowners_race_income_percent)[17],
    confint(homeowners_race_income_percent)[18],
    confint(homeowners_race_income_percent)[19],
    confint(homeowners_race_income_percent)[20],
    
    #2019
    confint(homeowners_percent2019)[1], #Overall
    confint(homeowners_income_percent2019)[1], #By Income
    confint(homeowners_income_percent2019)[2],
    confint(homeowners_income_percent2019)[3],
    confint(homeowners_income_percent2019)[4],
    confint(homeowners_income_percent2019)[5],
    
    confint(homeowners_race_percent2019)[1], #White
    confint(homeowners_race_income_percent2019)[1],
    confint(homeowners_race_income_percent2019)[2],
    confint(homeowners_race_income_percent2019)[3],
    confint(homeowners_race_income_percent2019)[4],
    confint(homeowners_race_income_percent2019)[5],
    
    confint(homeowners_race_percent2019)[2], #Black
    confint(homeowners_race_income_percent2019)[6],
    confint(homeowners_race_income_percent2019)[7],
    confint(homeowners_race_income_percent2019)[8],
    confint(homeowners_race_income_percent2019)[9],
    confint(homeowners_race_income_percent2019)[10],
    
    confint(homeowners_race_percent2019)[3], #Hispanic
    confint(homeowners_race_income_percent2019)[11],
    confint(homeowners_race_income_percent2019)[12],
    confint(homeowners_race_income_percent2019)[13],
    confint(homeowners_race_income_percent2019)[14],
    confint(homeowners_race_income_percent2019)[15],
    
    NA, #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(homeowners_race_percent2019)[4], #Other
    confint(homeowners_race_income_percent2019)[16],
    confint(homeowners_race_income_percent2019)[17],
    confint(homeowners_race_income_percent2019)[18],
    confint(homeowners_race_income_percent2019)[19],
    confint(homeowners_race_income_percent2019)[20]
    
    
  ),
  high = c(
    confint(homeowners_percent)[2], #Overall
    confint(homeowners_income_percent)[6], #By income
    confint(homeowners_income_percent)[7],
    confint(homeowners_income_percent)[8],
    confint(homeowners_income_percent)[9],
    confint(homeowners_income_percent)[10],
    
    confint(homeowners_race_percent)[6], #White
    confint(homeowners_race_income_percent)[21],
    confint(homeowners_race_income_percent)[22],
    confint(homeowners_race_income_percent)[23],
    confint(homeowners_race_income_percent)[24],
    confint(homeowners_race_income_percent)[25],
    
    confint(homeowners_race_percent)[7], #Black
    confint(homeowners_race_income_percent)[26],
    confint(homeowners_race_income_percent)[27],
    confint(homeowners_race_income_percent)[28],
    confint(homeowners_race_income_percent)[29],
    confint(homeowners_race_income_percent)[30],
    
    confint(homeowners_race_percent)[8], #Hispanic
    confint(homeowners_race_income_percent)[31],
    confint(homeowners_race_income_percent)[32],
    confint(homeowners_race_income_percent)[33],
    confint(homeowners_race_income_percent)[34],
    confint(homeowners_race_income_percent)[35],
    
    confint(homeowners_race_percent)[9], #Asian
    NA,
    NA,
    NA,
    NA,
    NA,

    confint(homeowners_race_percent)[10], #Other
    confint(homeowners_race_income_percent)[36],
    confint(homeowners_race_income_percent)[37],
    confint(homeowners_race_income_percent)[38],
    confint(homeowners_race_income_percent)[39],
    confint(homeowners_race_income_percent)[40],
    
    #2019
    confint(homeowners_percent2019)[2], #Overall
    confint(homeowners_income_percent2019)[6], #By income
    confint(homeowners_income_percent2019)[7],
    confint(homeowners_income_percent2019)[8],
    confint(homeowners_income_percent2019)[9],
    confint(homeowners_income_percent2019)[10],
    
    confint(homeowners_race_percent2019)[5], #White
    confint(homeowners_race_income_percent2019)[21],
    confint(homeowners_race_income_percent2019)[22],
    confint(homeowners_race_income_percent2019)[23],
    confint(homeowners_race_income_percent2019)[24],
    confint(homeowners_race_income_percent2019)[25],
    
    confint(homeowners_race_percent2019)[6], #Black
    confint(homeowners_race_income_percent2019)[26],
    confint(homeowners_race_income_percent2019)[27],
    confint(homeowners_race_income_percent2019)[28],
    confint(homeowners_race_income_percent2019)[29],
    confint(homeowners_race_income_percent2019)[30],
    
    confint(homeowners_race_percent2019)[7], #Hispanic
    confint(homeowners_race_income_percent2019)[31],
    confint(homeowners_race_income_percent2019)[32],
    confint(homeowners_race_income_percent2019)[33],
    confint(homeowners_race_income_percent2019)[34],
    confint(homeowners_race_income_percent2019)[35],
    
    NA, #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(homeowners_race_percent2019)[8], #Other
    confint(homeowners_race_income_percent2019)[36],
    confint(homeowners_race_income_percent2019)[37],
    confint(homeowners_race_income_percent2019)[38],
    confint(homeowners_race_income_percent2019)[39],
    confint(homeowners_race_income_percent2019)[40]
  )
  
)

write_csv(
  homeownership,
  "2. Data Output/homeownership.csv"
)

###########################################
#My analysis: home equity
###########################################

#Restrict the survey design to homeowners:

homeowners_scf_design <- subset(scf_design , housecl == 1)
homeowners_scf_design2019 <- subset(scf_design2019 , housecl == 1)
#Restrict the survey design to non asian non other:

nonasianother_scf_design <- subset(scf_design , 
                                   race %in% c(
                                     "white",
                                     "black",
                                     "hispanic"
                                   )
)
nonasianother_scf_design2019 <- subset(scf_design2019 , 
                                       race %in% c(
                                         "white",
                                         "black",
                                         "hispanic")
)

#restrict the survey to non asian non other homeowners
homeowners_nonasianother_scf_design <- subset(homeowners_scf_design , 
                                   race %in% c(
                                     "white",
                                     "black",
                                     "hispanic"
                                   )
)
homeowners_nonasianother_scf_design2019 <- subset(homeowners_scf_design2019 , 
                                       race %in% c(
                                         "white",
                                         "black",
                                         "hispanic")
)

  #Calculate Median Net worth by income
median_net_worth_by_homeownership <- scf_MIcombine(with(
  scf_design ,
  svyby(
    ~ networth ,
    ~ housecl ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

median_net_worth_by_homeownership2019 <-   scf_MIcombine(with(
  scf_design2019 ,
  svyby(
    ~ networth ,
    ~ housecl ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

#Calculate the median home equity

median_homeeq <-  scf_MIcombine(with(
  homeowners_scf_design ,
  svyquantile(~ homeeq ,
              0.5 , se = TRUE , interval.type = 'quantile')
)) 

median_homeeq2019 <-  scf_MIcombine(with(
  homeowners_scf_design2019 ,
  svyquantile(~ homeeq ,
              0.5 , se = TRUE , interval.type = 'quantile')
))


#Calculate Median Net worth by income
median_net_worth_by_homeownerinc <-   scf_MIcombine(with(
  scf_design ,
  svyby(
    ~ networth ,
    ~ homeownerinc ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

median_net_worth_by_homeownerinc2019 <-   scf_MIcombine(with(
  scf_design2019 ,
  svyby(
    ~ networth ,
    ~ homeownerinc ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

#Calculate home equity by income
median_homeeq_by_homeownerinc <-   scf_MIcombine(with(
  homeowners_scf_design ,
  svyby(
    ~ homeeq ,
    ~ homeownerinc ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

median_homeeq_by_homeownerinc2019 <-   scf_MIcombine(with(
  homeowners_scf_design2019 ,
  svyby(
    ~ homeeq ,
    ~ homeownerinc ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

#Calculate Median Net worth by race
median_net_worth_by_homeownerrace <-  scf_MIcombine(with(
  scf_design ,
  svyby(
    ~ networth ,
    ~ homeownerrace ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

median_net_worth_by_homeownerrace2019 <-  scf_MIcombine(with(
  scf_design2019 ,
  svyby(
    ~ networth ,
    ~ homeownerrace ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

#Calculate home equity by race
median_homeeq_by_homeownerrace <-   scf_MIcombine(with(
  homeowners_scf_design ,
  svyby(
    ~ homeeq ,
    ~ homeownerrace ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

median_homeeq_by_homeownerrace2019 <-   scf_MIcombine(with(
  homeowners_scf_design2019 ,
  svyby(
    ~ homeeq ,
    ~ homeownerrace ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

#Calculate Median Net worth by racecat


median_net_worth_by_homeownerracecat <-  scf_MIcombine(with(
  scf_design ,
  svyby(
    ~ networth ,
    ~ homeownerracecat ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))


median_net_worth_by_homeownerracecat2019 <-  scf_MIcombine(with(
  scf_design2019 ,
  svyby(
    ~ networth ,
    ~ homeownerracecat ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))
#Calculate home equity by racecat
median_homeeq_by_homeownerracecat <-   scf_MIcombine(with(
  homeowners_nonasianother_scf_design ,
  svyby(
    ~ homeeq ,
    ~ homeownerracecat ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

median_homeeq_by_homeownerracecat2019 <-   scf_MIcombine(with(
  homeowners_nonasianother_scf_design2019 ,
  svyby(
    ~ homeeq ,
    ~ homeownerracecat ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))


homeowner_net_worth <- tibble( 
  year = c(
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019
  ),
  race = c(
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    
   
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
   
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
   
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
   
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
   
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
   
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other"
  ),
  income = c(
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
  ),
  homeowner = c(
    1,1,1,1,1,1,
    0,0,0,0,0,0,
    1,1,1,1,1,1,
    0,0,0,0,0,0,
    1,1,1,1,1,1,
    0,0,0,0,0,0,
    1,1,1,1,1,1,
    0,0,0,0,0,0,
    1,1,1,1,1,1,
    0,0,0,0,0,0,
    1,1,1,1,1,1,
    0,0,0,0,0,0,
    1,1,1,1,1,1,
    0,0,0,0,0,0,
    1,1,1,1,1,1,
    0,0,0,0,0,0,
    1,1,1,1,1,1,
    0,0,0,0,0,0,
    1,1,1,1,1,1,
    0,0,0,0,0,0,
    1,1,1,1,1,1,
    0,0,0,0,0,0,
    1,1,1,1,1,1,
    0,0,0,0,0,0
  ),
  median = c(
    median_net_worth_by_homeownership[[1]][[2]], #Overall Homeowners
    median_net_worth_by_homeownerinc[[1]][[1]], #By income
    median_net_worth_by_homeownerinc[[1]][[2]],
    median_net_worth_by_homeownerinc[[1]][[3]],
    median_net_worth_by_homeownerinc[[1]][[4]],
    median_net_worth_by_homeownerinc[[1]][[5]],
    
    median_net_worth_by_homeownership[[1]][[1]], #Overall nonomeowners
    median_net_worth_by_homeownerinc[[1]][[6]], #By income
    median_net_worth_by_homeownerinc[[1]][[7]],
    median_net_worth_by_homeownerinc[[1]][[8]],
    median_net_worth_by_homeownerinc[[1]][[9]],
    median_net_worth_by_homeownerinc[[1]][[10]],
    
    median_net_worth_by_homeownerrace[[1]][[1]], #White
    median_net_worth_by_homeownerracecat[[1]][[1]],
    median_net_worth_by_homeownerracecat[[1]][[2]],
    median_net_worth_by_homeownerracecat[[1]][[3]],
    median_net_worth_by_homeownerracecat[[1]][[4]],
    median_net_worth_by_homeownerracecat[[1]][[5]],

    median_net_worth_by_homeownerrace[[1]][[6]], #White
    median_net_worth_by_homeownerracecat[[1]][[21]],
    median_net_worth_by_homeownerracecat[[1]][[22]],
    median_net_worth_by_homeownerracecat[[1]][[23]],
    median_net_worth_by_homeownerracecat[[1]][[24]],
    median_net_worth_by_homeownerracecat[[1]][[25]],

    median_net_worth_by_homeownerrace[[1]][[2]], #Black
    median_net_worth_by_homeownerracecat[[1]][[6]],
    median_net_worth_by_homeownerracecat[[1]][[7]],
    median_net_worth_by_homeownerracecat[[1]][[8]],
    median_net_worth_by_homeownerracecat[[1]][[9]],
    median_net_worth_by_homeownerracecat[[1]][[10]],
    
    median_net_worth_by_homeownerrace[[1]][[7]], #Black
    median_net_worth_by_homeownerracecat[[1]][[26]],
    median_net_worth_by_homeownerracecat[[1]][[27]],
    median_net_worth_by_homeownerracecat[[1]][[28]],
    median_net_worth_by_homeownerracecat[[1]][[29]],
    median_net_worth_by_homeownerracecat[[1]][[30]],
    
    median_net_worth_by_homeownerrace[[1]][[3]], #Hispanic
    median_net_worth_by_homeownerracecat[[1]][[11]],
    median_net_worth_by_homeownerracecat[[1]][[12]],
    median_net_worth_by_homeownerracecat[[1]][[13]],
    median_net_worth_by_homeownerracecat[[1]][[14]],
    median_net_worth_by_homeownerracecat[[1]][[15]],
    
    median_net_worth_by_homeownerrace[[1]][[8]], #Hispanic
    median_net_worth_by_homeownerracecat[[1]][[31]],
    median_net_worth_by_homeownerracecat[[1]][[32]],
    median_net_worth_by_homeownerracecat[[1]][[33]],
    median_net_worth_by_homeownerracecat[[1]][[34]],
    median_net_worth_by_homeownerracecat[[1]][[35]],
    
    median_net_worth_by_homeownerrace[[1]][[4]],#Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    median_net_worth_by_homeownerrace[[1]][[9]],#Asian
    NA,
    NA,
    NA,
    NA,
    NA,

    median_net_worth_by_homeownerrace[[1]][[5]], #Other
    median_net_worth_by_homeownerracecat[[1]][[16]],
    median_net_worth_by_homeownerracecat[[1]][[17]],
    median_net_worth_by_homeownerracecat[[1]][[18]],
    median_net_worth_by_homeownerracecat[[1]][[19]],
    median_net_worth_by_homeownerracecat[[1]][[20]],
    
    median_net_worth_by_homeownerrace[[1]][[10]], #Other
    median_net_worth_by_homeownerracecat[[1]][[36]],
    median_net_worth_by_homeownerracecat[[1]][[37]],
    median_net_worth_by_homeownerracecat[[1]][[38]],
    median_net_worth_by_homeownerracecat[[1]][[39]],
    median_net_worth_by_homeownerracecat[[1]][[40]],
    
    median_net_worth_by_homeownership2019[[1]][[2]], #Overall 2019 Homeowners
    median_net_worth_by_homeownerinc2019[[1]][[1]], #By income
    median_net_worth_by_homeownerinc2019[[1]][[2]],
    median_net_worth_by_homeownerinc2019[[1]][[3]],
    median_net_worth_by_homeownerinc2019[[1]][[4]],
    median_net_worth_by_homeownerinc2019[[1]][[5]],
    
    median_net_worth_by_homeownership2019[[1]][[1]], #Overall 2019 Homeowners
    median_net_worth_by_homeownerinc2019[[1]][[6]], #By income
    median_net_worth_by_homeownerinc2019[[1]][[7]],
    median_net_worth_by_homeownerinc2019[[1]][[8]],
    median_net_worth_by_homeownerinc2019[[1]][[9]],
    median_net_worth_by_homeownerinc2019[[1]][[10]],
    
    median_net_worth_by_homeownerrace2019[[1]][[1]], #White homeowner
    median_net_worth_by_homeownerracecat2019[[1]][[1]],
    median_net_worth_by_homeownerracecat2019[[1]][[2]],
    median_net_worth_by_homeownerracecat2019[[1]][[3]],
    median_net_worth_by_homeownerracecat2019[[1]][[4]],
    median_net_worth_by_homeownerracecat2019[[1]][[5]],
    
    median_net_worth_by_homeownerrace2019[[1]][[5]], #White not homeowner
    median_net_worth_by_homeownerracecat2019[[1]][[21]],
    median_net_worth_by_homeownerracecat2019[[1]][[22]],
    median_net_worth_by_homeownerracecat2019[[1]][[23]],
    median_net_worth_by_homeownerracecat2019[[1]][[24]],
    median_net_worth_by_homeownerracecat2019[[1]][[25]],

    median_net_worth_by_homeownerrace2019[[1]][[2]], #Black homeowner
    median_net_worth_by_homeownerracecat2019[[1]][[6]],
    median_net_worth_by_homeownerracecat2019[[1]][[7]],
    median_net_worth_by_homeownerracecat2019[[1]][[8]],
    median_net_worth_by_homeownerracecat2019[[1]][[9]],
    median_net_worth_by_homeownerracecat2019[[1]][[10]],
    
    median_net_worth_by_homeownerrace2019[[1]][[7]], #Black not homeowner
    median_net_worth_by_homeownerracecat2019[[1]][[26]],
    median_net_worth_by_homeownerracecat2019[[1]][[27]],
    median_net_worth_by_homeownerracecat2019[[1]][[28]],
    median_net_worth_by_homeownerracecat2019[[1]][[29]],
    median_net_worth_by_homeownerracecat2019[[1]][[30]],
    
    median_net_worth_by_homeownerrace2019[[1]][[3]], #Hispanic homeowner
    median_net_worth_by_homeownerracecat2019[[1]][[11]],
    median_net_worth_by_homeownerracecat2019[[1]][[12]],
    median_net_worth_by_homeownerracecat2019[[1]][[13]],
    median_net_worth_by_homeownerracecat2019[[1]][[14]],
    median_net_worth_by_homeownerracecat2019[[1]][[15]],
    
    median_net_worth_by_homeownerrace2019[[1]][[7]], #Hispanic not homeowner
    median_net_worth_by_homeownerracecat2019[[1]][[31]],
    median_net_worth_by_homeownerracecat2019[[1]][[32]],
    median_net_worth_by_homeownerracecat2019[[1]][[33]],
    median_net_worth_by_homeownerracecat2019[[1]][[34]],
    median_net_worth_by_homeownerracecat2019[[1]][[35]],
    
    NA, #Asian not present in data
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,

    median_net_worth_by_homeownerrace2019[[1]][[4]], #Other Homeowner
    median_net_worth_by_homeownerracecat2019[[1]][[16]],
    median_net_worth_by_homeownerracecat2019[[1]][[17]],
    median_net_worth_by_homeownerracecat2019[[1]][[18]],
    median_net_worth_by_homeownerracecat2019[[1]][[19]],
    median_net_worth_by_homeownerracecat2019[[1]][[20]],
    
    median_net_worth_by_homeownerrace2019[[1]][[8]], #Other not homeowner
    median_net_worth_by_homeownerracecat2019[[1]][[36]],
    median_net_worth_by_homeownerracecat2019[[1]][[37]],
    median_net_worth_by_homeownerracecat2019[[1]][[38]],
    median_net_worth_by_homeownerracecat2019[[1]][[39]],
    median_net_worth_by_homeownerracecat2019[[1]][[40]]
    
  ),
  
  low = c(
    confint(median_net_worth_by_homeownership)[2], #Overall homeowners
    confint(median_net_worth_by_homeownerinc)[1], #By Income
    confint(median_net_worth_by_homeownerinc)[2],
    confint(median_net_worth_by_homeownerinc)[3],
    confint(median_net_worth_by_homeownerinc)[4],
    confint(median_net_worth_by_homeownerinc)[5],
    
    confint(median_net_worth_by_homeownership)[1], #Overall not homeowners
    confint(median_net_worth_by_homeownerinc)[6], #By Income
    confint(median_net_worth_by_homeownerinc)[7],
    confint(median_net_worth_by_homeownerinc)[8],
    confint(median_net_worth_by_homeownerinc)[9],
    confint(median_net_worth_by_homeownerinc)[10],
    
    confint(median_net_worth_by_homeownerrace)[1], #White homeowners
    confint(median_net_worth_by_homeownerracecat)[1],
    confint(median_net_worth_by_homeownerracecat)[2],
    confint(median_net_worth_by_homeownerracecat)[3],
    confint(median_net_worth_by_homeownerracecat)[4],
    confint(median_net_worth_by_homeownerracecat)[5],

    
    confint(median_net_worth_by_homeownerrace)[6], #White not homeowners
    confint(median_net_worth_by_homeownerracecat)[21],
    confint(median_net_worth_by_homeownerracecat)[22],
    confint(median_net_worth_by_homeownerracecat)[23],
    confint(median_net_worth_by_homeownerracecat)[24],
    confint(median_net_worth_by_homeownerracecat)[25],

    
    confint(median_net_worth_by_homeownerrace)[2], #Black homeowners
    confint(median_net_worth_by_homeownerracecat)[6],
    confint(median_net_worth_by_homeownerracecat)[7],
    confint(median_net_worth_by_homeownerracecat)[8],
    confint(median_net_worth_by_homeownerracecat)[9],
    confint(median_net_worth_by_homeownerracecat)[10],
    
    confint(median_net_worth_by_homeownerrace)[7], #Black not homeowners
    confint(median_net_worth_by_homeownerracecat)[26],
    confint(median_net_worth_by_homeownerracecat)[27],
    confint(median_net_worth_by_homeownerracecat)[28],
    confint(median_net_worth_by_homeownerracecat)[29],
    confint(median_net_worth_by_homeownerracecat)[30],
    
    confint(median_net_worth_by_homeownerrace)[3], #Hispanic homeowners
    confint(median_net_worth_by_homeownerracecat)[11],
    confint(median_net_worth_by_homeownerracecat)[12],
    confint(median_net_worth_by_homeownerracecat)[13],
    confint(median_net_worth_by_homeownerracecat)[14],
    confint(median_net_worth_by_homeownerracecat)[15],
    
    
    confint(median_net_worth_by_homeownerrace)[8], #Hispanic not homeowners
    confint(median_net_worth_by_homeownerracecat)[31],
    confint(median_net_worth_by_homeownerracecat)[32],
    confint(median_net_worth_by_homeownerracecat)[33],
    confint(median_net_worth_by_homeownerracecat)[34],
    confint(median_net_worth_by_homeownerracecat)[35],
  
    confint(median_net_worth_by_homeownerrace)[4], #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    confint(median_net_worth_by_homeownerrace)[9], #Asian not homeowner
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(median_net_worth_by_homeownerrace)[5], #Other homeowners
    confint(median_net_worth_by_homeownerracecat)[16],
    confint(median_net_worth_by_homeownerracecat)[17],
    confint(median_net_worth_by_homeownerracecat)[18],
    confint(median_net_worth_by_homeownerracecat)[19],
    confint(median_net_worth_by_homeownerracecat)[20],
    
    
    confint(median_net_worth_by_homeownerrace)[10], #Other not homeowners
    confint(median_net_worth_by_homeownerracecat)[36],
    confint(median_net_worth_by_homeownerracecat)[37],
    confint(median_net_worth_by_homeownerracecat)[38],
    confint(median_net_worth_by_homeownerracecat)[39],
    confint(median_net_worth_by_homeownerracecat)[40],
    
    #2019
    confint(median_net_worth_by_homeownership2019)[2], #Overall homeowners
    confint(median_net_worth_by_homeownerinc2019)[1], #By Income
    confint(median_net_worth_by_homeownerinc2019)[2],
    confint(median_net_worth_by_homeownerinc2019)[3],
    confint(median_net_worth_by_homeownerinc2019)[4],
    confint(median_net_worth_by_homeownerinc2019)[5],
    
    confint(median_net_worth_by_homeownership2019)[1], #Overall not homeowners
    confint(median_net_worth_by_homeownerinc2019)[6], #By Income
    confint(median_net_worth_by_homeownerinc2019)[7],
    confint(median_net_worth_by_homeownerinc2019)[8],
    confint(median_net_worth_by_homeownerinc2019)[9],
    confint(median_net_worth_by_homeownerinc2019)[10],
    
    confint(median_net_worth_by_homeownerrace2019)[1], #White homeowners
    confint(median_net_worth_by_homeownerracecat2019)[1],
    confint(median_net_worth_by_homeownerracecat2019)[2],
    confint(median_net_worth_by_homeownerracecat2019)[3],
    confint(median_net_worth_by_homeownerracecat2019)[4],
    confint(median_net_worth_by_homeownerracecat2019)[5],
    
    
    confint(median_net_worth_by_homeownerrace2019)[5], #White not homeowners
    confint(median_net_worth_by_homeownerracecat2019)[21],
    confint(median_net_worth_by_homeownerracecat2019)[22],
    confint(median_net_worth_by_homeownerracecat2019)[23],
    confint(median_net_worth_by_homeownerracecat2019)[24],
    confint(median_net_worth_by_homeownerracecat2019)[25],
    
    confint(median_net_worth_by_homeownerrace2019)[2], #Black
    confint(median_net_worth_by_homeownerracecat2019)[6],
    confint(median_net_worth_by_homeownerracecat2019)[7],
    confint(median_net_worth_by_homeownerracecat2019)[8],
    confint(median_net_worth_by_homeownerracecat2019)[9],
    confint(median_net_worth_by_homeownerracecat2019)[10],
    
    confint(median_net_worth_by_homeownerrace2019)[6], #Black not homeowners
    confint(median_net_worth_by_homeownerracecat2019)[26],
    confint(median_net_worth_by_homeownerracecat2019)[27],
    confint(median_net_worth_by_homeownerracecat2019)[28],
    confint(median_net_worth_by_homeownerracecat2019)[29],
    confint(median_net_worth_by_homeownerracecat2019)[30],
    
    confint(median_net_worth_by_homeownerrace2019)[3], #Hispanic
    confint(median_net_worth_by_homeownerracecat2019)[11],
    confint(median_net_worth_by_homeownerracecat2019)[12],
    confint(median_net_worth_by_homeownerracecat2019)[13],
    confint(median_net_worth_by_homeownerracecat2019)[14],
    confint(median_net_worth_by_homeownerracecat2019)[15],
    
    confint(median_net_worth_by_homeownerrace2019)[7], #Hispanic not homeowners
    confint(median_net_worth_by_homeownerracecat2019)[31],
    confint(median_net_worth_by_homeownerracecat2019)[32],
    confint(median_net_worth_by_homeownerracecat2019)[33],
    confint(median_net_worth_by_homeownerracecat2019)[34],
    confint(median_net_worth_by_homeownerracecat2019)[35],
    
    NA, #Asian Homeowner
    NA,
    NA,
    NA,
    NA,
    NA,
    
    NA, #Asian not homeowner
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(median_net_worth_by_homeownerrace2019)[4], #Other homeowner
    confint(median_net_worth_by_homeownerracecat2019)[16],
    confint(median_net_worth_by_homeownerracecat2019)[17],
    confint(median_net_worth_by_homeownerracecat2019)[18],
    confint(median_net_worth_by_homeownerracecat2019)[19],
    confint(median_net_worth_by_homeownerracecat2019)[20],
    
    confint(median_net_worth_by_homeownerrace2019)[8], #Other not homeowners
    confint(median_net_worth_by_homeownerracecat2019)[36],
    confint(median_net_worth_by_homeownerracecat2019)[37],
    confint(median_net_worth_by_homeownerracecat2019)[38],
    confint(median_net_worth_by_homeownerracecat2019)[39],
    confint(median_net_worth_by_homeownerracecat2019)[40]
    
    
  ),
  high = c(
    confint(median_net_worth_by_homeownership)[4], #Overall homeowners
    confint(median_net_worth_by_homeownerinc)[11], #By Income
    confint(median_net_worth_by_homeownerinc)[12],
    confint(median_net_worth_by_homeownerinc)[13],
    confint(median_net_worth_by_homeownerinc)[14],
    confint(median_net_worth_by_homeownerinc)[15],
    
    confint(median_net_worth_by_homeownership)[3], #Overall not homeowners
    confint(median_net_worth_by_homeownerinc)[16], #By Income
    confint(median_net_worth_by_homeownerinc)[17],
    confint(median_net_worth_by_homeownerinc)[18],
    confint(median_net_worth_by_homeownerinc)[19],
    confint(median_net_worth_by_homeownerinc)[20],
    
    confint(median_net_worth_by_homeownerrace)[11], #White homeowners
    confint(median_net_worth_by_homeownerracecat)[41],
    confint(median_net_worth_by_homeownerracecat)[42],
    confint(median_net_worth_by_homeownerracecat)[43],
    confint(median_net_worth_by_homeownerracecat)[44],
    confint(median_net_worth_by_homeownerracecat)[45],
    
    
    confint(median_net_worth_by_homeownerrace)[16], #White not homeowners
    confint(median_net_worth_by_homeownerracecat)[61],
    confint(median_net_worth_by_homeownerracecat)[62],
    confint(median_net_worth_by_homeownerracecat)[63],
    confint(median_net_worth_by_homeownerracecat)[64],
    confint(median_net_worth_by_homeownerracecat)[65],
    
    
    confint(median_net_worth_by_homeownerrace)[12], #Black homeowners
    confint(median_net_worth_by_homeownerracecat)[46],
    confint(median_net_worth_by_homeownerracecat)[47],
    confint(median_net_worth_by_homeownerracecat)[48],
    confint(median_net_worth_by_homeownerracecat)[49],
    confint(median_net_worth_by_homeownerracecat)[50],
    
    confint(median_net_worth_by_homeownerrace)[17], #Black not homeowners
    confint(median_net_worth_by_homeownerracecat)[66],
    confint(median_net_worth_by_homeownerracecat)[67],
    confint(median_net_worth_by_homeownerracecat)[68],
    confint(median_net_worth_by_homeownerracecat)[69],
    confint(median_net_worth_by_homeownerracecat)[70],
    
    confint(median_net_worth_by_homeownerrace)[13], #Hispanic homeowners
    confint(median_net_worth_by_homeownerracecat)[51],
    confint(median_net_worth_by_homeownerracecat)[52],
    confint(median_net_worth_by_homeownerracecat)[53],
    confint(median_net_worth_by_homeownerracecat)[54],
    confint(median_net_worth_by_homeownerracecat)[55],
    
    
    confint(median_net_worth_by_homeownerrace)[18], #Hispanic not homeowners
    confint(median_net_worth_by_homeownerracecat)[71],
    confint(median_net_worth_by_homeownerracecat)[72],
    confint(median_net_worth_by_homeownerracecat)[73],
    confint(median_net_worth_by_homeownerracecat)[74],
    confint(median_net_worth_by_homeownerracecat)[75],
    
    confint(median_net_worth_by_homeownerrace)[14], #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    confint(median_net_worth_by_homeownerrace)[19], #Asian not homeowner
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(median_net_worth_by_homeownerrace)[15], #Other homeowners
    confint(median_net_worth_by_homeownerracecat)[56],
    confint(median_net_worth_by_homeownerracecat)[57],
    confint(median_net_worth_by_homeownerracecat)[58],
    confint(median_net_worth_by_homeownerracecat)[59],
    confint(median_net_worth_by_homeownerracecat)[60],
    
    
    confint(median_net_worth_by_homeownerrace)[20], #Other not homeowners
    confint(median_net_worth_by_homeownerracecat)[76],
    confint(median_net_worth_by_homeownerracecat)[77],
    confint(median_net_worth_by_homeownerracecat)[78],
    confint(median_net_worth_by_homeownerracecat)[79],
    confint(median_net_worth_by_homeownerracecat)[80],
    
    #2019
    confint(median_net_worth_by_homeownership2019)[4], #Overall homeowners
    confint(median_net_worth_by_homeownerinc2019)[11], #By Income
    confint(median_net_worth_by_homeownerinc2019)[12],
    confint(median_net_worth_by_homeownerinc2019)[13],
    confint(median_net_worth_by_homeownerinc2019)[14],
    confint(median_net_worth_by_homeownerinc2019)[15],
    
    confint(median_net_worth_by_homeownership2019)[3], #Overall not homeowners
    confint(median_net_worth_by_homeownerinc2019)[16], #By Income
    confint(median_net_worth_by_homeownerinc2019)[17],
    confint(median_net_worth_by_homeownerinc2019)[18],
    confint(median_net_worth_by_homeownerinc2019)[19],
    confint(median_net_worth_by_homeownerinc2019)[20],
    
    confint(median_net_worth_by_homeownerrace2019)[9], #White homeowners
    confint(median_net_worth_by_homeownerracecat2019)[41],
    confint(median_net_worth_by_homeownerracecat2019)[42],
    confint(median_net_worth_by_homeownerracecat2019)[43],
    confint(median_net_worth_by_homeownerracecat2019)[44],
    confint(median_net_worth_by_homeownerracecat2019)[45],
    
    
    confint(median_net_worth_by_homeownerrace2019)[13], #White not homeowners
    confint(median_net_worth_by_homeownerracecat2019)[61],
    confint(median_net_worth_by_homeownerracecat2019)[62],
    confint(median_net_worth_by_homeownerracecat2019)[63],
    confint(median_net_worth_by_homeownerracecat2019)[64],
    confint(median_net_worth_by_homeownerracecat2019)[65],
    
    confint(median_net_worth_by_homeownerrace2019)[10], #Black homeowners
    confint(median_net_worth_by_homeownerracecat2019)[46],
    confint(median_net_worth_by_homeownerracecat2019)[47],
    confint(median_net_worth_by_homeownerracecat2019)[48],
    confint(median_net_worth_by_homeownerracecat2019)[49],
    confint(median_net_worth_by_homeownerracecat2019)[50],
    
    
    confint(median_net_worth_by_homeownerrace2019)[14], #Black not homeowners
    confint(median_net_worth_by_homeownerracecat2019)[66],
    confint(median_net_worth_by_homeownerracecat2019)[67],
    confint(median_net_worth_by_homeownerracecat2019)[68],
    confint(median_net_worth_by_homeownerracecat2019)[69],
    confint(median_net_worth_by_homeownerracecat2019)[70],
    
    confint(median_net_worth_by_homeownerrace2019)[11], #Hispanic homeowners
    confint(median_net_worth_by_homeownerracecat2019)[51],
    confint(median_net_worth_by_homeownerracecat2019)[52],
    confint(median_net_worth_by_homeownerracecat2019)[53],
    confint(median_net_worth_by_homeownerracecat2019)[54],
    confint(median_net_worth_by_homeownerracecat2019)[55],
    
    
    confint(median_net_worth_by_homeownerrace2019)[15], #Hispanic not homeowners
    confint(median_net_worth_by_homeownerracecat2019)[71],
    confint(median_net_worth_by_homeownerracecat2019)[72],
    confint(median_net_worth_by_homeownerracecat2019)[73],
    confint(median_net_worth_by_homeownerracecat2019)[74],
    confint(median_net_worth_by_homeownerracecat2019)[75],
     
    NA, #Asian Homeowner
    NA,
    NA,
    NA,
    NA,
    NA,
    NA, #Asian Not Homeowner
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(median_net_worth_by_homeownerrace2019)[12], #Other homeowners
    confint(median_net_worth_by_homeownerracecat2019)[56],
    confint(median_net_worth_by_homeownerracecat2019)[57],
    confint(median_net_worth_by_homeownerracecat2019)[58],
    confint(median_net_worth_by_homeownerracecat2019)[59],
    confint(median_net_worth_by_homeownerracecat2019)[60],
    
    
    confint(median_net_worth_by_homeownerrace2019)[16], #Other not homeowners
    confint(median_net_worth_by_homeownerracecat2019)[76],
    confint(median_net_worth_by_homeownerracecat2019)[77],
    confint(median_net_worth_by_homeownerracecat2019)[78],
    confint(median_net_worth_by_homeownerracecat2019)[79],
    confint(median_net_worth_by_homeownerracecat2019)[80]
  )
  
)

write_csv(
  homeowner_net_worth,
  "2. Data Output/homeowner_net_worth.csv"
)

#Home equity 
home_equity <- tibble( 
  year = c(
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019
  ),
  race = c(
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other"
  ),
  income = c(
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
  ),
  median = c(
    median_homeeq[[1]][1], #Overall
    median_homeeq_by_homeownerinc[[1]][[1]], #By income
    median_homeeq_by_homeownerinc[[1]][[2]],
    median_homeeq_by_homeownerinc[[1]][[3]],
    median_homeeq_by_homeownerinc[[1]][[4]],
    median_homeeq_by_homeownerinc[[1]][[5]],
    
    median_homeeq_by_homeownerrace[[1]][1], #White
    median_homeeq_by_homeownerracecat[[1]][1],
    median_homeeq_by_homeownerracecat[[1]][2],
    median_homeeq_by_homeownerracecat[[1]][3],
    median_homeeq_by_homeownerracecat[[1]][4],
    median_homeeq_by_homeownerracecat[[1]][5],
    
    median_homeeq_by_homeownerrace[[1]][2], #Black
    median_homeeq_by_homeownerracecat[[1]][6],
    median_homeeq_by_homeownerracecat[[1]][7],
    median_homeeq_by_homeownerracecat[[1]][8],
    median_homeeq_by_homeownerracecat[[1]][9],
    median_homeeq_by_homeownerracecat[[1]][10],
    
    median_homeeq_by_homeownerrace[[1]][3], #Hispanic
    median_homeeq_by_homeownerracecat[[1]][11],
    median_homeeq_by_homeownerracecat[[1]][12],
    median_homeeq_by_homeownerracecat[[1]][13],
    median_homeeq_by_homeownerracecat[[1]][14],
    median_homeeq_by_homeownerracecat[[1]][15],
    
    median_homeeq_by_homeownerrace[[1]][4],#Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    
    median_homeeq_by_homeownerrace[[1]][5], #Other
    NA,
    NA,
    NA,
    NA,
    NA,
    
    median_homeeq2019[[1]][1], #Overall 2019
    median_homeeq_by_homeownerinc2019[[1]][1], #By income
    median_homeeq_by_homeownerinc2019[[1]][2],
    median_homeeq_by_homeownerinc2019[[1]][3],
    median_homeeq_by_homeownerinc2019[[1]][4],
    median_homeeq_by_homeownerinc2019[[1]][5],
    
    median_homeeq_by_homeownerrace2019[[1]][1], #White
    median_homeeq_by_homeownerracecat2019[[1]][1],
    median_homeeq_by_homeownerracecat2019[[1]][2],
    median_homeeq_by_homeownerracecat2019[[1]][3],
    median_homeeq_by_homeownerracecat2019[[1]][4],
    median_homeeq_by_homeownerracecat2019[[1]][5],
    
    median_homeeq_by_homeownerrace2019[[1]][2],#Black
    median_homeeq_by_homeownerracecat2019[[1]][6],
    median_homeeq_by_homeownerracecat2019[[1]][7],
    median_homeeq_by_homeownerracecat2019[[1]][8],
    median_homeeq_by_homeownerracecat2019[[1]][9],
    median_homeeq_by_homeownerracecat2019[[1]][10],
    
    median_homeeq_by_homeownerrace2019[[1]][3],#Hispanic
    median_homeeq_by_homeownerracecat2019[[1]][11],
    median_homeeq_by_homeownerracecat2019[[1]][12],
    median_homeeq_by_homeownerracecat2019[[1]][13],
    median_homeeq_by_homeownerracecat2019[[1]][14],
    median_homeeq_by_homeownerracecat2019[[1]][15],
    
    NA, #Asian not present in data
    NA,
    NA,
    NA,
    NA,
    NA,
    
    median_homeeq_by_homeownerrace2019[[1]][4], #Other
    NA,
    NA,
    NA,
    NA,
    NA
    
  ),
  
  low = c(
    confint(median_homeeq)[1], #Overall
    confint(median_homeeq_by_homeownerinc)[1], #By Income
    confint(median_homeeq_by_homeownerinc)[2],
    confint(median_homeeq_by_homeownerinc)[3],
    confint(median_homeeq_by_homeownerinc)[4],
    confint(median_homeeq_by_homeownerinc)[5],
    
    confint(median_homeeq_by_homeownerrace)[1], #White
    confint(median_homeeq_by_homeownerracecat)[1],
    confint(median_homeeq_by_homeownerracecat)[2],
    confint(median_homeeq_by_homeownerracecat)[3],
    confint(median_homeeq_by_homeownerracecat)[4],
    confint(median_homeeq_by_homeownerracecat)[5],
    
    confint(median_homeeq_by_homeownerrace)[2], #Black
    confint(median_homeeq_by_homeownerracecat)[6],
    confint(median_homeeq_by_homeownerracecat)[7],
    confint(median_homeeq_by_homeownerracecat)[8],
    confint(median_homeeq_by_homeownerracecat)[9],
    confint(median_homeeq_by_homeownerracecat)[10],
    
    confint(median_homeeq_by_homeownerrace)[3], #Hispanic
    confint(median_homeeq_by_homeownerracecat)[11],
    confint(median_homeeq_by_homeownerracecat)[12],
    confint(median_homeeq_by_homeownerracecat)[13],
    confint(median_homeeq_by_homeownerracecat)[14],
    confint(median_homeeq_by_homeownerracecat)[15],
    
    confint(median_homeeq_by_homeownerrace)[4], #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(median_homeeq_by_homeownerrace)[5], #Other
    NA,
    NA,
    NA,
    NA,
    NA,
    
    #2019
    confint(median_homeeq2019)[1], #Overall
    confint(median_homeeq_by_homeownerinc2019)[1], #By Income
    confint(median_homeeq_by_homeownerinc2019)[2],
    confint(median_homeeq_by_homeownerinc2019)[3],
    confint(median_homeeq_by_homeownerinc2019)[4],
    confint(median_homeeq_by_homeownerinc2019)[5],
    
    confint(median_homeeq_by_homeownerrace2019)[1], #White
    confint(median_homeeq_by_homeownerracecat2019)[1],
    confint(median_homeeq_by_homeownerracecat2019)[2],
    confint(median_homeeq_by_homeownerracecat2019)[3],
    confint(median_homeeq_by_homeownerracecat2019)[4],
    confint(median_homeeq_by_homeownerracecat2019)[5],
    
    confint(median_homeeq_by_homeownerrace2019)[2], #Black
    confint(median_homeeq_by_homeownerracecat2019)[6],
    confint(median_homeeq_by_homeownerracecat2019)[7],
    confint(median_homeeq_by_homeownerracecat2019)[8],
    confint(median_homeeq_by_homeownerracecat2019)[9],
    confint(median_homeeq_by_homeownerracecat2019)[10],
    
    confint(median_homeeq_by_homeownerrace2019)[3], #Hispanic
    confint(median_homeeq_by_homeownerracecat2019)[11],
    confint(median_homeeq_by_homeownerracecat2019)[12],
    confint(median_homeeq_by_homeownerracecat2019)[13],
    confint(median_homeeq_by_homeownerracecat2019)[14],
    confint(median_homeeq_by_homeownerracecat2019)[15],
    
    NA, #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(median_homeeq_by_homeownerrace2019)[4], #Other
    NA,
    NA,
    NA,
    NA,
    NA
    
    
  ),
  high = c(
    confint(median_homeeq)[2], #Overall
    confint(median_homeeq_by_homeownerinc)[6], #By income
    confint(median_homeeq_by_homeownerinc)[7],
    confint(median_homeeq_by_homeownerinc)[8],
    confint(median_homeeq_by_homeownerinc)[9],
    confint(median_homeeq_by_homeownerinc)[10],
    
    confint(median_homeeq_by_homeownerrace)[6], #White
    confint(median_homeeq_by_homeownerracecat)[16],
    confint(median_homeeq_by_homeownerracecat)[17],
    confint(median_homeeq_by_homeownerracecat)[18],
    confint(median_homeeq_by_homeownerracecat)[19],
    confint(median_homeeq_by_homeownerracecat)[20],
    
    confint(median_homeeq_by_homeownerrace)[7], #Black
    confint(median_homeeq_by_homeownerracecat)[21],
    confint(median_homeeq_by_homeownerracecat)[22],
    confint(median_homeeq_by_homeownerracecat)[23],
    confint(median_homeeq_by_homeownerracecat)[24],
    confint(median_homeeq_by_homeownerracecat)[25],
    
    confint(median_homeeq_by_homeownerrace)[8], #Hispanic
    confint(median_homeeq_by_homeownerracecat)[26],
    confint(median_homeeq_by_homeownerracecat)[27],
    confint(median_homeeq_by_homeownerracecat)[28],
    confint(median_homeeq_by_homeownerracecat)[29],
    confint(median_homeeq_by_homeownerracecat)[30],
    
    confint(median_homeeq_by_homeownerrace)[9], #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(median_homeeq_by_homeownerrace)[10], #Other
    NA,
    NA,
    NA,
    NA,
    NA,
    
    #2019
    confint(median_homeeq2019)[2], #Overall
    confint(median_homeeq_by_homeownerinc2019)[6], #By income
    confint(median_homeeq_by_homeownerinc2019)[7],
    confint(median_homeeq_by_homeownerinc2019)[8],
    confint(median_homeeq_by_homeownerinc2019)[9],
    confint(median_homeeq_by_homeownerinc2019)[10],
    
    confint(median_homeeq_by_homeownerrace2019)[5], #White
    confint(median_homeeq_by_homeownerracecat2019)[16],
    confint(median_homeeq_by_homeownerracecat2019)[17],
    confint(median_homeeq_by_homeownerracecat2019)[18],
    confint(median_homeeq_by_homeownerracecat2019)[19],
    confint(median_homeeq_by_homeownerracecat2019)[20],
    
    confint(median_homeeq_by_homeownerrace2019)[6], #Black
    confint(median_homeeq_by_homeownerracecat2019)[21],
    confint(median_homeeq_by_homeownerracecat2019)[22],
    confint(median_homeeq_by_homeownerracecat2019)[23],
    confint(median_homeeq_by_homeownerracecat2019)[24],
    confint(median_homeeq_by_homeownerracecat2019)[25],
    
    confint(median_homeeq_by_homeownerrace2019)[7], #Hispanic
    confint(median_homeeq_by_homeownerracecat2019)[26],
    confint(median_homeeq_by_homeownerracecat2019)[27],
    confint(median_homeeq_by_homeownerracecat2019)[28],
    confint(median_homeeq_by_homeownerracecat2019)[29],
    confint(median_homeeq_by_homeownerracecat2019)[30],
    
    NA, #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(median_homeeq_by_homeownerrace2019)[8], #Other
    NA,
    NA,
    NA,
    NA,
    NA
  )
  
)

write_csv(
  home_equity,
  "2. Data Output/home_equity.csv"
)

###########################################
#My analysis: Net worth
###########################################

    #Calculate Median Net worth 
median_net_worth <-  scf_MIcombine(with(
    scf_design ,
    svyquantile(~ networth ,
                0.5 , se = TRUE , interval.type = 'quantile')
  ))

median_net_worth2019 <-  scf_MIcombine(with(
    scf_design2019 ,
    svyquantile(~ networth ,
                0.5 , se = TRUE , interval.type = 'quantile')
  ))    


  #Calculate Median Net worth by income
median_net_worth_by_income <-   scf_MIcombine(with(
    scf_design ,
    svyby(
      ~ networth ,
      ~ inccat ,
      svyquantile ,
      0.5 ,
      se = TRUE ,
      interval.type = 'quantile' ,
      ci = TRUE
    )
  ))

median_net_worth_by_income2019 <-   scf_MIcombine(with(
  scf_design2019 ,
  svyby(
    ~ networth ,
    ~ inccat ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))
  
  #Calculate Median Net worth by race
median_net_worth_by_race <-  scf_MIcombine(with(
    scf_design ,
    svyby(
      ~ networth ,
      ~ race ,
      svyquantile ,
      0.5 ,
      se = TRUE ,
      interval.type = 'quantile' ,
      ci = TRUE
    )
  ))
  
median_net_worth_by_race2019 <-  scf_MIcombine(with(
  scf_design2019 ,
  svyby(
    ~ networth ,
    ~ race ,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

  #Calculate Median Net worth by race by income
median_net_worth_by_incomerace <-  scf_MIcombine(with(
    scf_design ,
    svyby(
      ~ networth ,
      ~ racecat,
      svyquantile ,
      0.5 ,
      se = TRUE ,
      interval.type = 'quantile' ,
      ci = TRUE
    )
  ))

median_net_worth_by_incomerace2019 <-  scf_MIcombine(with(
  scf_design2019 ,
  svyby(
    ~ networth ,
    ~ racecat,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))


net_worth <- tibble( 
  year = c(
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2022,2022,2022,2022,2022,2022,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019,
    2019,2019,2019,2019,2019,2019
    ),
  race = c(
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "Overall",
    "White",
    "White",
    "White",
    "White",
    "White",
    "White",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Black",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Hispanic",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Asian",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other"
  ),
  income = c(
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100",
    "Overall", "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
  ),
  median = c(
    median_net_worth[[1]][1], #Overall
    median_net_worth_by_income[[1]][1], #By income
    median_net_worth_by_income[[1]][2],
    median_net_worth_by_income[[1]][3],
    median_net_worth_by_income[[1]][4],
    median_net_worth_by_income[[1]][5],

    median_net_worth_by_race[[1]][1], #White
    median_net_worth_by_incomerace[[1]][1],
    median_net_worth_by_incomerace[[1]][2],
    median_net_worth_by_incomerace[[1]][3],
    median_net_worth_by_incomerace[[1]][4],
    median_net_worth_by_incomerace[[1]][5],

    median_net_worth_by_race[[1]][2], #Black
    median_net_worth_by_incomerace[[1]][6],
    median_net_worth_by_incomerace[[1]][7],
    median_net_worth_by_incomerace[[1]][8],
    median_net_worth_by_incomerace[[1]][9],
    median_net_worth_by_incomerace[[1]][10],

    median_net_worth_by_race[[1]][3], #Hispanic
    median_net_worth_by_incomerace[[1]][11],
    median_net_worth_by_incomerace[[1]][12],
    median_net_worth_by_incomerace[[1]][13],
    median_net_worth_by_incomerace[[1]][14],
    median_net_worth_by_incomerace[[1]][15],

    median_net_worth_by_race[[1]][4],#Asian
    NA,
    NA,
    NA,
    NA,
    NA,

    median_net_worth_by_race[[1]][5], #Other
    median_net_worth_by_incomerace[[1]][16],
    median_net_worth_by_incomerace[[1]][17],
    median_net_worth_by_incomerace[[1]][18],
    median_net_worth_by_incomerace[[1]][19],
    median_net_worth_by_incomerace[[1]][20],

    median_net_worth2019[[1]][1], #Overall 2019
    median_net_worth_by_income2019[[1]][1], #By income
    median_net_worth_by_income2019[[1]][2],
    median_net_worth_by_income2019[[1]][3],
    median_net_worth_by_income2019[[1]][4],
    median_net_worth_by_income2019[[1]][5],

    median_net_worth_by_race2019[[1]][1], #White
    median_net_worth_by_incomerace2019[[1]][1],
    median_net_worth_by_incomerace2019[[1]][2],
    median_net_worth_by_incomerace2019[[1]][3],
    median_net_worth_by_incomerace2019[[1]][4],
    median_net_worth_by_incomerace2019[[1]][5],

    median_net_worth_by_race2019[[1]][2],#Black
    median_net_worth_by_incomerace2019[[1]][6],
    median_net_worth_by_incomerace2019[[1]][7],
    median_net_worth_by_incomerace2019[[1]][8],
    median_net_worth_by_incomerace2019[[1]][9],
    median_net_worth_by_incomerace2019[[1]][10],

    median_net_worth_by_race2019[[1]][3],#Hispanic
    median_net_worth_by_incomerace2019[[1]][11],
    median_net_worth_by_incomerace2019[[1]][12],
    median_net_worth_by_incomerace2019[[1]][13],
    median_net_worth_by_incomerace2019[[1]][14],
    median_net_worth_by_incomerace2019[[1]][15],

    NA, #Asian not present in data
    NA,
    NA,
    NA,
    NA,
    NA,
    
    median_net_worth_by_race2019[[1]][4], #Other
    median_net_worth_by_incomerace2019[[1]][16],
    median_net_worth_by_incomerace2019[[1]][17],
    median_net_worth_by_incomerace2019[[1]][18],
    median_net_worth_by_incomerace2019[[1]][19],
    median_net_worth_by_incomerace2019[[1]][20]
    
  ),
  
  low = c(
    confint(median_net_worth)[1], #Overall
    confint(median_net_worth_by_income)[1], #By Income
    confint(median_net_worth_by_income)[2],
    confint(median_net_worth_by_income)[3],
    confint(median_net_worth_by_income)[4],
    confint(median_net_worth_by_income)[5],

    confint(median_net_worth_by_race)[1], #White
    confint(median_net_worth_by_incomerace)[1],
    confint(median_net_worth_by_incomerace)[2],
    confint(median_net_worth_by_incomerace)[3],
    confint(median_net_worth_by_incomerace)[4],
    confint(median_net_worth_by_incomerace)[5],

    confint(median_net_worth_by_race)[2], #Black
    confint(median_net_worth_by_incomerace)[6],
    confint(median_net_worth_by_incomerace)[7],
    confint(median_net_worth_by_incomerace)[8],
    confint(median_net_worth_by_incomerace)[9],
    confint(median_net_worth_by_incomerace)[10],

    confint(median_net_worth_by_race)[3], #Hispanic
    confint(median_net_worth_by_incomerace)[11],
    confint(median_net_worth_by_incomerace)[12],
    confint(median_net_worth_by_incomerace)[13],
    confint(median_net_worth_by_incomerace)[14],
    confint(median_net_worth_by_incomerace)[15],

    confint(median_net_worth_by_race)[4], #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(median_net_worth_by_race)[5], #Other
    confint(median_net_worth_by_incomerace)[16],
    confint(median_net_worth_by_incomerace)[17],
    confint(median_net_worth_by_incomerace)[18],
    confint(median_net_worth_by_incomerace)[19],
    confint(median_net_worth_by_incomerace)[20],

    #2019
    confint(median_net_worth2019)[1], #Overall
    confint(median_net_worth_by_income2019)[1], #By Income
    confint(median_net_worth_by_income2019)[2],
    confint(median_net_worth_by_income2019)[3],
    confint(median_net_worth_by_income2019)[4],
    confint(median_net_worth_by_income2019)[5],

    confint(median_net_worth_by_race2019)[1], #White
    confint(median_net_worth_by_incomerace2019)[1],
    confint(median_net_worth_by_incomerace2019)[2],
    confint(median_net_worth_by_incomerace2019)[3],
    confint(median_net_worth_by_incomerace2019)[4],
    confint(median_net_worth_by_incomerace2019)[5],

    confint(median_net_worth_by_race2019)[2], #Black
    confint(median_net_worth_by_incomerace2019)[6],
    confint(median_net_worth_by_incomerace2019)[7],
    confint(median_net_worth_by_incomerace2019)[8],
    confint(median_net_worth_by_incomerace2019)[9],
    confint(median_net_worth_by_incomerace2019)[10],

    confint(median_net_worth_by_race2019)[3], #Hispanic
    confint(median_net_worth_by_incomerace2019)[11],
    confint(median_net_worth_by_incomerace2019)[12],
    confint(median_net_worth_by_incomerace2019)[13],
    confint(median_net_worth_by_incomerace2019)[14],
    confint(median_net_worth_by_incomerace2019)[15],

    NA, #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(median_net_worth_by_race2019)[4], #Other
    confint(median_net_worth_by_incomerace2019)[16],
    confint(median_net_worth_by_incomerace2019)[17],
    confint(median_net_worth_by_incomerace2019)[18],
    confint(median_net_worth_by_incomerace2019)[19],
    confint(median_net_worth_by_incomerace2019)[20]
    
   
  ),
  high = c(
    confint(median_net_worth)[2], #Overall
    confint(median_net_worth_by_income)[6], #By income
    confint(median_net_worth_by_income)[7],
    confint(median_net_worth_by_income)[8],
    confint(median_net_worth_by_income)[9],
    confint(median_net_worth_by_income)[10],

    confint(median_net_worth_by_race)[6], #White
    confint(median_net_worth_by_incomerace)[21],
    confint(median_net_worth_by_incomerace)[22],
    confint(median_net_worth_by_incomerace)[23],
    confint(median_net_worth_by_incomerace)[24],
    confint(median_net_worth_by_incomerace)[25],
    
    confint(median_net_worth_by_race)[7], #Black
    confint(median_net_worth_by_incomerace)[26],
    confint(median_net_worth_by_incomerace)[27],
    confint(median_net_worth_by_incomerace)[28],
    confint(median_net_worth_by_incomerace)[29],
    confint(median_net_worth_by_incomerace)[30],
    
    confint(median_net_worth_by_race)[8], #Hispanic
    confint(median_net_worth_by_incomerace)[31],
    confint(median_net_worth_by_incomerace)[32],
    confint(median_net_worth_by_incomerace)[33],
    confint(median_net_worth_by_incomerace)[34],
    confint(median_net_worth_by_incomerace)[35],
    
    confint(median_net_worth_by_race)[9], #Asian
    NA,
    NA,
    NA,
    NA,
    NA,

    confint(median_net_worth_by_race)[10], #Other
    confint(median_net_worth_by_incomerace)[36],
    confint(median_net_worth_by_incomerace)[37],
    confint(median_net_worth_by_incomerace)[38],
    confint(median_net_worth_by_incomerace)[39],
    confint(median_net_worth_by_incomerace)[40],
    
    #2019
    confint(median_net_worth2019)[2], #Overall
    confint(median_net_worth_by_income2019)[6], #By income
    confint(median_net_worth_by_income2019)[7],
    confint(median_net_worth_by_income2019)[8],
    confint(median_net_worth_by_income2019)[9],
    confint(median_net_worth_by_income2019)[10],
    
    confint(median_net_worth_by_race2019)[5], #White
    confint(median_net_worth_by_incomerace2019)[21],
    confint(median_net_worth_by_incomerace2019)[22],
    confint(median_net_worth_by_incomerace2019)[23],
    confint(median_net_worth_by_incomerace2019)[24],
    confint(median_net_worth_by_incomerace2019)[25],
    
    confint(median_net_worth_by_race2019)[6], #Black
    confint(median_net_worth_by_incomerace2019)[26],
    confint(median_net_worth_by_incomerace2019)[27],
    confint(median_net_worth_by_incomerace2019)[28],
    confint(median_net_worth_by_incomerace2019)[29],
    confint(median_net_worth_by_incomerace2019)[30],
    
    confint(median_net_worth_by_race2019)[7], #Hispanic
    confint(median_net_worth_by_incomerace2019)[31],
    confint(median_net_worth_by_incomerace2019)[32],
    confint(median_net_worth_by_incomerace2019)[33],
    confint(median_net_worth_by_incomerace2019)[34],
    confint(median_net_worth_by_incomerace2019)[35],
    
    NA, #Asian
    NA,
    NA,
    NA,
    NA,
    NA,
    
    confint(median_net_worth_by_race2019)[8], #Other
    confint(median_net_worth_by_incomerace2019)[36],
    confint(median_net_worth_by_incomerace2019)[37],
    confint(median_net_worth_by_incomerace2019)[38],
    confint(median_net_worth_by_incomerace2019)[39],
    confint(median_net_worth_by_incomerace2019)[40]
  )
  
)

write_csv(
  net_worth,
  "2. Data Output/net worth.csv"
  )


#Calculate Median Net worth by financial literacy
median_net_worth_by_finlit <-  scf_MIcombine(with(
  scf_design ,
  svyby(
    ~ networth ,
    ~ finlit,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

median_net_worth_by_finlit2019 <-  scf_MIcombine(with(
  scf_design2019 ,
  svyby(
    ~ networth ,
    ~ finlit,
    svyquantile ,
    0.5 ,
    se = TRUE ,
    interval.type = 'quantile' ,
    ci = TRUE
  )
))

#Calculate mean financial literacy 
mean_finlit <-  scf_MIcombine(with(scf_design , svymean(~ finlit)))

mean_finlit2019 <-  scf_MIcombine(with(scf_design2019 , svymean(~ finlit)))

#Calculate mean financial literacy by race
mean_finlit_by_race <-  scf_MIcombine(with(scf_design ,
                                             svyby(~ finlit , ~ race , svymean)))

mean_finlit_by_race2019 <-  scf_MIcombine(with(scf_design2019 ,
                                               svyby(~ finlit , ~ race , svymean)))

#Calculate financial literacy by income

mean_finlit_by_income <-  scf_MIcombine(with(scf_design ,
                                           svyby(~ finlit , ~ inccat , svymean)))

mean_finlit_by_income2019 <-  scf_MIcombine(with(scf_design2019 ,
                                               svyby(~ finlit , ~ inccat , svymean)))

#Calculate financial literacy by race and income
mean_finlit_by_racecat <-  scf_MIcombine(with(scf_design ,
                                           svyby(~ finlit , ~ racecat , svymean)))

mean_finlit_by_racecat2019 <-  scf_MIcombine(with(scf_design2019 ,
                                               svyby(~ finlit , ~ racecat , svymean)))
 
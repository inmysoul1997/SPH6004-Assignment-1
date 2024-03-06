install.packages("tableone")
install.packages("gradDescent")
install.packages("caTools")
install.packages("pROC")
install.packages("MASS")


# 01 Loading required library
library(tableone)
library(dplyr)
library(tidyverse)
library(gradDescent)
library(ggplot2)
library(caTools)
library(pROC)
library(MASS)
library(lattice)

# 02 Load data set
rawdata <- read.csv("C:/Users/Vinh/School/SPH MPH/SPH6004/Assignment/01_data/sph6004_assignment1_data.csv", header=TRUE)


# Checking for data quality
  n_distinct(rawdata$id) #N = 50920

  
## Data cleaning:

# Bivariate gradient descent
## Recode AKI to 2 categories: 0 - No AKI event vs 1 - AKI event of any severity
  rawdata$aki_bin = rawdata$aki
  rawdata[rawdata$aki_bin == 0,]$aki_bin = 0
  rawdata[rawdata$aki_bin != 0,]$aki_bin = 1

## Recode 2 categories: 0 - Male vs 1 - Female
  rawdata$gender_bin = rawdata$gender
  rawdata[rawdata$gender_bin == "M",]$gender_bin = 0
  rawdata[rawdata$gender_bin == "F",]$gender_bin = 1
  rawdata$gender_bin <- as.numeric(rawdata$gender_bin)
  
## Reclassification of race into categories
  summary(rawdata$race)
  rawdata$race_cat <- rawdata$race
  rawdata$race_cat <- gsub(".*ASIAN.*", "ASIAN", rawdata$race_cat)
  rawdata$race_cat <- gsub(".*WHITE.*", "WHITE", rawdata$race_cat)
  rawdata$race_cat <- gsub(".*PORTUGUESE.*", "WHITE", rawdata$race_cat)
  rawdata$race_cat <- gsub(".*HISPANIC/LATINO.*", "HISPANIC/LATINO", rawdata$race_cat)
  rawdata$race_cat <- gsub(".*HISPANIC OR LATINO.*", "HISPANIC/LATINO", rawdata$race_cat)
  rawdata$race_cat <- gsub(".*BLACK.*", "BLACK/AFRICAN", rawdata$race_cat)
  rawdata$race_cat <- gsub(".*NATIVE.*", "NATIVE", rawdata$race_cat)
  rawdata$race_cat <- gsub(".*(UNABLE TO OBTAIN|DECLINED).*", "UNKNOWN", rawdata$race_cat)
  rawdata$race_cat <- gsub(".*(SOUTH AMERICAN|NATIVE|MULTIPLE).*", "OTHER", rawdata$race_cat)
  
  # rawdata %>% select(race, race_cat) # Checking for any inconsistency
  
  # Generate dummy variables for 5 main groups: White, Black, Hispanic, Asian, and Others/Unknown
  rawdata <- rawdata %>% mutate(dummy=1) %>% spread(key=race_cat,value=dummy, fill=0)
  rawdata <- rawdata %>% mutate(OTHERS = OTHER + UNKNOWN)
  rawdata <- rawdata[-c(168:169)] # Remove "Other" and "Unknown"
  rawdata <- rawdata %>% rename("rc.asian" = "ASIAN",
                                "rc.black" = "BLACK/AFRICAN",
                                "rc.latino" = "HISPANIC/LATINO",
                                "rc.other" = "OTHERS",
                                "rc.white" = "WHITE")
  

# Reorder columns to make categorical variables in front
  rawdata <- rawdata[,c(1:3,5,163,4,c(164:169),6:(ncol(rawdata)-7))]  
  

## Make categorical variables factors
  varsToFactor <- c("gender","race","aki")
  rawdata[varsToFactor] <- lapply(rawdata[varsToFactor], factor)


  
## Create Table 1
vars <- c("admission_age","gender","race_cat",
          "heart_rate_min","heart_rate_max","heart_rate_mean",
          "sbp_min","sbp_max","sbp_mean",
          "dbp_min","dbp_max","dbp_mean",
          "mbp_min","mbp_max","mbp_mean",
          "resp_rate_min","resp_rate_max","resp_rate_mean",
          "temperature_min","temperature_max","temperature_mean",
          "spo2_min","spo2_max","spo2_mean",          
          "glucose_min","glucose_max","glucose_mean",
          "lactate_min","lactate_max",
          "ph_min","ph_max",
          "so2_min","so2_max","po2_min","po2_max",
          "pco2_min", "pco2_max", "aado2_min", "aado2_max", "aado2_calc_min", "aado2_calc_max",
          "pao2fio2ratio_min", "pao2fio2ratio_max", "baseexcess_min", "baseexcess_max", 
          "bicarbonate_min","bicarbonate_max", "totalco2_min", "totalco2_max", 
          "hematocrit_min", "hematocrit_max", "hemoglobin_min", "hemoglobin_max",
          "carboxyhemoglobin_min", "carboxyhemoglobin_max", "methemoglobin_min", "methemoglobin_max",
          "temperature_min.1", "temperature_max.1", "chloride_min", "chloride_max", "calcium_min", "calcium_max",
          "glucose_min.1", "glucose_max.1", "potassium_min", "potassium_max", "sodium_min", "sodium_max",
          "hematocrit_min.1", "hematocrit_max.1", "hemoglobin_min.1", "hemoglobin_max.1",
          "platelets_min", "platelets_max", "wbc_min", "wbc_max", "albumin_min", "albumin_max",
          "globulin_min", "globulin_max", "total_protein_min", "total_protein_max",
          "aniongap_min", "aniongap_max", "bicarbonate_min.1", "bicarbonate_max.1", "bun_min", "bun_max",
          "calcium_min.1", "calcium_max.1", "chloride_min.1", "chloride_max.1",
          "glucose_min.2", "glucose_max.2", "sodium_min.1", "sodium_max.1",
          "potassium_min.1", "potassium_max.1", "abs_basophils_min", "abs_basophils_max", 
          "abs_eosinophils_min", "abs_eosinophils_max", "abs_lymphocytes_min", "abs_lymphocytes_max",
          "abs_monocytes_min", "abs_monocytes_max", "abs_neutrophils_min", "abs_neutrophils_max", "atyps_min", "atyps_max",
          "bands_min", "bands_max", "imm_granulocytes_min", "imm_granulocytes_max", "metas_min", "metas_max", 
          "nrbc_min", "nrbc_max", "d_dimer_min", "d_dimer_max", "fibrinogen_min", "fibrinogen_max",
          "thrombin_min", "thrombin_max", "inr_min", "inr_max", "pt_min", "pt_max",
          "ptt_min", "ptt_max", "alt_min", "alt_max", "alp_min", "alp_max", "ast_min", "ast_max", 
          "amylase_min", "amylase_max", "bilirubin_total_min", "bilirubin_total_max",
          "bilirubin_direct_min", "bilirubin_direct_max", "bilirubin_indirect_min", "bilirubin_indirect_max",
          "ck_cpk_min", "ck_cpk_max", "ck_mb_min", "ck_mb_max", 
          "ggt_min", "ggt_max", "ld_ldh_min", "ld_ldh_max", 
          "gcs_min", "gcs_motor", "gcs_verbal", "gcs_eyes", "gcs_unable", "height", "weight_admit"
          )

tableone <- CreateTableOne(vars = vars, strata = c("aki"), data = rawdata,
                           addOverall = TRUE)

options(max.print=999999)
print(tableone, quote = FALSE) ## Copy to Excel and/or Word docs for the report

## Check for variables that have more than half missing values
  exclude.vars <- c()
  for (x in c(13:(ncol(rawdata)))){
    ifelse(sum(is.na(rawdata[,x])) > (n_distinct(rawdata$id)/2), print(c(names(rawdata)[x])), next)
    exclude.vars <- c(exclude.vars,x)
  }
  
  # Exclude variables with more than half missing data points from the analysis
  ## Rationale: half of the data points will be omitted if those variables are included in the analysis
  data.cleaned = rawdata[,-exclude.vars]
  rm(exclude.vars)
  
  # Exclude participants with more than half missing data point?
  ## NOT USED -> rationale: The final predictor model that performs best may not even include the missing variables
  ## ie. the model performs reasonably well when using only the available data points.
  
# Plotting
  boxplot(admission_age ~ aki_bin, 
         xlab= "AKI outcome",
         ylab = "Admission age",
         data = rawdata, outline = FALSE
  )
  
  boxplot(heart_rate_mean ~ aki_bin, 
          xlab= "AKI outcome",
          ylab = "Mean heart rate",
          data = rawdata, outline = FALSE
  )
  
  boxplot(sbp_mean ~ aki_bin, 
          xlab= "AKI outcome",
          ylab = "Mean SBP",
          data = rawdata, outline = FALSE
  )
  
  boxplot(weight_admit ~ aki_bin, 
          xlab= "AKI outcome",
          ylab = "Mean weight at admission",
          data = rawdata, outline = FALSE
  )
  
  boxplot(totalco2_min ~ aki_bin, 
          xlab= "AKI outcome",
          ylab = "Min Total CO2",
          data = rawdata, outline = FALSE
  )
  
  boxplot(hemoglobin_min.1 ~ aki_bin, 
          xlab= "AKI outcome",
          ylab = "Min hemoglobin count",
          data = rawdata, outline = FALSE
  )

## PART I: SIMPLE TWO-SAMPLE T-TESTs TO IDENTIFY SIGNIFICANT PREDICTOS TO BE USED IN THE MAIN MODEL
listofsigindeps <- c(1:12)
for (i in 13:ncol(data.cleaned)) {
  ttest <- t.test(data.cleaned[,i] ~ data.cleaned$aki_bin, data = data.cleaned, var.equal = FALSE) 
  if(ttest$p.value <= 0.05) listofsigindeps <- c(listofsigindeps,i) else next
}

head(data.cleaned[,c(listofsigindeps)])
data.cleaned2 <- data.cleaned[,c(listofsigindeps)]


# Standardize independent variables/features
data.scaled2 <- data.cleaned2
for (x in c(13:(ncol(data.scaled2)))){
  data.scaled2[,x] = (data.scaled2[,x] - mean(data.scaled2[!is.na(data.scaled2[,x]),x]))/(sd(data.scaled2[!is.na(data.scaled2[,x]),x]))
}

## PART II: GENERATING PREDICTION MODEL AND FEATURES SELECTION:
  
### Step 1: Split data into training and testing sets
  set.seed(42069)
  ind = sample.split(Y = data.scaled2$id, SplitRatio = 0.7)
  data.train <- subset(data.scaled2, ind == TRUE)
  data.test <- subset(data.scaled2, ind == FALSE)
  outcome.validate <- data.test
  
### Step 2: Obtained dataset with full information only (no NA for any of the remaining features of interest)
  data.train.NAomit <- na.omit(data.train[1:ncol(data.train)])
  outcome <- data.train.NAomit$aki_bin
  features <- data.train.NAomit[,c(6:10,12:ncol(data.train.NAomit))] # Skip rc.white as rc.white is the omitted category
  
  
### Step 3: Perform gradient descent with momentum and L2 regularization:   
  #theta <- gd(x=indeps, y=outcome, num.iterations=500, threshold=1e-5, output.path=FALSE)
  
  #theta.momentum <- gd.momentum(x=indeps, y=outcome, num.iterations=500, threshold=1e-5, output.path=FALSE)
  
  #theta.momentum.reg <- gradient.descent.momentum(x=indeps, y=outcome, num.iterations=500, threshold=1e-5, output.path=FALSE)
  
  theta.momentum.L2 <- gd.momentum.regL2(x=features, y=outcome, 
                                         num.iterations=500, threshold=1e-5, output.path=FALSE)
  
  # GENERATE 100 ITERATIONS OF CANDIDATE LISTS
    theta.cand.list <- c(rep(0,ncol(features)+1))
    for (i in 1:100) {
      theta.cand <- gd.momentum.regL2(x=features, y=outcome, 
                                      num.iterations=500, threshold=1e-5, output.path=FALSE)
      theta.cand.list <- rbind(theta.cand.list, theta.cand)
      }  
    theta.cand.list <- theta.cand.list[-1,]

  # EVALUATE THE COMBINATION
      data.test.NAomit <- na.omit(data.test[1:ncol(data.test)])
      outcome.test <- data.test.NAomit$aki_bin
      features.test <- data.test.NAomit[,c(6:10,12:ncol(data.test.NAomit))] # Skip rc.white as rc.white is the omitted category
      
      features.test.list <- features.test
      features.test.list$constant <- c(1)
    
    gd.auroc <- c()
    ## LOOP STARTS HERE
    # Format the thetas
      for (i in 1:nrow(theta.cand.list)) {
        theta.vecs.temp <-data.frame(t(theta.cand.list[i,]))    ## THIS IS CORRECT
        theta.vecs.temp$constant <- theta.vecs.temp$rep.1..m.       ## THIS IS CORRECT
        theta.vecs.temp <- theta.vecs.temp[,-1]                     ## THIS IS CORRECT
      
        # CORRECT CHUNK
            yhat <- c()
            for (n in 1:nrow(features.test.list)) {
              yhat[n] <- 1 / (1 + exp(-as.matrix(features.test.list[n,]) %*% t(theta.vecs.temp)))
            }
    
            outcome.test2 <- data.frame(outcome.test, yhat)
            
        roc_object.temp <- roc(outcome.test2$outcome.test, outcome.test2$yhat)
        auc(roc_object.temp)
      gd.auroc[i] <- roc_object.temp$auc
      }
  
    theta.cand.list.auc <- data.frame(theta.cand.list, gd.auroc) ## evaluation of the performance of 100 iterations
  
    theta.cand.list.auc.sort <- theta.cand.list.auc[order(-gd.auroc),]
    theta.top10 <- theta.cand.list.auc.sort[c(1:10),]
  
  # PERFORM ENSEMBLE OF THE TOP 10 MODELS <--------- CLEAN UP HERE
    # Format the thetas
    for (i in c(1:nrow(theta.top10))) {
      
      theta.vecs.temp <-data.frame(theta.top10[i,])
      theta.vecs.temp$constant <- theta.vecs.temp$rep.1..m.
      theta.vecs.temp <- theta.vecs.temp[,-1]
      
      yhat <- c()
      for (n in 1:nrow(features.test.list)) {
        yhat[n] <- 1 / (1 + exp(-as.matrix(features.test.list[n,]) %*% t(theta.vecs.temp)))
      }
      yhat.pred.top10 <- cbind(data.frame(yhat.pred.top10, yhat))
    }
    
    # yhat.pred.top10$mean_prob <- rowMeans(yhat.pred.top10[,c(2:11)], na.rm=TRUE)
    
    
    outcome.test3 <- data.frame(outcome.test, yhat.pred.top10)
    roc_object.temp3 <- roc(outcome.test3$outcome.test, outcome.test3$mean_prob)
    auc(roc_object.temp3)
    
    
# Step 4: Multivariate GLM
  # Obtain list of variables to be used for glm - FOCUSING ON THE MEANS AS THE MIN AND MAX ARE HIGHY CORRELATED
  # GLM 1 - initial model
    glm1 <- glm(aki_bin ~ . -id -aki -gender -race -rc.white, # rc.white as the omitted category
                data = data.train, family = binomial)
    summary(glm1)

    yhat1 <- predict(glm1, newdata = data.test, type = "response")
    outcome.validate$yhat1 <- yhat1
    roc_object1 <- roc(outcome.validate$aki_bin, outcome.validate$yhat1 )
    auc(roc_object1)
    
  # GLM 2 - excluding features that are not statistically significant:
    glm2 <- glm(aki_bin ~ admission_age + gender_bin + rc.asian + rc.black + rc.latino + rc.other # rc.white as the omitted category
              +heart_rate_min + heart_rate_max + heart_rate_mean
              +sbp_max + sbp_mean + mbp_mean + temperature_min + temperature_max
              +spo2_max + ph_min + ph_max + pco2_min + pco2_max + totalco2_min
              +hematocrit_min.1+hemoglobin_min.1+aniongap_min+bun_min
              +calcium_min.1+chloride_min.1+chloride_max.1
              +sodium_min.1+potassium_max.1+ptt_max+gcs_verbal+gcs_eyes+weight_admit,
                data = data.train, family = binomial)
    summary(glm2)
    
    yhat2 <- predict(glm2, newdata = data.test, type = "response")
    outcome.validate$yhat2 <- yhat2
    roc_object2 <- roc(outcome.validate$aki_bin, outcome.validate$yhat2 )
    auc(roc_object2)

  # GLM 3 - excluding features that are not statistically significant:
    glm3 <- glm(aki_bin ~ admission_age + gender_bin + rc.asian + rc.black + rc.latino + rc.other # rc.white as the omitted category
                +heart_rate_min + heart_rate_max + heart_rate_mean
                +sbp_max + sbp_mean + temperature_min + temperature_max
                +spo2_max + ph_min + ph_max + pco2_min + pco2_max + totalco2_min
                +hematocrit_min.1 + hemoglobin_min.1 + aniongap_min+bun_min
                +calcium_min.1 + chloride_max.1
                +potassium_max.1 + ptt_max+gcs_verbal + gcs_eyes + weight_admit,
                data = data.train, family = binomial)
    summary(glm3)
    
    yhat3 <- predict(glm3, newdata = data.test, type = "response")
    outcome.validate$yhat3 <- yhat3
    roc_object3 <- roc(outcome.validate$aki_bin, outcome.validate$yhat3 )
    auc(roc_object3)  
  
  ####### ORDERED LOGIT
  # OLOGIT1
    ologit1 <- polr(aki ~ . -id -aki -gender -race -rc.white -aki_bin, # rc.white as the omitted category
                    data = data.train, Hess = TRUE)
    summary(ologit1)
  
      summary.ologit1 <- coef(summary(ologit1))
      pval <- pnorm(abs(summary.ologit1[, "t value"]),lower.tail = FALSE)* 2
      summary.ologit1 <- cbind(summary.ologit1, "p value" = round(pval,3))
      summary.ologit1
      
      yhat.ologit1 <- predict(ologit1, newdata = data.test, type = "probs")
      outcome.validate$yhat.ologit1 <- yhat.ologit1
      roc_object.ologit1 <- multiclass.roc(outcome.validate$aki, outcome.validate$yhat.ologit1)
      auc(roc_object.ologit1)
  
  # OLOGIT2
    ologit2 <- polr(aki ~  -id -aki -gender -race -rc.white -aki_bin # rc.white as the omitted category
                    + admission_age + gender_bin + rc.asian + rc.black + rc.latino + rc.other
                    + heart_rate_min + heart_rate_max + heart_rate_mean
                    + sbp_min + sbp_max + sbp_mean
                    + dbp_mean + mbp_mean + resp_rate_mean + temperature_min + temperature_max
                    + spo2_max + ph_max + po2_max + pco2_max + totalco2_min
                    + hematocrit_min.1 + hemoglobin_min.1 + platelets_min + platelets_max
                    + aniongap_min + bun_max + calcium_min.1
                    + chloride_min.1 + chloride_max.1 + glucose_min.2 + sodium_min.1
                    + potassium_max.1 + inr_min + pt_min
                    + ptt_min + ptt_max + gcs_motor + gcs_verbal + gcs_eyes
                    + weight_admit,
                    data = data.train, Hess = TRUE)
    summary(ologit2)
      
      summary.ologit2 <- coef(summary(ologit2))
      pval <- pnorm(abs(summary.ologit2[, "t value"]),lower.tail = FALSE)* 2
      summary.ologit2 <- cbind(summary.ologit2, "p value" = round(pval,3))
      summary.ologit2
      
      yhat.ologit2 <- predict(ologit2, newdata = data.test, type = "probs")
      outcome.validate$yhat.ologit2 <- yhat.ologit2
      roc_object.ologit2 <- multiclass.roc(outcome.validate$aki, outcome.validate$yhat.ologit2)
      auc(roc_object.ologit2)
  
   # OLOGIT3
      ologit3 <- polr(aki ~  -id -aki -gender -race -rc.white -aki_bin # rc.white as the omitted category
                      + admission_age + gender_bin + rc.asian + rc.black + rc.latino + rc.other
                      + heart_rate_min + heart_rate_max + heart_rate_mean
                      + sbp_min + sbp_max + sbp_mean
                      + dbp_mean + mbp_mean + resp_rate_mean + temperature_min + temperature_max
                      + spo2_max + ph_max + po2_max + pco2_max + totalco2_min
                      + hematocrit_min.1 + hemoglobin_min.1 + platelets_min + platelets_max
                      + aniongap_min + bun_max + calcium_min.1
                      + chloride_min.1 + chloride_max.1 + glucose_min.2
                      + potassium_max.1 + inr_min + pt_min
                      + ptt_min + ptt_max + gcs_motor + gcs_verbal + gcs_eyes
                      + weight_admit,
                      data = data.train, Hess = TRUE)
      summary(ologit3)
      
      summary.ologit3 <- coef(summary(ologit3))
      pval <- pnorm(abs(summary.ologit3[, "t value"]),lower.tail = FALSE)* 2
      summary.ologit3 <- cbind(summary.ologit3, "p value" = round(pval,3))
      summary.ologit3
      
      yhat.ologit3 <- predict(ologit3, newdata = data.test, type = "probs")
      outcome.validate$yhat.ologit3 <- yhat.ologit3
      roc_object.ologit3 <- multiclass.roc(outcome.validate$aki, outcome.validate$yhat.ologit3)
      auc(roc_object.ologit3)


# 1. import dataset
library(plyr)
library(writexl)
library(optmatch)

##wave IV dataset
load("/Users/ICPSR_21600/DS0022/21600-0022-Data.rda")

wave_IV<-da21600.0022
wave_IV.1<-wave_IV[,c("AID","H4ED2", "BIO_SEX4", "H4OD1Y", "H4WP3", "H4WP9", "H4IR4", "H4TO5", "H4TO35", "H4TO70", "H4SE26A", "H4SE26C")]
names(wave_IV.1)<-c("AID", "completion", "gender", "age", "biomom_jail", "biofa_jail", "race", "tobacco", "alcoho", "marijuana", "condom", "birth_control")

##wave I dataset
load("/Users/ICPSR_21600/DS0001/21600-0001-Data.rda")
wave_I<-da21600.0001
wave_I.1<-wave_I[, c("AID", "PA55", "PA10", "PC38")]
names(wave_I.1)<-c("AID", "household income", "parent marital", "learning disability")
wave_IV.2<-wave_IV.1[complete.cases(wave_IV.1),]

##wave II dataset
load("/Users/ICPSR_21600/DS0005/21600-0005-Data.rda")
wave_V<-da21600.0005
wave_V.1<-wave_V[, c("AID", "H2WP11", "H2WP12", "H2WP15", "H2WP16")]
names(wave_V.1)<-c("AID", "mom college exp", "mom HS exp", "dad college exp", "dad HS exp")

data1<-merge(wave_I.1, wave_V.1, by="AID")
data1<-data1[complete.cases(data1),]

##After the list-wise deletion, there are 762 subjects in the dataset
data3<-merge(data1, wave_IV.2, by="AID")

data<-data3

##recode variables
data$`parent marital`<-as.integer(ifelse(data$`parent marital`=="(2) (2) Married (skip to A12)", 1, 0)) ##married=1; single/widowed/divorced/seperate=0
data$`learning disability`<-as.integer(ifelse(data$`learning disability`=="(0) (0) No", 0, 1))
data$gender<-as.integer(ifelse(data$gender=="(1) (1) Male", 0, 1)) ## male=0, female=1
data$biofa_jail<-as.integer(ifelse(data$biofa_jail=="(0) (0) No", 0, 1))
data$biomom_jail<-as.integer(ifelse(data$biomom_jail=="(0) (0) No", 0, 1))
data$age<-2008-data$age
data$parent_hs_exp<-as.integer((data$`mom HS exp`+data$`dad HS exp`)/2)
data$parent_college_exp<-as.integer((data$`mom college exp`+data$`dad college exp`)/2)
data$race<-revalue(data$race,  c("(1) (1) White"=1, "(2) (2) Black or African American"=2, "(3) (3) American Indian or Alaska Native"=3, "(4) (4) Asian or Pacific Islander"=3)) ##1=white, 2=black, 3=other
data$race<-as.integer(data$race, levels = c(1, 2, 3), labels = c("white", "black", "other"))

#data$fight<-as.numeric(revalue(data$fight, c( "(0) (0) Never" = 1, "(1) (1) 1 or 2 times"= 2, "(2) (2) 3 or 4 times" = 2, "(3) (3) 5 or more times" = 3 ))) #1 = never, 2 = 1 or 2 times, 2 = 3 or 4 times, 3 = 5 or more times
#data$weapon<-as.numeric(revalue(data$weapon, c( "(0) (0) Never" = 1, "(1) (1) 1 or 2 times"= 2, "(2) (2) 3 or 4 times" = 2, "(3) (3) 5 or more times" = 3 ))) #1 = never, 2 = 1 or 2 times, 2 = 3 or 4 times, 3 = 5 or more times
#data$suicide<-ifelse(data$suicide=="(0) (0) No", 1, 2)

data$condom<-ifelse(data$condom=="(0) (0) Not selected", 1, 2)
data$birth_control<-ifelse(data$birth_control=="(0) (0) Not selected", 1, 2)

data$alco_recode<-as.numeric(revalue(data$alcoho, c("(0) (0) None" = 1, "(1) (1) 1 or 2 days in the past 12 months" = 2, "(2) (2) Once a month or less (3 to 12 days in the past 12 months" = 2, "(3) (3) 2 or 3 days a month" = 2, "(4) (4) 1 or 2 days a week" = 3, "(5) (5) 3 to 5 days a week" = 3,  "(6) (6) Every day or almost every day" = 3 ))) #1=never, 2=light use, 3=heavy use
data$drug_recode<- as.numeric(revalue(data$marijuana, c("(0) (0) None" = 1, "(1) (1) 1 or 2 days in the past 12 months" = 2, "(2) (2) Once a month or less (3 to 12 days in the past 12 months" = 2, "(3) (3) 2 or 3 days a month" = 2, "(4) (4) 1 or 2 days a week" = 3, "(5) (5) 3 to 5 days a week" = 3,  "(6) (6) Every day or almost every day" = 3 ))) #1=never, 2=light use, 3=heavy use
data$toba_recode<-as.integer(cut(data$tobacco, breaks = c(-1, 15, 31), label=c("never or light", "heavy"), ordered=TRUE))
data$treatment<-as.integer(cut(as.numeric(data$completion), breaks=c(-1, 6, 14), label=c("not complete college", "complete college")))
data$treatment<-as.integer(data$treatment-1) ##0="not complete college", 1="complete college"

data<-data[, c(1, 2:4, 10:14, 20, 21, 25, 18:19, 22:24)]
data<-data[data$`household income`>0,]




# 2. Function for Baseline balance check
# Load the PS functions from class 05.
### Functions to get SMD and var ratios on a covariate
smd_vr <- function(covar, trt, wts = rep(1, length(covar)))
{
  #... covar is covariate (CONVERT UNORDERED FACTORS TO DUMMIES)
  #... trt is selection
  #... wts is weights
  
  covar <- as.numeric(as.character(covar)) #... ordered and 0-1 factors to num
  covar.by.sel <- split(covar, trt)
  wts.by.sel <- split(wts, trt)
  wtmn.0 <- weighted.mean(covar.by.sel[[1]], w = wts.by.sel[[1]], na.rm = TRUE)
  wtmn.1 <- weighted.mean(covar.by.sel[[2]], w = wts.by.sel[[2]], na.rm = TRUE)
  wt.mn.diff <- wtmn.1 - wtmn.0
  wtvar.0 <- cov.wt(matrix(covar.by.sel[[1]], length(covar.by.sel[[1]]), 1), wt = wts.by.sel[[1]])[[1]]
  wtvar.1 <- cov.wt(matrix(covar.by.sel[[2]], length(covar.by.sel[[2]]), 1), wt = wts.by.sel[[2]])[[1]]
  pooled.wt.var <- (wtvar.0*sum(wts.by.sel[[1]]) + wtvar.1*sum(wts.by.sel[[2]]))/sum(wts)
  smds <- (wt.mn.diff)/sqrt(pooled.wt.var)
  out <- t(c(smds, wtvar.1/wtvar.0))
  colnames(out) <- c("St Mean Diff", "Var Ratio")
  out
}


smd_vr_DF <- function(covars, dat, trt, wts = rep(1, length(trt)), plot = FALSE)
{
  ### This function may be used to apply smd_vr over an entire data frame "dat"
  ### on selected covariates "covars" that can be found in the data frame "dat".
  ### We assume there are no factors in the data frame. The function also plots
  ### if requested.
  
  output <- matrix(0, length(covars), 2)
  colnames(output) <- c("St Mean Diff", "Var Ratio")
  
  ### Run smd_vr for each covariate with a for loop
  for(i in 1:length(covars)) {
    output[i,] <- smd_vr(trt = trt, covar = dat[,covars[i]], wts = wts)
  }
  rownames(output) <- covars
  
  ### If plot == TRUE make a plot
  if(plot) {
    plot(output, pch = 19, col = "red",
         xlim = (range(output[,1]) + c(-.2, .2)), 
         ylim = c(0, range(output[,2])[2] + .2))
    abline(v = c(-.1, .1), lwd = 2, lty = 2)
    abline(h = c(4/5, 5/4), lwd = 2, lty = 2)
    # Flag variables outside of area
    inds <- which(output[,1] < -.1 | output[,1] > .1 | output[,2] < 4/5 | output[,2] > 5/4)
    text(output[inds,], labels = rownames(output[inds,]), adj = c(0, -1), cex = .7)
  }
  output
}

### Function for continuous variables
ovlp <- function(trt, lps)
{
  ### This function displays an overlap histogram for "lps" across "trt" groups
  ### Statistical test of mean difference
  tt <- t.test(x = lps[trt==1], y = lps[trt==0], alternative = "two.sided",
               var.equal = FALSE)$p.value
  ### Statistical test of difference in distribution
  ks <- ks.test(x = lps[trt==1], y = lps[trt==0], alternative = "two.sided")$p.value
  
  par(mfrow = c(2,1))
  rc <- range(lps)
  brks <- seq(from = rc[1] - diff(rc)/20, to = rc[2] + diff(rc)/20, by = diff(rc)/20)
  hist(lps[trt==1], breaks = brks, xlab = "", main = "Treated Cases",
       freq = FALSE)
  d1 <- density(lps[trt==1])
  lines(d1, col = 2, lwd = 1, lty = 1)
  abline(v = c(range(lps[trt==1])), col = 3, lty = 2)
  hist(lps[trt==0], breaks = brks, xlab = "", main = "Control Cases",
       freq = FALSE)
  d2 <- density(lps[trt==0])
  lines(d2, col = 2, lwd = 1, lty = 1)
  abline(v = c(range(lps[trt==0])), col = 3, lty = 2)
  par(mfrow = c(1,1))
}

### Note that ovlp_ind requires that package optmatch be loaded
ovlp_ind <- function(trt, lps, caliper = 0.1)
{
  nt <- sum(trt == 1); nc <- sum(trt == 0)
  SDpool <- sqrt( ( (nt - 1)*var(lps[trt == 1]) + (nc - 1)*var(lps[trt == 0]) ) / 
                    (nt + nc - 2) )
  ### Get abs(distance) for each treat/control pairing
  diffs <- match_on(trt ~ lps, method = "euclidean")
  smds <- diffs/SDpool # standardize differences by dividing by pooled SD
  fun <- function(vec) {min(vec) <= caliper}
  trtOvlp <- apply(smds, 1, fun)   # TRUEs are overlapping
  ctrlOvlp <- apply(smds, 2, fun)  # FALSEs are not
  drop1 <- which(trt==1)[!trtOvlp]
  drop0 <- which(trt==0)[!ctrlOvlp]
  ind <- !logical(length(lps))
  ind[c(drop1, drop0)] <- FALSE
  ind
}

var_wt <- function(x, wt)
{
  wt_m <- x %*% wt / sum(wt)
  wt_var <- x^2 %*% wt / sum(wt) - m^2
  out <- c(wt_m, wt_var)
  out
}


##iptw
iptw_wts <- function(trt, ps, estimand = "ATE")
{
  if (estimand == "ATE") wts <- trt/ps + (1 - trt)/(1 - ps)
  if (estimand == "ATT") wts <- trt + (ps*(1 - trt))/(1 - ps)
  wts
}




# 3. Baseline balance check
(cov_names1 <- names(data)[-c(1, 12:20)])  ##covariates names

##baseline balance check without any adjustment
smd_vr_DF(covars = cov_names1, 
          dat = data, 
          trt = data$treatment, ##treatment variable
          plot = TRUE)

##a. check balance in baseline
cov_data<-data[-c(1, 13:20)]
library(ggplot2)
ps1<-glm(treatment ~ . , data = cov_data, family = binomial(link="logit"))
ps1.fitted<-ps1$fitted.values

#two ways to visualize the propensity score
logitPS1.1 <- data.frame(pr_score =log(ps1.fitted / (1 - ps1.fitted)),
                         treatment = cov_data$treatment)

logitPS1.2<-log(ps1.fitted / (1 - ps1.fitted))
# Plot logit(PS) by treatment group and control group
ggplot(logitPS1.1, aes(x=pr_score, color=as.factor(treatment), fill=as.factor(treatment))) +
  geom_histogram(position="identity", alpha=0.5)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Linear Propensity Score", x="propensity score")+
  theme_minimal()+theme_classic()

par(mfrow=c(2,1)) 
hist(logitPS1.2[cov_data$treatment == 1],
     xlab = "Linear Propensity Score",
     main = "Linear PSs for Exposed Cases")
hist(logitPS1.2[cov_data$treatment == 0],
     xlab = "Linear Propensity Score",
     main = "Linear PSs for Comparison Cases")


###check overlap on the estimated propensity score.
ovlp(trt=logitPS1.1$treatment, lps=logitPS1.1$pr_score)



##b. PS estimation and overlap
ind<-ovlp_ind(trt = logitPS1.1$treatment, lps=logitPS1.1$pr_score, caliper = 0.1)

#length(ind) - sum(ind)
data_ovlp<-data[ind,-c(1)]
table(data_ovlp$treatment)
ovlp(trt = data$treatment[ind], lps = logitPS1.1$pr_score[ind]) ##after deleting the non-overlapping cases, the overlap is good

ps2<-glm(treatment ~ `household income` + I(`household income`^2) + `parent marital` + `learning disability` + gender + age + biomom_jail + biofa_jail + race + parent_hs_exp + parent_college_exp, data = data_ovlp, family = binomial(link="logit"))  ##`household income` + I(`household income`^2) 
ps2.pred <- predict(ps2, type = "response")
lin_ps2 <- log(ps2.pred/(1 - ps2.pred))
### Check overlap 
ovlp(trt = data_ovlp$treatment, lps = lin_ps2)


##c. IPTW
### Check ATE weights 
ate_wts <- iptw_wts(trt = data_ovlp$treatment, ps = ps2.pred, estimand = "ATE") 
range(ate_wts)

### Check balance (ATE)
(smds_iptw <- smd_vr_DF(covars = cov_names1, dat = data_ovlp, 
                        trt = data_ovlp$treatment, wts = ate_wts, plot = TRUE))

### Weight trimming - with IPTW can get extreme weights, especially
### with ATE as estimand. Trimming is an option.
p995 <- quantile(ate_wts, prob = .995) # light trimming to the 99th percentile
ate_wts[which(ate_wts > p995)] <- p995

### Check balance (ATE)
(smds_iptw <- smd_vr_DF(covars = cov_names1, dat = data_ovlp, 
                        trt = data_ovlp$treatment, wts = ate_wts, plot = TRUE))

data_ovlp$weights<-ate_wts


data_ovlp.1<-data_ovlp[, c(11:17)]
names(data_ovlp.1)<-c ("trt", "cond", "bc", "alco", "drug", "toba", "wts")




# 4. Conduct LCA using the weighted dataset
library(poLCA)
formula1 <- cbind(alco_recode, drug_recode, toba_recode, condom, birth_control) ~ 1
formula2 <- cbind(alco_recode, drug_recode, toba_recode, condom, birth_control) ~ 1

min_bic<-10000
for (i in 1:8){
  lc<-poLCA(formula1, data=data_ovlp, nclass=i, graphs=FALSE, maxiter=3000, (weights=data_ovlp$weights), nrep=10, verbose=TRUE, calc.se=TRUE)
  if (lc$bic < min_bic){
    min_bic<-lc$bic
    LCA_best_model<-lc
  }
}

LCA_best_model
(lca1 <- poLCA(formula2, data=data_ovlp, nclass=4, graphs=FALSE, maxiter=3000, (weights=data_ovlp$weights) ) )
lca1

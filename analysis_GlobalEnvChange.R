# Analysis for Nature CC Paper - Shrum, Gould, Harrington-Ramirez, Iglesias

# Loading necessary packages
library(tidyverse)
library(car)
library(stargazer)
library(msm)


# Loading analysis ready, de-identified, county level only data
d <- read.csv("data/extremeData_WTP_Analysis_noID.csv")


#### Classifying and Counting Reporting Types ####
d$Unverified_Extreme <- ifelse(d$Reported_Extreme == 1 & d$Verified_Extreme == 0, 1, 0)

d$totalVerifiedTypes <- d$Verified_Drought + d$Verified_Heatwave + d$Verified_Hurricane + d$Verified_Tornado + d$Verified_Wildfire
d$totalReportedTypes <- d$Reported_Drought + d$Reported_Flood + d$Reported_Heatwave + d$Reported_Hurricane + d$Reported_Tornado + d$Reported_Wildfire

table(d$totalReportedTypes)
table(d$totalVerifiedTypes)
table(d$Verified_Hurricane, d$Verified_Tornado)

table(d$Reported_Extreme)
table(d$Verified_Extreme, d$Reported_Extreme)
table(d$Verified_Extreme, d$ReportedFlood_NoVerified_Extreme)
table(d$Verified_Extreme, d$Reported_Wildfire)
table(d$Verified_Extreme, d$Reported_Drought)
table(d$Verified_Extreme, d$Reported_Heatwave)
table(d$Verified_Extreme, d$Reported_Tornado)
table(d$Verified_Extreme, d$Reported_Hurricane)

# 2842 people have reported experiencing impacts from extreme weather events
# 1344 of those have reported experiencing impacts reported living in zip code where a reported extreme event occurred in the weather data
# 1498 people reported an unverified extreme event. Of those 1498...
  # 408 reported flooding (on which we have no external data) and had no other verified extreme events 
  # 534 reported wildfire and no other verified extreme events
  # 181 reported drought and no other verified extreme events
  # 381 reported a heatwave and no other verified extreme events
  # 332 reported a tornado or windstorm and no other verified extreme events. We only have data on tornados, so wind storms are unverified.
  # 491 reported a hurricane and not other verified extreme events


d %>%
  filter(d$Verified_Extreme == 0) %>%
  mutate(totalReportedUnverified = Reported_Flood + Reported_Wildfire + Reported_Drought + Reported_Hurricane + Reported_Tornado + Reported_Heatwave)  -> d_unverif


# Of the 4440 who do not have a verified event...
  # 3025 did not report an event
  # 796 reported 1 type of event, 398 reported 2 type of events, 166 reported 3 types of events, 41 reported 4 types, 11 reported 5 types, and 3 reported 6 types
table(d_unverif$totalReportedUnverified, d_unverif$Reported_Wildfire)
table(d_unverif$totalReportedUnverified, d_unverif$Reported_Flood)
table(d_unverif$totalReportedUnverified, d_unverif$Reported_Drought)
table(d_unverif$totalReportedUnverified, d_unverif$Reported_Hurricane)
table(d_unverif$totalReportedUnverified, d_unverif$Reported_Tornado)
table(d_unverif$totalReportedUnverified, d_unverif$Reported_Heatwave)

table(d$Registered_Extreme, d$Reported_Extreme)  


table(d$Registered_Extreme, d$Registered_Heatwave)
table(d$Registered_Extreme, d$Registered_Drought)

d$Registered_Extreme_excldHeatwaveDrought <- ifelse(d$Registered_Hurricane == 1 | d$Registered_Wildfire == 1 | d$Registered_Tornado == 1, 1, 0)


#### Extreme Event Experience Reporting ####

d$voter <- ifelse(d$biden == 1 | d$trump == 1 | d$thirdparty == 1, 1, 0)
summary(r_cc <- glm(extremeExperienceYN ~ ccHumanCaused, data = d, family = binomial(link = "logit")))
summary(r_party <- glm(extremeExperienceYN ~ ccHumanCaused  + age + income + collegeGrad + male + bipoc + biden + trump + thirdparty + state, data = d, family = binomial(link = "logit")))
summary(r_voter <- glm(extremeExperienceYN ~ ccHumanCaused  + age + income + collegeGrad + male + bipoc +  voter + state, data = d, family = binomial(link = "logit")))


summary(r_b <- glm(extremeExperienceYN ~ biden, data = d, family = binomial(link = "logit")))
summary(r_t <- glm(extremeExperienceYN ~ trump, data = d, family = binomial(link = "logit")))
summary(r_nv <- glm(extremeExperienceYN ~ nonvoter, data = d, family = binomial(link = "logit")))
summary(r_tp <- glm(extremeExperienceYN ~ thirdparty, data = d, family = binomial(link = "logit")))

confint(r_cc)
linearHypothesis(r_party, "biden = trump")
linearHypothesis(r_party, "biden = thirdparty")

stargazer(r_cc, r_voter, r_party, no.space = T, 
          single.row = T,
          align = TRUE,
          dep.var.labels = c("Reported Extreme Event"),
          covariate.labels = c("Climate Change Belief", "Age (years)",  "Income ($10,000's)", "College Graduate (Y/N)", "Male (Y/N)", "BIPOC (Y/N)", "Voter (Y/N)", "Biden Voter (Y/N)", "Trump Voter (Y/N)", "Third-Party Voter (Y/N)"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          title = "Climate Change Belief and Extreme Event Reporting",
          notes = "Logistic regression of the belief that climate change is caused by humans on the reporting of impacts from an extreme weather event with and without demographic covariates.",
          notes.align = "l")

stargazer(r_cc, r_party, 
          no.space = T, 
          single.row = T,
          align = TRUE,
          dep.var.labels = c("Reported Extreme Event"),
          covariate.labels = c("Climate Change Belief", "Age (years)",  "Income ($10,000's)", "College Graduate (Y/N)", "Male (Y/N)", "BIPOC (Y/N)", "Biden Voter (Y/N)", "Trump Voter (Y/N)", "Third-Party Voter (Y/N)"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("state"),
          title = "Climate Change Belief and Extreme Event Reporting",
          notes = "Logistic regression of the belief that climate change is caused by humans on the reporting of impacts from an extreme weather event with and without demographic covariates.",
          notes.align = "l")



stargazer(r_b, r_t, r_tp, r_nv,
          dep.var.labels = c("Reported Extreme Event"),
          covariate.labels = c("Biden Voter", "Trump Voter", "Third-Party Voter", "Non-Voter"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          title = "Political Affiliation and Extreme Event Reporting",
          notes = "Logistic regression of recent voting behavior on the reporting of impacts from an extreme weather event.")

# Bar Graph for proportions reporting extreme events
b <- NULL
b <- data.frame(VotingBehavior = c("Biden Voter", "Trump Voter", "Third-Party Voter", "Non-Voter"),
                ReportedExtremeEvent = c(51.05127, 53.02835, 53.40909, 38.60104))

chisq.test(b$ReportedExtremeEvent, p = c(0.25, 0.25, 0.25, 0.25))
     
ggplot(data = b, aes(x = reorder(VotingBehavior, +ReportedExtremeEvent), y = ReportedExtremeEvent)) +
  geom_bar(stat = "identity", color = c("#453781FF"), fill = c("#453781FF")) +
  ylim(0,100) +
  ylab("Reported Extreme Events (%)") +
  xlab("") +
  ggtitle("b) 2020 Voting Behavior")

ggsave("output/EEReporting_Voting.png", width = 10, height = 5, units = "in")
       

c1 <- NULL
c1 <- data.frame(ClimateBelief = c("Yes", "No"),
                ReportedExtremeEvent = c(48.83916, 49.86351))

ggplot(data = c1, aes(x = factor(ClimateBelief, c("Yes", "No")), y = ReportedExtremeEvent)) +
  geom_bar(stat = "identity", color = c("#3CBC75FF"), fill = c("#3CBC75FF")) +
  ylim(0,100) +
  xlab("") +
  ylab("Reported Extreme Events (%)") +
  ggtitle("a) Belief in Human-Caused Climate Change")

ggsave("output/EEReporting_ClimateBelief2.png", width = 5, height = 5, units = "in")





### WTP & ANY EXTREME EVENTS ####
# These models test binary: no extremes vs. 1 or more extremes
# Best fit model for WTP with reported impacts of extreme weather events (verified and all reports)
summary(r1 <- glm(wtp ~ bid + Reported_Extreme + income + age + bipoc + educ + male +  trump + biden + ccHumanCaused + state , data = d, family = binomial(link = "logit"))) 
summary(v1 <- glm(wtp ~ bid + Verified_Extreme + income + age + bipoc + educ + male +  trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"), subset = (ReportedFlood_NoVerified_Extreme == 0))) 
summary(v2 <- glm(wtp ~ bid + Verified_Extreme + income + age + bipoc + educ + male +  trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"), subset = (Verified_Extreme == 1 | extremeExperienceYN == 0))) 

summary(v3 <- glm(wtp ~ bid + Reported_Extreme + Verified_Extreme  + income + age + bipoc + educ + male +  trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"))) 
summary(v4 <- glm(wtp ~ bid + Verified_Extreme + Unverified_Extreme + income + age + bipoc + educ + male +  trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"))) 


# Table for Main Paper
stargazer(r1, v1, v2, v3, no.space = T, omit = c("income", "age", "bipoc", "educ", "male", "trump", "biden", "state", "Constant", "ccHumanCaused"),
          dep.var.labels = c("Support for Clean Energy Policy"),
          covariate.labels = c("Annual Policy Cost (\\$)"),
          add.lines = list(c("Demographic Controls", "Y", "Y", "Y"), c("Climate Belief Control", "Y", "Y", "Y"), c("State Fixed Effects", "Y", "Y", "Y"),
                           c("Flood Self-Reports Included", "Y", "N", "N"), c("Unverified Self-Reports Included", "Y", "Y", "N")),
          star.cutoffs = c(0.05, 0.01, 0.001)
          )

# Extended Table for Supplementary Materials
stargazer(r1, v1, v2, no.space = T,
          omit = c("state"),
          dep.var.labels = c("Support for Clean Energy Policy"),
          covariate.labels = c("Annual Policy Cost (\\$)", "Reported Extreme Event", "Verified Extreme Event", "Income", "Age", "BIPOC", "Education", "Male", "Trump Voter", "Biden Voter", "Climate Change Belief"),
          add.lines = list(c("State Fixed Effects", "Y", "Y", "Y"), c("Flood Self-Reports Included", "Y", "N", "N"), c("Unverified Self-Reports Included", "Y", "Y", "N")),
          star.cutoffs = c(0.05, 0.01, 0.001)
)


confint(r1)
confint(v1)
confint(v2)


#### WTP - Main Models ####

# Creating sample averages for control variables to estimate WTP for each model # 

d %>%
  filter(!is.na(d$wtp) & !is.na(d$bid) & !is.na(d$income) & !is.na(d$age) & !is.na(d$bipoc) & 
           !is.na(d$educ) & !is.na(d$male) & !is.na(d$trump) & !is.na(d$ccHumanCaused) &
           !is.na(d$Reported_Extreme) & !is.na(d$state)) -> d_m1

# Model 1
bid_m1 <- mean(d_m1$bid)
income_m1 <- mean(d_m1$income)
age_m1 <- mean(d_m1$age)
bipoc_m1 <- mean(d_m1$bipoc)
educ_m1 <- mean(d_m1$educ)
male_m1 <- mean(d_m1$male)
trump_m1 <- mean(d_m1$trump)
biden_m1 <- mean(d_m1$biden)
nonvoter_m1 <- mean(d_m1$nonvoter)
thirdparty_m1 <- mean(d_m1$thirdparty)
ccHumanCaused_m1 <- mean(d_m1$ccHumanCaused)

# Model 2
d %>%
  filter(!is.na(d$wtp) & !is.na(d$bid) & !is.na(d$income) & !is.na(d$age) & !is.na(d$bipoc) & 
           !is.na(d$educ) & !is.na(d$male) & !is.na(d$trump) & !is.na(d$ccHumanCaused) &
           !is.na(d$Verified_Extreme) & !is.na(d$state) &
           d$ReportedFlood_NoVerified_Extreme == 0) -> d_m2

bid_m2 <- mean(d_m2$bid)
income_m2 <- mean(d_m2$income)
age_m2 <- mean(d_m2$age)
bipoc_m2 <- mean(d_m2$bipoc)
educ_m2 <- mean(d_m2$educ)
male_m2 <- mean(d_m2$male)
trump_m2 <- mean(d_m2$trump)
biden_m2 <- mean(d_m2$biden)
nonvoter_m2 <- mean(d_m2$nonvoter)
thirdparty_m2 <- mean(d_m2$thirdparty)
ccHumanCaused_m2 <- mean(d_m2$ccHumanCaused)

# Model 3
d %>%
  filter(!is.na(d$wtp) & !is.na(d$bid) & !is.na(d$income) & !is.na(d$age) & !is.na(d$bipoc) & 
           !is.na(d$educ) & !is.na(d$male) & !is.na(d$trump) & !is.na(d$ccHumanCaused) &
           !is.na(d$Verified_Extreme) & !is.na(d$state)) %>%
  filter(Verified_Extreme == 1 | extremeExperienceYN == 0) -> d_m3

bid_m3 <- mean(d_m3$bid)
income_m3 <- mean(d_m3$income)
age_m3 <- mean(d_m3$age)
bipoc_m3 <- mean(d_m3$bipoc)
educ_m3 <- mean(d_m3$educ)
male_m3 <- mean(d_m3$male)
trump_m3 <- mean(d_m3$trump)
biden_m3 <- mean(d_m3$biden)
nonvoter_m3 <- mean(d_m3$nonvoter)
thirdparty_m3 <- mean(d_m3$thirdparty)
ccHumanCaused_m3 <- mean(d_m3$ccHumanCaused)

# WTP MODEL 1 #
# WTP with reported extreme weather event: $485.51, 95% CI: [$359.26, $611.76]
wtpReportedExtremeY_est <- -1*(r1$coefficients[1] + r1$coefficients[3] + r1$coefficients[4]*income_m1 + r1$coefficients[5]*age_m1 +
                                 r1$coefficients[6]*bipoc_m1 + r1$coefficients[7]*educ_m1 + r1$coefficients[8]*male_m1 +
                                 r1$coefficients[9]*trump_m1 + r1$coefficients[10]*biden_m1 + r1$coefficients[11]*ccHumanCaused_m1) /
  r1$coefficients[2]
wtpReportedExtremeY_se <- deltamethod(~(x1 + x3 + x4*income_m1 + x5*age_m1 + x6*bipoc_m1 + x7*educ_m1 + x8*male_m1 +
                                          x9*trump_m1 + x10*biden_m1 + x11*ccHumanCaused_m1)/x2, coef(r1), vcov(r1))
wtpReportedExtremeY_uci <- wtpReportedExtremeY_est + 1.96*wtpReportedExtremeY_se
wtpReportedExtremeY_lci <- wtpReportedExtremeY_est - 1.96*wtpReportedExtremeY_se
wtpReportedExtremeY <- c("Reported (Model 1)", "One or more", wtpReportedExtremeY_est, wtpReportedExtremeY_se, wtpReportedExtremeY_lci, wtpReportedExtremeY_uci)
wtpReportedExtremeY

# partial effect of reported extreme events on WTP: $112.04, 95% CI: [$57.00, $167.08]
wtpReportedExtreme_partial <- -1 * r1$coefficients[3]/r1$coefficients[2]
wtpReportedExtreme_partial
wtpReportedExtreme_partial_se <- deltamethod(~x3/x2, coef(r1), vcov(r1))
wtpReportedExtreme_partial_uci <- wtpReportedExtreme_partial + 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_lci <- wtpReportedExtreme_partial - 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_uci
wtpReportedExtreme_partial_lci

# WTP without reported extreme weather event: $374.74, 95% CI: [$273.27, $476.21]
wtpReportedExtremeN_est <- -1*(r1$coefficients[1] +r1$coefficients[4]*income_m1 + r1$coefficients[5]*age_m1 +
                                 r1$coefficients[6]*bipoc_m1 + r1$coefficients[7]*educ_m1 + 
                                 r1$coefficients[8]*male_m1 + r1$coefficients[9]*trump_m1 + r1$coefficients[10]*biden_m1 + 
                                 r1$coefficients[11]*ccHumanCaused_m1) / r1$coefficients[2]
wtpReportedExtremeN_est
wtpReportedExtremeN_se <- deltamethod(~(x1 + x4*income_m1 + x5*age_m1 + x6*bipoc_m1 + x7*educ_m1 + x8*male_m1 +
                                          x9*trump_m1 + x10*biden_m1 + x11*ccHumanCaused_m1)/x2, coef(r1), vcov(r1))
wtpReportedExtremeN_se
wtpReportedExtremeN_uci <- wtpReportedExtremeN_est + 1.96*wtpReportedExtremeN_se
wtpReportedExtremeN_uci
wtpReportedExtremeN_lci <- wtpReportedExtremeN_est - 1.96*wtpReportedExtremeN_se
wtpReportedExtremeN_lci
wtpReportedExtremeN <- c("Reported (Model 1)", "None", wtpReportedExtremeN_est, wtpReportedExtremeN_se, wtpReportedExtremeN_lci, wtpReportedExtremeN_uci)
wtpReportedExtremeN


# WTP Model 2 #
# Verified events with "No Verified Event" only excluding those who reported flooding and had no other verified events

# WTP with verified extreme weather event: $455.97, 95% CI: [$337.38, $574.57]
wtpVerifiedExtremeY_est <- -1*(v1$coefficients[1] + v1$coefficients[3] + v1$coefficients[4]*income_m2 + v1$coefficients[5]*age_m2 +
                                 v1$coefficients[6]*bipoc_m2 + v1$coefficients[7]*educ_m2 + v1$coefficients[8]*male_m2 +
                                 v1$coefficients[9]*trump_m2 + v1$coefficients[10]*biden_m2 + 
                                 v1$coefficients[11]*ccHumanCaused_m2) / v1$coefficients[2]
wtpVerifiedExtremeY_se <- deltamethod(~(x1 + x3 + x4*income_m2 + x5*age_m2 + x6*bipoc_m2 + x7*educ_m2 + x8*male_m2 +
                                          x9*trump_m2 + x10*biden_m2 + x11*ccHumanCaused_m2)/x2, coef(v1), vcov(v1))
wtpVerifiedExtremeY_uci <- wtpVerifiedExtremeY_est + 1.96*wtpVerifiedExtremeY_se
wtpVerifiedExtremeY_lci <- wtpVerifiedExtremeY_est - 1.96*wtpVerifiedExtremeY_se
wtpVerifiedExtremeY <- c("Verified 1 (Model 2)", "One or more", wtpVerifiedExtremeY_est, wtpVerifiedExtremeY_se, wtpVerifiedExtremeY_lci, wtpVerifiedExtremeY_uci)
wtpVerifiedExtremeY

# partial effect of verified extreme events on WTP = $71.38, 95% CI: [$15.85, $126.90]
wtpVerifiedExtreme_partial <- -1 * v1$coefficients[3]/v1$coefficients[2]
wtpVerifiedExtreme_partial
wtpVerifiedExtreme_partial_se <- deltamethod(~x3/x2, coef(v1), vcov(v1))
wtpVerifiedExtreme_partial_uci <- wtpVerifiedExtreme_partial + 1.96*wtpVerifiedExtreme_partial_se
wtpVerifiedExtreme_partial_lci <- wtpVerifiedExtreme_partial - 1.96*wtpVerifiedExtreme_partial_se
wtpVerifiedExtreme_partial_uci
wtpVerifiedExtreme_partial_lci

# WTP without verified extreme weather event: $384.60, 95% CI: [$283.90, $485.29]
wtpVerifiedExtremeN_est <- -1*(v1$coefficients[1] + v1$coefficients[4]*income_m2 + v1$coefficients[5]*age_m2 +
                                 v1$coefficients[6]*bipoc_m2 + v1$coefficients[7]*educ_m2 + v1$coefficients[8]*male_m2 +
                                 v1$coefficients[9]*trump_m2 + v1$coefficients[10]*biden_m2 + 
                                 v1$coefficients[11]*ccHumanCaused_m2) / v1$coefficients[2]
wtpVerifiedExtremeN_est
wtpVerifiedExtremeN_se <- deltamethod(~(x1 + x4*income_m2 + x5*age_m2 + x6*bipoc_m2 + x7*educ_m2 + x8*male_m2 +
                                          x9*trump_m2 + x10*biden_m2 + x11*ccHumanCaused_m2)/x2, coef(v1), vcov(v1))
wtpVerifiedExtremeN_se
wtpVerifiedExtremeN_uci <- wtpVerifiedExtremeN_est + 1.96*wtpVerifiedExtremeN_se
wtpVerifiedExtremeN_uci
wtpVerifiedExtremeN_lci <- wtpVerifiedExtremeN_est - 1.96*wtpVerifiedExtremeN_se
wtpVerifiedExtremeN_lci
wtpVerifiedExtremeN <- c("Verified 1 (Model 2)", "None", wtpVerifiedExtremeN_est, wtpVerifiedExtremeN_se, wtpVerifiedExtremeN_lci, wtpVerifiedExtremeN_uci)
wtpVerifiedExtremeN


# Model 3 #
# Verified events with "no verified events" excluding those who reported an event but who did not have that event verified. Instead of assuming they were lying or mistaken and putting them into the comparison group, we assume there was a data mismatch and exclude them.

# WTP with verified extreme weather event: $460.66, 95% CI: [$331.18, $590.14]
wtpVerified2ExtremeY_est <- -1*(v2$coefficients[1] + v2$coefficients[3] + v2$coefficients[4]*income_m3 + v2$coefficients[5]*age_m3 +
                                  v2$coefficients[6]*bipoc_m3 + v2$coefficients[7]*educ_m3 + v2$coefficients[8]*male_m3 +
                                  v2$coefficients[9]*trump_m3 + v2$coefficients[10]*biden_m3 + 
                                  v2$coefficients[11]*ccHumanCaused_m3) / v2$coefficients[2]
wtpVerified2ExtremeY_se <- deltamethod(~(x1 + x3 + x4*income_m3 + x5*age_m3 + x6*bipoc_m3 + x7*educ_m3 + x8*male_m3 +
                                           x9*trump_m3 + x10*biden_m3 + x11*ccHumanCaused_m3)/x2, coef(v2), vcov(v2))
wtpVerified2ExtremeY_uci <- wtpVerified2ExtremeY_est + 1.96*wtpVerified2ExtremeY_se
wtpVerified2ExtremeY_lci <- wtpVerified2ExtremeY_est - 1.96*wtpVerified2ExtremeY_se
wtpVerified2ExtremeY <- c("Verified 2 (Model 3)", "One or more", wtpVerified2ExtremeY_est, wtpVerified2ExtremeY_se, wtpVerified2ExtremeY_lci, wtpVerified2ExtremeY_uci)
wtpVerified2ExtremeY

# partial effect of verified extreme events on WTP = $106.11, 95% CI: [$41.95, $170.27]
wtpVerified2Extreme_partial <- -1 * v2$coefficients[3]/v2$coefficients[2]
wtpVerified2Extreme_partial
wtpVerified2Extreme_partial_se <- deltamethod(~x3/x2, coef(v2), vcov(v2))
wtpVerified2Extreme_partial_uci <- wtpVerified2Extreme_partial + 1.96*wtpVerified2Extreme_partial_se
wtpVerified2Extreme_partial_lci <- wtpVerified2Extreme_partial - 1.96*wtpVerified2Extreme_partial_se
wtpVerified2Extreme_partial_uci
wtpVerified2Extreme_partial_lci

# WTP without verified extreme weather event: $354.55, 95% CI: [$251.60, $457.51]
wtpVerified2ExtremeN_est <- -1*(v2$coefficients[1] + v2$coefficients[4]*income_m3 + v2$coefficients[5]*age_m3 +
                                  v2$coefficients[6]*bipoc_m3 + v2$coefficients[7]*educ_m3 + v2$coefficients[8]*male_m3 +
                                  v2$coefficients[9]*trump_m3 + v2$coefficients[10]*biden_m3 + 
                                  v2$coefficients[11]*ccHumanCaused_m3) / v2$coefficients[2]
wtpVerified2ExtremeN_est
wtpVerified2ExtremeN_se <- deltamethod(~(x1 + x4*income_m3 + x5*age_m3 + x6*bipoc_m3 + x7*educ_m3 + x8*male_m3 +
                                           x9*trump_m3 + x10*biden_m3 + x11*ccHumanCaused_m3)/x2, coef(v2), vcov(v2))
wtpVerified2ExtremeN_se
wtpVerified2ExtremeN_uci <- wtpVerified2ExtremeN_est + 1.96*wtpVerified2ExtremeN_se
wtpVerified2ExtremeN_uci
wtpVerified2ExtremeN_lci <- wtpVerified2ExtremeN_est - 1.96*wtpVerified2ExtremeN_se
wtpVerified2ExtremeN_lci
wtpVerified2ExtremeN <- c("Verified 2 (Model 3)", "None", wtpVerified2ExtremeN_est, wtpVerified2ExtremeN_se, wtpVerified2ExtremeN_lci, wtpVerified2ExtremeN_uci)
wtpVerified2ExtremeN


# Percent change in WTP
# Reported Extreme event increases WTP by 30%
# Verified (v1) extreme event increases WTP by 19%
# Verified (v2) extreme event increases WTP by 30%
(wtpReportedExtremeY_est - wtpReportedExtremeN_est)/wtpReportedExtremeN_est
(wtpVerifiedExtremeY_est - wtpVerifiedExtremeN_est)/wtpVerifiedExtremeN_est
(wtpVerified2ExtremeY_est - wtpVerified2ExtremeN_est)/wtpVerified2ExtremeN_est

# Creating dataframe of WTP estimates
wtpCombinedExtremeEvents <- data.frame(rbind(wtpReportedExtremeY, wtpVerifiedExtremeY, wtpVerified2ExtremeY, wtpReportedExtremeN, wtpVerifiedExtremeN, wtpVerified2ExtremeN))
wtpCombinedExtremeEvents %>%  # renaming columns
  rename(DataType = 1,
         ExtremeEvents = 2,
         WTP_PointEst = 3,
         WTP_se = 4,
         WTP_CI_lower = 5,
         WTP_CI_upper = 6) -> wtpCombinedExtremeEvents
# Converting to numeric columns
cols.num <- c("WTP_PointEst", "WTP_se", "WTP_CI_lower", "WTP_CI_upper")
wtpCombinedExtremeEvents[cols.num] <- sapply(wtpCombinedExtremeEvents[cols.num], as.numeric)

ggplot(data = wtpCombinedExtremeEvents, aes(fill = ExtremeEvents, y = WTP_PointEst, x = DataType)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = WTP_PointEst - WTP_se, ymax = WTP_PointEst + WTP_se), width = .1, position = position_dodge(.9)) +
  scale_fill_manual("Extreme Events", values = c("#453781FF", "#3CBC75FF")) +
  ggtitle("WTP and Extreme Weather Events") +
  xlab("") +
  ylab("Willingness to Pay for Clean Energy Policy ($/year)") +
  theme(legend.position = "bottom")

ggsave("output/WTP_AllExtremeEvents_v2.png", width = 4, height = 5, units = "in")




#### SubGroup Analysis ####

# Climate Belief
summary(cb_r_1 <- glm(wtp ~ bid + Reported_Extreme + income + age + bipoc + educ + male +  trump + biden + state, data = d, family = binomial(link = "logit"), subset = (ccHumanCaused == 1))) 
summary(cb_r_0 <- glm(wtp ~ bid + Reported_Extreme + income + age + bipoc + educ + male +  trump + biden + state, data = d, family = binomial(link = "logit"), subset = (ccHumanCaused == 0))) 

summary(cb_v_1 <- glm(wtp ~ bid + Verified_Extreme + income + age + bipoc + educ + male +  trump + biden + state, data = d, family = binomial(link = "logit"), subset = (Verified_Extreme == 1 & ccHumanCaused == 1 | extremeExperienceYN == 0 & ccHumanCaused == 1))) 
summary(cb_v_0 <- glm(wtp ~ bid + Verified_Extreme + income + age + bipoc + educ + male +  trump + biden + state, data = d, family = binomial(link = "logit"), subset = (Verified_Extreme == 1 & ccHumanCaused == 0 | extremeExperienceYN == 0 & ccHumanCaused == 0))) 

stargazer(cb_r_1, cb_v_1, cb_r_0, cb_v_0, 
          no.space = T,
          omit = c("state"),
          dep.var.labels = c("Support for Clean Energy Policy"),
          covariate.labels = c("Annual Policy Cost (\\$)", "Reported Extreme Event", "Verified Extreme Event", "Income", "Age", "BIPOC", "Education", "Male", "Trump Voter", "Biden Voter"),
          add.lines = list(c("State Fixed Effects", "Y", "Y", "Y", "Y"), c("Flood Self-Reports Included", "Y", "N", "Y", "N"), c("Unverified Self-Reports Included", "Y", "N", "Y", "N")),
          star.cutoffs = c(0.05, 0.01, 0.001)
)


# Voting Behavior
summary(vb_r_b <- glm(wtp ~ bid + Reported_Extreme + income + age + bipoc + educ + male + ccHumanCaused + state, data = d, family = binomial(link = "logit"), subset = (biden == 1))) 
summary(vb_r_t <- glm(wtp ~ bid + Reported_Extreme + income + age + bipoc + educ + male + ccHumanCaused + state, data = d, family = binomial(link = "logit"), subset = (trump == 1))) 

summary(vb_v_b <- glm(wtp ~ bid + Verified_Extreme + income + age + bipoc + educ + male + state + ccHumanCaused, data = d, family = binomial(link = "logit"), subset = (Verified_Extreme == 1 & biden == 1 | extremeExperienceYN == 0 & biden == 1))) 
summary(vb_v_t <- glm(wtp ~ bid + Verified_Extreme + income + age + bipoc + educ + male + state + ccHumanCaused, data = d, family = binomial(link = "logit"), subset = (Verified_Extreme == 1 & trump == 1 | extremeExperienceYN == 0 & trump == 1))) 

stargazer(vb_r_b, vb_v_b, vb_r_t, vb_v_t, 
          no.space = T,
          omit = c("state"),
          dep.var.labels = c("Support for Clean Energy Policy"),
          covariate.labels = c("Annual Policy Cost (\\$)", "Reported Extreme Event", "Verified Extreme Event", "Income", "Age", "BIPOC", "Education", "Male", "Climate Change Belief"),
          add.lines = list(c("State Fixed Effects", "Y", "Y", "Y", "Y"), c("Flood Self-Reports Included", "Y", "N", "Y", "N"), c("Unverified Self-Reports Included", "Y", "N", "Y", "N")),
          star.cutoffs = c(0.05, 0.01, 0.001)
)


#### Sub-Group Analysis of WTP Estimates ####

# Models: 
# cb_r_1, cb_v_1, cb_r_0, cb_v_0, 
# vb_r_b, vb_v_b, vb_r_t, vb_v_t

# Climate Believers; Reported Events
# partial effect of reported extreme events on WTP: $146.40, 95% CI: [$54.31, $238.48]
wtpReportedExtreme_partial <- -1 * cb_r_1$coefficients[3]/cb_r_1$coefficients[2]
wtpReportedExtreme_partial
wtpReportedExtreme_partial_se <- deltamethod(~x3/x2, coef(cb_r_1), vcov(cb_r_1))
wtpReportedExtreme_partial_uci <- wtpReportedExtreme_partial + 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_lci <- wtpReportedExtreme_partial - 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_uci
wtpReportedExtreme_partial_lci

# Climate Believers; Verified Events
# partial effect of reported extreme events on WTP: $127.46, 95% CI: [$33.28, $221.64]
wtpReportedExtreme_partial <- -1 * cb_v_1$coefficients[3]/cb_v_1$coefficients[2]
wtpReportedExtreme_partial
wtpReportedExtreme_partial_se <- deltamethod(~x3/x2, coef(cb_v_1), vcov(cb_v_1))
wtpReportedExtreme_partial_uci <- wtpReportedExtreme_partial + 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_lci <- wtpReportedExtreme_partial - 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_uci
wtpReportedExtreme_partial_lci

# Climate Deniers; Reported Events
# partial effect of reported extreme events on WTP: $76.26, 95% CI: [$10.48, $142.03]
wtpReportedExtreme_partial <- -1 * cb_r_0$coefficients[3]/cb_r_0$coefficients[2]
wtpReportedExtreme_partial
wtpReportedExtreme_partial_se <- deltamethod(~x3/x2, coef(cb_r_0), vcov(cb_r_0))
wtpReportedExtreme_partial_uci <- wtpReportedExtreme_partial + 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_lci <- wtpReportedExtreme_partial - 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_uci
wtpReportedExtreme_partial_lci

# Climate Deniers; Verified Events
# partial effect of reported extreme events on WTP: $80.16, 95% CI: [-$4.67, $164.99]
wtpReportedExtreme_partial <- -1 * cb_v_0$coefficients[3]/cb_v_0$coefficients[2]
wtpReportedExtreme_partial
wtpReportedExtreme_partial_se <- deltamethod(~x3/x2, coef(cb_v_0), vcov(cb_v_0))
wtpReportedExtreme_partial_uci <- wtpReportedExtreme_partial + 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_lci <- wtpReportedExtreme_partial - 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_uci
wtpReportedExtreme_partial_lci


# Biden Voters; Reported Events
# partial effect of reported extreme events on WTP: $143.81, 95% CI: [$53.26, $234.36]
wtpReportedExtreme_partial <- -1 * vb_r_b$coefficients[3]/vb_r_b$coefficients[2]
wtpReportedExtreme_partial
wtpReportedExtreme_partial_se <- deltamethod(~x3/x2, coef(vb_r_b), vcov(vb_r_b))
wtpReportedExtreme_partial_uci <- wtpReportedExtreme_partial + 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_lci <- wtpReportedExtreme_partial - 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_uci
wtpReportedExtreme_partial_lci

# Biden Voters; Verified Events
# partial effect of reported extreme events on WTP: $116.32, 95% CI: [$17.41, $215.24]
wtpReportedExtreme_partial <- -1 * vb_v_b$coefficients[3]/vb_v_b$coefficients[2]
wtpReportedExtreme_partial
wtpReportedExtreme_partial_se <- deltamethod(~x3/x2, coef(vb_v_b), vcov(vb_v_b))
wtpReportedExtreme_partial_uci <- wtpReportedExtreme_partial + 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_lci <- wtpReportedExtreme_partial - 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_uci
wtpReportedExtreme_partial_lci

# Trump Voters; Reported Events
# partial effect of reported extreme events on WTP: $85.26, 95% CI: [-$7.03, $177.55]
wtpReportedExtreme_partial <- -1 * vb_r_t$coefficients[3]/vb_r_t$coefficients[2]
wtpReportedExtreme_partial
wtpReportedExtreme_partial_se <- deltamethod(~x3/x2, coef(vb_r_t), vcov(vb_r_t))
wtpReportedExtreme_partial_uci <- wtpReportedExtreme_partial + 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_lci <- wtpReportedExtreme_partial - 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_uci
wtpReportedExtreme_partial_lci

# Trump Voters; Verified Events
# partial effect of reported extreme events on WTP: $107.51, 95% CI: [-$28.79, $243.80]
wtpReportedExtreme_partial <- -1 * vb_v_t$coefficients[3]/vb_v_t$coefficients[2]
wtpReportedExtreme_partial
wtpReportedExtreme_partial_se <- deltamethod(~x3/x2, coef(vb_v_t), vcov(vb_v_t))
wtpReportedExtreme_partial_uci <- wtpReportedExtreme_partial + 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_lci <- wtpReportedExtreme_partial - 1.96*wtpReportedExtreme_partial_se
wtpReportedExtreme_partial_uci
wtpReportedExtreme_partial_lci


ggplot(data = wtpCombinedExtremeEvents, aes(fill = ExtremeEvents, y = WTP_PointEst, x = DataType)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = WTP_PointEst - WTP_se, ymax = WTP_PointEst + WTP_se), width = .1, position = position_dodge(.9)) +
  scale_fill_manual("Extreme Events", values = c("#453781FF", "#3CBC75FF")) +
  ggtitle("WTP and Extreme Weather Events") +
  xlab("") +
  ylab("Willingness to Pay for Clean Energy Policy ($/year)") +
  theme(legend.position = "bottom")



#### Event Reports and Data by Type ####
d$Reported_HurricaneOnly <- ifelse(d$Reported_Hurricane == 1 & d$totalReportedTypes == 1, 1, 0)
d$Reported_DroughtOnly <- ifelse(d$Reported_Drought == 1 & d$totalReportedTypes == 1, 1, 0)
d$Reported_HeatwaveOnly <- ifelse(d$Reported_Heatwave == 1 & d$totalReportedTypes == 1, 1, 0)
d$Reported_TornadoOnly <- ifelse(d$Reported_Tornado == 1 & d$totalReportedTypes == 1, 1, 0)
d$Reported_WildfireOnly <- ifelse(d$Reported_Wildfire == 1 & d$totalReportedTypes == 1, 1, 0)
d$Reported_FloodOnly <- ifelse(d$Reported_Flood == 1 & d$totalReportedTypes == 1, 1, 0)



# Main model for event specific WTP: et4
summary(et4 <- glm(wtp ~ bid  + Reported_Wildfire + Reported_Drought + Reported_Heatwave + Reported_Tornado + Reported_Hurricane + Reported_Flood + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"))) 
summary(et4_i <- glm(wtp ~ bid  + Reported_Wildfire*Reported_Drought*Reported_Heatwave + Reported_Tornado*Reported_Hurricane*Reported_Flood + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"))) 

linearHypothesis(et4, "Reported_Wildfire = Reported_Hurricane")

stargazer(et4, et4_i, 
          no.space = T, 
          omit = c("income", "age", "bipoc", "educ", "male", "trump", "biden", "state", "Constant", "ccHumanCaused"),
          dep.var.labels = c("Support for Clean Energy Policy"),
          covariate.labels = c("Annual Policy Cost (\\$)"),
          add.lines = list(c("Demographic Controls", "Y", "Y", "Y", "Y"), c("Climate Belief Control", "Y", "Y", "Y", "Y"), c("State Fixed Effects", "Y", "Y", "Y", "Y")),
          star.cutoffs = c(0.05, 0.01, 0.001))

# Compare those who reported one extreme event to those who reported none
# Significant: Tornados/Windstorms, Wildfire
# Not Significant: Drought, Heatwave, Flood, Hurricane, 
summary(et4_to <- glm(wtp ~ bid  + Reported_TornadoOnly + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"), subset = Reported_TornadoOnly == 1 | extremeExperienceYN == 0))
summary(et4_do <- glm(wtp ~ bid  + Reported_DroughtOnly + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"), subset = Reported_DroughtOnly == 1 | extremeExperienceYN == 0)) 
summary(et4_hwo <- glm(wtp ~ bid  + Reported_HeatwaveOnly + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"), subset = Reported_HeatwaveOnly == 1 | extremeExperienceYN == 0))
summary(et4_fo <- glm(wtp ~ bid  + Reported_FloodOnly + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"), subset = Reported_FloodOnly == 1 | extremeExperienceYN == 0)) 
summary(et4_ho <- glm(wtp ~ bid  + Reported_HurricaneOnly + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"), subset = Reported_HurricaneOnly == 1 | extremeExperienceYN == 0)) 
summary(et4_wo <- glm(wtp ~ bid  + Reported_WildfireOnly + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"), subset = Reported_WildfireOnly == 1 | extremeExperienceYN == 0))

stargazer(et4_to, et4_do, et4_hwo, et4_fo, et4_ho, et4_wo, 
          no.space = T, 
          omit = c("income", "age", "bipoc", "educ", "male", "trump", "biden", "state", "Constant", "ccHumanCaused"),
          dep.var.labels = c("Support for Clean Energy Policy"),
          covariate.labels = c("Annual Policy Cost (\\$)"),
          add.lines = list(c("Demographic Controls", "Y", "Y", "Y", "Y", "Y", "Y"), c("Climate Belief Control", "Y", "Y", "Y", "Y", "Y", "Y"), c("State Fixed Effects", "Y", "Y", "Y", "Y", "Y", "Y"),
                           c("Excludes Multi-Event Type Reports", "Y", "Y", "Y", "Y", "Y", "Y")),
          star.cutoffs = c(0.05, 0.01, 0.001))


# Exploring the overlaps with wildfire, drought, and heatwaves
# Heatwaves matter on their own. But when a heatwave combines with a wildfire, there is no additional impact of the heatwave.
# When a wildfire and a drought both occur, the impact of the wildfire is reduced.
# Three way interactions are not significant, but they show the same pattern.
summary(et4_wd <- glm(wtp ~ bid  + Reported_Wildfire*Reported_Drought + Reported_Heatwave + Reported_Tornado + Reported_Hurricane + Reported_Flood + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"))) 
summary(et4_wh <- glm(wtp ~ bid  + Reported_Wildfire + Reported_Drought + Reported_Heatwave*Reported_Wildfire + Reported_Tornado + Reported_Hurricane + Reported_Flood + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"))) 
summary(et4_whd <- glm(wtp ~ bid  + Reported_Wildfire*Reported_Drought*Reported_Heatwave + Reported_Tornado + Reported_Hurricane + Reported_Flood + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"))) 

# Exploring the overlap with drought and heatwaves (rho = 0.415)
# When a drought and heatwave both occur, there is impact on WTP. But after controlling for the co-occurrence, heatwaves have a significant relationship with drought.
# We can also see this by taking out everyone who reported drought and looking at the remaining variables -- removing drought reporters increases the strength of both wildfires and heatwaves.
summary(et4_dh <- glm(wtp ~ bid  + Reported_Wildfire + Reported_Drought*Reported_Heatwave + Reported_Tornado + Reported_Hurricane + Reported_Flood + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"))) 

stargazer(et4, et4_wd, et4_wh, et4_dh, et4_whd,
          no.space = T, 
          omit = c("income", "age", "bipoc", "educ", "male", "trump", "biden", "state", "Constant", "ccHumanCaused"),
          dep.var.labels = c("Support for Clean Energy Policy"),
          covariate.labels = c("Annual Policy Cost (\\$)"),
          add.lines = list(c("Demographic Controls", "Y", "Y", "Y", "Y", "Y"), c("Climate Belief Control", "Y", "Y", "Y", "Y", "Y"), c("State Fixed Effects", "Y", "Y", "Y", "Y", "Y")),
          star.cutoffs = c(0.05, 0.01, 0.001))

# Exploring overlap between hurricanes, floods, and tornados/windstorms
# Two-way interactions between hurricanes, floods, and torndaos/windstorms are not significant and do not substantially change individual event variables 
# A three-way interaction between hurricanes, floods, and tornados/windstorms leads floods and tornado/windstorms to become marginally significant, but the interactions are not significant.
summary(et4_ht <- glm(wtp ~ bid  + Reported_Wildfire + Reported_Drought + Reported_Heatwave + Reported_Tornado*Reported_Hurricane + Reported_Flood + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"))) 
summary(et4_hf <- glm(wtp ~ bid  + Reported_Wildfire + Reported_Drought + Reported_Heatwave + Reported_Tornado + Reported_Hurricane*Reported_Flood + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"))) 
summary(et4_ft <- glm(wtp ~ bid  + Reported_Wildfire + Reported_Drought + Reported_Heatwave + Reported_Tornado*Reported_Flood + Reported_Hurricane + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"))) 
summary(et4_hft <- glm(wtp ~ bid  + Reported_Wildfire + Reported_Drought + Reported_Heatwave + Reported_Tornado*Reported_Hurricane*Reported_Flood + income + age + bipoc + educ + male + trump + biden + ccHumanCaused + state, data = d, family = binomial(link = "logit"))) 

stargazer(et4, et4_ht, et4_hf, et4_ft, et4_hft,
          no.space = T, 
          omit = c("income", "age", "bipoc", "educ", "male", "trump", "biden", "state", "Constant", "ccHumanCaused"),
          dep.var.labels = c("Support for Clean Energy Policy"),
          covariate.labels = c("Annual Policy Cost (\\$)"),
          add.lines = list(c("Demographic Controls", "Y", "Y", "Y", "Y", "Y"), c("Climate Belief Control", "Y", "Y", "Y", "Y", "Y"), c("State Fixed Effects", "Y", "Y", "Y", "Y", "Y")),
          star.cutoffs = c(0.05, 0.01, 0.001))


#### Event-Type Partial WTP Estimates ####

#et4 is the focal model - calculate wtp for the five types of reported events

# partial effect of reported WILDFIRE on WTP: $146.45, 95% CI: [$60.66, $232.25]
wtpReported_Wildfire_partial <- -1 * et4$coefficients[3]/et4$coefficients[2]
wtpReported_Wildfire_partial
wtpReported_Wildfire_partial_se <- deltamethod(~x3/x2, coef(et4), vcov(et4))
wtpReported_Wildfire_partial_uci <- wtpReported_Wildfire_partial + 1.96*wtpReported_Wildfire_partial_se
wtpReported_Wildfire_partial_lci <- wtpReported_Wildfire_partial - 1.96*wtpReported_Wildfire_partial_se
wtpReported_Wildfire_partial_uci
wtpReported_Wildfire_partial_lci

# partial effect of reported DROUGHT on WTP: -$45.10, 95% CI: [-$121.21, $31.01]
wtpReported_Drought_partial <- -1 * et4$coefficients[4]/et4$coefficients[2]
wtpReported_Drought_partial
wtpReported_Drought_partial_se <- deltamethod(~x4/x2, coef(et4), vcov(et4))
wtpReported_Drought_partial_uci <- wtpReported_Drought_partial + 1.96*wtpReported_Drought_partial_se
wtpReported_Drought_partial_lci <- wtpReported_Drought_partial - 1.96*wtpReported_Drought_partial_se
wtpReported_Drought_partial_uci
wtpReported_Drought_partial_lci

# partial effect of reported HEATWAVE on WTP: $53.98, 95% CI: [-$20.78, $128.73]
wtpReported_Heatwave_partial <- -1 * et4$coefficients[5]/et4$coefficients[2]
wtpReported_Heatwave_partial
wtpReported_Heatwave_partial_se <- deltamethod(~x5/x2, coef(et4), vcov(et4))
wtpReported_Heatwave_partial_uci <- wtpReported_Heatwave_partial + 1.96*wtpReported_Heatwave_partial_se
wtpReported_Heatwave_partial_lci <- wtpReported_Heatwave_partial - 1.96*wtpReported_Heatwave_partial_se
wtpReported_Heatwave_partial_uci
wtpReported_Heatwave_partial_lci

# partial effect of reported TORNADO on WTP: $33.63, 95% CI: [-$35.61, $102.88]
wtpReported_Tornado_partial <- -1 * et4$coefficients[6]/et4$coefficients[2]
wtpReported_Tornado_partial
wtpReported_Tornado_partial_se <- deltamethod(~x6/x2, coef(et4), vcov(et4))
wtpReported_Tornado_partial_uci <- wtpReported_Tornado_partial + 1.96*wtpReported_Tornado_partial_se
wtpReported_Tornado_partial_lci <- wtpReported_Tornado_partial - 1.96*wtpReported_Tornado_partial_se
wtpReported_Tornado_partial_uci
wtpReported_Tornado_partial_lci

# partial effect of reported HURRICANE on WTP: $70.70, 95% CI: [$3.77, $137.63]
wtpReported_Hurricane_partial <- -1 * et4$coefficients[7]/et4$coefficients[2]
wtpReported_Hurricane_partial
wtpReported_Hurricane_partial_se <- deltamethod(~x7/x2, coef(et4), vcov(et4))
wtpReported_Hurricane_partial_uci <- wtpReported_Hurricane_partial + 1.96*wtpReported_Hurricane_partial_se
wtpReported_Hurricane_partial_lci <- wtpReported_Hurricane_partial - 1.96*wtpReported_Hurricane_partial_se
wtpReported_Hurricane_partial_uci
wtpReported_Hurricane_partial_lci

# partial effect of reported FLOOD on WTP: $56.67, 95% CI: [-$16.40, $129.75]
wtpReported_Flood_partial <- -1 * et4$coefficients[8]/et4$coefficients[2]
wtpReported_Flood_partial
wtpReported_Flood_partial_se <- deltamethod(~x8/x2, coef(et4), vcov(et4))
wtpReported_Flood_partial_uci <- wtpReported_Flood_partial + 1.96*wtpReported_Flood_partial_se
wtpReported_Flood_partial_lci <- wtpReported_Flood_partial - 1.96*wtpReported_Flood_partial_se
wtpReported_Flood_partial_uci
wtpReported_Flood_partial_lci

wtpEvents <- data.frame(
  Event = c("Wildfire", "Drought", "Heatwave", "Tornado", "Hurricane", "Flood"),
  wtp = c(wtpReported_Wildfire_partial,wtpReported_Drought_partial, wtpReported_Heatwave_partial, wtpReported_Tornado_partial, wtpReported_Hurricane_partial, wtpReported_Flood_partial),
  se = c(wtpReported_Wildfire_partial_se,wtpReported_Drought_partial_se, wtpReported_Heatwave_partial_se, wtpReported_Tornado_partial_se, wtpReported_Hurricane_partial_se, wtpReported_Flood_partial_se),
  wtp_lowerCI = c(wtpReported_Wildfire_partial_lci,wtpReported_Drought_partial_lci, wtpReported_Heatwave_partial_lci, wtpReported_Tornado_partial_lci, wtpReported_Hurricane_partial_lci, wtpReported_Flood_partial_lci),
  wtp_upperCI = c(wtpReported_Wildfire_partial_uci,wtpReported_Drought_partial_uci, wtpReported_Heatwave_partial_uci, wtpReported_Tornado_partial_uci, wtpReported_Hurricane_partial_uci, wtpReported_Flood_partial_uci))

ggplot(
  wtpEvents,
  geom_style = c("pointrange", "errorbar", "ribbon"),
  multi_style = c("dodge", "facet"),
  aggr_eff = c("none", "post", "pre", "both"),
  aggr_eff.par = list(col = "grey50", lwd = 1, lty = 1)
)

# A Plot of Beauty
ggplot(data = wtpEvents) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  geom_pointrange(aes(x = wtp, y = Event, color = Event, fill = Event), xmin = wtpEvents$wtp_lowerCI, xmax = wtpEvents$wtp_upperCI,
                  shape = 23, show.legend = FALSE) +
  xlim(-125,225) +
  theme_light() +
  ylab("") +
  xlab("Marginal WTP for Clean Energy") +
  geom_vline(xintercept = 0, color = "grey80", linetype = "dashed")
  
ggsave("output/ReportedExtremeEventsTypes_WTP_MarginalImpact.png", width = 6.5, height = 4, units = "in")




# Correlation tables
d %>%
  dplyr::select(Reported_Wildfire, Reported_Drought, Reported_Heatwave, Reported_Hurricane, Reported_Flood, Reported_Tornado) -> r
cor(r)

d %>%
  dplyr::select(Verified_Wildfire, Verified_Drought, Verified_Heatwave, Verified_Hurricane, Verified_Tornado) -> r
cor(r)

d %>% 
  dplyr::select(Registered_Wildfire, Registered_Drought, Registered_Heatwave, Registered_Hurricane, Registered_Tornado) -> r
cor(r)

d %>%
  dplyr::select(Registered_Wildfire, Registered_Drought, Registered_Heatwave, Registered_Hurricane, Registered_Tornado, Reported_Wildfire, Reported_Drought, Reported_Heatwave, Reported_Hurricane, Reported_Flood, Reported_Tornado) -> r
cor(r)

# Strong correlations among reported wildfires, drought, and heatwaves and among reported hurricane, flood, and tornado
# People don't seem to respond to drought and heatwaves alone, but I wonder if their impact increases in combos



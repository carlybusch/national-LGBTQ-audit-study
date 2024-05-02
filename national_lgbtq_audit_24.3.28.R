#### Data analysis
#### National LGBTQ+ audit study
#### March 28, 2024

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ordinal)
library(sjstats)
library(reghelper)
library(reshape2)
library(nnet)

my_data <- read.csv("24.3.28_deid_nationalaudit.csv")
audit_data <- read.csv("24.3.28_deid_nationalaudit_fulldf.csv")

#### setting reference groups ----
my_data$religion3 <- factor(my_data$religion3, 
                            levels = c("nonreligious", "otherreligion",
                                       "christian"))
my_data$religion3 <- relevel(my_data$religion3, ref = "nonreligious")

my_data$lgbtq_accept_uni <- factor(my_data$lgbtq_accept_uni,
                                   levels = c("low", "mid", "high"))

my_data$lgbtq_accept_grewup <- factor(my_data$lgbtq_accept_grewup,
                                      levels = c("low", "mid", "high"))

audit_data$religion3 <- factor(audit_data$religion3, 
                               levels = c("nonreligious", "otherreligion",
                                          "christian"))
audit_data$religion3 <- relevel(audit_data$religion3, ref = "nonreligious")

audit_data$lgbtq_accept_uni <- factor(audit_data$lgbtq_accept_uni,
                                      levels = c("low", "mid", "high"))

my_data$language2 <- factor(my_data$language2, 
                            levels = c("english", "spanish",
                                       "other"))
my_data$language2 <- relevel(my_data$language2, ref = "english")

my_data$race4 <- factor(my_data$race4, 
                        levels = c("other", "asian",
                                   "black", "latinx","white"))
my_data$race4 <- relevel(my_data$race4, ref = "white")

### calculating cronbach's alpha for the scales ----
library(ltm)
#hireability
cronbach.alpha(my_data[,c("hire.comp.like1_1", "hire.comp.like1_2", "hire.comp.like1_3")], standardized = T, na.rm = T)
#competence
cronbach.alpha(my_data[,c("hire.comp.like1_4", "hire.comp.like1_5", "hire.comp.like2_1")], standardized = T, na.rm = T)
#likeability
cronbach.alpha(my_data[,c("hire.comp.like2_2", "hire.comp.like2_3", "hire.comp.like2_4")], standardized = T, na.rm = T)
#homophily
cronbach.alpha(my_data[,c("attitude.homophily1_1", "attitude.homophily1_2", "attitude.homophily1_3", "attitude.homophily1_4",
                          "attitude.homophily1_5", "attitude.homophily1_6", "attitude.homophily1_7", "attitude.homophily1_8",
                          "attitude.homophily2_1", "attitude.homophily2_2", "attitude.homophily2_3", "attitude.homophily2_4",
                          "attitude.homophily2_5", "attitude.homophily2_6", "attitude.homophily2_7")], standardized = T, na.rm = T)
#rapport
cronbach.alpha(my_data[,c("stu.inst.rapport1_1", "stu.inst.rapport1_2", "stu.inst.rapport1_3",
                          "stu.inst.rapport1_4", "stu.inst.rapport1_5", "stu.inst.rapport2_1",
                          "stu.inst.rapport2_2", "stu.inst.rapport2_3", "stu.inst.rapport2_4")], standardized = T, na.rm = T)
#approachability
cronbach.alpha(my_data[,c("approachability1_1", "approachability1_2", "approachability1_3", "approachability1_4",
                          "approachability1_5", "approachability1_6", "approachability1_7", "approachability1_8",
                          "approachability1_9", "approachability1_10", "approachability2_1", "approachability2_2",
                          "approachability2_3", "approachability2_4", "approachability2_5", "approachability2_6",
                          "approachability2_7", "approachability2_8",  "approachability2_9", "approachability2_10")], standardized = T, na.rm = T)
#sense of belonging
cronbach.alpha(my_data[,c("course.cohesion_1", "course.cohesion_3", "course.cohesion_5")], standardized = T, na.rm = T)
#feelings of morale
cronbach.alpha(my_data[,c("course.cohesion_2", "course.cohesion_4", "course.cohesion_6")], standardized = T, na.rm = T)
#class comfort
cronbach.alpha(my_data[,c("class.comfort_1", "class.comfort_2", "class.comfort_3", "class.comfort_4")], standardized = T, na.rm = T)


### RQ1: full models (main and interactive) -----

full_main_mod <- do.call(rbind, lapply(c("hireability_mean", "competence_mean", 
                                         "likeability_mean", "homophily_mean",
                                         "rapport_mean", "approachability_mean",
                                         "cohesion_sob_mean", "cohesion_fom_mean", "class_comfort_mean"),
                                       function(x){
                                         mod_out <- as.data.frame(summary(
                                           lm(formula = as.formula(
                                             paste0(x, "~ trt_grp + lgbtq_accept_uni + lgbtq2 + gender3 + race4 + religion3 +  
                                                       mentalhealth + language2")),
                                             data = my_data))$coefficients[,-3])
                                         colnames(mod_out) <- c("est", "se", "pval")
                                         mod_out$aic <- AIC(lm(formula = formula(
                                           paste0(x, "~ trt_grp + lgbtq_accept_uni + lgbtq2 + gender3 + race4 + religion3 +  
                                                       mentalhealth + language2")),
                                           data = my_data))
                                         mod_out$rsquared <- summary(
                                           lm(formula = formula(
                                             paste0(x, "~ trt_grp + lgbtq_accept_uni + lgbtq2 + gender3 + race4 + religion3 +  
                                                       mentalhealth + language2")),
                                             data = my_data))$r.squared
                                         mod_out$outcome <- x
                                         mod_out$predictor <- rownames(mod_out)
                                         rownames(mod_out) <- NULL
                                         std_mod_out <- as.data.frame(beta(
                                           lm(formula = as.formula(
                                             paste0(x, "~ trt_grp + lgbtq_accept_uni + lgbtq2 + gender3 + race4 + religion3 +  
                                                       mentalhealth + language2")),
                                             data = my_data))$coefficients[,-3])
                                         colnames(std_mod_out) <- c("std_est", "std_se", "std_pval")
                                         std_mod_out$outcomeb <- x
                                         std_mod_out$predictorb <- rownames(std_mod_out)
                                         rownames(std_mod_out) <- NULL
                                         combo <- cbind(mod_out, std_mod_out)
                                         combo <- combo %>% dplyr::select(outcome, predictor, everything())
                                         return(combo)
                                       }))

full_main_mod |> filter(pval < .05) |>
  filter(predictor != "(Intercept)")

#interactive models

full_int_mod <- do.call(rbind, lapply(c("hireability_mean", "competence_mean", 
                                        "likeability_mean", "homophily_mean",
                                        "rapport_mean", "approachability_mean",
                                        "cohesion_sob_mean", "cohesion_fom_mean", "class_comfort_mean"),
                                      function(x){
                                        mod_out <- as.data.frame(summary(
                                          lm(formula = formula(
                                            paste0(x, "~ trt_grp*lgbtq_accept_uni + trt_grp*lgbtq2 + trt_grp*gender3 + 
                                            trt_grp*race4 + trt_grp*religion3 + trt_grp*mentalhealth + trt_grp*language2")),
                                            data = my_data))$coefficients[,-3])
                                        colnames(mod_out) <- c("est", "se", "pval")
                                        mod_out$aic <- AIC(lm(formula = formula(
                                          paste0(x, "~ trt_grp*lgbtq_accept_uni + trt_grp*lgbtq2 + trt_grp*gender3 + 
                                            trt_grp*race4 + trt_grp*religion3 + trt_grp*mentalhealth + trt_grp*language2")),
                                          data = my_data))
                                        mod_out$rsquared <- summary(
                                          lm(formula = formula(
                                            paste0(x, "~ trt_grp*lgbtq_accept_uni + trt_grp*lgbtq2 + trt_grp*gender3 + 
                                            trt_grp*race4 + trt_grp*religion3 + trt_grp*mentalhealth + trt_grp*language2")),
                                            data = my_data))$r.squared
                                        mod_out$outcome <- x
                                        mod_out$predictor <- rownames(mod_out)
                                        rownames(mod_out) <- NULL
                                        std_mod_out <- as.data.frame(beta(
                                          lm(formula = as.formula(
                                            paste0(x, "~ trt_grp*lgbtq_accept_uni + trt_grp*lgbtq2 + trt_grp*gender3 + 
                                            trt_grp*race4 + trt_grp*religion3 + trt_grp*mentalhealth + trt_grp*language2")),
                                            data = my_data))$coefficients[,-3])
                                        colnames(std_mod_out) <- c("std_est", "std_se", "std_pval")
                                        std_mod_out$outcomeb <- x
                                        std_mod_out$predictorb <- rownames(std_mod_out)
                                        rownames(std_mod_out) <- NULL
                                        combo <- cbind(mod_out, std_mod_out)
                                        combo <- combo %>% dplyr::select(outcome, predictor, everything())
                                        return(combo)
                                      }))

full_int_mod |> filter(pval < .05) |>
  filter(predictor != "(Intercept)")

instructor_main <- full_main_mod |> filter(outcome %in% c("hireability_mean", "competence_mean", 
                                                          "likeability_mean", "homophily_mean",
                                                          "rapport_mean", "approachability_mean"))

writexl::write_xlsx(instructor_main, path = "~/Desktop/tables3_main_effects.xlsx")

class_main <- full_main_mod |> filter(outcome %in% c("cohesion_sob_mean", "cohesion_fom_mean", "class_comfort_mean"))

writexl::write_xlsx(class_main, path = "~/Desktop/tables5_class_main_effects.xlsx")

instructor_int <- full_int_mod |> filter(outcome %in% c("hireability_mean", "competence_mean", 
                                                          "likeability_mean", "homophily_mean",
                                                          "rapport_mean", "approachability_mean"))

writexl::write_xlsx(instructor_int, path = "~/Desktop/tables4_int_effects.xlsx")

class_int <- full_int_mod |> filter(outcome %in% c("cohesion_sob_mean", "cohesion_fom_mean", "class_comfort_mean"))

writexl::write_xlsx(class_int, path = "~/Desktop/tables6_class_int_effects.xlsx")

### sub RQ1: investigating effects based on interactive terms ----
full_int_mod |> filter(pval<.05 & predictor != "(Intercept)") 

#significant effects: black-hireability; lgbtq-homophily; asian-homophily; christian-homophily; christian-approachability

summary(lm(hireability_mean ~ trt_grp + lgbtq_accept_uni + lgbtq2 + gender3 + religion3 +  
                                                       mentalhealth + language2, data = my_data[my_data$race4 == "black",]))
summary(lm(hireability_mean ~ trt_grp + lgbtq_accept_uni + lgbtq2 + gender3 + religion3 +  
             mentalhealth + language2, data = my_data[my_data$race4 == "white",]))

full_main_mod |> filter(outcome == "hireability_mean" & predictor != "(Intercept)") |> filter(pval<.05)


summary(lm(homophily_mean ~ trt_grp + lgbtq_accept_uni + race4 + gender3 + religion3 +  
             mentalhealth + language2, data = my_data[my_data$lgbtq2 == "yes",]))
summary(lm(homophily_mean ~ trt_grp + lgbtq_accept_uni + race4 + gender3 + religion3 +  
             mentalhealth + language2, data = my_data[my_data$lgbtq2 == "no",]))

summary(lm(homophily_mean ~ trt_grp + lgbtq_accept_uni + lgbtq2 + gender3 + religion3 +  
             mentalhealth + language2, data = my_data[my_data$race4 == "asian",]))
summary(lm(homophily_mean ~ trt_grp + lgbtq_accept_uni + lgbtq2 + gender3 + religion3 +  
             mentalhealth + language2, data = my_data[my_data$race4 == "white",]))

summary(lm(homophily_mean ~ trt_grp + lgbtq_accept_uni + lgbtq2 + race4 + gender3 +  
             mentalhealth + language2, data = my_data[my_data$religion3 == "christian",]))
summary(lm(homophily_mean ~ trt_grp + lgbtq_accept_uni + lgbtq2 + race4 + gender3 +  
             mentalhealth + language2, data = my_data[my_data$religion3 == "nonreligious",]))


summary(lm(homophily_mean ~ lgbtq_accept_uni + lgbtq2 + race4 + gender3 + religion3 +  
             mentalhealth + language2, data = my_data[my_data$trt_grp == 1,]))
summary(lm(homophily_mean ~ lgbtq_accept_uni + lgbtq2 + race4 + gender3 + religion3 +  
             mentalhealth + language2, data = my_data[my_data$trt_grp == 0,]))

full_main_mod |> filter(outcome == "homophily_mean" & predictor != "(Intercept)") |> filter(pval<.05)


summary(lm(approachability_mean ~ trt_grp + lgbtq_accept_uni + lgbtq2 + race4 + gender3 +  
             mentalhealth + language2, data = my_data[my_data$religion3 == "christian",]))
summary(lm(approachability_mean ~ trt_grp + lgbtq_accept_uni + lgbtq2 + race4 + gender3 +  
             mentalhealth + language2, data = my_data[my_data$religion3 == "nonreligious",]))

full_main_mod |> filter(outcome == "approachability_mean" & predictor != "(Intercept)") |> filter(pval<.05)

### RQ2: LGBTQ-specific questions -----
###perception of instructor LGBTQ disclosure (checkboxes) ----
#positive
pos_reasons <- c("The current generation of college students is accepting and welcoming of LGBTQ\\+ individuals",
                 "The students at my institution are accepting of LGBTQ\\+ individuals",
                 "Revealing an LGBTQ\\+ identity helps humanize the instructor and make them more relatable",
                 "Revealing an LGBTQ\\+ identity would help all students feel more welcome and included in the course",
                 "Revealing an LGBTQ\\+ identity would benefit the instructor by allowing them to be themselves",
                 "It does not matter to students whether the instructor is LGBTQ\\+ and overall would be seen positively",
                 "Revealing an LGBTQ\\+ identity would benefit LGBTQ\\+ students by giving them a role model and making them more comfortable")

positive_select_tbl <- do.call(rbind, lapply(pos_reasons,
                                             function(x){
                                               tmp <- as.data.frame(str_count(
                                                 string = audit_data$why.positive.select, pattern = x) |> sum())
                                               colnames(tmp) <- c("count")
                                               tmp$reason <- x
                                               return(tmp)
                                             }))

positive_select_tbl$denom <- length(which(audit_data$why.positive.select!=""))
positive_select_tbl$pct <- positive_select_tbl$count/positive_select_tbl$denom
positive_select_tbl <- positive_select_tbl |> arrange(desc(count))

#negative
neg_reasons <- c("Revealing an LGBTQ\\+ identity is too personal or unprofessional for the classroom",
                 "Revealing an LGBTQ\\+ identity is too political for the classroom",
                 "An instructor’s LGBTQ\\+ identity is irrelevant to the course",
                 "Some students carry bias against LGBTQ\\+ individuals")

negative_select_tbl <- do.call(rbind, lapply(neg_reasons,
                                             function(x){
                                               tmp <- as.data.frame(str_count(
                                                 string = audit_data$why.negative.select, pattern = x) |> sum())
                                               colnames(tmp) <- c("count")
                                               tmp$reason <- x
                                               return(tmp)
                                             }))
negative_select_tbl$denom <- length(which(audit_data$why.negative.select!=""))
negative_select_tbl$pct <- negative_select_tbl$count/negative_select_tbl$denom
negative_select_tbl <- negative_select_tbl |> arrange(desc(count))

##investigating too personal vs too political

str_count(string = audit_data$why.negative.select, pattern = "Revealing an LGBTQ\\+ identity is too personal or unprofessional for the classroom,Revealing an LGBTQ\\+ identity is too political for the classroom") |> sum()


#neutral
neu_reasons <- c("Undergraduates are indifferent towards and do not care about others’ LGBTQ\\+ identities",
                 "An instructor’s LGBTQ\\+ identity is irrelevant to the course",
                 "Some students would see it positively and some would see it negatively, and those would even out",
                 "The current generation of college students have a neutral view of LGBTQ\\+ identities",
                 "The students at my institution have a neutral view of LGBTQ\\+ identities")

neutral_select_tbl <- do.call(rbind, lapply(neu_reasons,
                                            function(x){
                                              tmp <- as.data.frame(str_count(
                                                string = audit_data$why.neutral.select, pattern = x) |> sum())
                                              colnames(tmp) <- c("count")
                                              tmp$reason <- x
                                              return(tmp)
                                            }))
neutral_select_tbl$denom <- length(which(audit_data$why.neutral.select!=""))
neutral_select_tbl$pct <- neutral_select_tbl$count/neutral_select_tbl$denom
neutral_select_tbl <- neutral_select_tbl |> arrange(desc(count))

#beneficial
benefit_reasons <- c("In cases where it is relevant to course content",
                     "When it helps make the instructor seem more relatable or open",
                     "When it would help create a more inclusive class environment",
                     "In cases where it would benefit LGBTQ\\+ students",
                     "It would always be beneficial for a college science instructor to reveal their LGBTQ\\+ identity during class",
                     "It would not necessarily be beneficial or detrimental for a college science instructor to reveal their LGBTQ\\+ identity during class",
                     "It would never be beneficial for a college science instructor to reveal their LGBTQ\\+ identity during class")

benefit_select_tbl <- do.call(rbind, lapply(benefit_reasons,
                                            function(x){
                                              tmp <- as.data.frame(str_count(
                                                string = audit_data$when.beneficial.sele, pattern = x) |> sum())
                                              colnames(tmp) <- c("count")
                                              tmp$reason <- x
                                              return(tmp)
                                            }))
benefit_select_tbl$denom <- length(which(audit_data$when.beneficial.sele!=""))
benefit_select_tbl$pct <- benefit_select_tbl$count/benefit_select_tbl$denom
benefit_select_tbl <- benefit_select_tbl |> arrange(desc(count))

#detrimental
detriment_reasons <- c("In cases where students are prejudiced against LGBTQ\\+ individuals",
                       "In cases where it causes the instructor to lose credibility or respect",
                       "In cases where it makes students feel uncomfortable or isolated in the class",
                       "When it wastes class time or is not relevant to content",
                       "When it is too in your face for the classroom",
                       "When it is too political for the classroom",
                       "When it causes the instructor to feel uncomfortable",
                       "When the instructor is a negative representation of the LGBTQ\\+ community due to poor teaching",
                       "It would always be detrimental for a college science instructor to reveal their LGBTQ\\+ identity during class",
                       "It would never be detrimental for a college science instructor to reveal their LGBTQ\\+ identity during class")

detriment_select_tbl <- do.call(rbind, lapply(detriment_reasons,
                                              function(x){
                                                tmp <- as.data.frame(str_count(
                                                  string = audit_data$when.detrimental.sel, pattern = x) |> sum())
                                                colnames(tmp) <- c("count")
                                                tmp$reason <- x
                                                return(tmp)
                                              }))
detriment_select_tbl$denom <- length(which(audit_data$when.detrimental.sel!=""))
detriment_select_tbl$pct <- detriment_select_tbl$count/detriment_select_tbl$denom
detriment_select_tbl <- detriment_select_tbl |> arrange(desc(count))

tables10 <- rbind(benefit_select_tbl, detriment_select_tbl)
writexl::write_xlsx(tables10, path = "~/Desktop/tables10_benefit_detriment.xlsx")



#appropriate
appropriate_reasons <- c("It would make the classroom environment more welcoming and inclusive",
                         "It would help humanize the instructor and make them more approachable and relatable",
                         "It is relevant to future scientists to be able to interact with different kinds of people",
                         "It would help to normalize LGBTQ\\+ identities",
                         "It would increase LGBTQ\\+ visibility and provide LGBTQ\\+ students with a role model",
                         "It is the same as a straight instructor mentioning their spouse",
                         "It is an important aspect of the instructor’s identity",
                         "It is the instructor’s decision to choose to reveal their LGBTQ\\+ identity",
                         "There is no reason it would be inappropriate")

appropriate_select_tbl <- do.call(rbind, lapply(appropriate_reasons,
                                                function(x){
                                                  tmp <- as.data.frame(str_count(
                                                    string = audit_data$why.appropriate.sele, pattern = x) |> sum())
                                                  colnames(tmp) <- c("count")
                                                  tmp$reason <- x
                                                  return(tmp)
                                                }))
appropriate_select_tbl$denom <- length(which(audit_data$why.appropriate.sele!=""))
appropriate_select_tbl$pct <- appropriate_select_tbl$count/appropriate_select_tbl$denom
appropriate_select_tbl <- appropriate_select_tbl |> arrange(desc(count))

#not appropriate
notapp_reasons <- c("An instructor’s LGBTQ\\+ identity does not affect teaching or learning and is irrelevant to the class",
                    "It would ruin students’ perception of the instructor",
                    "It is unnecessary because straight instructors do not disclose that they’re straight")

notappropriate_select_tbl <- do.call(rbind, lapply(notapp_reasons,
                                                   function(x){
                                                     tmp <- as.data.frame(str_count(
                                                       string = audit_data$why.not.approp.selec, pattern = x) |> sum())
                                                     colnames(tmp) <- c("count")
                                                     tmp$reason <- x
                                                     return(tmp)
                                                   }))
notappropriate_select_tbl$denom <- length(which(audit_data$why.not.approp.selec!=""))
notappropriate_select_tbl$pct <- notappropriate_select_tbl$count/notappropriate_select_tbl$denom
notappropriate_select_tbl <- notappropriate_select_tbl |> arrange(desc(count))

###multinom regression for neg, neu, pos -----
my_data$impact.clean <- factor(my_data$impact.clean, 
                               levels = c("negative", "neutral",
                                          "positive"))
my_data$impact.clean <- relevel(my_data$impact.clean, ref = "neutral")

impact_moddf <- as.data.frame(cbind(
  t(as.data.frame(summary(multinom(impact.clean ~ trt_grp + lgbtq_accept_uni + lgbtq2 + gender3 + race4 + religion3 +  
                                     mentalhealth + language2,
                                   data = my_data,
                                   na.action = na.omit))$coefficients)),
  t(as.data.frame(summary(multinom(impact.clean ~ trt_grp + lgbtq_accept_uni + lgbtq2 + gender3 + race4 + religion3 +  
                                     mentalhealth + language2,
                                   data = my_data,
                                   na.action = na.omit))$standard.errors))))

colnames(impact_moddf) <- c("negative_est", "positive_est", "negative_se", "positive_se")
impact_moddf$predictor <- rownames(impact_moddf)
rownames(impact_moddf) <- NULL

impact_moddf$negative_z <-impact_moddf$negative_est/impact_moddf$negative_se
impact_moddf$negative_p <- (1 - pnorm(abs(impact_moddf$negative_z), 0, 1))*2
impact_moddf$negative_or <- exp(impact_moddf$negative_est)

impact_moddf$positive_z <-impact_moddf$positive_est/impact_moddf$positive_se
impact_moddf$positive_p <- (1 - pnorm(abs(impact_moddf$positive_z), 0, 1))*2
impact_moddf$positive_or <- exp(impact_moddf$positive_est)

impact_moddf <- impact_moddf %>% dplyr::select(predictor, negative_est, negative_se, negative_or,
                                               negative_z, negative_p, positive_est, positive_se,
                                               positive_or, positive_z, positive_p)

impact_moddf |> filter(negative_p < .05)
impact_moddf |> filter(positive_p < .05)

writexl::write_xlsx(impact_moddf, path = "~/Desktop/tables8_impact_regression.xlsx")

###appropriate regression ----

my_data$appropriate.likert <- factor(my_data$appropriate.likert, 
                                     levels = c("Strongly disagree", "Disagree",
                                                "Somewhat disagree", "Somewhat agree",
                                                "Agree", "Strongly agree"))
table(my_data$appropriate.likert)
table(audit_data$appropriate.likert)

table(my_data$appropriate.likert, my_data$trt_grp) |> colSums()


appropriate_moddf <- as.data.frame(summary(ordinal::clm(appropriate.likert ~ trt_grp + lgbtq_accept_uni + lgbtq2 + gender3 + race4 + religion3 +  
                                                          mentalhealth + language2, data = my_data))$coefficients[,-3])
colnames(appropriate_moddf) <- c("est", "se", "pval")
appropriate_moddf$predictor <- rownames(appropriate_moddf)
rownames(appropriate_moddf) <- NULL
appropriate_moddf <- appropriate_moddf %>% filter(predictor != "Strongly disagree|Disagree" &
                                                    predictor != "Disagree|Somewhat disagree" &
                                                    predictor != "Somewhat disagree|Somewhat agree" &
                                                    predictor != "Somewhat agree|Agree" &
                                                    predictor != "Agree|Strongly agree")
appropriate_moddf$or <- exp(appropriate_moddf$est)

appropriate_moddf |> filter(pval<.05)

writexl::write_xlsx(appropriate_moddf, path = "~/Desktop/tables11_approp_regress.xlsx")

#### FIGURES ####
###Figure 2 ------
{
  fig2a <- full_main_mod |>
    filter(predictor == "trt_grp") |>
    filter(outcome == "hireability_mean"|
             outcome == "competence_mean"|
             outcome == "likeability_mean"|
             outcome == "homophily_mean"|
             outcome == "rapport_mean"|
             outcome == "approachability_mean") |>
    mutate(psig = ifelse(pval < .05, "sig", "nonsig")) |>
    mutate(outcome = forcats::fct_relevel(outcome, "approachability_mean",
                                          "rapport_mean",
                                          "homophily_mean",
                                          "likeability_mean",
                                          "competence_mean",
                                          "hireability_mean")) %>%
    ggplot(aes(y = outcome, color = psig)) +
    geom_point(aes(x = std_est), size = 3.5) +
    geom_errorbarh(aes(xmin = std_est - 1.96*std_se, xmax = std_est + 1.96*std_se), 
                   position=ggstance::position_dodgev(height=0.5),
                   linewidth = 1.25) +  
    geom_vline(aes(xintercept = 0), linetype="dashed", linewidth=.7, 
               color = "grey15") + 
    scale_color_manual(values = c("dodgerblue4", "gray")) +
    labs(x = "Standardized beta ± 95% CI", y = "", title = "A.") + 
    scale_y_discrete(labels = c("Approachability",
                                "Student-instructor\nrapport",
                                "Attitude homophily",
                                "Likeability",
                                "Competence",
                                "Hireability")) +
    theme_classic() +
    theme(axis.ticks.y = element_blank(),
          axis.title.x = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          axis.text.x = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          axis.line.y = element_blank(),
          axis.text.y = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          plot.title = element_blank(),
          legend.position = "none")
  
  
  fig2a
  
  fig2b <- my_data |>
    filter(!is.na(lgbtq2)) |>
    ggplot(aes(y = lgbtq2, x = homophily_mean, fill = as.factor(trt_grp))) +
    ggridges::geom_density_ridges(bandwidth = 0.25, alpha = 0.5) +
    labs(x = "Attitude homophily", y = "", title = "B.") + 
    scale_x_continuous(limits = c(1,7), breaks = c(1:7)) +
    scale_color_manual(values = c("gray", "dodgerblue4")) +
    scale_fill_manual(values = c("gray", "dodgerblue4"), labels = c("control", "reveal")) +
    scale_y_discrete(labels = c("Not LGBTQ+", "LGBTQ+")) +
    guides(fill = guide_legend(title = "Condition")) +
    theme_classic() +
    theme(axis.ticks.y = element_blank(),
          axis.title.x = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          axis.text.x = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          axis.line.y = element_blank(),
          axis.text.y = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          plot.title = element_blank(),
          legend.text = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          legend.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          legend.position = "right")
  
  
  fig2b
  
  fig2c <- my_data |>
    filter(!is.na(religion3)) |>
    ggplot(aes(y = religion3, x = homophily_mean, fill = as.factor(trt_grp))) +
    ggridges::geom_density_ridges(bandwidth = 0.25, alpha = 0.5) +
    labs(x = "Attitude homophily", y = "", title = "C.") + 
    scale_x_continuous(limits = c(1,7), breaks = c(1:7)) +
    scale_color_manual(values = c("gray", "dodgerblue4")) +
    scale_fill_manual(values = c("gray", "dodgerblue4"), labels = c("control", "reveal")) +
    guides(fill = guide_legend(title = "Condition")) +
    scale_y_discrete(labels = c("Nonreligious", "Other religion", "Christian")) +
    theme_classic() +
    theme(axis.ticks.y = element_blank(),
          axis.title.x = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          axis.text.x = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          axis.line.y = element_blank(),
          axis.text.y = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          plot.title = element_blank(),
          legend.text = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          legend.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          legend.position = "none")
  
  
  fig2c
  
  fig2d <- my_data |>
    filter(!is.na(religion3)) |>
    ggplot(aes(y = religion3, x = approachability_mean, fill = as.factor(trt_grp))) +
    ggridges::geom_density_ridges(bandwidth = 0.3, alpha = 0.5) +
    labs(x = "Approachability", y = "", title = "D.") + 
    scale_x_continuous(limits = c(1,7), breaks = c(1:7)) +
    scale_color_manual(values = c("gray", "dodgerblue4")) +
    scale_fill_manual(values = c("gray", "dodgerblue4"), labels = c("control", "reveal")) +
    guides(fill = guide_legend(title = "Condition")) +
    scale_y_discrete(labels = c("Nonreligious", "Other religion", "Christian")) +
    theme_classic() +
    theme(axis.ticks.y = element_blank(),
          axis.title.x = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          axis.text.x = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          axis.line.y = element_blank(),
          axis.text.y = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          plot.title = element_blank(),
          legend.text = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          legend.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          legend.position = "right")
  
  
  fig2d
  
  fig2top <- ggpubr::ggarrange(fig2a, fig2b, fig2c, fig2d, labels = c("A.", "B.", "C.", "D."), 
                               font.label = element_text(family="Helvetica", color = "black", size = 8, face = "bold"))
  fig2top
  
  fig2bottom <- full_int_mod |>
    filter(predictor == "trt_grp:lgbtq_accept_unimid" | 
             predictor == "trt_grp:lgbtq_accept_unihigh") |>
    filter(outcome == "hireability_mean"|
             outcome == "competence_mean"|
             outcome == "likeability_mean"|
             outcome == "homophily_mean"|
             outcome == "rapport_mean"|
             outcome == "approachability_mean") |>
    mutate(psig = ifelse(pval < .05, "sig", "nonsig")) |>
    mutate(outcome = forcats::fct_relevel(outcome, "hireability_mean",
                                          "competence_mean",
                                          "likeability_mean",
                                          "homophily_mean",
                                          "rapport_mean",
                                          "approachability_mean"
    )) %>%
    ggplot(aes(y = predictor, color = predictor)) +
    geom_point(aes(x = std_est), size = 2) +
    geom_errorbarh(aes(xmin = std_est - 1.96*std_se, xmax = std_est + 1.96*std_se), 
                   position=ggstance::position_dodgev(height=0.5),
                   linewidth = 1) +  
    geom_vline(aes(xintercept = 0), linetype="dashed", linewidth=.7, 
               color = "grey15") + 
    scale_color_manual(values = c("gray30", "gray80"), labels = c("High", "Mid")) +
    guides(color = guide_legend(title = "Treatment x \nState LGBTQ+ acceptance")) +
    labs(x = "Standardized beta ± 95% CI", y = "", title = "E.") + 
    facet_wrap(.~outcome, labeller = labeller(outcome = c("approachability_mean" = "Approachability",
                                                          "rapport_mean" = "Student-instructor rapport",
                                                          "homophily_mean" = "Attitude homophily",
                                                          "likeability_mean" = "Likeability",
                                                          "competence_mean" = "Competence",
                                                          "hireability_mean" = "Hireability")),
               ncol = 1, strip.position = "left") +
    theme_classic() +
    theme(axis.ticks.y = element_blank(),
          axis.title.x = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          axis.text.x = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text.y.left =  element_text(family="Helvetica", color = "black", size = 8, face = "bold", angle = 0),
          strip.background = element_blank(),
          plot.title = element_blank(),
          legend.position = "right",
          legend.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
          legend.text = element_text(family="Helvetica", color = "black", size = 8, face = "bold"))
  
  fig2bottom  
  
  fig2 <- ggpubr::ggarrange(fig2top, fig2bottom, ncol = 1, heights = c(2,1), labels = c("", "E."), 
                            font.label = element_text(family="Helvetica", color = "black", size = 8, face = "bold"))
  fig2
}

ggsave(fig2, file = "~/Desktop/auditfig2.pdf", device = "pdf", units = "in", width = 7, height = 6)

###Figure 3 -----

fig3adf <- data.frame(table(audit_data$impact.overall, audit_data$lgbtq_accept_uni)) |>
  filter(Var1 != "") |>
  mutate(Var1 = forcats::fct_relevel(Var1, c("Would be perceived negatively by undergraduate students in the class.",
                                             "Would be perceived as neutral by undergraduate students in the class.",
                                             "Would be perceived positively by undergraduate students in the class.")))

fig3adf$denom <- NA
fig3adf[fig3adf$Var2 == "low",]$denom <- sum(fig3adf[fig3adf$Var2 == "low",]$Freq)
fig3adf[fig3adf$Var2 == "mid",]$denom <- sum(fig3adf[fig3adf$Var2 == "mid",]$Freq)
fig3adf[fig3adf$Var2 == "high",]$denom <- sum(fig3adf[fig3adf$Var2 == "high",]$Freq)

fig3adf$pct <- fig3adf$Freq/fig3adf$denom

fig3 <-  fig3adf |>
  ggplot(aes(x = Var2, y = pct, fill = Var2)) +
  geom_col() +
  geom_text(aes(label = scales::percent(pct, accuracy = .1)), 
            position=position_dodge(width=0.9), size=2.5, fontface="bold", color = "black", vjust = -.2) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .78), breaks = seq(0:.75,by = .25),
                     labels = scales::label_percent(accuracy = .1)) +
  scale_fill_manual(values = c("gray", "lightsteelblue2", "dodgerblue4")) +
  labs(x = "", y = "Percent (%)", title = "A. Undergraduates would perceive a science instructor revealing their LGBTQ+ identity...",
       fill = "State LGBTQ+\nacceptance") + 
  facet_wrap(.~Var1, strip.position = "bottom",
             labeller = labeller(Var1 = c("Would be perceived negatively by undergraduate students in the class." = "negatively",
                                          "Would be perceived as neutral by undergraduate students in the class." = "as neutral",
                                          "Would be perceived positively by undergraduate students in the class." = "positively"))) +
  theme_classic() +
  theme(axis.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        legend.text = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        legend.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        plot.title = element_blank(),
        strip.text = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside")

fig3

ggsave(fig3, file = "~/Desktop/auditfig3.pdf", device = "pdf", units = "in", width = 6, height = 1.75)


###Figure 4 -----

fig3bdf <- data.frame(table(audit_data$appropriate.likert, audit_data$lgbtq_accept_uni)) |>
  filter(Var1 != "") |>
  mutate(Var1 = forcats::fct_relevel(Var1, c("Strongly disagree", "Disagree", "Somewhat disagree",
                                             "Somewhat agree", "Agree", "Strongly agree")))

fig3bdf$denom <- NA
fig3bdf[fig3bdf$Var2 == "low",]$denom <- sum(fig3bdf[fig3bdf$Var2 == "low",]$Freq)
fig3bdf[fig3bdf$Var2 == "mid",]$denom <- sum(fig3bdf[fig3bdf$Var2 == "mid",]$Freq)
fig3bdf[fig3bdf$Var2 == "high",]$denom <- sum(fig3bdf[fig3bdf$Var2 == "high",]$Freq)

fig3bdf$pct <- fig3bdf$Freq/fig3bdf$denom

fig4 <- fig3bdf |>
  ggplot(aes(x = Var2, y = pct, fill = Var2)) +
  geom_col() +
  geom_text(aes(label = scales::percent(pct, accuracy = .1)), 
            position=position_dodge(width=0.9), size=2.5, fontface="bold", color = "black", vjust = -.2) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .35), breaks = seq(0:.3,by = .1),
                     labels = scales::label_percent(accuracy = .1)) +
  scale_fill_manual(values = c("gray", "lightsteelblue2", "dodgerblue4")) +
  labs(x = "", y = "Percent (%)", title = "B. It is completely appropriate for science instructors to reveal their LGBTQ+ identities.",
       fill = "State LGBTQ+\nacceptance") + 
  facet_wrap(.~Var1, strip.position = "bottom", nrow = 1,
             labeller = labeller(Var1 = c("Strongly disagree" = "Strongly disagree",
                                          "Disagree"  = "Disagree",
                                          "Somewhat disagree" = "Somewhat\ndisagree",
                                          "Somewhat agree"  = "Somewhat\nagree",
                                          "Agree" = "Agree",
                                          "Strongly agree" = "Strongly agree"))) +
  theme_classic() +
  theme(axis.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        legend.text = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        legend.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        plot.title = element_blank(),
        strip.text = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside")

fig4


ggsave(fig4, file = "~/Desktop/auditfig4.pdf", device = "pdf", units = "in", width = 8, height = 1.75)


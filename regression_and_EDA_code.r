# overlay plots of T/C scores
plot_scores <- function(dataset, score, lab){
  dataset$score <- score
  new_df <- dataset[na.omit(dataset$score),]
  new_df$Group <- factor(new_df$treatment_ind, levels = c(0,1), labels = c("Control","Treatment"))
  ggplot(new_df, aes(score, fill = Group, colour = Group)) + geom_density(alpha = 0.2) + ggtitle(lab, subtitle = NULL) +
    xlim(0,100)
}

plot_scores(d, d$employment_score, "Employment Score")
plot_scores(d, d$education_score, "Education Score")
plot_scores(d, d$retirement_score, "Retirement Score")
plot_scores(d, d$cybersecurity_score, "Cybersecurity Score")


# UPDATED REGRESSIONS: Education (make sure reults='asis' above!)

# define income class - not a strong candidate for regression
table(d$income)
par(mar=c(10,3,1,1))
boxplot(education_score~income, data=d, las = 2, horizontal=F)


define_income_class <- function(income_range) {
  if(income_range %in% c("Less than $10,000", "$20,000 - $29,999", "$30,000 - $39,999")) {
    return("low")
  } else if(income_range %in% c("$80,000 - $89,999", "$90,000 - $99,999", "$100,000 - $149,999", "More than $150,000")) {
    return("high")
  } else if(income_range %in% c("$40,000 - $49,999","$50,000 - $59,999","$60,000 - $69,999","$70,000 - $79,999")){
    return("middle")
  } else {
    return("unknown")
  }
}
d$income_group <- relevel(factor(sapply(d$income, define_income_class)), "middle")
table(d$income_group)


# grouping for education level - not a strong predictor
table(d$highest_education)
par(mar=c(10,3,1,1))
boxplot(education_score~highest_education, data=d, las = 2, horizontal=F)

define_education <- function(ed) {
  if(ed %in% c("4 year degree", "Doctorate", "Professional degree")) {
    return(1)
  } else {
    return(0)
  }
}
d$high_ed_ind <- sapply(d$highest_education, define_education)
table(d$high_ed_ind)


#ADD AGE GROUP (e.g. 45+; to be revised/informed by EDA later)
summary(d$age)
boxplot(education_score~age, data=d)

define_age <- function(age) {
  if(age %in% c("18 - 24", "25 - 34", "35 - 44")) {
    return("18 - 44")
  } else if (age %in% c("45 - 54", "55 - 64", "65 - 74")){
    return("over 45")
  } 
  else {
    return("other")
  }
}
d$age_group <- relevel(factor(sapply(d$age, define_age)), "18 - 44")
summary(d$age_group)


lm.ed.1 <- lm(education_score ~ treatment_ind, data = d)
lm.ed.2 <- lm(education_score ~ treatment_ind + political_party + high_ed_ind + age_group, data = d)
lm.ed.3 <- lm(education_score ~ treatment_ind + treatment_ind*political_party + high_ed_ind + age_group, data = d)

# SWITCH FROM "text" TO "latex" FOR FINAL OUTPUTS
stargazer(lm.ed.1, lm.ed.2, lm.ed.3, title = "Value on Education", type = "text",
          column.labels = c("Treatment Only","With Covariates", "With HTEs"),
          dep.var.labels = "Education Feeling Score (0 to 100)")
	  
# employment
lm.emp.1 <- lm(employment_score ~ treatment_ind, data = d)
lm.emp.2 <- lm(employment_score ~ treatment_ind + political_party + income_group + age_group, data = d)
lm.emp.3 <- lm(employment_score ~ treatment_ind + treatment_ind*political_party + income_group + age_group, data = d)

# SWITCH TO "LATEX" FOR FINAL OUTPUT
stargazer(lm.emp.1, lm.emp.2, lm.emp.3, title = "Employment Optimism", type = "text",
          column.labels = c("Treatment Only","With Covariates", "With HTEs"),
          dep.var.labels = "Employment Feeling Score (0 to 100)")


# retirement
lm.ret.1 <- lm(retirement_score ~ treatment_ind, data = d)
lm.ret.2 <- lm(retirement_score ~ treatment_ind + political_party + age_group + high_ed_ind + income_group, data = d)
lm.ret.3 <- lm(retirement_score ~ treatment_ind + treatment_ind*political_party + age_group + high_ed_ind + income_group, data = d)

# SWITCH TO "LATEX" FOR FINAL OUTPUT
stargazer(lm.ret.1, lm.ret.2, lm.ret.3, title = "Retirement Optimism", type = "text",
          column.labels = c("Treatment Only","With Covariates", "With HTEs"),
          dep.var.labels = "Retirement Feeling Score (0 to 100)")


# cybersecurity
lm.sec.1 <- lm(cybersecurity_score ~ treatment_ind, data = d)
lm.sec.2 <- lm(cybersecurity_score ~ treatment_ind + political_party + age_group, data = d)
lm.sec.3 <- lm(cybersecurity_score ~ treatment_ind + treatment_ind*political_party + age_group, data = d)

# SWITCH TO "LATEX" FOR FINAL OUTPUT
stargazer(lm.sec.1, lm.sec.2, lm.sec.3, title = "Cybersecurity Risk", type = "text",
          column.labels = c("Treatment Only","With Covariates", "With HTEs"),
          dep.var.labels = "Cybersecurity Risk Feeling Score (0 to 100)")
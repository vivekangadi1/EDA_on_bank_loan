setwd("F:/Computer_science/ML/UPGRAD/EDA case Study")
SDS_PATH <- file.path("F:/Computer_science/ML/UPGRAD/EDA case Study")
library(rsample)
library(recipes)
library(dplyr)
library(ggplot2)
library(data.table)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(scales)
library(corrr)
library(Hmisc)
#Reading the data


raw.loan.data <- read.csv("loan.csv",stringsAsFactors = F)
#Checking the data


head(raw.loan.data)
#Structure of data


glimpse(raw.loan.data)

#Removing the unwanted columns which has only na.thus reducing number of columns which has lots of NA's
#or one single value. as these dont make sense to any computations or is of any relevance


loan.removed.columns <-raw.loan.data %>% select(-c(total_il_high_credit_limit,total_bc_limit,total_bal_ex_mort,tot_hi_cred_lim,
                                                   percent_bc_gt_75,pct_tl_nvr_dlq,num_tl_op_past_12m,num_tl_90g_dpd_24m,num_tl_30dpd,
                                                   num_tl_120dpd_2m,num_sats,num_rev_tl_bal_gt_0,num_rev_accts,num_op_rev_tl,num_il_tl,
                                                   num_bc_tl,num_bc_sats,num_actv_rev_tl,num_actv_bc_tl,num_actv_bc_tl,mths_since_recent_revol_delinq,
                                                   mths_since_recent_inq,mths_since_recent_bc_dlq,mths_since_recent_bc,mort_acc,mo_sin_rcnt_tl,
                                                   mo_sin_rcnt_rev_tl_op,mo_sin_old_rev_tl_op,mo_sin_old_il_acct,delinq_amnt,bc_util,
                                                   bc_open_to_buy,avg_cur_bal,acc_open_past_24mths,inq_last_12m,total_cu_tl,inq_fi,
                                                   total_rev_hi_lim,all_util,max_bal_bc,open_rv_24m,open_rv_12m,il_util,total_bal_il,
                                                   mths_since_rcnt_il,open_il_24m,open_il_12m,open_il_6m,open_acc_6m,tot_cur_bal,tot_coll_amt,
                                                   acc_now_delinq,verification_status_joint,dti_joint,annual_inc_joint,mths_since_last_major_derog,
                                                   num_accts_ever_120_pd,mths_since_last_record,mths_since_last_delinq))  

glimpse(loan.removed.columns)
write.csv(file = "loanremoved.csv",loan.removed.columns)


missing_values <- loan.removed.columns %>% summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

plot <- missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  coord_flip()
plot
#removing the columns which are dominated by one value.like tax_liens,chargeoff_within_12_mths,
#application_type,policy_code,collections_12_mths_ex_med


clean.data <- loan.removed.columns %>% select(-c(tax_liens,initial_list_status,chargeoff_within_12_mths,pub_rec_bankruptcies,application_type,policy_code,collections_12_mths_ex_med)) 

good_features <- filter(missing_values,missing_percentage<0.15)
good_features <- (good_features$feature)
clean.data <- clean.data[,(colnames(clean.data) %in% good_features)]
summary(clean.data)

a <- clean.data %>%
  summarise_all(funs(sum(is.na(.))))
sum(is.na(clean.data))
colnames(clean.data)
ncol(clean.data)
nrow(clean.data)
clean.data <- na.omit(clean.data)
sum(is.na(clean.data))

write.csv(file = "cleanedData.csv",clean.data)

ggplot(clean.data,aes(x=factor(loan_status)))+geom_bar(stat='count',position = "dodge")
ggplot(clean.data,aes(x=factor(loan_status),fill=factor(home_ownership)))+geom_bar(stat='count')
ggplot(clean.data,aes(x=factor(loan_status),fill=factor(verification_status)))+geom_bar(stat='count')
ggplot(clean.data,aes(x=factor(loan_status),fill=factor(grade)))+geom_bar(stat='count')


#finding the default,current,fullypaid rate.

clean.data$loan_statusCurrent <- ifelse(clean.data$loan_status=="Current",1,0)

current <- mean(clean.data$loan_statusCurrent)

clean.data$loan_statusChrgedOff <- ifelse(clean.data$loan_status=="Charged Off",1,0)

chargedOff <- mean(clean.data$loan_statusChrgedOff)

clean.data$loan_statusCurrent <- ifelse(clean.data$loan_status=="Fully Paid",1,0)

fullypaid<- mean(clean.data$loan_statusCurrent)

total <- current+chargedOff+fullypaid

#we get to know the rate of all status
#current is 2.8%
#default Charged off is 14.17%
#fully paid is 82.961%
#we get toknow that there is 14.17% defauters which is a very high rate.next we try to find out
#the reason for defaut

ggplot(filter(clean.data,loan_status %in% c("Charged Off")),aes(x=factor(home_ownership)))+geom_bar(stat='count',position = "dodge")
verification.status <- ggplot(filter(clean.data,loan_status %in% c("Charged Off")),aes(x=factor(loan_status),fill=factor(verification_status)))+geom_bar(stat='count',position = "dodge")
verification.status
grade <- ggplot(filter(clean.data,loan_status %in% c("Charged Off")),aes(x=factor(loan_status),fill=factor(grade)))+geom_bar(stat='count',position = "dodge")
grade
#from the verfication status we get to know the ppl who have not been verified have the very chances of deafaulter
#and from the grade factor we get to know that grade b,c,d are very susptible to be a defaulter
#this still does not give a clear picture as the ppl who have not been verfied have also paid the 
#loan and grade b,c,d have also paid the loan. lets take diffrent approach

#now lets convert all the character to factors
clean.data[sapply(clean.data, is.character)] <- lapply(clean.data[sapply(clean.data, is.character)], 
                                                       as.factor)

univariate_categorical <- function(dataset,var,var_name){
  
  dataset %>% ggplot(aes(x = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = percent) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1)
    ) 
    
}
univariate_categorical_fill <- function(dataset,var,filname,var_name,filname1){
  
  dataset %>% ggplot(aes(x = as.factor(var),fill=filname)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    labs(title = var_name, y = "Percent", x = var_name,fill=filname1)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1),
      
    ) 
  
  
}
univariate_categorical_fill_dodge <- function(dataset,var,filname,var_name,filname1){
  
  dataset %>% ggplot(aes(x = as.factor(var),fill=filname)) +
    geom_bar(aes(y = (..count..)/sum(..count..)),position = "dodge") +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    labs(title = var_name, y = "Percent", x = var_name,fill=filname1)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1),
      
    ) 
  
  
}
glimpse(clean.data)
univariate_categorical(clean.data,clean.data$loan_status,"Default Distribution on status")

univariate_categorical(clean.data,clean.data$purpose,"Default Distribution on status")
univariate_categorical_fill(clean.data,clean.data$loan_status,clean.data$purpose,"fill based on purpose and loan status","purpose")
#from the above graph we see that debtconsolidation is the major purpose for the loan.
#and most of the defaultors also in the debt consolidation brackect 
#altought many of the debt consolidator have fully paid the loan
#debt consolidation 46.9%
#creditcard 12.9%
#home_improvement 7.5%
#major_purchase 5.4%
#others 10.1%
#let us now filter out the purpose whose contribution is less than 5% and remove 
#the others as it does not give us any inference
clean.data <- filter(clean.data,clean.data$purpose %in% c("debt_consolidation","credit_card","home_improvement","major_purchase"))
univariate_categorical(clean.data,clean.data$purpose,"Default Distribution on status after filter")
univariate_categorical_fill(clean.data,clean.data$loan_status,clean.data$purpose,"fill based on purpose and loan status after filter","purpose")
#term
#imp
univariate_categorical(clean.data,clean.data$term,"Default Distribution on term")
#72.7% overall loan on 36month term
#27.3% overall loan on 60month term
#imp
univariate_categorical_fill_dodge(clean.data,clean.data$loan_status,clean.data$term,"distribution and fill based on purpose and loan status and term","terms")
univariate_categorical_fill(clean.data,clean.data$purpose,clean.data$term,"fill based on purpose and term","purpose")

clean.data$issue_d <- paste("01-",clean.data$issue_d,sep="")
clean.data$issue_d <- as.Date(clean.data$issue_d,"%d-%B-%y")
clean.data$year <- as.factor(format(clean.data$issue_d,"%Y"))
#percentage based on the year of loan issue
univariate_categorical(clean.data,clean.data$year,"Distribution based on the yearly basis")

clean.data$loanbyfunded <- clean.data$funded_amnt/clean.data$loan_amnt

product_wise <- function(dataset,var,var_name){
  
  dataset %>% ggplot(aes(x = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = percent) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 30, hjust = 1)
    )+facet_wrap(~purpose) 
  
}

product_wise_fill <- function(dataset,var,var_name){
  
  dataset %>% ggplot(aes(x = as.factor(var),fill=loan_status)) +
    geom_bar(aes(y = (..count..)/sum(..count..)),position = "dodge",jitter=T) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = percent) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 30, hjust = 1)
    ) +facet_wrap(~purpose)
  
}


product_wise(clean.data,clean.data$year,"purpose Distribution 2007-2011 ")
product_wise_fill(clean.data,clean.data$year,"purpose Distribution 2007-2011 based on fill ")
#we see that in the 2007-2008 the major are of purpose was "major_purchase"
#where as from 2009-2010 the focus was shifted home_improvement along with focus on major purchase
#but in the year 2010 major_purchase was dropped from the plan and it was focused solely on 
#debts_consolidation and with minor focus on home_improvement.the year 2011 was focused both on
#credit_card and debt_consolidation and closing its home_improvement plan in 2011
#we also get to see that both major_purchase(0.7%) and home_improvement(0.9%) have contribution
#less than 1% individually charged off where as credit card contributes 1.4% and debt_consolidation
#contributes almost 9.5%.so the major conecerned are while releasing funds shoud be on debt_consolidation
#there should be more thorough back ground verfication before granting the loan for the debt_consolidation




product_wise_fill(clean.data,clean.data$grade,"")
#from the above graphs we also see the we can grant loan to e,f,g grade customer without much worry
#as they have very less chance of getting chaged off from all the major groups.but the interesting 
#observation here is all the grade from a-g in the group major_purchase,home_improvement,credit_Card
#have almost paid there loan or they are still paying but in major concer is in debt_consolidation 
#where grade a,b,c,b are cause of concern with grade and grade being the major contributors of becoming 
#charged-off or defaulters

clean.data$int_rate <- extract_numeric(clean.data$int_rate)
#lets calculate the mean for each grade
tapply(clean.data$int_rate,clean.data$grade, mean)
#we see that grade e,f,g are given loans with high interest but they are the customers who pay the loan
#very promptly.in the other words less interest is causing more defaulters
#lets calculate the mean for each ppurpose
tapply(clean.data$int_rate,clean.data$purpose, mean)
#debt consolidation has a very high rate of interest and it also has relatively high rate of defaulter,
#there should be a stratergy drawn to solve this problem.either increase the interest















##################################################################################################
# Derivative metrics for the analysis
##################################################################################################

clean.data$bin_loan_amnt<-ifelse(clean.data$loan_amnt<=5000,"Small",
                           ifelse(clean.data$loan_amnt>5000 & clean.data$loan_amnt<=15000,"Medium",
                                  ifelse(clean.data$loan_amnt>15000 & clean.data$loan_amnt<=25000,"High","VeryHigh")))


clean.data$bin_funded_amnt_inv<-ifelse(clean.data$funded_amnt_inv<=5000,"Small",
                                 ifelse(clean.data$funded_amnt_inv>5000 & clean.data$funded_amnt_inv<=15000,"Medium",
                                        ifelse(clean.data$funded_amnt_inv>15000 & clean.data$funded_amnt_inv<=25000,"High","VeryHigh")))

clean.data$bin_int_rate<-ifelse(clean.data$int_rate<=10,"Low_rate",
                          ifelse(clean.data$int_rate>10 & clean.data$int_rate<=15,"Medium_rate","High_rate"))

describe(clean.data$dti) 

clean.data$bin_dti<-ifelse(clean.data$dti<=10,"Low_dti",
                     ifelse(clean.data$dti>10 & clean.data$dti<=20,"Medium_dti","High_dti"))


describe(clean.data$funded_amnt) 

clean.data$bin_funded_amnt<-ifelse(clean.data$funded_amnt<=5000,"Small",
                             ifelse(clean.data$funded_amnt>5000 & clean.data$funded_amnt<=15000,"Medium",
                                    ifelse(clean.data$funded_amnt>15000 & clean.data$funded_amnt<=25000,"High","VeryHigh")))


describe(clean.data$installment) 

clean.data$bin_installment<-ifelse(clean.data$installment<=200,"Small",
                             ifelse(clean.data$installment>200 & clean.data$installment<=400,"Medium",
                                    ifelse(clean.data$installment>400 & clean.data$installment<=600,"High","VeryHigh")))

describe(clean.data$annual_inc) 


clean.data$bin_annual_inc<-ifelse(clean.data$annual_inc<=50000,"Small",
                            ifelse(clean.data$annual_inc>50000 & clean.data$annual_inc<=100000,"Medium",
                                   ifelse(clean.data$annual_inc>100000 & clean.data$annual_inc<=150000,"High","VeryHigh")))

clean.data$emp_length <- extract_numeric(clean.data$emp_length)

describe(clean.data$emp_length) # 50% clean.data applicants are less than or having 4 years of experience
# 23% clean.data applicants are having more than 10 years of experience

# Missing values in employement length 
sum(is.na(clean.data))/nrow(clean.data)*100 # 2.37% 

# Rather than treating these missing values, best is to get rid of these observations. 
clean.data <- data.frame(na.omit(clean.data))  

# Let's divide the employee experience in 4 levels such as "freshers","junior","senior"& "expert"
# employment <=1 year should be considered as "freshers"
#  1 years-3year of experience should be considered as "junior"
#  4- 7 years of experience should be considered as "senior"
#  more than 7 year of experience should be considered as "expert"

clean.data$emp_length <- as.factor(ifelse(clean.data$emp_length<=1,"freshers",ifelse(clean.data$emp_length>1&clean.data$emp_length<=3,"junior",ifelse(clean.data$emp_length>3&clean.data$emp_length<=7,"senior","expert"))))

# Let's see the distribution again: 
product_wise(clean.data,clean.data$emp_length,"Applicant Experience Distribution- Top-4 products")
product_wise_fill(clean.data,clean.data$emp_length,"Applicant Experience Distribution- Top-4 products")
# "Major purchase" and "home improvement" were majorly approved for freshers(less than 1 year of experience)
# Debt consolidation and credit card loans were majorly approved for experts(more than 10 years of experience)

#################################################################################################


continuous_dist <- function(dataset,con_var,var_name){
  
  con_summary <- tapply(con_var, clean.data$purpose, summary) 
  
  P1 <- dataset %>% ggplot(aes(x=con_var)) + geom_line(stat = 'density',color='red')+facet_wrap(~purpose)+ggtitle(var_name)+xlab(var_name)
  
  return(P1)
}


continuous_dist(clean.data,clean.data$loan_amnt,"loan Distribution")
continuous_dist(clean.data,clean.data$funded_amnt,"loan Distribution")
clean.data$loanbyfunded <- clean.data$funded_amnt/clean.data$loan_amnt
continuous_dist(clean.data,clean.data$loanbyfunded,"ratio of loanamt/fundedamt")
continuous_dist(clean.data,clean.data$int_rate,"interest Distribution")
continuous_dist(clean.data,clean.data$annual_inc,"annual Distribution")

# Feature correlations to the purpose
corr1 <- correlate(clean.data)

correlation_plot<- function(pur){
clean.data.numeric <- pur
clean.data.numeric$issue_d <- NULL
clean.data.numeric[sapply(clean.data.numeric, is.character)] <- lapply(clean.data.numeric[sapply(clean.data, is.character)], 
                                                                       as.factor)
clean.data.numeric[sapply(clean.data.numeric, is.integer)] <- lapply(clean.data.numeric[sapply(clean.data, is.integer)], 
                                                                     as.numeric)
clean.data.numeric[sapply(clean.data.numeric, is.factor)] <- lapply(clean.data.numeric[sapply(clean.data, is.factor)], 
                                           as.numeric)



clean.data.numeric$issue_d <- NULL

corrr_analysis <- clean.data.numeric %>%
  correlate() %>%
  focus(loan_statusNum) %>%
  rename(feature = rowname) %>%
  arrange(abs(loan_statusNum)) %>%
  mutate(feature = as.factor(feature)) 
corrr_analysis

# Correlation visualization
plot <- corrr_analysis %>%
  ggplot(aes(x = loan_statusNum, y = fct_reorder(feature, desc(loan_statusNum)))) +
  geom_point() +
  # Positive Correlations - Contribute to churn
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[2]], 
               data = corrr_analysis %>% filter(loan_statusNum > 0)) +
  geom_point(color = palette_light()[[2]], 
             data = corrr_analysis %>% filter(loan_statusNum > 0)) +
  # Negative Correlations - Prevent churn
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[1]], 
               data = corrr_analysis %>% filter(loan_statusNum < 0)) +
  geom_point(color = palette_light()[[1]], 
             data = corrr_analysis %>% filter(loan_statusNum < 0)) +
  # Vertical lines
  geom_vline(xintercept = 0, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = -0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = 0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  # Aesthetics
  theme_tq() +
  labs(title = "Churn Correlation Analysis",
       subtitle = "Positive Correlations (contribute to churn), Negative Correlations (prevent churn)",
       y = "Feature Importance")

return(plot)
}
c("debt_consolidation","credit_card","home_improvement","major_purchase")
correlation_plot(filter(clean.data,clean.data$purpose %in% c("major_purchase")))
correlation_plot(filter(clean.data,clean.data$purpose %in% c("home_improvement")))
correlation_plot(filter(clean.data,clean.data$purpose %in% c("credit_card")))
correlation_plot(filter(clean.data,clean.data$purpose %in% c("debt_consolidation")))

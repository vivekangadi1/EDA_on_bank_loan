setwd("F:/Computer_science/ML/UPGRAD/EDA case Study")
SDS_PATH <- file.path("F:/Computer_science/ML/UPGRAD/EDA case Study")
library(rsample)
library(recipes)
library(dplyr)
library(ggplot2)
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


clean.data <- loan.removed.columns %>% select(-c(tax_liens,chargeoff_within_12_mths,pub_rec_bankruptcies,application_type,policy_code,collections_12_mths_ex_med)) 

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
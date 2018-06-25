setwd("F:/Computer_science/ML/UPGRAD/EDA case Study")

library(rsample)
library(recipes)
#Reading the data


raw.loan.data <- read.csv("loan.csv",stringsAsFactors = F)
#Checking the data


head(raw.loan.data)
#Structure of data


glimpse(raw.loan.data)

#Removing the unwanted columns
remove.columns <- c(total_il_high_credit_limit,total_bc_limit,total_bal_ex_mort,tot_hi_cred_lim,
                    percent_bc_gt_75,pct_tl_nvr_dlq,num_tl_op_past_12m,num_tl_90g_dpd_24m,num_tl_30dpd,
                    num_tl_120dpd_2m,num_sats,num_rev_tl_bal_gt_0,num_rev_accts,num_op_rev_tl,num_il_tl,
                    num_bc_tl,num_bc_sats,num_actv_rev_tl,num_actv_bc_tl,num_actv_bc_tl,mths_since_recent_revol_delinq,
                    mths_since_recent_inq,mths_since_recent_bc_dlq,mths_since_recent_bc,mort_acc,mo_sin_rcnt_tl,
                    mo_sin_rcnt_rev_tl_op,mo_sin_old_rev_tl_op,mo_sin_old_il_acct,delinq_amnt,bc_util,
                    bc_open_to_buy,avg_cur_bal,acc_open_past_24mths,inq_last_12m,total_cu_tl,inq_fi,
                    total_rev_hi_lim,all_util,max_bal_bc,open_rv_24m,open_rv_12m,il_util,total_bal_il,
                    mths_since_rcnt_il,open_il_24m,open_il_12m,open_il_6m,open_acc_6m,tot_cur_bal,tot_coll_amt,
                    acc_now_delinq,verification_status_joint,dti_joint,annual_inc_joint,mths_since_last_major_derog)



loan.removed.columns <-raw.loan.data %>% select(-c(total_il_high_credit_limit,total_bc_limit,total_bal_ex_mort,tot_hi_cred_lim,
                                                   percent_bc_gt_75,pct_tl_nvr_dlq,num_tl_op_past_12m,num_tl_90g_dpd_24m,num_tl_30dpd,
                                                   num_tl_120dpd_2m,num_sats,num_rev_tl_bal_gt_0,num_rev_accts,num_op_rev_tl,num_il_tl,
                                                   num_bc_tl,num_bc_sats,num_actv_rev_tl,num_actv_bc_tl,num_actv_bc_tl,mths_since_recent_revol_delinq,
                                                   mths_since_recent_inq,mths_since_recent_bc_dlq,mths_since_recent_bc,mort_acc,mo_sin_rcnt_tl,
                                                   mo_sin_rcnt_rev_tl_op,mo_sin_old_rev_tl_op,mo_sin_old_il_acct,delinq_amnt,bc_util,
                                                   bc_open_to_buy,avg_cur_bal,acc_open_past_24mths,inq_last_12m,total_cu_tl,inq_fi,
                                                   total_rev_hi_lim,all_util,max_bal_bc,open_rv_24m,open_rv_12m,il_util,total_bal_il,
                                                   mths_since_rcnt_il,open_il_24m,open_il_12m,open_il_6m,open_acc_6m,tot_cur_bal,tot_coll_amt,
                                                   acc_now_delinq,verification_status_joint,dti_joint,annual_inc_joint,mths_since_last_major_derog))  

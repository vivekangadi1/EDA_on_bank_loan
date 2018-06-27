
raw.loan.data <- read.csv("loan.csv",stringsAsFactors = F)
missing_values <- raw.loan.data %>%
  summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  coord_flip()
good_features <- filter(missing_values,missing_percentage<0.1)
good_features <- (good_features$feature) 
loan <- raw.loan.data[,(colnames(raw.loan.data) %in% good_features)]
summary(loan)
clean.data <- loan %>% select(-c(tax_liens,pub_rec_bankruptcies,chargeoff_within_12_mths,application_type,policy_code,collections_12_mths_ex_med)) 
a <- clean.data %>%
  summarise_all(funs(sum(is.na(.))))
sum(is.na(clean.data))

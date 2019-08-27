
library(dplyr)
library(plyr)

ref_promo_participants <- read.csv(file.choose())
promo_referrals <- read.csv(file.choose())
data1 <- ref_promo_participants
data2 <- promo_referrals

data1$bucket_timestamp <- as.Date(data1$bucket_timestamp, format = "%m/%d/%Y")
data1$bucket <- as.character(data1$bucket)
bucketCount <- count(data1, 'bucket')
data3 <- data1[data1$bucket == "off",]
data1 <- data1[!(data1$bucket == "off"),]

data1$promo_startdate <- data1$bucket_timestamp
data1$promo_startdate <- if_else(data1$bucket == "48hr", data1$promo_startdate + 2, data1$promo_startdate)
data1$promo_startdate <- if_else(data1$bucket == "168hr", data1$promo_startdate + 7, data1$promo_startdate)
data1$promo_enddate <- data1$promo_startdate + 30

data2$receiver_account_timestamp <- as.Date(data2$receiver_account_timestamp, format = "%m/%d/%Y")
data2$receiver_quote_timestamp <- as.Date(data2$receiver_quote_timestamp, format = "%m/%d/%Y")
data2$receiver_policy_timestamp <- as.Date(data2$receiver_policy_timestamp, format = "%m/%d/%Y")

data4 <- merge(data1, data2, by.x = "user_id", by.y = "sender_user_id")
data4$sender_earned_amount_in_dollars[is.na(data4$sender_earned_amount_in_dollars)] <- 0
data4$receiver_earned_amount_in_dollars[is.na(data4$receiver_earned_amount_in_dollars)] <- 0
data5 <- merge(data3, data2, by.x = "user_id", by.y = "sender_user_id")
data5$sender_earned_amount_in_dollars[is.na(data5$sender_earned_amount_in_dollars)] <- 0
data5$receiver_earned_amount_in_dollars[is.na(data5$receiver_earned_amount_in_dollars)] <- 0

bucketCountPromoConverstion <- count(data4, 'bucket')
bucketCountNoPromoConversion <- count(data5,'bucket')

bucketPromoQuotePolicyConversion <- data4 %>% group_by(bucket,receiver_quote,receiver_policy) %>% tally()
bucketNoPromoQuotePolicyConversion <- data5 %>% group_by(bucket,receiver_quote,receiver_policy) %>% tally()

table1 <- as.data.frame(tapply(data4$receiver_account,list(data4$bucket,data4$sender_earned_amount_in_dollars,data4$receiver_quote),sum))
table2 <- as.data.frame(tapply(data5$receiver_account,list(data5$bucket,data5$sender_earned_amount_in_dollars,data5$receiver_quote),sum))
table3 <- as.data.frame(tapply(data4$receiver_quote,list(data4$bucket,data4$sender_earned_amount_in_dollars),sum))
table4 <- as.data.frame(tapply(data5$receiver_quote,list(data5$bucket,data5$sender_earned_amount_in_dollars),sum))
table5 <- as.data.frame(tapply(data4$receiver_policy,list(data4$bucket,data4$sender_earned_amount_in_dollars),sum))
table6 <- as.data.frame(tapply(data5$receiver_policy,list(data5$bucket,data5$sender_earned_amount_in_dollars),sum))

table7 <- as.data.frame(tapply(data4$sender_earned_amount_in_dollars,list(data4$bucket,data4$receiver_earned_amount_in_dollars),sum))
bucketPromoReferralCost <- data4 %>% group_by(bucket,sender_earned_amount_in_dollars,receiver_earned_amount_in_dollars) %>% tally()

table8 <- as.data.frame(tapply(data5$sender_earned_amount_in_dollars,list(data5$bucket,data5$receiver_earned_amount_in_dollars),sum))
bucketNoPromoReferralCost <- data5 %>% group_by(bucket,sender_earned_amount_in_dollars,receiver_earned_amount_in_dollars) %>% tally()
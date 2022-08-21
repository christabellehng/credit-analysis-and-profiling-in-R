### Group 3: Alex, Angelyn, Christabelle, I-En, Venus, Wei Hao

# Environment can be cleared at separation (lines 125, 252, 498, 958) for neatness
# Section numbers refer to group report

############## Data Pre-processing-----

library(readr)
library(dplyr)

#reformat loan date
loan_date <- read_csv("loan_new.csv",
                      col_types = cols(loan_date = col_date(format="%B %Y"),
                                       prev_pym = col_date(format = "%B %Y")))
summary(loan_date)
str(loan_date)

#convert base to 36 and plus to 60
tenure.numbers <- loan_date %>%
  mutate(tenure_days = as.numeric(ifelse(tenure_plan == "Base", "1095", "1825"))) %>%
  mutate(tenure_months = as.numeric(ifelse(tenure_plan == "Base", "36", "60")))


# Exploratory Data Analysis of Original Dataset (Section 4)-----

#integer encoding
integer.encoded <- loan_date %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

str(integer.encoded)
summary(integer.encoded)

#correlation plot
corrplot.var <- integer.encoded[ , -c(6,13)]
library(corrplot)
corrplot(cor(corrplot.var), type = "upper")

#ggplot
library(ggplot2)

#Plot Loan Purpose against Interest Rate
ggplot(data = loan_date, aes(x = listing_title, y = int_rate, color = int_rate)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  labs(title = 'Loan Purpose against Interest Rate',
       y = 'Interest Rate', x = 'Loan Purpose')

#Plot Loan Purpose against Days Past Due
ggplot(data = loan_date, aes(x = listing_title, y = dpd, color = dpd)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  labs(title = 'Loan Purpose against Days Past Due',
       y = 'Days Past Due', x = 'Loan Purpose')

#Loan Rating against Days Past Due
ggplot(data = loan_date, aes(x = rating, y = dpd, color = dpd)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2) + 
  geom_jitter(shape=16, position=position_jitter(0.3)) + 
  labs(title = 'Loan Rating against Days Past Due',
       y = 'Days Past Due', x = 'Rating')

#Loan Date against Loan Status
ggplot(data = loan_date, aes(x = loan_date, y = status, color = status)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2) + 
  geom_jitter(shape=16, position=position_jitter(0.3)) + 
  labs(title = 'Loan Date against Loan Status',
       y = 'Loan Status', x = 'Loan Date')

#Loan Date against Loan Status
ggplot(data = loan_date, aes(x = status, y = amount_borrowed, color = status)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2) + 
  geom_jitter(shape=16, position=position_jitter(0.3)) + 
  labs(title = 'Loan Status against Loan Amount',
       y = 'Loan Amount', x = 'Loan Status')

#Debt Proceeds against Charge-off Loans
ggplot(data = loan_date %>% filter(status == "charge-off"), aes(x = status, y = debt_proceeds_received, color = debt_proceeds_received)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2) + 
  geom_jitter(shape=16, position=position_jitter(0.3)) + 
  labs(title = 'Debt Proceeds against Defaulted Loans',
       y = 'Debt Proceeds Collected', x = 'Charge-off Loans')

 
# Anomalies and Discrepancy (Section 4.4, 10.1.2)----

# mutate % to form new columns
loan.anomalies <- tenure.numbers %>%
  mutate("percentage_of_total_paid" = ((pr_paid + debt_proceeds_received + interest_paid)/(mth_instlm*tenure_months)) * 100) %>%
  mutate("percentage_of_late_penalty_paid" = late_penalty_paid/amount_borrowed * 100) %>%
  mutate("percentage_discount" = 100 - percentage_of_total_paid) %>%
  mutate("days_difference" = prev_pym - loan_date) %>%
  mutate("days_early/exceeded" = tenure_days - days_difference)

#Plot percentage discount over days early for completed loans
completed.anomalies <- filter(loan.anomalies, status %in% "complete")

plot(x=completed.anomalies$`days_early/exceeded`, y=completed.anomalies$percentage_discount,
     pch=4, 
     xlab="Days paid early", ylab="Percentage discount")

#Plot density of amount of late penalty paid
loanswithlatepenalty <- loan.anomalies %>%
  select(late_penalty_paid, status) %>%
  filter(late_penalty_paid >0)

#normal density graph
loanswithlatepenalty %>%
  ggplot(aes(x=late_penalty_paid)) +
  geom_density(fill="plum4", color="#e9ecef", alpha=0.8) +
  ggtitle("Amount of Late Penalty Paid")

#plot late penalty fee density by status (multiple density graph)
loanswithlatepenalty %>%
  ggplot(aes(x=late_penalty_paid, group=status, fill=status)) +
  geom_density(alpha=0.8) +
  ggtitle("Amount of Late Penalty Paid")

#split late penalty fee against status graph for better visibility (small multiple density graph)
ggplot(data=loanswithlatepenalty, aes(x=late_penalty_paid, group=status, fill=status)) +
  geom_density(adjust=1.5) +
  facet_wrap(~status) 


#----------------------------------------------------------------
############### REGRESSION model for chargeoff to predict debt proceeds (Section 5)-----

library(readr)
library(dplyr)
library(caret) 
library(lattice)
library(ggplot2)
library(forecast)
library(corrplot)

#reformat loan date
loan_date <- read_csv("loan_new.csv",
                      col_types = cols(loan_date = col_date(format="%B %Y"),
                                       prev_pym = col_date(format = "%B %Y")))
summary(loan_date)
str(loan_date)

#extracting the rows with charge-off only
chargeoff.extracted <- loan_date[loan_date$status == "charge-off", ]

#convert base to 36 and plus to 60
tenure_months<- chargeoff.extracted %>%
  mutate(tenure_months = as.numeric(ifelse(tenure_plan == "Base", "36", "60")))

#calculating percentage paid 
percent.paid <- tenure_months %>%
  mutate(percent_paid = (pr_paid + interest_paid + debt_proceeds_received)/(mth_instlm*tenure_months))

# TRIAL A ###############################

#encode the new data (without the original tenure column because of its formatting) to numeric
chargeoff.preOHE <- percent.paid[, -c(2)] %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor,as.numeric)

str(chargeoff.preOHE)

#creating categorical dataframe for the type of loan
chargeoff.chr <- data.frame(percent.paid$listing_title)
chargeoff.dmv <- dummyVars(" ~ .", data = chargeoff.chr)
chargeoff.ohe <- data.frame(predict(chargeoff.dmv, chargeoff.chr))

#bind the OHE listing
chargeoff.OHEC <- cbind(chargeoff.preOHE,chargeoff.ohe)

#corrplot to identify correlations and remove columns that are correlated, remove date and listing columns, status and pr_bal
library(corrplot)
corrplot(cor(chargeoff.preOHE[, -c(5:7,12,14)]), method = "circle")

#removing variables that have multicollinearity - int rate, mthly instl, & unnecessary values
chargeoff.OHE.clean <- chargeoff.OHEC[, -c(2:3,5:7,12,14)]

#extract out only wedding loans due to small sample size and partition separately
wedding.extracted <- chargeoff.OHE.clean[chargeoff.OHE.clean$percent.paid.listing_titlewedding == "1", ]

#extract out non-wedding
non.wedding.extracted <- chargeoff.OHE.clean[chargeoff.OHE.clean$percent.paid.listing_titlewedding != "1", ]

#create training and test data sets by binding both wedding and non-wedding
set.seed(1)
train.index.non.wedding<- sample(nrow(non.wedding.extracted), 0.6*nrow(non.wedding.extracted))
train.index.wedding <- sample(nrow(wedding.extracted), 0.6*nrow(wedding.extracted))

train.mlm.chargeoff.A <- rbind(wedding.extracted[train.index.wedding, ],non.wedding.extracted[train.index.non.wedding, ])
test.mlm.chargeoff.A <- rbind(wedding.extracted[-train.index.wedding, ],non.wedding.extracted[-train.index.non.wedding, ])

debt.mlm.chargeoff.A <- lm(debt_proceeds_received ~., data = train.mlm.chargeoff.A)

summary(debt.mlm.chargeoff.A)
options(scipen = 999)

library(forecast)

debt.mlm.pred.chargeoff.A <- predict(debt.mlm.chargeoff.A, test.mlm.chargeoff.A)

debt.mlm.residual.sel.chargeoff.A <- test.mlm.chargeoff.A$debt_proceeds_received - debt.mlm.pred.chargeoff.A

debt.mlm.results.chargeoff.A <- data.frame ("Actual" = test.mlm.chargeoff.A$debt_proceeds_received,
                                          "Predicted" = debt.mlm.pred.chargeoff.A,
                                          "Residual" = debt.mlm.residual.sel.chargeoff.A)

accuracy(debt.mlm.pred.chargeoff.A, test.mlm.chargeoff.A$debt_proceeds_received)

# TRIAL B ######################################

#encode the new data (without the original tenure column because of its formatting) to numeric
chargeoff.int<- percent.paid[, -c(2)] %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor,as.numeric)

str(chargeoff.int)

#corrplot to identify correlations and remove columns that are correlated, remove date and listing columns, status and pr_bal
library(corrplot)
corrplot(cor(chargeoff.int[, -c(5:7,12,14)]), method = "circle")

#removing variables that have multicollinearity - int rate, mthly instl, & unnecessary values
chargeoff.clean <- chargeoff.int[, -c(2:3,5:7,12,14)]

#create training and test data sets 
set.seed(1)
train.index.chargeoff<- sample(nrow(chargeoff.clean), 0.6*nrow(chargeoff.clean))

train.mlm.chargeoff.B <- chargeoff.clean[train.index.chargeoff, ]
test.mlm.chargeoff.B <- chargeoff.clean[-train.index.chargeoff, ]

debt.mlm.chargeoff.B <- lm(debt_proceeds_received ~., data = train.mlm.chargeoff.B)

summary(debt.mlm.chargeoff.B)
options(scipen = 999)

library(forecast)

debt.mlm.pred.chargeoff.B <- predict(debt.mlm.chargeoff.B, test.mlm.chargeoff.B)

debt.mlm.residual.sel.chargeoff.B <- test.mlm.chargeoff.B$debt_proceeds_received - debt.mlm.pred.chargeoff.B

debt.mlm.results.chargeoff.B <- data.frame ("Actual" = test.mlm.chargeoff.B$debt_proceeds_received,
                                          "Predicted" = debt.mlm.pred.chargeoff.B,
                                          "Residual" = debt.mlm.residual.sel.chargeoff.B)

accuracy(debt.mlm.pred.chargeoff.B, test.mlm.chargeoff.B$debt_proceeds_received)

summary(chargeoff.clean)


#----------------------------------------------------------------
############### CLUSTERING to streamline listing titles ----
# Clustering on individual listing titles was carried out to determine common listing titles
# Outlier detection was not conducted here, but in the next section after streamlining

#reformat loan date
loan_date <- read_csv("loan_new.csv",
                      col_types = cols(loan_date = col_date(format="%B %Y"),
                                       prev_pym = col_date(format = "%B %Y")))
summary(loan_date)
str(loan_date)

#convert base to 36 and plus to 60
tenure.numbers <- loan_date %>%
  mutate(tenure_days = as.numeric(ifelse(tenure_plan == "Base", "1095", "1825"))) %>%
  mutate(tenure_months = as.numeric(ifelse(tenure_plan == "Base", "36", "60")))

# mutate % and remove variables 
#loan date, pr paid, pr bal, interest paid, debt proceeds, late penalty, prev_pym and tenure months and tenure days)
loan.mutated <- tenure.numbers %>%
  mutate("percentage_of_total_paid" = ((pr_paid + debt_proceeds_received + interest_paid)/(mth_instlm*tenure_months)) * 100) %>%
  mutate("percentage_of_late_penalty_paid" = late_penalty_paid/amount_borrowed * 100) %>% 
  select(-c(6, 8:13, 16, 17))

#integer encoding
loan.integer.encoded <- loan.mutated %>%
  select(-c(listing_title, status)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

#OHEC for listing title
loan.listing.title <- loan.mutated %>%
  select(c(listing_title))

loan.dmv <- dummyVars("~.", data = loan.listing.title) 

loan.ohe <- data.frame(predict(loan.dmv, loan.listing.title)) 

#combine listing table and status back with rest of encoded data set
loan.encoded <- loan.integer.encoded %>%
  bind_cols(loan.ohe, status = loan.mutated$status)

# Clustering for Charge-off loans (Section 6.1)------
#filter out chargeoff and remove status
chargeoff.encoded <- loan.encoded %>%
  filter (status %in% c("charge-off")) %>%
  select (-c(status))

str(chargeoff.encoded)

#normalisation of data 
chargeoff.loans.norm <- sapply(chargeoff.encoded,scale)
rownames(chargeoff.loans.norm) <- rownames(chargeoff.encoded)  #add row names

# Elbow to find optimal number of clusters
set.seed(1) 
k.max <- 10
wss <- (nrow(chargeoff.loans.norm)-1)*sum(apply(chargeoff.loans.norm,2,var))
for (i in 2:k.max) wss[i] <- sum(kmeans(chargeoff.loans.norm,
                                        centers=i,
                                        iter.max = 20,
                                        algorithm = "Hartigan-Wong")$withinss)

#plot the elbow plot 
plot(1:k.max, wss, type="b", xlab="Number of Clusters K",
     ylab="Total within-cluster sum of squares",
     main="Elbow Plot to find Optimal Number of Clusters in charge-off loans",
     pch=19, cex=1)

#Optimal Cut-off Line, optimal point is chosen as 7
abline(v = 7, lty =2)

#k means clustering with 7 clusters
set.seed(1)
km.chargeoff <- kmeans(chargeoff.loans.norm, 7, algorithm = "Hartigan-Wong")

#number of records in each cluster
km.chargeoff$size 
# results: [1]  6060 10129  2273  1761  3921  1763  7155
# cluster 4 has the smallest number of records of 1761; 
# cluster 2 has the biggest number of records of 10129

# Cluster Centroids
km.chargeoff$centers

# within cluster sum of squares
km.chargeoff$withinss
# results:  [1]  40568.27  58315.20 288496.87  14344.78  25427.51  16082.02  28298.31
# cluster 4 has the smallest within cluster sum of squares of 14344.78
# cluster 2 has the bigger within cluster sum of squares of 58315.20

#dist between cluster centers
dist(km.chargeoff$centers)

# Assign Cluster Numbers to dataset
chargeoff.loans.clustn <- chargeoff.encoded %>%
  bind_cols(data.frame(km.chargeoff$cluster))

#plot cluster (colour plot)
#install.packages("factoextra")
library(factoextra)
fviz_cluster(km.chargeoff, 
             data=chargeoff.loans.norm,
             ellipse.type="convex",
             outlier.shape = 23)

# Analysis of cluster plot
chargeoff.summary <- chargeoff.loans.clustn %>%
  group_by(km.chargeoff.cluster) %>%
  summarise_all("mean")

# Clustering for Default Loans (Section 6.2)----
#filter out default and remove status + NA columns
default.encoded <- loan.encoded %>%
  filter (status %in% c("default")) %>%
  select (-c(status, listing_titleenergy_effc_financing, listing_titlemedical 
             ,listing_titlerelocation, listing_titlesme, listing_titletravel, listing_titlewedding))

#normalisation of data 
default.loans.norm <- sapply(default.encoded,scale)
rownames(default.loans.norm) <- rownames(default.encoded)  #add row names

# Elbow to find optimal number of clusters
set.seed(1) 
k.max <- 10
wss <- (nrow(default.loans.norm)-1)*sum(apply(default.loans.norm,2,var))
for (i in 2:k.max) wss[i] <- sum(kmeans(default.loans.norm,
                                        centers=i,
                                        iter.max = 20,
                                        algorithm = "Hartigan-Wong")$withinss)

#plot the elbow plot 
plot(1:k.max, wss, type="b", xlab="Number of Clusters K",
     ylab="Total within-cluster sum of squares",
     main="Elbow Plot to find Optimal Number of Clusters in Default loans",
     pch=19, cex=1)

#Optimal Cut-off Line, optimal point is chosen as 3
abline(v = 3, lty =2)

#k means clustering with 6 clusters
set.seed(1)
km.default <- kmeans(default.loans.norm, 3, algorithm = "Hartigan-Wong")

#number of records in each cluster
km.default$size 
# results: [1]  4 44 40
# cluster 1 has the smallest number of records of 4; 
# cluster 2 has the biggest number of records of 44

# Cluster Centroids
km.default$centers

# within cluster sum of squares
km.default$withinss
# results:  [1]  16.23338 540.11099 408.10470
# cluster 1 has the smallest within cluster sum of squares of 16.23338
# cluster 2 has the bigger within cluster sum of squares of 540.11099

#dist between cluster centers
dist(km.default$centers)

# Assign Cluster Numbers to dataset
default.loans.clustn <- default.encoded %>%
  bind_cols(data.frame(km.default$cluster))

#plot cluster (colour plot)
#install.packages("factoextra")
library(factoextra)
fviz_cluster(km.default, 
             data=default.loans.norm,
             ellipse.type="convex",
             outlier.shape = 23)

# Analysis of cluster plot
default.summary <- default.loans.clustn %>%
  group_by(km.default.cluster) %>%
  summarise_all("mean")

# Clustering for Complete Loans (Section 6.3)-----
#filter out complete and remove dates
complete.encoded <- loan.encoded %>%
  filter (status %in% c("complete")) %>%
  select (-c(status, dpd))

#normalisation of data 
complete.loans.norm <- sapply(complete.encoded,scale)
rownames(complete.loans.norm) <- rownames(complete.encoded)  #add row names

# Elbow to find optimal number of clusters
set.seed(1) 
k.max <- 15
wss <- (nrow(complete.loans.norm)-1)*sum(apply(complete.loans.norm,2,var))
for (i in 2:k.max) wss[i] <- sum(kmeans(complete.loans.norm,
                                        centers=i,
                                        iter.max = 20,
                                        algorithm = "Hartigan-Wong")$withinss)

#plot the elbow plot 
plot(1:k.max, wss, type="b", xlab="Number of Clusters K",
     ylab="Total within-cluster sum of squares",
     main="Elbow Plot to find Optimal Number of Clusters in complete loans",
     pch=19, cex=1)

#Optimal Cut-off Line, optimal point is chosen as 10
abline(v = 10, lty =2)

#k means clustering with 6 clusters
set.seed(1)
km.complete <- kmeans(complete.loans.norm, 10, algorithm = "Hartigan-Wong")

#number of records in each cluster
km.complete$size 
# results:  [1] 13391  1863  8699  3246 28814 20242 12315   910 14023 24311
# cluster 8 has the smallest number of records of 910; 
# cluster 5 has the biggest number of records of 28814

# Cluster Centroids
km.complete$centers

# within cluster sum of squares
km.complete$withinss
# results:  977234.669   4011.957 159019.836  48924.563  46769.300 156435.784 120269.466   6703.709   79756.338  79259.551
# cluster 2 has the smallest within cluster sum of squares of 4011.957
# cluster 1 has the bigger within cluster sum of squares of 977234.669

#dist between cluster centers
dist(km.complete$centers)

# Assign Cluster Numbers to dataset
complete.loans.clustn <- complete.encoded %>%
  bind_cols(data.frame(km.complete$cluster))

#plot cluster (colour plot)
#install.packages("factoextra")
library(factoextra)
fviz_cluster(km.complete, 
             data=complete.loans.norm,
             ellipse.type="convex",
             outlier.shape = 23)

# Analysis of cluster plot
complete.summary <- complete.loans.clustn %>%
  group_by(km.complete.cluster) %>%
  summarise_all("mean")

#----------------------------------------------------------------
############## TO PROFILE BORROWERS  -----
#reformat loan date
loan_date <- read_csv("loan_new.csv",
                      col_types = cols(loan_date = col_date(format="%B %Y"),
                                       prev_pym = col_date(format = "%B %Y")))
summary(loan_date)
str(loan_date)

#convert base to 36 and plus to 60
tenure.numbers <- loan_date %>%
  mutate(tenure_days = as.numeric(ifelse(tenure_plan == "Base", "1095", "1825"))) %>%
  mutate(tenure_months = as.numeric(ifelse(tenure_plan == "Base", "36", "60")))

# mutate % and remove variables 
#loan date, pr paid, pr bal, interest paid, debt proceeds, late penalty, prev_pym and tenure months and tenure days)
loan.mutated <- tenure.numbers %>%
  mutate("percentage_of_total_paid" = ((pr_paid + debt_proceeds_received + interest_paid)/(mth_instlm*tenure_months)) * 100) %>%
  mutate("percentage_of_late_penalty_paid" = late_penalty_paid/amount_borrowed * 100) %>% 
  select(-c(6, 8:13, 16, 17))

#integer encoding
loan.integer.encoded <- loan.mutated %>%
  select(-c(listing_title, status)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

#OHEC for listing title
library(caret)
loan.listing.title <- loan.mutated %>%
  select(c(listing_title))

loan.dmv <- dummyVars("~.", data = loan.listing.title) 

loan.ohe <- data.frame(predict(loan.dmv, loan.listing.title)) 

#combine listing table and status back with rest of encoded data set
loan.encoded <- loan.integer.encoded %>%
  bind_cols(loan.ohe, status = loan.mutated$status)

#renaming charge-off and default to uncompleted
loan.status<- loan.encoded %>%
  mutate(status = sub("charge-off", "uncompleted", status)) %>%
  mutate(status = sub("default", "uncompleted", status))

#streamline listing titles and remove original columns
loan.streamlined<- loan.status %>%
  mutate("listing_titlehousingloan" = (listing_titleenergy_effc_financing + listing_titlehome + listing_titlehomereno +listing_titlerelocation),
         "listing_titlepersonalloan" = (listing_titlemajor_purchase + listing_titlemedical + listing_titletravel + listing_titlevehicle + listing_titlewedding),
         "listing_titlemiscloan" = (listing_titleother + listing_titlesme)) %>%
  select(-c(11:21))

str(loan.streamlined)
summary(loan.streamlined)

# Streamlined Clustering for Uncompleted Loans (Section 7) -----
#filter out uncompleted
uncompleted.encoded <- loan.streamlined %>%
  filter (status %in% c("uncompleted")) %>%
  select (-c(status))

#normalisation of data 
uncompleted.loans.norm <- sapply(uncompleted.encoded,scale)
rownames(uncompleted.loans.norm) <- rownames(uncompleted.encoded)  #add row names

# Elbow to find optimal number of clusters
set.seed(1) 
k.max <- 10
wss <- (nrow(uncompleted.loans.norm)-1)*sum(apply(uncompleted.loans.norm,2,var))
for (i in 2:k.max) wss[i] <- sum(kmeans(uncompleted.loans.norm,
                                        centers=i,
                                        iter.max = 20,
                                        algorithm = "Hartigan-Wong")$withinss)

#plot the elbow plot 
plot(1:k.max, wss, type="b", xlab="Number of Clusters K",
     ylab="Total within-cluster sum of squares",
     main="Elbow Plot to find Optimal Number of Clusters for Uncompleted Loans",
     pch=19, cex=1)

#Optimal Cut-off Line, optimal point is chosen as 7
abline(v = 7, lty =2)

#k means clustering with 7 clusters
set.seed(1)
km.uncompleted <- kmeans(uncompleted.loans.norm, 7, algorithm = "Hartigan-Wong")

#number of records in each cluster
km.uncompleted$size 
# results: [1] 4794 5902 8841 3092 2184 3419 4918
# cluster 5 has the smallest number of records of 2184 (outlying cluster); 
# cluster 3 has the biggest number of records of 8841

# Cluster Centroids
km.uncompleted$centers

# within cluster sum of squares
km.uncompleted$withinss
# results: [1] 14301.85 38187.00 48838.89 20467.88 18717.57 63179.75 26798.01
# cluster 1 has the smallest within cluster sum of squares of 14301.85
# cluster 6 has the bigger within cluster sum of squares of 63179.75

#dist between cluster centers
dist(km.uncompleted$centers)

# Assign Cluster Numbers to dataset
uncompleted.loans.clustn <- uncompleted.encoded %>%
  bind_cols(data.frame(km.uncompleted$cluster))

#plot cluster (colour plot)
library(factoextra)
fviz_cluster(km.uncompleted, 
             data=uncompleted.loans.norm,
             ellipse.type="convex",
             outlier.shape = 23)

# green plot
library(cluster)
clusplot(uncompleted.loans.norm, km.uncompleted$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=1)

# Analysis of cluster plot
uncompleted.summary <- uncompleted.loans.clustn %>%
  group_by(km.uncompleted.cluster) %>%
  summarise_all("mean")

#pick out cluster 7 as the red flag cluster, the standard risk is about 40% recoverable, highlight the one that is 30% 
cluster.7 <- filter(uncompleted.loans.clustn, km.uncompleted.cluster %in% c("7"))

#pie chart for cluster 7 loans with less than 40% paid
clust7.lessthan40 <- cluster.7 %>%
  filter(percentage_of_total_paid < 40) %>%
  count(listing_titlecreditcard, listing_titledebt_refinancing, listing_titlehousingloan, listing_titlemiscloan, listing_titlepersonalloan)

pie(clust7.lessthan40$n, labels = paste0(round(100 * clust7.lessthan40$n/sum(clust7.lessthan40$n), 2), "%"),
    main = "Cluster 7 Loans with less than 40% paid")
legend("bottomleft", legend = c("Creditcard", "Debtrefinancing", "Misc", "Personal"),
       fill =  c("lightcyan","mistyrose", "lightblue","white"))

#pie chart for cluster 7 loans with more than 40% paid
clust7.morethan40 <- cluster.7 %>%
  filter(percentage_of_total_paid > 40) %>%
  count(listing_titlecreditcard, listing_titledebt_refinancing, listing_titlehousingloan, listing_titlemiscloan, listing_titlepersonalloan)

pie(clust7.morethan40$n, labels = paste0(round(100 * clust7.morethan40$n/sum(clust7.morethan40$n), 2), "%"),
    main = "Cluster 7 Loans with more than 40% paid")
legend("bottomleft", legend = c("Creditcard", "Debtrefinancing", "Misc", "Personal"),
       fill =  c("lightcyan","mistyrose", "lightblue","white"))

#pie chart for ratings
clust7.ratings <- cluster.7 %>%
  group_by(rating) %>%
  count()

pie(clust7.ratings$n, labels = paste0(round(100 * clust7.ratings$n/sum(clust7.ratings$n), 2), "%"),
    main = "Ratings for Cluster 7 Loans")
legend("bottomleft", legend = c("B", "C"),
       fill =  c("white", "lightblue"))

#pie chart for tenure plan
clust7.tenure<- cluster.7 %>%
  group_by(tenure_plan) %>%
  count()

pie(clust7.tenure$n, labels = paste0(round(100 * clust7.tenure$n/sum(clust7.tenure$n), 2), "%"),
    main = "Tenure Plan for Cluster 7 Loans")
legend("bottomleft", legend = c("Base", "Plus"),
       fill =  c("white", "lightblue"))

#Outlier Detection for Uncompleted

#Calculate Distances between Objects and cluster centers
uncompleted.centers <- km.uncompleted$centers[km.uncompleted$cluster, ]

#Euclidean Distance
uncompleted.dist <- sqrt(rowSums((uncompleted.loans.norm - uncompleted.centers)^2))

#Calculate Mean Distance by Cluster
uncompleted.mdist <- tapply(uncompleted.dist, km.uncompleted$cluster, mean)

#Divide each distance by its mean for its cluster
uncompleted.distscore <- uncompleted.dist/(uncompleted.mdist[km.uncompleted$cluster])

#Make it into a dataframe
uncompleted.distfact <- data.frame(uncompleted.distscore)

#Attach the column names
colnames(uncompleted.distfact) <- "uncompleted.dist.factor"

#Min-Max Normalisation
uncompleted.minmaxscore <- data.frame((uncompleted.distscore - min(uncompleted.distscore))/
                                        (max(uncompleted.distscore) -min(uncompleted.distscore)))

#Attach the column names
colnames(uncompleted.minmaxscore) <- "uncompleted.score.minmax"

#Adding of Columns to dataframe
uncompleted.df <- uncompleted.loans.clustn %>%
  bind_cols(uncompleted.distfact, uncompleted.minmaxscore)

#Predetermine Threshold Minmax score
P <- 0.7

#Plot Outliers (i think we have to edit this)
plot(x = uncompleted.df$uncompleted.score.minmax,
     y = uncompleted.df$uncompleted.dist.factor,
     main = "Outliers based on Distance Score %",
     xlab = "MinMax Score",
     ylab = "Distance Score",
     col = ifelse(uncompleted.minmaxscore > P,"red","black"), 
     pch = 13) 

#Determine number of Outliers based on Distance Score % Criterion
x <- length( which(uncompleted.df$uncompleted.score.minmax > P))
#1 outlier detected

#Outliers (Above Distance Minmax Score of 50%)
uncomplted.minmax.order <- order(uncompleted.distscore, decreasing = TRUE)[1:x]

#Coerce outliers into dataframe
uncompleted.minmax.outl <- data.frame(uncompleted.df[uncomplted.minmax.order,])

# Streamlined Clustering for Completed Loans (Section 7)------

#filter out completed loans
completed.streamlined <- loan.streamlined %>%
  filter (status %in% c("complete")) %>%
  select (-c(status, dpd))

#normalisation of data 
completed.streamlined.norm <- sapply(completed.streamlined,scale)
rownames(completed.streamlined.norm) <- rownames(completed.streamlined)  #add row names

# Elbow to find optimal number of clusters
set.seed(1) 
k.max <- 10
wss <- (nrow(completed.streamlined.norm)-1)*sum(apply(completed.streamlined.norm,2,var))
for (i in 2:k.max) wss[i] <- sum(kmeans(completed.streamlined.norm,
                                        centers=i,
                                        iter.max = 20,
                                        algorithm = "Hartigan-Wong")$withinss)

#plot the elbow plot 
plot(1:k.max, wss, type="b", xlab="Number of Clusters K",
     ylab="Total within-cluster sum of squares",
     main="Elbow Plot to find Optimal Number of Clusters for Completed Streamlined Loans",
     pch=19, cex=1)

#Optimal Cut-off Line
abline(v = 6, lty =2)

#k means clustering with 6 clusters
set.seed(1)
km.completedstreamlined <- kmeans(completed.streamlined.norm, 6, algorithm = "Hartigan-Wong")

#number of records in each cluster
km.completedstreamlined$size 
# results: [1] 27476  2161 22168  4979 57089 13941
# cluster 2 has the smallest number of records of 2161 (outlying cluster); 
# cluster 5 has the biggest number of records of 57089

# Cluster Centroids
km.completedstreamlined$centers

# within cluster sum of squares
km.completedstreamlined$withinss
# results: [1] 117811.289   6041.101 182100.252  69940.203 235221.974 212673.045
# cluster 7 has the smallest within cluster sum of squares of 6041.101
# cluster 6 has the bigger within cluster sum of squares of 235221.974

#dist between cluster centers
dist(km.completedstreamlined$centers)

# Assign Cluster Numbers to dataset
completed.streamlined.clustn <- completed.streamlined %>%
  bind_cols(data.frame(km.completedstreamlined$cluster))

#plot cluster (colour plot)
#install.packages("factoextra")
library(factoextra)
fviz_cluster(km.completedstreamlined, 
             data=completed.streamlined.norm,
             ellipse.type="convex",
             outlier.shape = 23)

# green plot
library(cluster)
clusplot(completed.streamlined.norm, km.completedstreamlined$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=1)

# Analysis of cluster plot
completed.streamlined.summary <- completed.streamlined.clustn %>%
  group_by(km.completedstreamlined.cluster) %>%
  summarise_all("mean")

#Outlier Detection for completed

#Calculate Distances between Objects and cluster centers
completedstreamlined.centers <- km.completedstreamlined$centers[km.completedstreamlined$cluster, ]

#Euclidean Distance
completedstreamlined.dist <- sqrt(rowSums((completed.streamlined.norm - completedstreamlined.centers)^2))

#Calculate Mean Distance by Cluster
completedstreamlined.mdist <- tapply(completedstreamlined.dist, km.completedstreamlined$cluster, mean)

#Divide each distance by its mean for its cluster
completedstreamlined.distscore <- completedstreamlined.dist/(completedstreamlined.mdist[km.completedstreamlined$cluster])

#Make it into a dataframe
completedstreamlined.distfact <- data.frame(completedstreamlined.distscore)

#Attach the column names
colnames(completedstreamlined.distfact) <- "completedstreamlined.dist.factor"

#Min-Max Normalisation
completedstreamlined.minmaxscore <- data.frame((completedstreamlined.distscore - min(completedstreamlined.distscore))/
                                      (max(completedstreamlined.distscore) -min(completedstreamlined.distscore)))

#Attach the column names
colnames(completedstreamlined.minmaxscore) <- "completedstreamlined.score.minmax"

#Adding of Columns to dataframe
completedstreamlined.df <- completed.streamlined.clustn %>%
  bind_cols(completedstreamlined.distfact, completedstreamlined.minmaxscore)

#Predetermine Threshold Minmax score
P <- 0.7

#Plot Outliers 
plot(x = completedstreamlined.df$completedstreamlined.score.minmax,
     y = completedstreamlined.df$completedstreamlined.dist.factor,
     main = "Outliers based on Distance Score %",
     xlab = "MinMax Score",
     ylab = "Distance Score",
     col = ifelse(completedstreamlined.minmaxscore > P,"red","black"), 
     pch = 13) 

#Determine number of Outliers based on Distance Score % Criterion
x <- length( which(completedstreamlined.df$completedstreamlined.score.minmax > P))

x  #2 outliers detected

#Outliers (Above Distance Minmax Score of 50%)
completedstreamlined.minmax.order <- order(completedstreamlined.distscore, decreasing = TRUE)[1:x]

#Coerce outliers into dataframe
completedstreamlined.minmax.outl <- data.frame(completedstreamlined.df[completedstreamlined.minmax.order,])




# Classification Tree (Section 8)----

#encoding
classification.status<- loan_date %>%
  mutate(status = sub("charge-off", "uncompleted", status)) %>%
  mutate(status = sub("default", "uncompleted", status))

classification.encoded <- classification.status %>%
  mutate_if(is.character, as.factor) 

#select variables
selected.classification<- classification.encoded %>%
  select(c(amount_borrowed, tenure_plan, int_rate, rating, listing_title, status))

#split the data into test and train sets
set.seed(1)
train.index.classification <- sample(nrow(selected.classification), 0.6*nrow(selected.classification))

train.mlm.classification <- selected.classification[train.index.classification, ]
test.mlm.classification <- selected.classification[-train.index.classification, ]

#classification tree
library("rpart")
library("rpart.plot")

#simple plot without controlling cp and minsplit did not have yield a tree
#biggest tree - overgrown and overfitted, therefore not used -> !! plotting may cause R to crash !!
loan.rpart<- rpart(status ~.,
                         data = train.mlm.classification,
                         method = "class",
                         cp = 0,
                         minsplit = 1)

prp(loan.rpart, type = 5, extra = 1, under = T, 
    split.font = 1, varlen = -30)

#deep tree that has the best fit- selecting minsplit to be 900
loan.rpart.deep <- rpart(status ~.,
                         data = train.mlm.classification,
                         method = "class",
                         cp = 0,
                         minsplit = 900)

prp(loan.rpart.deep, type = 5, extra = 1, under = T, 
    split.font = 1, varlen = -30)

#rules for deep tree
rpart.rules(loan.rpart.deep)
printcp(loan.rpart.deep)

#finding optimal CP
library("dplyr")

optimal.cp <- data.frame(loan.rpart.deep$cptable)
cp.test <- optimal.cp %>% 
  mutate(criteria = ifelse(rel.error + xstd < xerror, 
                           "yes",
                           "no")) # From here, the Optimal CP = 0


#Confusion Matrix
loan.pred <- predict(object = loan.rpart.deep, 
                     newdata = test.mlm.classification, 
                     type = "class")
confusionMatrix(data = loan.pred, 
                reference = test.mlm.classification$status)

#Plot ROC
library(pROC)
loan.rpart.pred.prob <- predict(loan.rpart.deep,
                                test.mlm.classification,
                                type = "prob")
loan.rpart.combined <- cbind(test.mlm.classification, loan.rpart.pred.prob)

par(mar=c(10,5,1,0.5))
rpart.deep.ROC<-plot.roc(loan.rpart.combined$status,
                         loan.rpart.combined$"uncompleted",
                         main = "ROC for rpart deep")
print(rpart.deep.ROC) # Display ROC curve of rpart model

library(mltools)
mcc(preds =  loan.pred,
    actuals = as.factor(test.mlm.classification$status))


# Pruning with the C5.0 Algorithm
library(C50)

# Using C5.0 Decision Trees
loan.c5<-C5.0(as.factor(status)~., data = train.mlm.classification)
summary(loan.c5)
plot(loan.c5)

library(partykit)

loan.c5.model <- as.party.C5.0(loan.c5) # convert into party format

# C5.0 Decision Rules Set
loan.c5.rules<-C5.0(as.factor(status)~., data = train.mlm.classification, rules= TRUE)
summary(loan.c5.rules) # to read rules

# on test set
loan.c5.pred <- predict(loan.c5,test.mlm.classification,type="class")
# generate confusion matrix
confusionMatrix(table(loan.c5.pred, test.mlm.classification$status),
                positive = "complete")


#----------------------------------------------------------------
############# REGRESSION to determine how much to lend ----
#reformat loan date
loan_date <- read_csv("loan_new.csv",
                      col_types = cols(loan_date = col_date(format="%B %Y"),
                                       prev_pym = col_date(format = "%B %Y")))

#renaming charge-off and default to uncompleted, removing unwanted columns
loan.clean<- loan_date %>%
  mutate(status = sub("charge-off", "uncompleted", status)) %>%
  mutate(status = sub("default", "uncompleted", status)) %>%
  select(-c(4, 6, 8:14))

#streamline listing titles (done differently from earlier to integer encode)
streamlined.clean<- loan.clean %>%
  mutate(listing_title = sub("creditcard", "creditcard_loan", listing_title)) %>%
  mutate(listing_title = sub("debt_refinancing", "debtrefinancing_loan", listing_title)) %>%
  mutate(listing_title = sub("energy_effc_financing", "housing_loan", listing_title)) %>%
  mutate(listing_title = sub("homereno", "housing_loan", listing_title)) %>%
  mutate(listing_title = sub("home", "housing_loan", listing_title)) %>%
  mutate(listing_title = sub("major_purchase", "personal_loan", listing_title)) %>%
  mutate(listing_title = sub("medical", "personal_loan", listing_title)) %>%
  mutate(listing_title = sub("other", "misc_loan", listing_title)) %>%
  mutate(listing_title = sub("relocation", "housing_loan", listing_title)) %>%
  mutate(listing_title = sub("sme", "misc_loan", listing_title)) %>%
  mutate(listing_title = sub("travel", "personal_loan", listing_title)) %>%
  mutate(listing_title = sub("vehicle", "personal_loan", listing_title)) %>%
  mutate(listing_title = sub("wedding", "personal_loan", listing_title))

#The 5 streamline groups are credit card, debt financing, home, major purchase, others

# Completed Loans Regression Model (Section 9.1)----
#filter out completed
completed.regression <- streamlined.clean %>%
  filter (status %in% c("complete")) %>%
  select (-c(status))

#integer encoding for all character values 
completed.regression.encoded <- completed.regression %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

#Set Seed for reproducing partition
set.seed(1) 
completed.index <- sample(nrow(completed.regression.encoded), 0.6*nrow(completed.regression.encoded))

#Split data into Training (0.6) and Test (0.4) Sets
completedtrain.mlm <- completed.regression.encoded[completed.index,]
completedtest.mlm<- completed.regression.encoded[-completed.index,]  

#Run lm() of Price on all predictors in training set
completed.mlm <- lm(amount_borrowed ~.,data = completedtrain.mlm)

summary(completed.mlm)
options(scipen = 999)

str(completed.mlm)

#Matrix Multiplication (Only for Numeric Data)
completed.mlm$coefficients %*% c(1, 1, 0.3, 2, 2 ) 
#for a person that wants a base plan, has a 20% interest rate, B rating and for debt refinancing
#loan out 10932.76 at most to have the highest probability of being completed

#intercept, tenure plan, int rate, rating, listing title
## Tenure plan: 1 = Base, 2 = Plus
## rating: 1 = A, 2 = B, 3 = C
## listing title: 1 = credit card loan, 2 = debt refinancing loan,  3 = housing loan,  4 = misc loan,  5 = personal loan

install.packages("forecast")
library(forecast)

completed.mlm.pred <- predict(completed.mlm, completedtest.mlm)

completed.mlm.residual.sel <- completedtest.mlm$amount_borrowed - completed.mlm.pred

completed.mlm.results <- data.frame ("Actual" = completedtest.mlm$amount_borrowed,
                                     "Predicted" = completed.mlm.pred,
                                     "Residual" = completed.mlm.residual.sel)

accuracy(completed.mlm.pred, completedtest.mlm$amount_borrowed)
############### ME     RMSE      MAE       MPE     MAPE
# Test set -13.81302 7668.062 6116.173 -45.93472 70.41026

#backward elimination - no difference
completed.mlm.bwe <- step(completed.mlm, direction = "backward")
summary(completed.mlm.bwe)  
completed.mlm.bwe.pred <- predict(completed.mlm.bwe, completedtest.mlm)
accuracy(completed.mlm.bwe.pred, completedtest.mlm$amount_borrowed)

#forward elimination - no difference
completed.mlm.fwd <- step(completed.mlm, direction = "forward")
summary(completed.mlm.fwd)  
completed.mlm.fwd.pred <- predict(completed.mlm.fwd, completedtest.mlm)
accuracy(completed.mlm.fwd.pred, completedtest.mlm$amount_borrowed)

#stepwise elimination - no difference
completed.mlm.swr <- step(completed.mlm, direction = "both")
summary(completed.mlm.swr)  
completed.mlm.swr.pred <- predict(completed.mlm.swr, completedtest.mlm)
accuracy(completed.mlm.swr.pred, completedtest.mlm$amount_borrowed)

#The team ran backward, forward and stepwise elimination for the completed model 
#but did not analyse them as there was no difference

# Uncompleted Loans Regression Model (Section 9.2) -----
#filter out uncompleted
uncompleted.regression <- streamlined.clean %>%
  filter (status %in% c("uncompleted")) %>%
  select (-c(status))

#integer encoding for all character values 
uncompleted.regression.encoded <- uncompleted.regression %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

#Set Seed for reproducing partition
set.seed(1) 
uncompleted.index <- sample(nrow(uncompleted.regression.encoded), 0.6*nrow(uncompleted.regression.encoded))

#Split data into Training (0.6) and Test (0.4) Sets
uncompletedtrain.mlm <- uncompleted.regression.encoded[uncompleted.index,]
uncompletedtest.mlm <- uncompleted.regression.encoded[-uncompleted.index,]  

#Run lm() of Price on all predictors in training set
uncompleted.mlm <- lm(amount_borrowed ~.,data = uncompletedtrain.mlm)

summary(uncompleted.mlm)
options(scipen = 999)

str(uncompleted.mlm)

#Matrix Multiplication (Only for Numeric Data)
uncompleted.mlm$coefficients %*% c(1, 1, 0.3, 2, 2) 
#for a person that wants a base plan, has a 20% interest rate, B rating and for debt refinancing
#loan out 13667.14 at most to have the lowest probability of being uncompleted


install.packages("forecast")
library(forecast)

uncompleted.mlm.pred <- predict(uncompleted.mlm, uncompletedtest.mlm)

uncompleted.mlm.residual.sel <- uncompletedtest.mlm$amount_borrowed - uncompleted.mlm.pred

uncompleted.mlm.results <- data.frame ("Actual" = uncompletedtest.mlm$amount_borrowed,
                                       "Predicted" = uncompleted.mlm.pred,
                                       "Residual" = uncompleted.mlm.residual.sel)

accuracy(uncompleted.mlm.pred, uncompletedtest.mlm$amount_borrowed)
############# ME     RMSE      MAE       MPE     MAPE
# Test set 68.48487 7627.884 6098.493 -37.69458 61.53225

#backward elimination - negligible decrease in ME and RMSE
uncompleted.mlm.bwe <- step(uncompleted.mlm, direction = "backward")
summary(uncompleted.mlm.bwe)  
uncompleted.mlm.bwe.pred <- predict(uncompleted.mlm.bwe, uncompletedtest.mlm)
accuracy(uncompleted.mlm.bwe.pred, uncompletedtest.mlm$amount_borrowed)

#forward elimination - no difference
uncompleted.mlm.fwd <- step(uncompleted.mlm, direction = "forward")
summary(uncompleted.mlm.fwd)  
uncompleted.mlm.fwd.pred <- predict(uncompleted.mlm.fwd, uncompletedtest.mlm)
accuracy(uncompleted.mlm.fwd.pred, uncompletedtest.mlm$amount_borrowed)

#stepwise elimination - negligible increase in ME and decrease in RMSE
uncompleted.mlm.swr <- step(uncompleted.mlm, direction = "both")
summary(uncompleted.mlm.swr)  
uncompleted.mlm.swr.pred <- predict(uncompleted.mlm.swr, uncompletedtest.mlm)
accuracy(uncompleted.mlm.swr.pred, uncompletedtest.mlm$amount_borrowed)

#The team ran backward, forward and stepwise elimination for the uncompleted model 
#but did not analyse them deeply as the difference is negligible.



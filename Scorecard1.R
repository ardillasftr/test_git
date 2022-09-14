#Import package
library(DataExplorer)
library(ggplot2) #data visualization
library(InformationValue)
library(dplyr) #data transformation
library(grid)
library(gridExtra)
library(scorecard)
library(rbin) #binning, woe, iv
library(data.table)
library(tidyr) #data tidying
library(caTools) #split data
library(Information)
library(compareGroups)
library(ROSE)
library(corrplot)
library(car) #dipake buat stepaic
library(MASS) #dipake buat stepaic
library(reticulate) #plot pake function python
library(writexl)
library(DescTools)
library(Hmisc)
library(funModeling)
library(scoringTools)


################################## IMPORT DATA #######################################################

data_german <- read.csv("C:/Users/IDX-109/OneDrive - Compare88 Group Pte Ltd/Documents/idx partners/R/german_credit_data2.csv", sep=";", header=T)
data_german %>% glimpse()
#check data type
str(data_german)

#Drop kolom index
data_german <- drop_columns(data_german, "X")

head(data_german)

summary(data_german)

#ubah data type Risk jadi numeric
data_german$Risk <- replace(data_german$Risk, data_german$Risk=="good", 1)
data_german$Risk <- replace(data_german$Risk, data_german$Risk=="bad", 0)
data_german$Risk <- as.numeric(data_german$Risk)

head(data_german)

#check missing value
#data_german[!complete.cases(data_german),]
data_german <- set_missing(data_german, list(0L, "missing"))

head(data_german)

#target variable on dataset
pie(table(data_german$Risk))
ggplot(data=data_german) + geom_bar(mapping = aes(x = Risk))
#geom_bar makes the height of the bar proportional to the number of cases in each group (or if the weight aesthetic is supplied, the sum of the weights).
#aes = aesthetic = variable

#split data
set.seed(42)
data_split <- sample.split(data_german, SplitRatio=0.7)
train = subset(data_german, data_split == TRUE)
test = subset(data_german, data_split == FALSE)
head(train)
data_split
############################################### IV & woe ################################################
IVs <- create_infotables(data=train, NULL, y="Risk", bin=20)
IVs$Summary

#IV 
features_iv <- c("Checking.account", "Credit.amount", "Duration", "Saving.account", "Age", "Housing", "Purpose", "Sex", "Job")
IV <- c(0.6688, 0.2846, 0.2562, 0.1581, 0.1337, 0.0870, 0.0655, 0.0278, 0.0164)

IV <- data.frame(features_iv, IV)
IV[]

train <- drop_columns(train, "Job")
test <- drop_columns(test, "Job")

#coba pake woe function
bins_cont <- woebin(train, y="Risk", x=c('Age', 'Credit.amount', 'Duration'), positive=1)
bins_char <- woebin(train, y="Risk", x=c('Checking.account', 'Saving.accounts', 'Purpose', 'Housing', 'Sex'), positive=1)

#woe manual train
train_woe <- train
train_woe <- train_woe %>% mutate(Age_woe = case_when(Age<=26 ~ -0.365,
                                   Age<= 43 & Age>=27 ~ 0.085,
                                   Age>=44 ~ 0.217))

train_woe <- train_woe %>% mutate(Duration_woe = case_when(Duration<=12 ~ 0.408,
                                             Duration<=24 & Duration>=13 ~ 0.012,
                                             Duration>=25 ~ -0.563))
train_woe <- train_woe %>% mutate(Credit.amount_woe = case_when(Credit.amount<=3914 ~ 0.196,
                                                     Credit.amount>= 3915 & Credit.amount<= 6999 ~ -0.280,
                                                     Credit.amount>=7000 ~ -0.876))
train_woe <- train_woe %>% mutate(Saving.accounts_woe = case_when(Saving.accounts=="missing" ~ 0.696,
                                                       Saving.accounts=="quite rich" | Saving.accounts=="rich" ~ 0.523,
                                                       Saving.accounts=="moderate" ~ 0.251,
                                                       Saving.accounts=="little" ~ -0.281))
train_woe <- train_woe %>% mutate(Purpose_woe = case_when(Purpose=="business" ~ -0.175,
                                               Purpose=="education" | Purpose=="vacation/others" ~ -0.287,
                                               Purpose=="car" | Purpose=="domestic appliances" | Purpose=="furniture/equipment" | Purpose=="repairs" ~ -0.092,
                                               Purpose=="radio/TV" ~ 0.394))
train_woe <- train_woe %>% mutate(Housing_woe = case_when(Housing=="free" | Housing=="rent" ~ -0.446,
                                               Housing=="own" ~ 0.196))
train_woe <- train_woe %>% mutate(Checking.account_woe = case_when(Checking.account=="missing" ~ 1.199,
                                                        Checking.account=="rich" ~ 0.310,
                                                        Checking.account=="moderate" ~ -0.345,
                                                        Checking.account=="little" ~ -0.827))
train_woe <- train_woe %>% mutate(Sex_woe = case_when(Sex=="male" ~ 0.113,
                                           Sex=="female" ~ -0.244))

head(train_woe)

#drop features asli
train_woe <- drop_columns(train_woe, c("Age", "Sex", "Housing", "Saving.accounts", "Checking.account", "Credit.amount", "Duration","Purpose"))

#woe manual test
test_woe <- test
test_woe <- test_woe %>% mutate(Age_woe = case_when(Age<=26 ~ -0.365,
                                              Age<= 43 & Age>=27 ~ 0.085,
                                              Age>=44 ~ 0.217))

test_woe <- test_woe %>% mutate(Duration_woe = case_when(Duration<=12 ~ 0.408,
                                                   Duration<=24 & Duration>=13 ~ 0.012,
                                                   Duration>=25 ~ -0.563))
test_woe <- test_woe %>% mutate(Credit.amount_woe = case_when(Credit.amount<=3914 ~ 0.196,
                                                        Credit.amount>= 3915 & Credit.amount<= 6999 ~ -0.280,
                                                        Credit.amount>=7000 ~ -0.876))
test_woe <- test_woe %>% mutate(Saving.accounts_woe = case_when(Saving.accounts=="missing" ~ 0.696,
                                                          Saving.accounts=="quite rich" | Saving.accounts=="rich" ~ 0.523,
                                                          Saving.accounts=="moderate" ~ 0.251,
                                                          Saving.accounts=="little" ~ -0.281))
test_woe <- test_woe %>% mutate(Purpose_woe = case_when(Purpose=="business" ~ -0.175,
                                                  Purpose=="education" | Purpose=="vacation/others" ~ -0.287,
                                                  Purpose=="car" | Purpose=="domestic appliances" | Purpose=="furniture/equipment" | Purpose=="repairs" ~ -0.092,
                                                  Purpose=="radio/TV" ~ 0.394))
test_woe <- test_woe %>% mutate(Housing_woe = case_when(Housing=="free" | Housing=="rent" ~ -0.446,
                                                  Housing=="own" ~ 0.196))
test_woe <- test_woe %>% mutate(Checking.account_woe = case_when(Checking.account=="missing" ~ 1.199,
                                                           Checking.account=="rich" ~ 0.310,
                                                           Checking.account=="moderate" ~ -0.345,
                                                           Checking.account=="little" ~ -0.827))
test_woe <- test_woe %>% mutate(Sex_woe = case_when(Sex=="male" ~ 0.113,
                                              Sex=="female" ~ -0.244))

head(test_woe)

#drop features asli
test_woe <- drop_columns(test_woe, c("Age", "Sex", "Housing", "Saving.accounts", "Checking.account", "Credit.amount", "Duration","Purpose"))

#IV setelah binning
#IV_woe <- data.frame()
#IV_woe

#CORRELATION
cor(train_woe)
corrplot(cor(train_woe), method = 'number') # colorful number
train_woe$score<-NULL

##################################### MODELLING #######################################################
str(train_woe)
nrow(train_woe)
nrow(test_woe)

table(train_woe$Risk)

#logreg semua feature
model_woe <- glm(Risk~., data=train_woe, family="binomial")
model_woe

#regresi dgn stepwise
model_stepwise <- MASS::stepAIC(model_woe, direction = "both", trace=TRUE)

sum_model <- broom::tidy(model_stepwise) %>% dplyr::select(term, estimate, std.error, p.value) %>%
  mutate(W=(estimate/std.error)^2) %>%
  rename(var=term, coef=estimate) %>% arrange(desc(W))
as.data.frame(sum_model) %>% arrange(W)
sum_model

#apply model di train dan test ITERASI AWAL
pred_train <- predict(model_stepwise, train_woe, type="response")
pred_test <- predict(model_stepwise, test_woe, type="response")

#model evaluation ITERASI AWAL
#ROC AUC
roc.curve(train_woe$Risk, pred_train, plotit = TRUE, col=1)
roc.curve(test_woe$Risk, pred_test, plotit = TRUE, col=2, add=TRUE)
legend("bottomright", c("Data Train Iterasi Awal", "Data Test Iterasi Awal"), col=1:2, lwd=1)

#K-S
ks_stat(train_woe$Risk, pred_train)
ks_stat(test_woe$Risk, pred_test)

#drop sesuai dengan features yang ada di stepwise dan p-value diatas 0.05
train_woe <- drop_columns(train_woe, c("Sex_woe", "Age_woe", "Credit.amount_woe", "Purpose_woe", "Housing_woe"))
test_woe <- drop_columns(test_woe, c("Sex_woe", "Age_woe", "Credit.amount_woe", "Purpose_woe", "Housing_woe"))
head(train_woe)

#logreg feature terseleksi
model_woe2 <- glm(Risk ~ Checking.account_woe + Duration_woe + Saving.accounts_woe, data=train_woe, family="binomial")
model_woe2

#regresi dgn stepwise 2
model_stepwise2 <- MASS::stepAIC(model_woe2, direction = "both", trace=TRUE)

sum_model <- broom::tidy(model_stepwise2) %>% dplyr::select(term, estimate, std.error, p.value) %>%
  mutate(W=(estimate/std.error)^2) %>%
  rename(var=term, coef=estimate) %>% arrange(desc(W))
as.data.frame(sum_model) %>% arrange(W)
sum_model

#apply model di train dan test ITERASI FINAL
pred_train <- predict(model_stepwise2, train_woe, type="response")
pred_test <- predict(model_stepwise2, test_woe, type="response")

#model evaluation ITERASI FINAL
#ROC AUC
auc_train = roc.curve(train_woe$Risk, pred_train, plotit = TRUE, col=1)
auc_test = roc.curve(test_woe$Risk, pred_test, plotit = TRUE, col=2, add=TRUE)
legend("bottomright", c("Data Train Iterasi Akhir", "Data Test Iterasi Akhir"), col=1:2, lwd=1)
auc_train
auc_test
#K-S
ks_stat(train_woe$Risk, pred_train)
ks_stat(test_woe$Risk, pred_test)

#Gini
Gini_train <- 2*auc_train$auc-1
Gini_train
Gini_test <- 2*auc_test$auc-1
Gini_test

################################################## SCORECARD ##############################################
#SCALING
#bikin table woe
features <- c("Checking.account_woe", "Checking.account_woe", "Checking.account_woe", "Checking.account_woe", "Duration_woe", "Duration_woe", "Duration_woe","Saving.acounts_woe", "Saving.acounts_woe", "Saving.acounts_woe", "Saving.acounts_woe")
bin <- c("missing", "rich", "moderate", "little","[4, 12]", "[13, 24]", "[25, 60]", "missing", "quite rich, rich", "moderate", "little")
woe <- c(1.199, 0.31, -0.345, -0.827, 0.408, 0.012, -0.563, 0.696, 0.523, 0.251, -0.281)

table_woe <- data.frame(features, bin, woe)
table_woe[]

factor_value <- 20/log(2)
offset_value <- 600-factor_value*log(50)

beta <- model_stepwise2$coefficients
betawoe <- rep(beta[2:4], times=as.vector((table(table_woe[,1]))))
betawoe

table_woe$score <- round((betawoe*table_woe[,3]+beta[1]/3)*factor_value+offset_value/3)

table_woe
beta
beta[2:4]


#SCORECARD
#data test
head(train_woe)
train_score <- train_woe
train_score <- train_score %>% mutate(Saving.accounts_score = case_when(Saving.accounts_woe==0.696 ~ 182,
                                                                        Saving.accounts_woe==0.523 ~ 179,
                                                                        Saving.accounts_woe==0.251 ~ 174,
                                                                        Saving.accounts_woe==-0.281 ~ 166))
train_score <- train_score %>% mutate(Duration_score = case_when(Duration_woe==0.408 ~ 183,
                                                                 Duration_woe==0.012 ~ 171,
                                                                 Duration_woe==-0.563 ~ 153))
train_score <- train_score %>% mutate(Checking.account_score = case_when(Checking.account_woe==1.199 ~ 205,
                                                                         Checking.account_woe==0.31 ~ 179,
                                                                         Checking.account_woe==-0.345 ~ 161,
                                                                         Checking.account_woe==-0.827 ~ 147))

#data test
test_score <- test_woe
test_score <- test_score %>% mutate(Saving.accounts_score = case_when(Saving.accounts_woe==0.696 ~ 182,
                                                                      Saving.accounts_woe==0.523 ~ 179,
                                                                      Saving.accounts_woe==0.251 ~ 174,
                                                                      Saving.accounts_woe==-0.281 ~ 166))
test_score <- test_score %>% mutate(Duration_score = case_when(Duration_woe==0.408 ~ 183,
                                                                 Duration_woe==0.012 ~ 171,
                                                                 Duration_woe==-0.563 ~ 153))
test_score <- test_score %>% mutate(Checking.account_score = case_when(Checking.account_woe==1.199 ~ 205,
                                                                       Checking.account_woe==0.31 ~ 179,
                                                                       Checking.account_woe==-0.345 ~ 161,
                                                                       Checking.account_woe==-0.827 ~ 147))

#apply score ke data train
train_score <- drop_columns(train_score, c("Duration_woe", "Saving.accounts_woe", "Checking.account_woe"))
test_score <- drop_columns(test_score, c("Duration_woe", "Saving.accounts_woe", "Checking.account_woe"))
head(train_score)

train$score <- train_score$Saving.accounts_score+train_score$Duration_score+train_score$Checking.account_score
head(train)

#apply score ke data test
test$score <- test_score$Saving.accounts_score+test_score$Duration_score+test_score$Checking.account_score
head(test)

#grafik score
#grouping data
data_ggp <- data.frame(x=train$score,
                       my_group=train$Risk)
data_ggp

#histogram
hist(train$score, color=train$Risk)

grafik <- ggplot(data_ggp, aes(x, fill=my_group)) +
  geom_histogram()
grafik

head(train)
pred_train
head(train_score)
write_xlsx(train, "C:\\Users\\IDX-109\\OneDrive - Compare88 Group Pte Ltd\\Documents\\idx partners\\R\\score_train.xlsx")
write_xlsx(test, "C:\\Users\\IDX-109\\OneDrive - Compare88 Group Pte Ltd\\Documents\\idx partners\\R\\score_test.xlsx")

#BINNING RISK RANK

binning_var <- rbin_equal_freq(train, Risk, score, bins=10)
binning_var

for (df_bin in c("train", "test")){
  temp <- get(df_bin)
  print(df_bin)
  temp$binning = case_when(temp$score<=binning_var$bins$upper_cut[1] ~ 1,
                           temp$score<=binning_var$bins$upper_cut[2] ~ 2,
                           temp$score<=binning_var$bins$upper_cut[3] ~ 3,
                           temp$score<=binning_var$bins$upper_cut[4] ~ 4,
                           temp$score<=binning_var$bins$upper_cut[5] ~ 5,
                           temp$score<=binning_var$bins$upper_cut[6] ~ 6,
                           temp$score<=binning_var$bins$upper_cut[7] ~ 7,
                           temp$score<=binning_var$bins$upper_cut[8] ~ 8,
                           temp$score<=binning_var$bins$upper_cut[9] ~ 9,
                           TRUE ~ 10)
  assign(paste0("bin", df_bin),
         temp %>% group_by(binning, Risk) %>% summarise(n=n()) %>% spread(Risk, n))
  print(get(paste0("bin", df_bin)))
  rm(temp)
}

bintest$binning

bintest


eq <- equal_freq(var=test$score, n_bins=10)
summary(eq)

#=========================================================================
#cobain doang
num <- train %>% select_if(is.numeric)
head(num)

#box plot
num %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = variable, y = value)) + geom_boxplot() +
  facet_wrap(~variable, scales = "free")

auc_test$false.positive.rate

#============================================================================= REJECT INFERENCE

#buat data dummy, 20% dari data asli
set.seed(4848) #fix random
data_germann <- read.csv("C:/Users/IDX-109/OneDrive - Compare88 Group Pte Ltd/Documents/idx partners/R/german_credit_data2.csv", sep=";", header=T)
split <- sample.split(data_germann$Risk, SplitRatio = 0.8)
german_approved <- subset(data_germann, split == TRUE)
german_reject <- subset(data_germann, split == FALSE)

# drop kolom risk (indikator good/bad) di data reject
german_reject$Risk=NULL

table(german_approved$Risk)

#iv & woe
#drop features yang punya iv <2%
german_approved <-  drop_columns(german_approved, "Job")
german_reject <-  drop_columns(german_reject, "Job")


german_reject_woe <- german_reject

#apply woe ke data
german_reject_woe <- german_reject_woe %>% mutate(Age_woe = case_when(Age<=26 ~ -0.365,
                                                      Age<= 43 & Age>=27 ~ 0.085,
                                                      Age>=44 ~ 0.217))

german_reject_woe <- german_reject_woe %>% mutate(Duration_woe = case_when(Duration<=12 ~ 0.408,
                                                           Duration<=24 & Duration>=13 ~ 0.012,
                                                           Duration>=25 ~ -0.563))
german_reject_woe <- german_reject_woe %>% mutate(Credit.amount_woe = case_when(Credit.amount<=3914 ~ 0.196,
                                                                Credit.amount>= 3915 & Credit.amount<= 6999 ~ -0.280,
                                                                Credit.amount>=7000 ~ -0.876))
german_approved_woe <- german_approved_woe %>% mutate(Saving.accounts_woe = case_when(Saving.accounts=="missing" ~ 0.696,
                                                                  Saving.accounts=="quite rich" | Saving.accounts=="rich" ~ 0.523,
                                                                  Saving.accounts=="moderate" ~ 0.251,
                                                                  Saving.accounts=="little" ~ -0.281))
german_approved_woe <- german_approved_woe %>% mutate(Purpose_woe = case_when(Purpose=="business" ~ -0.175,
                                                          Purpose=="education" | Purpose=="vacation/others" ~ -0.287,
                                                          Purpose=="car" | Purpose=="domestic appliances" | Purpose=="furniture/equipment" | Purpose=="repairs" ~ -0.092,
                                                          Purpose=="radio/TV" ~ 0.394))
german_approved_woe <- german_approved_woe %>% mutate(Housing_woe = case_when(Housing=="free" | Housing=="rent" ~ -0.446,
                                                          Housing=="own" ~ 0.196))
german_approved_woe <- german_approved_woe %>% mutate(Checking.account_woe = case_when(Checking.account=="missing" ~ 1.199,
                                                                   Checking.account=="rich" ~ 0.310,
                                                                   Checking.account=="moderate" ~ -0.345,
                                                                   Checking.account=="little" ~ -0.827))
german_approved_woe <- german_approved_woe %>% mutate(Sex_woe = case_when(Sex=="male" ~ 0.113,
                                                      Sex=="female" ~ -0.244))

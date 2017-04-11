train <- read.csv("train.csv",header = TRUE)
test <- read.csv("test.csv",header = TRUE)

test.survived <- data.frame(survived=rep("None",nrow(test)),test[,])

data.combined <- rbind(train,test.survived)

str(data.combined)

data.combined$survived <- as.factor(data.combined$survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

table(data.combined$survived)
table(data.combined$Pclass)

library(ggplot2)


train$Pclass <- as.factor(train$Pclass)
ggplot(train,aes(x=Pclass,fill=factor(!survived))) +
  geom_bar(width=0.5)+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill="Survived")
  
head(as.character(train$Name))

length(unique(as.character(data.combined$Name)))

dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

data.combined[which(data.combined$Name %in% dup.names),]

library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mrses[1:5,]

male <- data.combined[which(train$Sex=="male"),]
male[1:5,]

extractTitle <- function(Name){
  name <- as.character(Name)
  
  if(length(grep("Miss.",name))>0){
    return("Miss.")
  }
  
  else if(length(grep("Master.",name))>0){
    return("Master.")
  }
  
  else if(length(grep("Mrs..",name))>0){
    return("Mrs.")
  }
  
  else if(length(grep("Mr.",name))>0){
    return("Mr.")
  }
  else
    return("Other")
}

Titles <- NULL
for(i in 1:nrow(data.combined)){
  Titles <- c(Titles,extractTitle(data.combined[i,"Name"])) 
}
data.combined$Title<- as.factor(Titles)

ggplot(data.combined[1:891,],aes(x=Title,fill=survived)) +
  geom_bar(width=0.5)+
  facet_wrap(~Pclass)+
  ggTitle("Pclass")+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill="Survived")

table(data.combined$Sex)

ggplot(data.combined[1:891,],aes(x=Sex,fill=survived)) +
  geom_bar(width=0.5)+
  facet_wrap(~Pclass)+
  ggTitle("Pclass")+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill="Survived")

summary(data.combined$Age)


ggplot(data.combined[1:891,],aes(x=Age,fill=survived)) +
  facet_wrap(~Sex + Pclass)+
  geom_histogram(binwidth=10)+
  xlab("Age")+
  ylab("Total Count")

boys <- data.combined[which(data.combined$Title=="Master."),]
summary(boys$Age)

misses <- data.combined[which(data.combined$Title=="Miss."),]
summary(misses$Age)

ggplot(misses[misses$survived!="None",],aes(x=Age,fill=survived)) +
  geom_histogram(binwidth=5)+
  facet_wrap(~Pclass)+
  ggTitle("Age for Miss by Pclass")+
  xlab("Age")+
  ylab("Total Count")

misses.alone <- misses[which(misses$SibSp==0 & misses$Parch==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <=14.5))

summary(data.combined$SibSp)
length(which(data.combined$SibSp>0))

length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

ggplot(data.combined[1:891,],aes(x=SibSp,fill=survived)) +
  geom_bar(width=1)+
  facet_wrap(~Pclass+Title)+
  ggTitle("Pclass,Title")+
  xlab("Sibsp")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

data.combined$Parch <- as.factor(data.combined$Parch)

ggplot(data.combined[1:891,],aes(x=Parch,fill=survived)) +
  geom_bar(width=1)+
  facet_wrap(~Pclass+Title)+
  ggTitle("Pclass,Title")+
  xlab("Parch")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

temp.SibSp <- c(train$SibSp,test$SibSp)
temp.Parch <- c(train$Parch,test$Parch) 
data.combined$family.size <- as.factor(temp.Parch+temp.SibSp+1)

ggplot(data.combined[1:891,],aes(x=family.size,fill=survived)) +
  geom_bar(width=1)+
  facet_wrap(~Pclass+Title)+
  ggTitle("Pclass,Title")+
  xlab("Family Size")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

str(data.combined$Ticket)

data.combined$Ticket <- as.character(data.combined$Ticket) 
data.combined$Ticket[1:20]

ticket.first.char <- ifelse(data.combined$Ticket==""," ",substr(data.combined$Ticket,1,1))
unique(ticket.first.char)
length(unique(ticket.first.char))

data.combined$Ticket.first.char <- as.factor(ticket.first.char)

ggplot(data.combined[1:891,],aes(x=Ticket.first.char,fill=survived)) +
  geom_bar(width=1)+
  ggTitle("Ticket First char Analysis")+
  xlab("Ticket First char")+
  ylab("Total Count")+
  labs(fill="Survived")

ggplot(data.combined[1:891,],aes(x=Ticket.first.char,fill=survived)) +
  geom_bar(width=1)+
  facet_wrap(~Pclass)+
  ggTitle("Ticket First char Analysis")+
  xlab("Ticket First char")+
  ylab("Total Count")+
  ylim(0,150)+
  labs(fill="Survived")

ggplot(data.combined[1:891,],aes(x=Ticket.first.char,fill=survived)) +
  geom_bar(width=1)+
  facet_wrap(~Pclass+ Title)+
  ggTitle("Ticket First char Analysis")+
  xlab("Ticket First char")+
  ylab("Total Count")+
  ylim(0,200)+
  labs(fill="Survived")

summary(data.combined$Fare)
length(unique(data.combined$Fare))

ggplot(data.combined[1:891,],aes(x=Fare,fill=survived)) +
  geom_histogram(binwidth=10)+
  facet_wrap(~Pclass+ Title)+
  ggTitle("Combined Fare Distribution")+
  xlab("Fare")+
  ylab("Total Count")+
  ylim(0,50)+
  labs(fill="Survived")

str(data.combined$Cabin)

data.combined$Cabin <- as.character(data.combined$Cabin) 
data.combined$Cabin[1:100]

data.combined[which(data.combined$Cabin==""),"Cabin"]<- "U"
data.combined$Cabin[1:100]

cabin.first.char<- as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)
data.combined$cabin.first.char<-as.factor(cabin.first.char)

ggplot(data.combined[1:891,],aes(x=cabin.first.char ,fill=survived)) +
  geom_bar(width=1)+
  facet_wrap(~Pclass)+
  ggTitle("Survival based on Cabin.first.char")+
  xlab("Cabin.first.char")+
  ylab("Total Count")+
  ylim(0,500)+
  labs(fill="Survived")

ggplot(data.combined[1:891,],aes(x=cabin.first.char ,fill=survived)) +
  geom_bar(width=1)+
  facet_wrap(~Pclass+Title)+
  ggTitle("Survival based on Cabin.first.char")+
  xlab("Cabin.first.char")+
  ylab("Total Count")+
  ylim(0,500)+
  labs(fill="Survived")

data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin," "),"Y","N"))

ggplot(data.combined[1:891,],aes(x=Cabin.multiple ,fill=survived)) +
  geom_bar()+
  facet_wrap(~Pclass+Title)+
  ggTitle("PClass,Title")+
  xlab("Cabin.multiple")+
  ylab("Total Count")+
  ylim(0,350)+
  labs(fill="Survived")

str(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891,],aes(x=Embarked,fill=survived)) +
  geom_bar()+
  facet_wrap(~Pclass+Title)+
  ggTitle("PClass,Title")+
  xlab("Cabin.multiple")+
  ylab("Total Count")+
  ylim(0,350)+
  labs(fill="Survived")

# Train a Random Forest with the default parameters using pclass & Title

library(randomForest)

rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# Train a Random Forest using pclass, Title, & sibsp
rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)



# Train a Random Forest using pclass, Title, & parch
rf.train.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)



# Train a Random Forest using pclass, Title, sibsp, parch
rf.train.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)



# Train a Random Forest using pclass, Title, & family.size
rf.train.5 <- data.combined[1:891, c("Pclass", "Title", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)



# Train a Random Forest using pclass, Title, sibsp, & family.size
rf.train.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)



# Train a Random Forest using pclass, Title, parch, & family.size
rf.train.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

# Train a Random Forest using pclass, Title, parch, & family.size
rf.train.8 <- data.combined[1:891, c("Pclass", "Title", "SibSp","Parch", "family.size")]

set.seed(1234)
rf.8 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.8
varImpPlot(rf.8)

# Subset our test records and features
test.submit.df <- data.combined[892:1309, c("Pclass", "Title", "family.size")]

# Make predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20160215_1.csv", row.names = FALSE)

library(caret)
library(doSNOW)

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

# Check stratification
table(rf.label)
342 / 549

table(rf.label[cv.10.folds[[30]]])
308 / 494


# Set up caret's trainControl object per above.
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.folds)


# Set up doSNOW package for multi-core training. This is helpful as we're going
# to be training a lot of trees.
# NOTE - This works on Windows and Mac, unlike doMC
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)


# Set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.1


# The above is only slightly more pessimistic than the rf.5 OOB prediction, but 
# not pessimistic enough. Let's try 5-fold CV repeated 10 times.
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.2


# 5-fold CV isn't better. Move to 3-fold CV repeated 10 times. 
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = ctrl.3)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.3


# Install and load packages
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# let's use 3-fold CV repeated 10 times 

# Create utility function
rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, 
                    trControl = ctrl)
  
  #Shutdown cluster
  stopCluster(cl)
  
  return (rpart.cv)
}

# Grab features
features <- c("Pclass", "Title", "family.size")
rpart.train.1 <- data.combined[1:891, features]

# Run CV and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

# Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)






# Both rpart and rf confirm that Title is important, let's investigate further
table(data.combined$Title)

# Parse out last name and Title
data.combined[1:25, "Name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

# Add last names to dataframe in case we find it useful later
data.combined$last.name <- last.names

# Now for Titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
Titles <- sapply(name.splits, "[", 2)
unique(Titles)

# What's up with a Title of 'the'?
data.combined[which(Titles == "the"),]

# Re-map Titles to be more exact
Titles[Titles %in% c("Dona.", "the")] <- "Lady."
Titles[Titles %in% c("Ms.", "Mlle.")] <- "Miss."
Titles[Titles == "Mme."] <- "Mrs."
Titles[Titles %in% c("Jonkheer.", "Don.")] <- "Sir."
Titles[Titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(Titles)

# Make Title a factor
data.combined$new.Title <- as.factor(Titles)

# Visualize new version of Title
ggplot(data.combined[1:891,], aes(x = new.Title, fill = survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) + 
  ggTitle("Surival Rates for new.Title by Pclass")

# Collapse Titles based on visual analysis
indexes <- which(data.combined$new.Title == "Lady.")
data.combined$new.Title[indexes] <- "Mrs."

indexes <- which(data.combined$new.Title == "Dr." | 
                   data.combined$new.Title == "Rev." |
                   data.combined$new.Title == "Sir." |
                   data.combined$new.Title == "Officer")
data.combined$new.Title[indexes] <- "Mr."

# Visualize 
ggplot(data.combined[1:891,], aes(x = new.Title, fill = survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggTitle("Surival Rates for Collapsed new.Title by Pclass")


# Grab features
features <- c("Pclass", "new.Title", "family.size")
rpart.train.2 <- data.combined[1:891, features]

# Run CV and check out results
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

# Plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# Dive in on 1st class "Mr."
indexes.first.mr <- which(data.combined$new.Title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

# One female?
first.mr.df[first.mr.df$Sex == "female",]

# Update new.Title feature
indexes <- which(data.combined$new.Title == "Mr." & 
                   data.combined$Sex == "female")
data.combined$new.Title[indexes] <- "Mrs."

# Any other gender slip-ups?
length(which(data.combined$Sex == "female" & 
               (data.combined$new.Title == "Master." |
                  data.combined$new.Title == "Mr.")))

# Refresh data frame
indexes.first.mr <- which(data.combined$new.Title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]

# Let's look at surviving 1st class "Mr."
summary(first.mr.df[first.mr.df$survived == "1",])
View(first.mr.df[first.mr.df$survived == "1",])

# Take a look at some of the high fares
indexes <- which(data.combined$Ticket == "PC 17755" |
                   data.combined$Ticket == "PC 17611" |
                   data.combined$Ticket == "113760")
View(data.combined[indexes,])

# Visualize survival rates for 1st class "Mr." by fare
ggplot(first.mr.df, aes(x = Fare, fill = survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rates by fare")


# Engineer features based on all the passengers with the same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

# Refresh 1st class "Mr." dataframe
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)



# Visualize new features
ggplot(first.mr.df[first.mr.df$survived != "None",], aes(x = ticket.party.size, fill = survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by ticket.party.size")

ggplot(first.mr.df[first.mr.df$survived != "None",], aes(x = avg.fare, fill = survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by avg.fare")


# Hypothesis - ticket.party.size is highly correlated with avg.fare
summary(data.combined$avg.fare)

# One missing value, take a look
data.combined[is.na(data.combined$avg.fare), ]

# Get records for similar passengers and summarize avg.fares
indexes <- with(data.combined, which(Pclass == "3" & Title == "Mr." & family.size == 1 &
                                       Ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

# Use median since close to mean and a little higher than mean
data.combined[is.na(avg.fare), "avg.fare"] <- 7.840

# Leverage caret's preProcess function to normalize data
preproc.data.combined <- data.combined[, c("ticket.party.size", "avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

# Hypothesis refuted for all data
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

# How about for just 1st class all-up?
indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes], 
    postproc.data.combined$avg.fare[indexes])
# Hypothesis refuted again


# OK, let's see if our feature engineering has made any difference
features <- c("Pclass", "new.Title", "family.size", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]

# Run CV and check out results
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

# Plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

    
    
    
#
# Rpart scores 0.80383
#
# Subset our test records and features
test.submit.df <- data.combined[892:1309, features]

# Make predictions
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, file = "RPART_SUB_20160619_1.csv", row.names = FALSE)


#
# Random forest scores 0.80861
#
features <- c("Pclass", "new.Title", "ticket.party.size", "avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp


test.submit.df <- data.combined[892:1309, features]

# Make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "RF_SUB_20160619_1.csv", row.names = FALSE)



# Let's explore our collection of features using mutual information to
# gain some additional insight. 
    
install.packages("infotheo")
library(infotheo)

mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$SibSp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
mutinformation(rf.label, discretize(data.combined$Fare[1:891]))
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$Title[1:891])
mutinformation(rf.label, data.combined$family.size[1:891])
mutinformation(rf.label, data.combined$Ticket.first.char[1:891])
mutinformation(rf.label, data.combined$Cabin.multiple[1:891])
mutinformation(rf.label, data.combined$new.Title[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))


# OK, now let's leverage the tsne algorithm to create a 2-D representation of our data 
# suitable for visualization starting with folks our model gets right very often - folks
# with Titles other than 'Mr."
install.packages("Rtsne")
library(Rtsne)
most.correct <- data.combined[data.combined$new.Title != "Mr.",]
indexes <- which(most.correct$survived != "None")


# NOTE - Bug fix for original version. Rtsne needs a seed to ensure consistent
# output between runs.
set.seed(984357)
tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2], 
                 color = most.correct$survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.Title Other than 'Mr.'")


# To get a baseline, let's use conditional mutual information on the tsne X and
# Y features for females and boys in 1st and 2nd class. The intuition here is that
# the combination of these features should be higher than any individual feature
# we looked at above.
condinformation(most.correct$survived[indexes], discretize(tsne.1$Y[indexes,]))


# As one more comparison, we can leverage conditional mutual information using
# the top two features used in our tree plot - new.Title and Pclass
condinformation(rf.label, data.combined[1:891, c("new.Title", "Pclass")])


# OK, now let's take a look at adult males since our model has the biggest 
# potential upside for improving (i.e., the tree predicts incorrectly for 86
# adult males). Let's visualize with tsne.
misters <- data.combined[data.combined$new.Title == "Mr.",]
indexes <- which(misters$survived != "None")

tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2], 
                 color = misters$survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.Title of 'Mr.'")


# Now conditional mutual information for tsne features for adult males
condinformation(misters$survived[indexes], discretize(tsne.2$Y[indexes,]))


#
# Idea - How about creating tsne featues for all of the training data and
# using them in our model?
#
tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2], 
                 color = data.combined$survived[1:891])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for all Training Data")

# Now conditional mutual information for tsne features for all training
condinformation(data.combined$survived[1:891], discretize(tsne.3$Y[1:891,]))

# Add the tsne features to our data frame for use in model building
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]

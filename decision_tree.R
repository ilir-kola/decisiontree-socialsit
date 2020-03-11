library(rpart)
#load data
social_situations <- read.csv(choose.files())

#partition training and test set
smp_size <- floor(0.7 * nrow(social_situations))

#set seed to save partition
set.seed(123)
train_ind <- sample(seq_len(nrow(social_situations)), size = smp_size)
train <- social_situations[train_ind, ]
test <- social_situations[-train_ind, ]

#train tree
fit <- rpart(priority~., method="class", data = train, maxdepth=4)

#plot tree. based on the partition, trees can look different
rpart.plot(fit)
predictions <- predict(fit, social_situations[-train_ind,], type="class")
#make predictions
table <- as.matrix(table(factor(social_situations[-train_ind, "priority"], levels=1:5), factor(predictions, levels=1:5)))
n = sum(table)
diag = diag(table)
accuracy = sum(diag) / n 

smp_size <- floor(0.8 * nrow(social_situations))
#repeat 1000 times to calculate mean accuracy
i=0
overall_accuracy = 0
while(i<1000){
train_ind <- sample(seq_len(nrow(social_situations)), size = smp_size)
train <- social_situations[train_ind, ]
test <- social_situations[-train_ind, ]

#train tree
fit <- rpart(priority~., method="class", data = train, maxdepth=4)

#plot tree
#rpart.plot(fit)
predictions <- predict(fit, social_situations[-train_ind,], type="class")
#make predictions
table <- as.matrix(table(factor(social_situations[-train_ind, "priority"], levels=1:5), factor(predictions, levels=1:5)))
n = sum(table)
diag = diag(table)
accuracy = sum(diag) / n 
overall_accuracy = overall_accuracy + accuracy
i=i+1}
overall_accuracy/i
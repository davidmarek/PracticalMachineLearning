library(caret)
library(doMC)

registerDoMC(cores = 15)

# LOADING
testing <- read.csv('pml-testing.csv')
training <- read.csv('pml-training.csv')

# PREPROCESSING
testing <- testing[-c(1:7)]
training <- training[-c(1:7)]

all_factor_columns <- sapply(training, is.factor)
all_factor_columns['classe'] <- FALSE
testing <- testing[!all_factor_columns]
training <- training[!all_factor_columns]

testing <- testing[,!(colSums(is.na(testing)) > 0)]
training <- training[,!(colSums(is.na(training)) > 0)]

trainingCor <- cor(training[, 1:dim(training)[2] - 1])
highlyCorrelated <- findCorrelation(trainingCor)
training <- training[, -highlyCorrelated]
testing <- testing[, -highlyCorrelated]

# TRAINING
set.seed(2015)
trainIndex <- createDataPartition(training$classe, p = 0.7, list = FALSE)
trainData <- training[trainIndex,]
testData <- training[-trainIndex,]

seeds <- vector(mode = "list", length = 31)
for(i in 1:31) seeds[[i]] <- sample.int(1000, ncol(trainData))
seeds[[31]] <- sample.int(1000, 1)

train_control <- trainControl(method = 'cv', number = 10, repeats=3)
model <- train(classe ~ ., data = trainData, trControl = train_control, method = 'rf')
save(model, trainIndex, file = 'model.Rdata')

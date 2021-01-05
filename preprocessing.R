preprocessed.train <- train
remove(train)

for (col in names(preprocessed.train)) {
  
  if (!(col %in% c("area_percentage", "height_percentage"))) 
    preprocessed.train[,col] <- as.factor(preprocessed.train[,col])
}

ggplot(preprocessed.train, aes(x=height_percentage))+ geom_bar()

preprocessed.train[which(as.numeric(preprocessed.train$count_floors_pre_eq) > 4), "count_floors_pre_eq"] <- 5
preprocessed.train[which(as.numeric(preprocessed.train$age) > 110), "age"] <- 111
preprocessed.train[which(as.numeric(preprocessed.train$area_percentage) > 25 & as.numeric(preprocessed.train$area_percentage) <=50), "area_percentage"] <- 26
preprocessed.train[which(as.numeric(preprocessed.train$area_percentage) > 50), "area_percentage"] <- 27
preprocessed.train[which(as.numeric(preprocessed.train$height_percentage) > 15), "height_percentage"] <- 16

names(preprocessed.train)
columns <- c("has_secondary_use_agriculture", "has_secondary_use_hotel", "has_secondary_use_rental", 
  "has_secondary_use_institution", "has_secondary_use_school", "has_secondary_use_industry",
  "has_secondary_use_health_post", "has_secondary_use_gov_office", "has_secondary_use_use_police", 
  "has_secondary_use_other", "damage_grade", "building_id")
features <- c()
for (col in names(preprocessed.train))
  if (!(col %in% columns))
    features <- c(features, col)

features <- preprocessed.train[, features]
labels <- preprocessed.train[, "damage_grade"]

set.seed(567)
train_size <- ceiling(nrow(features)*0.7)
index <- sample(1:nrow(preprocessed.train), train_size, replace = TRUE)
features.train <- features[index, ]
labels.train <- labels[index]

library(party)
memory.limit(size=100000)
tree.model <- ctree(labels.train~., features.train)
size(preprocessed.train)






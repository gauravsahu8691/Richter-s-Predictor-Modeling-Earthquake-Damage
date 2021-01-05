X <- train$geo_level_1_id
y <- train$damage_grade
library(party)

model <- ctree(y ~ X)
plot(model)
table(test$damage_grade, predict(model, test$geo_level_1_id))

library(dplyr)
library(ggplot2)
features <- read.csv("train_values.csv")
labels <- read.csv("train_labels.csv")
train <- cbind(features, damage_grade=labels$damage_grade)
remove(features, labels)

# Response Variable
train$damage_grade <- as.factor(train$damage_grade)
prop.table(table(train$damage_grade))
ggplot(train, aes(x=damage_grade)) + geom_bar()

# Geo Level 1 Id
train$geo_level_1_id <- as.factor(train$geo_level_1_id)
ggplot(train, aes(x=geo_level_1_id)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~geo_level_1_id)


# Geo Level 2 Id
train$geo_level_2_id <- as.factor(train$geo_level_2_id)
length(unique(train$geo_level_2_id))

# Geo Level 3 Id
train$geo_level_3_id <- as.factor(train$geo_level_3_id)
length(unique(train$geo_level_3_id))

# Number of Floors
train$count_floors_pre_eq <- as.factor(train$count_floors_pre_eq)
ggplot(train, aes(x=count_floors_pre_eq)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~count_floors_pre_eq)
train %>% group_by(damage_grade) %>% summarise(mean(as.numeric(count_floors_pre_eq)))

# Age
ggplot(train, aes(x=age)) + geom_bar()
ggplot(train, aes(x=damage_grade, y=age, fill=damage_grade)) + geom_boxplot()
train %>% group_by(damage_grade) %>% summarise(mean(age, trim = 0.2))
train %>% group_by(geo_level_1_id) %>% summarise(mean(as.numeric(age))) %>% View()

# Area Percentage
ggplot(train, aes(x=area_percentage)) + geom_bar()
ggplot(train, aes(x=damage_grade, y=area_percentage, fill=damage_grade)) + geom_boxplot()
train %>% group_by(damage_grade) %>% summarise(mean(area_percentage), mean(area_percentage, trim = 0.2))

# Height Percentage
ggplot(train, aes(x=height_percentage)) + geom_bar()
ggplot(train, aes(x=damage_grade, y=height_percentage, fill=damage_grade)) + geom_boxplot()
train %>% group_by(damage_grade) %>% summarise(mean(height_percentage), mean(height_percentage, trim = 0.2))

# Land Surface Condition
train$land_surface_condition <- as.factor(train$land_surface_condition)
prop.table(table(train$land_surface_condition))
ggplot(train, aes(x=land_surface_condition)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~land_surface_condition)

contingency_table <- table(train$damage_grade, train$land_surface_condition)
contingency_table / rowSums(contingency_table)

# Foundation Type
train$foundation_type <- as.factor(train$foundation_type)
prop.table(table(train$foundation_type))
ggplot(train, aes(x=foundation_type)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~foundation_type)

contingency_table <- table(train$damage_grade, train$foundation_type)
contingency_table / rowSums(contingency_table)

# Roof Type
train$roof_type <- as.factor(train$roof_type)
prop.table(table(train$roof_type))
ggplot(train, aes(x=roof_type)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~roof_type)

contingency_table <- table(train$damage_grade, train$roof_type)
contingency_table / rowSums(contingency_table)

# Ground Floor Type
train$ground_floor_type <- as.factor(train$ground_floor_type)
prop.table(table(train$ground_floor_type))
ggplot(train, aes(x=ground_floor_type)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~ground_floor_type)

contingency_table <- table(train$damage_grade, train$ground_floor_type)
contingency_table / rowSums(contingency_table)

# Other Floor Type
train$other_floor_type <- as.factor(train$other_floor_type)
prop.table(table(train$other_floor_type))
ggplot(train, aes(x=other_floor_type)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~other_floor_type)

contingency_table <- table(train$damage_grade, train$other_floor_type)
contingency_table / rowSums(contingency_table)

# Position
train$position <- as.factor(train$position)
prop.table(table(train$position))
ggplot(train, aes(x=position)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~position)

contingency_table <- table(train$position, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Plan Configuration
train$plan_configuration <- as.factor(train$plan_configuration)
prop.table(table(train$plan_configuration))
ggplot(train, aes(x=plan_configuration)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~plan_configuration)

contingency_table <- table(train$plan_configuration, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Superstructure Adobe Mud
train$has_superstructure_adobe_mud <- as.factor(train$has_superstructure_adobe_mud)
prop.table(table(train$has_superstructure_adobe_mud))
ggplot(train, aes(x=has_superstructure_adobe_mud)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_superstructure_adobe_mud)

contingency_table <- table(train$has_superstructure_adobe_mud, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Superstructure Mud Mortar Stone
train$has_superstructure_mud_mortar_stone <- as.factor(train$has_superstructure_mud_mortar_stone)
prop.table(table(train$has_superstructure_mud_mortar_stone))
ggplot(train, aes(x=has_superstructure_mud_mortar_stone)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_superstructure_mud_mortar_stone)

contingency_table <- table(train$has_superstructure_mud_mortar_stone, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Superstructure Stone Flag
train$has_superstructure_stone_flag <- as.factor(train$has_superstructure_stone_flag)
prop.table(table(train$has_superstructure_stone_flag))
ggplot(train, aes(x=has_superstructure_stone_flag)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_superstructure_stone_flag)

contingency_table <- table(train$has_superstructure_stone_flag, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Superstructure Cement Mortar Stone
train$has_superstructure_cement_mortar_stone <- as.factor(train$has_superstructure_cement_mortar_stone)
prop.table(table(train$has_superstructure_cement_mortar_stone))
ggplot(train, aes(x=has_superstructure_cement_mortar_stone)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_superstructure_cement_mortar_stone)

contingency_table <- table(train$has_superstructure_cement_mortar_stone, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Superstructure Mud Mortar Bricks
train$has_superstructure_mud_mortar_brick <- as.factor(train$has_superstructure_mud_mortar_brick)
prop.table(table(train$has_superstructure_mud_mortar_brick))
ggplot(train, aes(x=has_superstructure_mud_mortar_brick)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_superstructure_mud_mortar_brick)

contingency_table <- table(train$has_superstructure_mud_mortar_stone, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Superstructure Cement Mortar Bricks
train$has_superstructure_cement_mortar_brick <- as.factor(train$has_superstructure_cement_mortar_brick)
prop.table(table(train$has_superstructure_cement_mortar_brick))
ggplot(train, aes(x=has_superstructure_cement_mortar_brick)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_superstructure_cement_mortar_brick)

contingency_table <- table(train$has_superstructure_mud_mortar_stone, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Superstructure Timber
train$has_superstructure_timber <- as.factor(train$has_superstructure_timber)
prop.table(table(train$has_superstructure_timber))
ggplot(train, aes(x=has_superstructure_timber)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_superstructure_timber)

contingency_table <- table(train$has_superstructure_timber, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Superstructure Bamboo
train$has_superstructure_bamboo <- as.factor(train$has_superstructure_bamboo)
prop.table(table(train$has_superstructure_bamboo))
ggplot(train, aes(x=has_superstructure_bamboo)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_superstructure_bamboo)

contingency_table <- table(train$has_superstructure_bamboo, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Superstructure RC Non Engineered
train$has_superstructure_rc_non_engineered <- as.factor(train$has_superstructure_rc_non_engineered)
prop.table(table(train$has_superstructure_rc_non_engineered))
ggplot(train, aes(x=has_superstructure_rc_non_engineered)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_superstructure_rc_non_engineered)

contingency_table <- table(train$has_superstructure_rc_non_engineered, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Superstructure RC Engineered
train$has_superstructure_rc_engineered <- as.factor(train$has_superstructure_rc_engineered)
prop.table(table(train$has_superstructure_rc_engineered))
ggplot(train, aes(x=has_superstructure_rc_engineered)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_superstructure_rc_engineered)

contingency_table <- table(train$has_superstructure_rc_engineered, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Superstructure Other
train$has_superstructure_other <- as.factor(train$has_superstructure_other)
prop.table(table(train$has_superstructure_other))
ggplot(train, aes(x=has_superstructure_other)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_superstructure_other)

contingency_table <- table(train$has_superstructure_other, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Legal Ownership Status
train$legal_ownership_status <- as.factor(train$legal_ownership_status)
prop.table(table(train$legal_ownership_status))
ggplot(train, aes(x=legal_ownership_status)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~legal_ownership_status)

contingency_table <- table(train$legal_ownership_status, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Count Families
train$count_families <- as.factor(train$count_families)
prop.table(table(train$count_families))
ggplot(train, aes(x=count_families)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~count_families)

contingency_table <- table(train$count_families, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Secondary Use
train$has_secondary_use <- as.factor(train$has_secondary_use)
prop.table(table(train$has_secondary_use))
ggplot(train, aes(x=has_secondary_use)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_secondary_use)

contingency_table <- table(train$has_secondary_use, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Secondary Use Agricultural
train$has_secondary_use_agriculture <- as.factor(train$has_secondary_use_agriculture)
prop.table(table(train$has_secondary_use_agriculture))
ggplot(train, aes(x=has_secondary_use_agriculture)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_secondary_use_agriculture)

contingency_table <- table(train$has_secondary_use_agriculture, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Secondary Use Hotel
train$has_secondary_use_hotel <- as.factor(train$has_secondary_use_hotel)
prop.table(table(train$has_secondary_use_hotel))
ggplot(train, aes(x=has_secondary_use_hotel)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_secondary_use_hotel)

contingency_table <- table(train$has_secondary_use_hotel, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Secondary Use Rental
train$has_secondary_use_rental <- as.factor(train$has_secondary_use_rental)
prop.table(table(train$has_secondary_use_rental))
ggplot(train, aes(x=has_secondary_use_rental)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_secondary_use_rental)

contingency_table <- table(train$has_secondary_use_rental, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Secondary Use Institution
train$has_secondary_use_institution <- as.factor(train$has_secondary_use_institution)
prop.table(table(train$has_secondary_use_institution))
ggplot(train, aes(x=has_secondary_use_institution)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_secondary_use_institution)

contingency_table <- table(train$has_secondary_use_institution, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Secondary Use School
train$has_secondary_use_school <- as.factor(train$has_secondary_use_school)
prop.table(table(train$has_secondary_use_school))
ggplot(train, aes(x=has_secondary_use_school)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_secondary_use_school)

contingency_table <- table(train$has_secondary_use_school, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Secondary Use Industry
train$has_secondary_use_industry <- as.factor(train$has_secondary_use_industry)
prop.table(table(train$has_secondary_use_industry))
ggplot(train, aes(x=has_secondary_use_industry)) + geom_bar()
ggplot(train, aes(x=damage_grade, fill=damage_grade)) + geom_bar() + facet_wrap(~has_secondary_use_industry)

contingency_table <- table(train$has_secondary_use_industry, train$damage_grade)
contingency_table / rowSums(contingency_table)

# Has Secondary Use Industry
train$has_secondary_use_health_post <- as.factor(train$has_secondary_use_health_post)
table(train$has_secondary_use_health_post)

# Has Secondary Use Govt Office
train$has_secondary_use_gov_office <- as.factor(train$has_secondary_use_gov_office)
table(train$has_secondary_use_gov_office)

# Has Secondary Use Police
train$has_secondary_use_use_police <- as.factor(train$has_secondary_use_use_police)
table(train$has_secondary_use_use_police)

# Has Secondary Use Other
train$has_secondary_use_other <- as.factor(train$has_secondary_use_other)
table(train$has_secondary_use_other)




































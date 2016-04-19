#!/usr/bin/env Rscript

# load packages
# library(znc0)
library(dplyr)

# create features
z_clicks <- sample_clicks %>%
    arrange(Session_ID, -as.integer(Timestamp)) %>%
    select(Session_ID, Timestamp) %>%
    group_by(Session_ID) %>%
    summarize(drtn = max(Timestamp) - min(Timestamp), count_pgvw = n())
# most viewed category of items?
# year, month, day (1 or 15), day of week, hour?
z_buys <- sample_buys %>%
    select(Session_ID, Quantity) %>%
    group_by(Session_ID) %>%
    summarize(ind_buy = sign(sum(Quantity)))

# join for data set
z_combined <- left_join(z_clicks, z_buys) %>%
    mutate(ind_buy = replace(ind_buy, list = is.na(ind_buy), values = 0))

# partition into train and test
sample_rate <- 0.70
train <- z_combined[1:floor(nrow(z_combined) * sample_rate), ]
test <- z_combined[nrow(train):nrow(z_combined), ]

# fit model
model <- glm(
    formula = ind_buy ~ drtn + count_pgvw,
    family = binomial(link = "logit"),
    data = train
)
# summary(model)
# pscl::pR2(model)

# accuracy
fitted_results <- predict(
    object = model,
    newdata = subset(test, select = c(2, 3)),
    type = "response"
)
fitted_results <- ifelse(fitted_results > 0.50, 1, 0)
misclassification_error <- mean(fitted_results != test$ind_buy)
1 - mean(as.integer(train$ind_buy)) # naive rule accuracy







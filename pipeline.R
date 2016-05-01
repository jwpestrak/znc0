#!/usr/bin/env Rscript

# load packages-----------------------------------------------------------------
library(znc0)
library(readr)
library(ROSE)
library(dplyr)

# load data---------------------------------------------------------------------
# (samples of clicks and buys are available by default)
# (not necessary to load all data into RAM just to test pipeline and functions)
# should we use only samples?
ind_sample <- FALSE
if (!ind_sample) {
    # load clickstream
    sample_clicks <- read_csv(
        # file = "/my/path/to/yoochoose-clicks.dat",
        file = "tmp/yoochoose-clicks.dat",
        col_names = c("Session_ID", "Timestamp", "Item_ID", "Category"),
        col_types = list(
            col_character(), #Session_ID
             col_datetime(), #Timestamp
            col_character(), #Item_ID
            col_character()  #Category
        )
    )
    # load buys
    sample_buys <- read_csv(
        # file = "/my/path/to/yoochoose-clicks.dat",
        file = "tmp/yoochoose-buys.dat",
        col_names = c("Session_ID", "Timestamp", "Item_ID", "Price", "Quantity"),
        col_types = list(
            col_character(), #Session_ID
             col_datetime(), #Timestamp
            col_character(), #Item_ID
              col_integer(), #Price
              col_integer()  #Quantity
        )
    )
}

# exploratory data analysis-----------------------------------------------------

# numerical summaries

# one-dimensional
sample_clicks %>% magrittr::extract2("Item_ID") %>% table(useNA = "always") %>% "/"(nrow(sample_clicks)) %>% sort(decreasing = TRUE) %>% head(n = 10)
sample_clicks %>% magrittr::extract2("Category") %>% table(useNA = "always") %>% "/"(nrow(sample_clicks)) %>% sort(decreasing = TRUE) %>% head(n = 10)
sample_clicks %>% group_by(Session_ID) %>% summarize(dcnt_item = n_distinct(Item_ID)) %>% ungroup() %>% magrittr::extract2("dcnt_item") %>% quantile(probs = seq(0, 1, 0.10))
sample_clicks %>% group_by(Session_ID) %>% summarize(dcnt_ctgy = n_distinct(Category)) %>% ungroup() %>% magrittr::extract2("dcnt_ctgy") %>% quantile(probs = seq(0, 1, 0.10))
sample_clicks %>% group_by(Session_ID) %>% summarize(drtn = max(Timestamp) - min(Timestamp)) %>% ungroup() %>% magrittr::extract2("drtn") %>% quantile(probs = seq(0, 1, 0.10))
sample_clicks %>% select(Category, Item_ID) %>% group_by(Category) %>% summarize(dcnt_item = n_distinct(Item_ID)) %>% ungroup() %>% arrange(-dcnt_item)

abc <- sample_clicks %>% group_by(Session_ID) %>% summarize(dcnt_item = n_distinct(Item_ID)) %>% ungroup(); c(quantile(abc$dcnt_item, 0.25), quantile(abc$dcnt_item, 0.75))
abc <- sample_clicks %>% group_by(Session_ID) %>% summarize(dcnt_item = n_distinct(Item_ID)) %>% ungroup(); c(quantile(abc$dcnt_item, 0.05), quantile(abc$dcnt_item, 0.95))
abc <- sample_clicks %>% group_by(Session_ID) %>% summarize(drtn = max(Timestamp) - min(Timestamp)) %>% ungroup(); c(quantile(abc$drtn, 0.25), quantile(abc$drtn, 0.75))
abc <- sample_clicks %>% group_by(Session_ID) %>% summarize(drtn = max(Timestamp) - min(Timestamp)) %>% ungroup(); c(quantile(abc$drtn, 0.05), quantile(abc$drtn, 0.95))
sample_clicks %>% group_by(Session_ID) %>% summarize(drtn = max(Timestamp) - min(Timestamp)) %>% ungroup() %>% magrittr::extract2("drtn") %>% sd()
# bounce rate?
ggplot2::ggplot(data = sample_clicks %>% group_by(Session_ID) %>% summarize(drtn = max(Timestamp) - min(Timestamp), dcnt_item = n_distinct(Item_ID)) %>% ungroup(), mapping = ggplot2::aes(x = drtn, y = dcnt_item)) + ggplot2::geom_point()

# two-dimensional
cor(as.numeric(z_clicks$drtn), z_clicks$count_pgvw)

# create features
z_clicks <- make_features(df_clk = sample_clicks)

# create target
z_target <- data_frame(
    Session_ID = unique(sample_buys$Session_ID),
    ind_buy    = TRUE
)

# join
z_combined <- left_join(
    x = z_clicks,
    y = z_target,
    by = "Session_ID"
) %>%
    mutate(
        ind_buy = replace(ind_buy, list = is.na(ind_buy), values = FALSE),
        ind_buy = as.factor(ind_buy),
        drtn = as.integer(drtn),
        mnth = factor(as.character(mnth), ordered = FALSE),
        dow = factor(as.character(dow), ordered = FALSE)
    )

# most viewed category of items?
# year, month, day (1 or 15), day of week, hour?

# partition into train and test
sample_rate <- 0.70
sample_idx <- sample(1:nrow(z_combined), floor(nrow(z_combined) * sample_rate))
train <- z_combined[sample_idx, ]
test <- z_combined[-sample_idx, ]

# fit model
fit <- glm(
    formula = ind_buy ~ mnth + dow + hr + drtn + cnt_wbpg + cnt_itm + cnt_ctgry,
    family = binomial(link = "logit"),
    data = train
)
# summary(fit)
# pscl::pR2(fit)

# accuracy
pred <- predict(
    object = fit,
    newdata = test,
    type = "response"
)
ROSE::roc.curve(
    response = test$ind_buy,
    predicted = pred
)

train_rose <- ROSE::ROSE(
    formula = ind_buy ~ mnth + dow + hr + drtn + cnt_wbpg + cnt_itm + cnt_ctgry,
    # formula = ind_buy ~ hr + drtn + cnt_wbpg + cnt_itm + cnt_ctgry,
    data = train,
    seed = 3
)$data
fit_rose <- glm(
    formula = ind_buy ~ mnth + dow + hr + drtn + cnt_wbpg + cnt_itm + cnt_ctgry,
    family = binomial(link = "logit"),
    data = train_rose
)
# summary(fit_rose)
# pscl::pR2(fit_rose)

# accuracy
pred_rose <- predict(
    object = fit_rose,
    newdata = test,
    type = "response"
)
ROSE::roc.curve(
    response = test$ind_buy,
    predicted = pred_rose
)



fitted_results <- ifelse(fitted_results > 0.50, 1, 0)
misclassification_error <- mean(fitted_results != test$ind_buy)
1 - mean(as.integer(train$ind_buy)) # naive rule accuracy







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
ind_sample <- TRUE
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

# Feature Generation -----------------------------------------------------------

# duration (secs) per session
df_dr <- sample_clicks %>%
    select(Session_ID, Timestamp) %>%
    group_by(Session_ID) %>%
    summarize(drtn = as.integer(max(Timestamp) - min(Timestamp))) %>%
    ungroup()

# count product click per session
df_pc <- sample_clicks %>%
    select(Session_ID) %>%
    group_by(Session_ID) %>%
    summarize(cnt_pc = n()) %>%
    ungroup()

# counts of each Category grouping
category_groups <- data_frame(
    Category = c("0", "S", as.character(1:12)),
    cgrp = c("x0", "xS", rep("xC", 12))
)
df_cg <- sample_clicks %>%
    select(Session_ID, Category) %>%
    left_join(
        y = category_groups,
        by = c("Category")
    ) %>%
    mutate(cgrp = replace(cgrp, is.na(cgrp), "xB")) %>% # NAs are brand-specific
    select(Session_ID, cgrp) %>%
    reshape2::dcast(
        formula = Session_ID ~ cgrp,
        fill = 0,
        drop = FALSE,
        fun.aggregate = length
    ) %>%
    tbl_df()

# join together
dta_inpt <- inner_join(
    x  = df_dr,
    y  = inner_join(
         x  = df_pc,
         y  = df_cg,
         by = "Session_ID"
    ),
    by = "Session_ID"
) %>%
    left_join(
        y = data_frame(Session_ID = unique(sample_buys$Session_ID), ind_buy = "Y"),
        by = "Session_ID"
    ) %>%
    mutate(
        ind_buy = replace(ind_buy, is.na(ind_buy), "N"),
        ind_buy = factor(ind_buy)
    )

# # z_clicks <- make_features(df_clk = sample_clicks)
#
# # create target
# z_target <- data_frame(
#     Session_ID = unique(sample_buys$Session_ID),
#     ind_buy    = TRUE
# )
#
# # join
# z_combined <- left_join(
#     x = z_clicks,
#     y = z_target,
#     by = "Session_ID"
# ) %>%
#     mutate(
#         ind_buy = replace(ind_buy, list = is.na(ind_buy), values = FALSE),
#         ind_buy = as.factor(ind_buy),
#         drtn = as.integer(drtn),
#         mnth = factor(as.character(mnth), ordered = FALSE),
#         dow = factor(as.character(dow), ordered = FALSE)
#     )
#
# # most viewed category of items?
# # year, month, day (1 or 15), day of week, hour?

# Partition into train and test-------------------------------------------------

# Let's split data based on cnt_pc
dta_inpt_gt2 <- dta_inpt %>% filter(cnt_pc > 2)
dta_inpt_lte2 <- dta_inpt %>% filter(cnt_pc <= 2)
# dta_inpt <- dta_inpt_gt2

sample_rate <- 0.70
sample_idx <- sample(1:nrow(dta_inpt), floor(nrow(dta_inpt) * sample_rate))
train <- dta_inpt[sample_idx, ]
test <- dta_inpt[-sample_idx, ]

# Fit classification models-----------------------------------------------------

# formula
frml <- formula(ind_buy ~ drtn + cnt_pc + x0 + xS + xC + xB)

# generate random over-sampling examples
train_rose <- ROSE(
    formula = frml,
    data = train
)$data

# fit model - recursive partitioning
fit_rprt <- rpart::rpart(
    formula = frml,
    method = "class",
    data = train_rose
)
save(fit_rprt, file = "inst/fit/fit_rprt.rda")
# fit model - logistic regression
fit_lreg <- glm(
    formula = frml,
    family = binomial(link = "logit"),
    data = train_rose
)
save(fit_lreg, file = "inst/fit/fit_lreg.rda")
# pscl::pR2(fit)

# accuracy
pred_rprt <- predict(
    object = fit_rprt,
    newdata = test,
    type = "class"
)
roc.curve(
    response = test$ind_buy,
    predicted = pred_rprt,
    main = "ROC curve - recursive partitioning"
)

pred_lreg <- predict(
    object = fit_lreg,
    newdata = test,
    type = "response"
)
roc.curve(
    response = test$ind_buy,
    predicted = pred_lreg,
    main = "ROC curve - logistic regression"
)

# Generate predictions for solution test set

# feature generation for solution clicks
# duration (secs) per session
df_dr_solution <- solution_clicks %>%
    select(Session_ID, Timestamp) %>%
    group_by(Session_ID) %>%
    summarize(drtn = as.integer(max(Timestamp) - min(Timestamp))) %>%
    ungroup()

# count product click per session
df_pc_solution <- solution_clicks %>%
    select(Session_ID) %>%
    group_by(Session_ID) %>%
    summarize(cnt_pc = n()) %>%
    ungroup()

# counts of each Category grouping
df_cg_solution <- solution_clicks %>%
    select(Session_ID, Category) %>%
    left_join(
        y = category_groups,
        by = c("Category")
    ) %>%
    mutate(cgrp = replace(cgrp, is.na(cgrp), "xB")) %>% # NAs are brand-specific
    select(Session_ID, cgrp) %>%
    reshape2::dcast(
        formula = Session_ID ~ cgrp,
        fill = 0,
        drop = FALSE,
        fun.aggregate = length
    ) %>%
    tbl_df()

# join together
dta_inpt_solution <- inner_join(
    x  = df_dr_solution,
    y  = inner_join(
         x  = df_pc_solution,
         y  = df_cg_solution,
         by = "Session_ID"
    ),
    by = "Session_ID"
) %>%
    left_join(
        y = data_frame(Session_ID = unique(solution_buys$Session_ID), ind_buy = "Y"),
        by = "Session_ID"
    ) %>%
    mutate(
        ind_buy = replace(ind_buy, is.na(ind_buy), "N"),
        ind_buy = factor(ind_buy)
    )

# apply estimated (by logistic regression) model
solution_pred_lreg <- predict(
    object = fit_lreg,
    newdata = dta_inpt_solution,
    type = "response"
)
roc.curve(
    response = dta_inpt_solution$ind_buy,
    predicted = solution_pred_lreg,
    main = "ROC curve - logistic regression"
)
# apply estimated (by recursive partition) model
solution_pred_rprt <- predict(
    object = fit_rprt,
    newdata = dta_inpt_solution,
    type = "class"
)
roc.curve(
    response = dta_inpt_solution$ind_buy,
    predicted = solution_pred_rprt,
    main = "ROC curve - recursive partition"
)







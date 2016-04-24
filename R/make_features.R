#'
#'
make_features <- function(df_clk) {

    # Extract features based upon timestamp
    x <- df_clk %>%
        dplyr::select(Session_ID, Timestamp) %>%
        dplyr::mutate(
            yr   = lubridate::year(Timestamp),
            mnth = lubridate::month(Timestamp, label = TRUE),
            dow  = lubridate::wday(Timestamp, label = TRUE),
            hr   = lubridate::hour(Timestamp)
        ) %>%
        dplyr::arrange(Session_ID, Timestamp) %>%
        dplyr::select(-Timestamp) %>%
        dplyr::group_by(Session_ID) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()

    # Extract duration based upon timestamp
    y <- df_clk %>%
        dplyr::select(Session_ID, Timestamp) %>%
        dplyr::group_by(Session_ID) %>%
        dplyr::summarize(drtn = max(Timestamp) - min(Timestamp)) %>%
        dplyr::ungroup()

    # Extract counts
    z <- df_clk %>%
        dplyr::group_by(Session_ID) %>%
        summarize(
            cnt_wbpg  = n_distinct(Timestamp),
            cnt_itm   = n_distinct(Item_ID),
            cnt_ctgry = n_distinct(Category)
        ) %>%
        dplyr::ungroup()

    w <- Reduce(f = union, x = list(x$Session_ID, y$Session_ID, z$Session_ID))
    if (length(w) != nrow(x)) {warning("Not all Session IDs may have been included!")}

    return(Reduce(function(x, y) {dplyr::inner_join(x, y, by = "Session_ID")}, list(x, y, z)))
}

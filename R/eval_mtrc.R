#'
eval_mtrc <- function(tssn, prds, acts) {

    # cardinality of S (all sessions in test set)
    card_S <- length(unique(tssn))

    # cardinality of Sb (sessions in test set which end in buy)
    card_Sb <- length(unique(prds$Session_ID))

    # (in)correct buy predictions (binary classification [bc])
    score_bc_pos <- length(intersect(prds$Session_ID, acts$Session_ID)) / card_S
    score_bc_neg <- length(setdiff(prds$Session_ID, acts$Session_ID)) / card_S
    score_bc <- score_bc_pos + score_bc_neg

    # Jaccard indices
    score_jc_pos <- rep(NA_real_, card_Sb)
    for (i in 1:card_Sb) {
        score_jc_pos[i] <- sets::set_similarity(
            x = sets::as.set(prds$Items[i]),
            y = sets::as.set(acts$Items[i]),
            method = "Jaccard"
        )
    }

    #return total score
    (score_bc + sum(score_jc_pos))
}

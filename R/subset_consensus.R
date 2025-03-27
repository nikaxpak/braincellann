# R/subset_consensus.R

#' Identify Consensus Cell Types and Calculate Scores with Feature Importance
#'
#' @param df A data frame containing cell type annotations across multiple columns.
#' @param feature_importances A named vector containing feature importances (e.g., 'Bretigea_1_Microglia' -> 0.105).
#' @return A data frame with consensus cell types and their scaled scores.
#'
#' @noRd
subset_consensus <- function(df, weights_path = "inst/extdata/weights_normalized.csv") {
  # Read the CSV with both raw and normalized weights
  # Assumes columns: feature, importance, normalized_importance (and optionally cell_type)
  weights_df <- read.csv(weights_path, stringsAsFactors = FALSE)

  # Convert columns to numeric (in case they're read as characters)
  weights_df$importance <- as.numeric(weights_df$importance)
  weights_df$normalized_importance <- as.numeric(weights_df$normalized_importance)

  # Create two named vectors: one for raw weights, one for normalized weights
  feature_importances_raw  <- setNames(weights_df$importance, weights_df$feature)
  feature_importances_norm <- setNames(weights_df$normalized_importance, weights_df$feature)

  # Helper function to compute consensus and score for one row
  #  - Uses raw weights to determine which cell type is best
  #  - Uses normalized weights to compute the final consensus score
  find_consensus_and_score <- function(row) {
    # Exclude the first two columns (assumed metadata)
    predictions <- row[-(1:2)]
    group_labels <- names(predictions)

    # Split predictions into groups of 2
    # (assuming each pair is one database/measurement)
    groups <- split(predictions, ceiling(seq_along(predictions) / 2))

    # Identify the unique predictions per group
    group_appearances <- lapply(groups, unique)
    cell_type_counts <- table(unlist(group_appearances))

    # Special case: if Bretigea_1 predicts "Oligodendrocyte" or "OPC"
    if ("Bretigea_1" %in% names(predictions) &&
        predictions["Bretigea_1"] %in% c("Oligodendrocyte", "OPC")) {
      selected_type <- predictions["Bretigea_1"]

      # Check all other features that match Bretigea_1's prediction
      other_features <- predictions[names(predictions) != "Bretigea_1" & predictions == selected_type]
      if (length(other_features) > 0 && all(other_features == selected_type)) {

        # RAW score for picking the consensus
        base_feat <- paste0("Bretigea_1_", selected_type)
        raw_score <- ifelse(base_feat %in% names(feature_importances_raw),
                            feature_importances_raw[base_feat], 0)
        raw_score <- raw_score + sum(sapply(names(other_features), function(feat) {
          feat_name <- paste0(feat, "_", selected_type)
          if (feat_name %in% names(feature_importances_raw)) feature_importances_raw[feat_name] else 0
        }))

        # NORMALIZED score for final consensus score
        norm_score <- ifelse(base_feat %in% names(feature_importances_norm),
                             feature_importances_norm[base_feat], 0)
        norm_score <- norm_score + sum(sapply(names(other_features), function(feat) {
          feat_name <- paste0(feat, "_", selected_type)
          if (feat_name %in% names(feature_importances_norm)) feature_importances_norm[feat_name] else 0
        }))

        return(c(Consensus = selected_type, Score = norm_score))
      }
    }

    # Standard consensus: consider candidates that appear in at least 2 groups
    consensus_candidates <- names(cell_type_counts[cell_type_counts >= 2])
    if (length(consensus_candidates) > 0) {
      # 1) Use raw weights to pick the best candidate
      candidate_raw_scores <- sapply(consensus_candidates, function(candidate) {
        s <- 0
        for (i in seq_along(groups)) {
          for (j in seq_along(groups[[i]])) {
            current_label <- group_labels[(i - 1) * 2 + j]
            feat_name <- paste0(current_label, "_", candidate)
            if (feat_name %in% names(feature_importances_raw)) {
              s <- s + feature_importances_raw[feat_name]
            }
          }
        }
        return(s)
      })
      best_candidate <- consensus_candidates[which.max(candidate_raw_scores)]

      # 2) Once best candidate is chosen, sum *normalized* weights only for columns
      #    that actually predicted best_candidate in this row
      best_candidate_norm_score <- 0
      for (i in seq_along(groups)) {
        group <- groups[[i]]
        for (j in seq_along(group)) {
          # Only add to score if the row's actual prediction == best_candidate
          if (!is.na(group[j]) && group[j] == best_candidate) {
            current_label <- group_labels[(i - 1) * 2 + j]
            feat_name <- paste0(current_label, "_", best_candidate)
            if (feat_name %in% names(feature_importances_norm)) {
              best_candidate_norm_score <- best_candidate_norm_score + feature_importances_norm[feat_name]
            }
          }
        }
      }
      return(c(Consensus = best_candidate, Score = best_candidate_norm_score))
    } else {
      # No candidate with >=2 group support
      return(c(Consensus = NA, Score = NA))
    }}

  # Apply the helper function to each row
  consensus_info <- t(apply(df, 1, find_consensus_and_score))
  consensus_df <- as.data.frame(consensus_info, stringsAsFactors = FALSE)
  names(consensus_df) <- c("Consensus", "Score")
  consensus_df$Score <- as.numeric(consensus_df$Score)

  # Combine with original data and remove rows with no consensus
  result_df <- cbind(df, consensus_df)
  result_df <- result_df[!is.na(result_df$Consensus), ]
  return(result_df)
}


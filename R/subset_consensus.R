# R/subset_consensus.R

#' Identify Consensus Cell Types and Calculate Scores with Feature Importance
#'
#' @param df A data frame containing cell type annotations across multiple columns.
#' @param feature_importances A named vector containing feature importances (e.g., 'Bretigea_1_Microglia' -> 0.105).
#' @return A data frame with consensus cell types and their scaled scores.
#'
#' @noRd

subset_consensus <- function(df) {

  feature_importances <- c(
    'Bretigea_1_Microglia' = 0.10534423060378194,
    'Bretigea_1_Astrocyte' = 0.10119023795445715,
    'Bretigea_1_Oligodendrocyte' = 0.059798286,
    'Bretigea_1_Endothelial' = 0.054660831,
    'PanglaoDB_1_Endothelial' = 0.047365168964057014,
    'cell_type_1_Neuron' = 0.043544331,
    'CellMarker_1_Astrocyte' = 0.040819327,
    'CellMarker_1_Oligodendrocyte' = 0.032590162,
    'cell_type_2_Neuron' = 0.030488439,
    'HumanProteinAtlas_1_Neuron' = 0.028815387,
    'cell_type_1_Microglia' = 0.027871681,
    'PanglaoDB_2_OPC' = 0.026462154085044928,
    'cell_type_2_Microglia' = 0.026314308,
    'cell_type_1_OPC' = 0.025928987135901093,
    'HumanProteinAtlas_1_Endothelial' = 0.025586259192442343,
    'cell_type_1_Astrocyte' = 0.019222877525104674,
    'CellMarker_1_Microglia' = 0.019123490648607258,
    'PanglaoDB_1_B cell' = 0.019085656039053916,
    'HumanProteinAtlas_2_Neuron' = 0.018856194876306007,
    'cell_type_2_Oligodendrocyte' = 0.018431641,
    'Bretigea_1_Neuron' = 0.017137698981102876,
    'cell_type_2_OPC' = 0.016683335,
    'CellMarker_1_Endothelial' = 0.014364052570275444,
    'CellMarker_1_Neuron' = 0.014273394387421773,
    'CellMarker_2_Microglia' = 0.013491839737368677,
    'CellMarker_1_OPC' = 0.011992972750455808,
    'PanglaoDB_2_Endothelial' = 0.011762679464457223,
    'PanglaoDB_2_B cell' = 0.011568765110012005,
    'cell_type_2_Astrocyte' = 0.011105882635877118,
    'PanglaoDB_1_Neuron' = 0.010954553874500284,
    'HumanProteinAtlas_2_Endothelial' = 0.010610705103308946,
    'PanglaoDB_1_Oligodendrocyte' = 0.010486710491576994,
    'CellMarker_1_NK cell' = 0.008881335,
    'Bretigea_1_OPC' = 0.007240003,
    'PanglaoDB_1_OPC' = 0.006993589,
    'cell_type_1_Oligodendrocyte' = 0.006961914,
    'Bretigea_2_Oligodendrocyte' = 0.006744688,
    'CellMarker_1_B cell' = 0.005097431,
    'CellMarker_2_Neuron' = 0.004304881,
    'CellMarker_2_Astrocyte' = 0.004237117,
    'PanglaoDB_2_Neuron' = 0.003980856,
    'CellMarker_2_Endothelial' = 0.003552217,
    'PanglaoDB_1_T cell' = 0.003407766,
    'PanglaoDB_1_Microglia' = 0.003225728,
    'CellMarker_2_OPC' = 0.003034732,
    'CellMarker_2_NK cell' = 0.00209988,
    'CellMarker_2_B cell' = 0.00163962,
    'Bretigea_2_OPC' = 0.001509208,
    'CellMarker_1_T cell' = 0.001156792,
    'PanglaoDB_2_Oligodendrocyte' = 0
  )

  # Function to calculate candidate scores and find consensus cell type
  find_consensus_and_score <- function(row) {
    cell_types <- row[-(1:2)]
    groups <- split(cell_types, ceiling(seq_along(cell_types) / 2))

    group_labels <- names(row[-(1:2)])
    group_appearances <- sapply(groups, function(group) unique(group))
    cell_type_counts <- table(unlist(group_appearances))

    # Check if Bretigea_1 shows 'Oligodendrocyte' or 'OPC'
    if ('Bretigea_1' %in% names(cell_types) &&
        cell_types['Bretigea_1'] %in% c('Oligodendrocyte', 'OPC')) {

      # Temporary variable to store the cell type from Bretigea_1
      cell_type <- cell_types['Bretigea_1']

      # Identify all other features that match the Bretigea_1 cell type
      other_features <- cell_types[names(cell_types) != 'Bretigea_1' &
                                     cell_types %in% cell_type]

      if (length(other_features) > 0) {
        # Check if all other relevant features match with Bretigea_1's cell type
        if (all(other_features == cell_type)) {
          # Calculate score based on Bretigea_1 and agreeing features
          score <- feature_importances[paste0('Bretigea_1_', cell_type)]

          # Add scores of other groups that match the Bretigea_1 cell type
          score <- score + sum(sapply(names(other_features), function(ct) {
            feature_name <- paste0(ct, "_", cell_types[ct])
            return(ifelse(feature_name %in% names(feature_importances),
                          feature_importances[feature_name], 0))
          }))

          return(c(Consensus = cell_type, Score = score))
        }
      }
    }

    # If the above condition is not met, proceed with standard consensus logic
    consensus_candidates <- names(cell_type_counts[cell_type_counts >= 2])

    if (length(consensus_candidates) > 0) {
      candidate_scores <- sapply(consensus_candidates, function(candidate) {
        score <- 0

        for (i in seq_along(groups)) {
          group <- groups[[i]]
          for (cell_type in group) {
            # Construct the feature name
            feature_name <- paste0(group_labels[(i - 1) * 2 + 1], "_", cell_type)

            # Add feature importance if the feature exists
            if (feature_name %in% names(feature_importances)) {
              importance <- feature_importances[feature_name]
              score <- score + importance
            }
          }
        }
        return(score)
      })

      consensus_cell_type <- consensus_candidates[which.max(candidate_scores)]
      consensus_score <- max(candidate_scores)

      return(c(Consensus = consensus_cell_type, Score = consensus_score))
    } else {
      return(c(Consensus = NA, Score = NA))
    }
  }

  consensus_info <- t(apply(df, 1, find_consensus_and_score))

  consensus_df <- as.data.frame(consensus_info, stringsAsFactors = FALSE)
  names(consensus_df) <- c("Consensus", "Score")
  consensus_df$Score <- as.numeric(consensus_df$Score)

  # Normalize the scores
  min_score <- min(consensus_df$Score, na.rm = TRUE)
  max_score <- max(consensus_df$Score, na.rm = TRUE)
  consensus_df$Score <- (consensus_df$Score - min_score) / (max_score - min_score)

  consensus_df$Score <- round(consensus_df$Score, 2)

  result_df <- cbind(df, consensus_df)
  result_df <- result_df[!is.na(result_df$Consensus), ]

  return(result_df)
}

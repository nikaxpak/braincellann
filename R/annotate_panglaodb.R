# R/annotate_panglaodb.R

#' Annotate Gene Markers with PanglaoDB Database
#'
#' This function annotates a list of gene markers using the PanglaoDB database.
#'
#' @param df A data frame containing gene markers and an index column.
#' @return A data frame with PanglaoDB annotations, including columns PanglaoDB_1 and PanglaoDB_2.
#'
#' @noRd
annotate_panglaodb <- function(df) {
  # Load PanglaoDB data using get_resource (assumes this function is defined elsewhere)
  data.frame <- get_resource("PanglaoDB")

  panglaoDB <- data.frame %>%
    filter(organ %in% c("Brain", "Immune system", "Vasculature"),
           canonical_marker == 'TRUE',
           human == 'TRUE')

  panglaoDB <- panglaoDB %>%
    rename(markers = genesymbol)

  merged_df <- merge(df, panglaoDB, by = "markers", all.x = TRUE)
  merged_df <- merged_df[order(merged_df$index), ]

  subset <- merged_df %>%
    select(markers, index, cell_type)

  combined_df <- subset %>%
    group_by(index) %>%
    summarise(markers = paste(unique(markers), collapse = ", "),
              cell_type = paste(unique(cell_type), collapse = ", ")) %>%
    separate(cell_type, into = paste0("PanglaoDB_", 1:2), sep = ",")

  annotated_panglaodb <- combined_df[, -which(names(combined_df) == "index")]
  return(annotated_panglaodb)
}

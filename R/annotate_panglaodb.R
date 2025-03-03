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
  path <- system.file("extdata", "PanglaoDB_markers_27_Mar_2020.tsv", package = "braincellann")
  data.frame <- read.delim(path, header = TRUE, sep = "\t")

  panglaoDB <- data.frame %>%
    filter(organ %in% c("Brain", "Immune system", "Vasculature"),
           canonical.marker == '1')

  panglaoDB <- panglaoDB %>%
    rename(markers = official.gene.symbol)

  merged_df <- merge(df, panglaoDB, by = "markers", all.x = TRUE)
  merged_df <- merged_df[order(merged_df$index), ]

  subset <- merged_df %>%
    select(markers, index, cell.type)

  combined_df <- subset %>%
    group_by(index) %>%
    summarise(markers = paste(unique(markers), collapse = ", "),
              cell.type = paste(unique(cell.type), collapse = ", ")) %>%
    separate(cell.type, into = paste0("PanglaoDB_", 1:2), sep = ",")

  annotated_panglaodb <- combined_df[, -which(names(combined_df) == "index")]
  return(annotated_panglaodb)
}

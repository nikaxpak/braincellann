# R/annotate_bretigea.R

#' Annotate Gene Markers with BRETIGEA Database
#'
#' This function annotates a list of gene markers using the BRETIGEA database.
#'
#' @param df A data frame containing gene markers and an index column. Must include a column named \code{markers}.
#'
#' @return A data frame with BRETIGEA annotations, including columns \code{Bretigea_1} and \code{Bretigea_2}.
#'
#' @noRd
annotate_bretigea <- function(df) {
  # Load BRETIGEA data from package
  bretigea_file <- system.file("extdata", "markers_df_human_brain.rda", package = "braincellann")

  if (bretigea_file == "") {
    stop("BRETIGEA data file not found in the package.")
  }

  load(bretigea_file)  # Loads 'markers_df_human_brain'

  if (!exists("markers_df_human_brain")) {
    stop("BRETIGEA data not found in the .rda file.")
  }

  bretigea_db <- markers_df_human_brain

  # Merge and annotate
  merged_df <- merge(df, markers_df_human_brain, by = "markers", all.x = TRUE)

  annotated_bretigea <- merged_df[order(merged_df$index), ]

  annotated_bretigea <- annotated_bretigea %>%
    group_by(index) %>%
    summarise(markers = paste(unique(markers), collapse = ", "),
              cell = paste(unique(cell), collapse = ", ")) %>%
    separate(cell, into = paste0("Bretigea_", 1:2), sep = ",")

  return(annotated_bretigea)
}

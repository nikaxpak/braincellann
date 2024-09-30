# R/annotate_cellmarker.R

#' Annotate Gene Markers with CellMarker 2.0 Database
#'
#' This function annotates a list of gene markers using the CellMarker 2.0 database.
#'
#' @param df A data frame containing gene markers and an index column.
#' @return A data frame with CellMarker annotations, including columns CellMarker_1 and CellMarker_2.
#'
#' @noRd
annotate_cellmarker <- function(df) {
  # Load CellMarker data from package
  cellmarker_file <- system.file("extdata", "Cell_marker_Human.xlsx", package = "braincellann")

  if (cellmarker_file == "") {
    stop("CellMarker data file not found in the package.")
  }

  cellmarker_db <- readxl::read_excel(cellmarker_file)

  cellmarker_filtered <- cellmarker_db %>%
    dplyr::filter(tissue_class == "Brain") %>%
    dplyr::rename(markers = marker)

  # Merge and annotate
  cellmarker <- cellmarker_db %>%
    filter(tissue_class == "Brain")

  cellmarker <- cellmarker %>%
    rename(markers = marker)

  merged_df <- merge(df, cellmarker, by = "markers", all.x = TRUE)
  merged_df <- merged_df[order(merged_df$index), ]

  subset <- merged_df %>%
    select(markers, index, cell_name)

  combined_df <- subset %>%
    group_by(index) %>%
    summarise(markers = paste(unique(markers), collapse = ", "),
              cell_name = paste(unique(cell_name), collapse = ", ")) %>%
    separate(cell_name, into = paste0("CellMarker_", 1:2), sep = ",")

  annotated_cellmarker <- combined_df[, -which(names(combined_df) == "index")]

  return(annotated_cellmarker)
}


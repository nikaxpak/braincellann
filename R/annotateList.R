# R/annotateList.R

#' Annotate a List of Gene Markers with Multiple Databases
#'
#' This function takes a list of gene markers and annotates them using BRETIGEA,
#' CellMarker 2.0, PanglaoDB, and Human Protein Atlas databases.
#'
#' @param input_list A list of gene markers.
#' @return A data frame containing the annotated gene markers with consensus cell types.
#' @export
annotateList <- function(input_list) {
  # Input validation
  if (!is.list(input_list)) {
    stop("input_list must be a list.")
  }

  # Convert list to data frame
  df <- data.frame(markers = input_list, stringsAsFactors = FALSE)
  colnames(df) <- c("index", "markers")

  # Perform individual annotations
  annotated_bretigea <- annotate_bretigea(df)
  annotated_cellmarker <- annotate_cellmarker(df)
  annotated_panglaodb <- annotate_panglaodb(df)

  # HPA annotations return a list
  hpa_annotations <- annotate_hpa(df)
  annotated_hpa <- hpa_annotations$annotated_hpa
  annotated_hpa2 <- hpa_annotations$annotated_hpa2

  # Combine all annotations using left_join on 'index'
  annotated_df <- cbind(annotated_bretigea, annotated_cellmarker, annotated_panglaodb, annotated_hpa, annotated_hpa2)
  duplicated_cols <- duplicated(names(annotated_df))
  annotated_df <- annotated_df[!duplicated_cols]

  # Rename cell types to standard labels
  annotated_df <- rename_cell_types(annotated_df)

  # Define list of interests
  interests <- c("Neuron", "Endothelial", "Astrocyte",
                 "Microglia", "Oligodendrocyte", "OPC")

  # Subset where at least two columns point to the same value
  at_least_two_same <- function(row) {
    any(duplicated(row[row %in% interests]))
  }

  # Apply the function to each row of the dataframe
  subset_rows <- apply(annotated_df, 1, at_least_two_same)

  # Subset the dataframe based on rows where at least two columns have the same value of interest
  subset_df <- annotated_df[subset_rows, ]

  subset_df <- subset_df %>%
    mutate(across(everything(), ~na_if(., 'NA')))
  # Function to identify consensus cell type and calculate consensus score
  final_df <- subset_consensus(subset_df)

  return(final_df)
}

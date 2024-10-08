# R/annotate_hpa.R

#' Annotate Gene Markers with Human Protein Atlas Database
#'
#' This function annotates a list of gene markers using the Human Protein Atlas (HPA) database.
#'
#' @param df A data frame containing gene markers and an index column.
#' @return A list
#'
#' @noRd
#' @keywords internal
annotate_hpa <- function(df) {
  # First HPA Annotation
  hpa.df <- hpaDownload(downloadList = 'Normal tissue')

  tissueList <- c('cerebral cortex', 'cerebellum', 'hypothalamus', 'substantia nigra')
  levelList <- c('Medium', 'High', 'Ascending')
  reliability <- c('Approved', 'Enhanced', 'Supported')

  subset <- hpaSubset(data=hpa.df,
                      targetGene=df[,1],
                      targetTissue=tissueList)

  hpa <- data.frame(
    markers = subset[["normal_tissue"]][["gene"]],
    tissue = subset[["normal_tissue"]][["tissue"]],
    cell_type = subset[["normal_tissue"]][["cell_type"]],
    level = subset[["normal_tissue"]][["level"]],
    reliability = subset[["normal_tissue"]][["reliability"]]
  )

  hpa <- hpa %>%
    filter(level %in% c('Medium', 'High', 'Ascending', 'Low'),
           reliability %in% c('Approved', 'Enhanced', 'Supported'))

  merged_df <- merge(df, hpa, by = "markers", all.x = TRUE)

  merged_df <- merged_df[order(merged_df$index), ]

  subset <- merged_df %>%
    select(markers, index, cell_type)

  combined_df <- subset %>%
    group_by(index) %>%
    summarise(markers = paste(unique(markers), collapse = ", "),
              cell_type = paste(unique(cell_type), collapse = ", ")) %>%
    separate(cell_type, into = paste0("HumanProteinAtlas_", 1:2), sep = ",")

  annotated_hpa <- combined_df[, -which(names(combined_df) == "index")]

  # Annotate with Human Protein Atlas 2
  # Change the timeout to 120 seconds
  original_timeout <- getOption("timeout")
  options(timeout = 300)

  hpa.df2 <- hpaDownload(downloadList = "RNA single cell type tissue cluster")

  subset <- hpaSubset(data=hpa.df2,
                      targetGene=df[,1],
                      targetTissue="brain")

  hpa <- data.frame(
    markers = subset[["rna_single_cell_type_tissue"]][["gene"]],
    cell_type = subset[["rna_single_cell_type_tissue"]][["cell_type"]],
    ntpm = subset[["rna_single_cell_type_tissue"]][["ptpm"]]
  )

  # Restore original timeout
  options(timeout = original_timeout)

  merged_df <- merge(df, hpa, by = "markers", all.x = TRUE)

  merged_df <- merged_df[order(merged_df$index), ]

  # Step 1: Arrange and then deduplicate by keeping the highest-ranked occurrence of each cell_type within each index
  prepared_df <- merged_df %>%
    arrange(index, desc(ntpm)) %>%
    group_by(index, cell_type) %>%
    filter(row_number() == 1) %>%
    ungroup()

  # Step 2: Now, rank within each 'index' without worrying about duplicates
  prepared_df <- prepared_df %>%
    group_by(index) %>%
    mutate(rank = row_number()) %>%
    filter(rank <= 2) %>%
    ungroup()

  # Step 3: Create a unique identifier for pivoting, as before
  prepared_df <- prepared_df %>%
    mutate(cell_type_id = paste0("cell_type_", rank)) %>%
    select(-rank, -ntpm) # Proceeding without 'ntpm'

  # Step 4: Pivot wider to format the dataframe
  annotated_hpa2 <- prepared_df %>%
    pivot_wider(id_cols = c(index, markers),
                names_from = cell_type_id,
                values_from = cell_type,
                values_fill = list(cell_type = NA))

  return(list(annotated_hpa = annotated_hpa, annotated_hpa2 = annotated_hpa2))
}

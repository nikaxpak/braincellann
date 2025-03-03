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
  # Load HPA Annotation Data
  hpa_path <- system.file("extdata", "normal_ihc_data.tsv", package = "braincellann")
  hpa.df <- read.delim(hpa_path, header = TRUE, sep = "\t")

  tissueList <- c('Cerebral cortex', 'Cerebellum', 'Hypothalamus', 'Substantia nigra')
  levelList <- c('Medium', 'High', 'Ascending')
  reliability <- c('Approved', 'Enhanced', 'Supported')

  # Filter HPA data based on tissue, level, and reliability
  subset <- hpa.df %>%
    filter(Tissue %in% tissueList,
           Level %in% levelList,
           Reliability %in% reliability,
           Gene.name %in% df[[2]])

  hpa <- subset %>%
    select(Gene.name, Tissue, Cell.type, Level, Reliability) %>%
    rename(markers = Gene.name, cell_type = Cell.type)

  merged_df <- merge(df, hpa, by = "markers", all.x = TRUE)
  merged_df <- merged_df[order(merged_df$index), ]

  # Summarize to get the combined DataFrame
  combined_df <- merged_df %>%
    mutate(Level = factor(Level, levels = c("High", "Medium", "Ascending"))) %>%
    group_by(index) %>%
    arrange(Level, .by_group = TRUE) %>%
    slice_head(n = 2) %>%
    summarise(
      markers = paste(unique(markers), collapse = ", "),
      cell_type = paste(unique(cell_type), collapse = ", ")
    ) %>%
    separate(cell_type, into = paste0("HumanProteinAtlas.IHC_", 1:2), sep = ", ")

  # Ensure all columns are character type before applying na_if
  annotated_hpa <- combined_df %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~na_if(., "NA")))

  # Load RNA Single Cell Cluster Data
  # URL of the file
  file_url <- "https://www.proteinatlas.org/download/tsv/rna_single_cell_cluster.tsv.zip"

  # Temporary file paths
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempdir()

  # Download the file
  download.file(file_url, destfile = temp_zip, mode = "wb")

  # Unzip the file
  unzip(temp_zip, exdir = temp_dir)

  # Verify the contents of the zip file
  zip_contents <- unzip(temp_zip, list = TRUE)
  print(zip_contents)

  # Find the correct file name
  tsv_file_name <- zip_contents$Name[grep("\\.tsv$", zip_contents$Name)]
  tsv_file <- file.path(temp_dir, tsv_file_name)

  hpa.df2 <- read.table(tsv_file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

  # Standardize column names for the second dataset
  colnames(hpa.df2) <- trimws(colnames(hpa.df2))

  # Adjust column selection based on available columns
  hpa2 <- hpa.df2 %>%
    filter(Tissue == "brain") %>%
    select(Gene.name, Cell.type, nTPM) %>%
    rename(markers = Gene.name)

  # Merge with the original dataframe
  merged_df2 <- merge(df, hpa2, by.x = "markers", all.x = TRUE)
  merged_df2 <- merged_df2[order(merged_df2$index), ]

  # Step 1: Group by `markers` (genes) and rank by `nTPM` within each gene
  prepared_df <- merged_df2 %>%
    arrange(markers, desc(nTPM)) %>%  # Sort by gene and nTPM (descending)
    group_by(markers) %>%             # Group by gene
    mutate(rank = row_number()) %>%   # Rank rows within each gene
    filter(rank <= 2) %>%            # Keep top 2 rows per gene
    ungroup()

  # Step 2: Create a unique identifier for pivoting
  prepared_df <- prepared_df %>%
    mutate(cell_type_id = paste0("HumanProteinAtlas.Sc_", rank)) %>%
    select(-rank, -nTPM) # Remove rank and nTPM columns

  # Step 3: Pivot wider to format the dataframe
  annotated_hpa2 <- prepared_df %>%
    pivot_wider(id_cols = c(index, markers),
                names_from = cell_type_id,
                values_from = Cell.type,
                values_fill = list(Cell.type = NA))

  # Order both annotated_hpa and annotated_hpa2 by `index` in ascending order
  annotated_hpa <- annotated_hpa %>%
    arrange(index)

  annotated_hpa2 <- annotated_hpa2 %>%
    arrange(index)

  return(list(annotated_hpa = annotated_hpa, annotated_hpa2 = annotated_hpa2))
}

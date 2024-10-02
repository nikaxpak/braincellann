# R/rename_cell_types.R

#' Rename Cell Types to Standardize Labels
#'
#' This function standardizes cell type labels across multiple annotation columns.
#'
#' @param df A data frame containing cell type annotations across multiple columns.
#' @return A data frame with standardized cell type labels.
#'
#' @noRd
rename_cell_types <- function(df) {
  # Define cell type lists
  neurons <- c("neu", "Neurons", " Neurons", "Neuron", " Neuron",
               "Inhibitory neuron", " Inhibitory neuron", "Adrenergic neurons",
               "Cortical somatostatin (SST) interneuron", " Cortical somatostatin (SST) interneuron",
               "Sensory neuron", " Sensory neuron", "Somatostatin interneuron",
               "Motor neuron", "Young Neuron", "Immature neurons", " Immature neurons",
               "Dopaminergic neurons", "Parvalbumin interneuron",
               "Glutamatergic neuron", " Glutaminergic neurons",
               "Excitatory neuron", " Excitatory neuron", "excitatory neurons",
               "inhibitory neurons", "neuronal projections", " neuronal projections",
               "Neuroendocrine cells", "Neuroblast", " Neuroblasts",
               "Pyramidal cells", "Purkinje neurons", "GABAergic neurons", " GABAergic neurons",
               "Interneurons", " Interneurons", "Cholinergic neurons", " Cholinergic neurons",
               " Cholinergic neuron", " von Economo neuron(VEN)", " Serotonergic neurons",
               "Trigeminal neurons", " Lake et al.Science.In1", "Lake et al.Science.In3",
               "Lake et al.Science.In4", " Lake et al.Science.In4", "Lake et al.Science.In5",
               " Lake et al.Science.In5", "Lake et al.Science.In6", "Lake et al.Science.In8",
               "Lake et al.Science.Ex1", " Lake et al.Science.Ex1", "Lake et al.Science.Ex2",
               "Lake et al.Science.Ex3", "Lake et al.Science.Ex4", " Lake et al.Science.Ex6",
               "Lake et al.Science.Ex8", "Purkinje cells - nucleus",
               " Purkinje cells - dendrites"," Purkinje cells - dendrites",
               "neuronal cells", " neuronal cells", "Purkinje cells", " Purkinje cells",
               "Purkinje cells - cytoplasm/membrane", " Purkinje cells - cytoplasm/membrane",
               "neuronal projections")

  endothelial <- c("end", "Endothelial cell", " Endothelial cell",
                   "Endothelial cells", " Endothelial cells",
                   "endothelial cells", " endothelial cells")

  astrocytes <- c("ast", "astrocytes", "Astrocyte", " Astrocyte",
                  "Astrocytes", " Astrocytes", "A2 astrocyte",
                  "A1 astrocyte", " A1 astrocyte",
                  "Reactive astrocyte", " Reactive astrocyte", "Mature Astrocyte")

  microglia <- c("mic", " mic", "microglial cells",
                 "Microglia", " Microglia",
                 "Microglial cell", " Microglial cell",
                 "M1 microglial cell", " M1 microglial cell",
                 "Homeostatic microglial cell", " Homeostatic microglial cell",
                 " Disease-associated microglial cell", "Macrophage", "Macrophages",
                 "Microglia-derived tumor-associated macrophage(Mg-TAM)")

  oligodendrocytes <- c("oli", " oli", "Mature oligodendrocyte", " Mature oligodendrocyte",
                        "oligodendrocytes", "Oligodendrocyte‐like cell",
                        "Oligodendrocyte", " Oligodendrocyte",
                        "Oligodendrocytes", " Oligodendrocytes",
                        "Immune oligodendroglial cell(imOLG)")

  opc <- c("opc", " opc", "oligodendrocyte precursor cells",
           "Oligodendrocyte precursor cell", " Oligodendrocyte precursor cell",
           "Oligodendrocyte progenitor cell", " Oligodendrocyte progenitor cell",
           "Oligodendrocyte progenitor cells", " Oligodendrocyte progenitor cells")


  # Function to rename cell types
  rename_if_in_list <- function(x) {
    x <- trimws(x)  # Remove leading and trailing spaces
    dplyr::case_when(
      x %in% neurons ~ "Neuron",
      x %in% endothelial ~ "Endothelial",
      x %in% astrocytes ~ "Astrocyte",
      x %in% microglia ~ "Microglia",
      x %in% oligodendrocytes ~ "Oligodendrocyte",
      x %in% opc ~ "OPC",
      TRUE ~ x  # Return x if it doesn't match any condition
    )
  }

  # Apply renaming to all relevant columns
  df_renamed <- df %>%
    dplyr::mutate(across(dplyr::everything(), ~rename_if_in_list(.)))

  # Filter out rows that do not contain the specified cell types
  valid_cell_types <- c("Neuron", "Endothelial", "Astrocyte", "Microglia", "Oligodendrocyte", "OPC")

  # Replace any values not in the valid cell types with NA
  df_filtered <- df_renamed %>%
    dplyr::mutate(across(-c(index, markers), ~ifelse(. %in% valid_cell_types, ., NA)))

  # Ensure 'index' and 'markers' are retained
  if (!"index" %in% names(df_filtered)) {
    stop("'index' column is missing after renaming cell types.")
  }
  if (!"markers" %in% names(df_filtered)) {
    stop("'markers' column is missing after renaming cell types.")
  }

  return(df_filtered)
}

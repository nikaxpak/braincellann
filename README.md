# braincellann

**braincellann** or **BrainCellAnnotate** is an R package designed for annotating gene markers specific to cell types in the human brain using comprehensive public databases, including the Human Protein Atlas (HPA), CellMarker, PanglaoDB, and BRETIGEA.
The package presents data for comparison in an accessible and convenient way by simplifying literature search.

## Features
* **Gene Marker Annotation:** Eeasily compare cell labels across multiple curated reference databases and reach consensus.
* **Consensus Calculation:** Identify consensus cell types from multiple sources and calculate a confidence score based on the frequency of appearances and database-specific weights.
* **Tissue-specific and species-specific:** Markers and cell types are specific for human brain tissue.

## Installation
To install braincellann, use the following command:

```r
# Install devtools if you don't have it
install.packages("devtools")

# Install braincellann from GitHub
devtools::install_github("your-username/braincellann")

```
## Example

```r
library(braincellann)

gene_list <- c("VIP", "PVALB", "APOE", "CSF1", "IL34")

# Convert the list of genes into a data frame with an index column (if needed)
# This mimics how the package expects input for annotation functions
df_genes <- data.frame(
  index = seq_along(gene_list),
  gene_marker = gene_list
)

# Annotate gene markers
annotated_data <- annotateList(df_genes)


```

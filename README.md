<div align="center">
    <img src="(https://github.com/nikaxpak/braincellann/blob/main/braincellann.png)" alt="Alt text" width="800"/>
</div>

# braincellann

**braincellann** or **BrainCellAnnotate** is an R package designed for annotating gene markers specific to cell types in the human brain using comprehensive public databases, including the Human Protein Atlas (HPA), CellMarker, PanglaoDB, and BRETIGEA.
The package presents data for comparison in an accessible and convenient way by simplifying literature search.

## Features
* **Gene Marker Annotation:** Easily compare cell labels across multiple curated reference databases and reach consensus annotation.
* **Consensus Calculation:** Identify consensus cell types from multiple sources and calculate a confidence score based on the frequency of appearances and database-specific weights.
* **Database-specific Weights:** Some databases predict certain cell types more accurately than others. Weights for specific values from these databases were assigned using feature importance weights from Random Forest to match cell type annotations from the [Allen Institute's ABC Whole-Brain Human Atlas](https://portal.brain-map.org/atlases-and-data/bkp/abc-atlas).
* **Tissue-specific and Species-specific:** Markers and cell types are specific to human brain tissue.

!!! Annotation with immunohistochemistry and single-cell data from the Human Protein Atlas is temporarily unavailable due to changes made to the data on their website. It will be fixed by November 10.

## Installation
To install braincellann, use the following command:

```r
# Install devtools and BiocManager if you don't have them
install.packages("devtools")
install.packages("BiocManager")

# Install additional required packages
BiocManager::install("HPAanalyze")
BiocManager::install("OmnipathR")

# Install braincellann from GitHub
devtools::install_github("nikaxpak/braincellann")

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

## 16S Stewardship Impact
This repository contains the code and data supporting our manuscript:  
**_[“Implementing portable, real-time 16S rRNA sequencing in the healthcare sector enhances antimicrobial stewardship.”](https://doi.org/10.1101/2024.09.23.24314079)_**

The code (implemented in R) includes a complete workflow, from metadata wrangling to visualisation of clinical microbiology and microbiome sequencing data.

## Repository structure

- `/data/raw/`: Raw input data  
- `/data/processed/`: Cleaned and derived datasets  
- `/figures/`: Exported figures used in the manuscript  
- `/scripts/`: Data wrangling and visualization scripts  

## How to reproduce

```r
# From R console
source("scripts/Carlisle_et_al_16S_pipeline.R")

📚 R Packages

The following R packages must be installed:

install.packages(c(
  "dplyr", 
  "tidyverse", 
  "ggplot2", 
  "gtable", 
  "ggridges", 
  "viridis", 
  "svglite",
  "patchwork",
  "pals"
))

remotes::install_github("vankesteren/firatheme")
remotes::install_github("jthomasmock/gtExtras")

# hrbrthemes (for extra themes and fonts)
install.packages("hrbrthemes")
```
## 📖 Citation

If you use this work or adapt any part of the workflow, please cite our paper:
https://doi.org/10.1101/2024.09.23.24314079

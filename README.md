# SerialFM
## Repository details

This repository contains the code used to collect data and generate analyses for the following paper:

[Dimsdale-Zucker, H. R., Flegal, K. E., Atkins, A. S., & Reuter-Lorenz, P. A. (2018). Serial position-dependent false memory effects. Memory. doi: https://doi.org/10.1080/09658211.2018.1513039](https://s3.amazonaws.com/hrz-website/papers/dimsdale-zucker_reuter-lorenz_2018.pdf)

## Setup
1. Copy `config.yml.example` to `config.yml` and edit fields so that correct for current computer.
1. Some of the custom R functions can be downloaded [here](https://github.com/hallez/halle). 

## Getting the data
Email Halle Dimsdale-Zucker (hallez@umich.edu)

## Running the scripts
1. These scripts are setup as an R package. Thus, they assume that you have first opened `serialfm.Rproj`. This should open RStudio.
1. Data were initially analyzed in SPSS, but all analyses for the publication were done in R. To convert from SPSS to CSV, `convert_spss_to_csv.R`
  * If starting from the CSV file, can skip. 
1. To rerun the analyses and statistics, `analyze_data.R`
1. To regenerate graphs, use `graphs.R`. This pulls from existing Excel sheets from the original manual data scoring pipeline.

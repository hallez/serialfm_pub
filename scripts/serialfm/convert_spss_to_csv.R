#' ---
#' title: "Load data"
#' author: "Halle R. Dimsdale-Zucker"
#' output:
#'  html_document:
#'    toc: true
#'    number_sections: true
#'    theme: spacelab
#' ---

#' # Setup stuff
# Load all of the functions in the /R folder
# Also ensures all packages listed as "imports" are installed
devtools::load_all()

#' # Load in config file
# use packge::function syntax to avoid confusion
project_dir <- ("../../")
config <- yaml::yaml.load_file(paste0(project_dir,"config.yml"))

#' # Setup paths
raw_behavioral_dir <- paste0(project_dir,halle::ensure_trailing_slash(config$directories$raw_behavioral))
analyzed_behavioral_dir <- paste0(project_dir,halle::ensure_trailing_slash(config$directories$analyzed_behavioral))

dir.create(raw_behavioral_dir)

#' # Load in data
fname <- "SerialFMANDSerialFM.M_072312.sav"
fpath <- paste0(halle::ensure_trailing_slash(config$directories$dropbox),
                halle::ensure_trailing_slash("data"),
                halle::ensure_trailing_slash("HRZ_Sternword"),
                fname)

if(file.exists(fpath)){
  #+ warning = FALSE
  # NB: will return a warning because file was saved out w/ version 18
  # but package was created in 2000 when SPSS was on version 7
  spss_data <- haven::read_sav(fpath)

  fname_out <- paste0(halle::ensure_trailing_slash(raw_behavioral_dir),
                      "group_data.csv")

  write.csv(spss_data, fname_out, row.names = FALSE)
}


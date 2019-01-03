#' ---
#' title: Graph data
#' author: "Halle R. Dimsdale-Zucker"
#' output:
#'  html_document:
#'    toc: true
#'    toc_depth: 5
#'    toc_float:
#'      collapsed: false
#'      smooth_scroll: false
#'    number_sections: true
#'    theme: spacelab
#' ---

#+ initialize, warning = FALSE, message = FALSE
# Load all of the functions in the /R folder
# Also ensures all packages listed as "imports" are installed
devtools::load_all()
# add dplyr with library()
# NB: this is non-standard
# correct way would be to make this script into a function,
# store in the R/ directory,
# and use @importFrom dplyr "%>%",
# but this is incompatible with getting in-line results with code in knitr output
library(dplyr)

#' # Load in config file
# use packge::function syntax to avoid confusion
project_dir <- ("../../")
config <- yaml::yaml.load_file(paste0(project_dir,"config.yml"))

#' # Flags
SAVE_FLAG <- 1

#' # Setup paths
raw_behavioral_dir <- paste0(project_dir,halle::ensure_trailing_slash(config$directories$raw_behavioral))
analyzed_behavioral_dir <- paste0(project_dir,halle::ensure_trailing_slash(config$directories$analyzed_behavioral))
dropbox_dir <- halle::ensure_trailing_slash(config$directories$dropbox)
dropbox_graphs_dir <- paste0(dropbox_dir, halle::ensure_trailing_slash("writeups"), halle::ensure_trailing_slash("serialfm"), halle::ensure_trailing_slash("graphs"))

#' # Bar graphs
# proportion scored so that each bar sums to zero
# correct, semFM, phonoFM, misc (?)

#' # Serial position curves (Figure 2)
# presentation position (x) by number of items recalled (y)
#' ## Load data
#' ### SerialFM
fname_serial_position_expt1 <- paste0(dropbox_dir,
                                      halle::ensure_trailing_slash("data"),
                                      halle::ensure_trailing_slash("HRZ_Sternword"),
                                      halle::ensure_trailing_slash("SerialFM"),
                                      halle::ensure_trailing_slash("SerialFM_analysis"),
                                      "SerialFM_SPC.xls")

expt1_spc <- gdata::read.xls(fname_serial_position_expt1,
                         sheet = "Sheet1",
                         stringsAsFactors = FALSE)

# add a column with the experiment label
# this will allow both experiments to be merged later
expt1_spc <-
  expt1_spc %>%
  dplyr::mutate(experiment = "SerialFM")

#' ### SerialFM_M
fname_serial_position_expt2 <- paste0(dropbox_dir,
                                      halle::ensure_trailing_slash("data"),
                                      halle::ensure_trailing_slash("HRZ_Sternword"),
                                      halle::ensure_trailing_slash("SerialFM_M"),
                                      halle::ensure_trailing_slash("SerialFM_M_Analysis"),
                                      "SerialFM_M_SPC.xls")

expt2_spc <- gdata::read.xls(fname_serial_position_expt2,
                             sheet = "Sheet1",
                             stringsAsFactors = FALSE)

# add a column with the experiment label
# this will allow both experiments to be merged later
expt2_spc <-
  expt2_spc %>%
  dplyr::mutate(experiment = "SerialFM_M")

#' ### Merge the two experiments
# this way, only have to go through tidying, etc. once
spc <- NULL

spc <- dplyr::full_join(expt1_spc, expt2_spc,
                              by = intersect(names(expt1_spc), names(expt2_spc)))

#' ## Tidy up
spc_tidy <- NULL

spc_tidy <-
  spc %>%
  # remove average rows
  # they're not labelled as such,
  # but they don't have any subject ID
  # and that's how you can spot them
  dplyr::filter(!is.na(Subject.Number)) %>%
  # rename
  dplyr::rename(subject_number = Subject.Number) %>%
  # have one column for position
  # and another column for mean number of responses in that position
  tidyr::gather(position, number_of_responses, -subject_number, -experiment) %>%
  # convert mean number of responses to probability of recall 0-100%
  # luckily, everyone had 42 trials
  dplyr::mutate(num_resp_divided_by_num_trials = number_of_responses/42) %>%
  # remove "position" label in `position` column
  dplyr::mutate(no_position_lbl = stringr::str_replace_all(position, "Position(.*?)", "\\1")) %>%
  # to control what order the positions are graphed as, make into a factor
  dplyr::mutate(position_num = as.factor(no_position_lbl)) %>%
  # set up levels so in correct order
  dplyr::mutate(position_num_fac = factor(position_num, levels=c(1:12))) %>%
  # eliminate unnecessary and redundant columns
  dplyr::select(-position, -no_position_lbl, -position_num) %>%
  dplyr::rename(position = position_num_fac)


#' ## Graph serial position curves
#' ### SerialFM
spc_tidy %>%
  dplyr::filter(experiment == "SerialFM") %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(mean_responses = mean(num_resp_divided_by_num_trials, na.rm = TRUE)) %>%
  graph_serial_position_curves()

if(SAVE_FLAG){
  ggplot2::ggsave(file = paste0(dropbox_graphs_dir, 'spc_serial_fm.pdf'),
                  width = 10,
                  height = 7)
}

#' ### SerialFM_M
spc_tidy %>%
  dplyr::filter(experiment == "SerialFM_M") %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(mean_responses = mean(num_resp_divided_by_num_trials, na.rm = TRUE)) %>%
  graph_serial_position_curves()

if(SAVE_FLAG){
  ggplot2::ggsave(file = paste0(dropbox_graphs_dir, 'spc_serial_fm_m.pdf'),
                  width = 10,
                  height = 7)
}


#' # Number of responses, by output position
# for ouput positions 1-12,
# have 3 bars at each (A, B, C)
# where the ABC bars are each broken down by correct and semFM
# could include phonoFM (but so small may not be able to see)
#' ## Load data
#' ### SerialFM
fname_by_output_expt1 <- paste0(dropbox_dir,
                          halle::ensure_trailing_slash("data"),
                          halle::ensure_trailing_slash("HRZ_Sternword"),
                          halle::ensure_trailing_slash("SerialFM"),
                          "RecallRespBYPosition_SerialFM.xls")

expt1_correct <- gdata::read.xls(fname_by_output_expt1,
                                        sheet = "CorrectABC_By#RespPerTrial",
                                 stringsAsFactors = FALSE)
expt1_semfm <- gdata::read.xls(fname_by_output_expt1,
                               sheet = "SemLureABC_By#RespPerTrial",
                               stringsAsFactors = FALSE)
expt1_phonofm <- gdata::read.xls(fname_by_output_expt1,
                                 sheet = "PhonoABC_By#RespPerTrial",
                                 stringsAsFactors = FALSE)
expt1_misc <- gdata::read.xls(fname_by_output_expt1,
                              sheet = "MiscErrors_By#RespPerTrial",
                              stringsAsFactors = FALSE)

# put all of these sheets into a single data frame
expt1_by_output <- NULL
expt1_by_output <- merge(expt1_correct, expt1_semfm, by = "Subject.Number")
expt1_by_output <- merge(expt1_by_output, expt1_phonofm, by = "Subject.Number")
expt1_by_output <- merge(expt1_by_output, expt1_misc, by = "Subject.Number")

# add a column with the experiment label
# this will allow both experiments to be merged later
expt1_by_output <-
  expt1_by_output %>%
  dplyr::mutate(experiment = "SerialFM")

#' ### SerialFM_M
fname_by_output_expt2 <- paste0(dropbox_dir,
                                halle::ensure_trailing_slash("data"),
                                halle::ensure_trailing_slash("HRZ_Sternword"),
                                halle::ensure_trailing_slash("SerialFM_M"),
                                "RecallRespBYPosition_SerialFMM.xls")

expt2_correct <- gdata::read.xls(fname_by_output_expt2,
                                 sheet = "CorrectABC_By#RespPerTrial",
                                 stringsAsFactors = FALSE)
expt2_semfm <- gdata::read.xls(fname_by_output_expt2,
                               sheet = "LureSemABC_By#RespPerTrial",
                               stringsAsFactors = FALSE)
expt2_phonofm <- gdata::read.xls(fname_by_output_expt2,
                                 sheet = "PhonoABC_By#RespPerTrial",
                                 stringsAsFactors = FALSE)
expt2_misc <- gdata::read.xls(fname_by_output_expt2,
                              sheet = "MiscErrors_By#RespPerTrial",
                              stringsAsFactors = FALSE)

# put all of these sheets into a single data frame
expt2_by_output <- NULL
expt2_by_output <- merge(expt2_correct, expt2_semfm, by = "Subject.Number")
expt2_by_output <- merge(expt2_by_output, expt2_phonofm, by = "Subject.Number")
expt2_by_output <- merge(expt2_by_output, expt2_misc, by = "Subject.Number")

# add a column with the experiment label
# this will allow both experiments to be merged later
expt2_by_output <-
  expt2_by_output %>%
  dplyr::mutate(experiment = "SerialFM_M")

#' ### Merge the two experiments
# this way, only have to go through tidying, etc. once
by_output <- NULL

# this will throw a warning
# it just means that the different datasets had different values
# no data is lost
# http://stackoverflow.com/questions/30468412/dplyr-join-warning-joining-factors-with-different-levels
by_output <- dplyr::full_join(expt1_by_output, expt2_by_output,
                              by = intersect(names(expt1_by_output), names(expt2_by_output)))

#' ## Tidy data
#' ### Split up by output position
by_output_tidy <-
  by_output %>%
  # remove "AVERAGE" subject
  dplyr::filter(Subject.Number != "AVERAGE") %>%
  # rename columns
  dplyr::rename(subject_number = Subject.Number,
                CorrectA_1 = CorrectA_Output1,
                CorrectB_1 = CorrectB_Output1,
                CorrectC_1 = CorrectC_Output1,
                SemLureA_1 = SemLureA_Output1,
                SemLureB_1 = SemLureB_Output1,
                SemLureC_1 = SemLureC_Output1,
                PhonoA_1 = PhonoA_Output1,
                PhonoB_1 = PhonoB_Output1,
                PhonoC_1 = PhonoC_Output1,
                PhonoA_3 = PHonoA_3,
                PhonoB_3 = PHonoB_3,
                PhonoC_3 = PHonoC_3,
                Misc_1 = Misc_Output1) %>%
  # remove extraneous columns
  dplyr::select(-starts_with("Avg"),
                -contains("ABC")) %>%
  # split output position into its own column
  # based on: http://stackoverflow.com/questions/30592094/r-spreading-multiple-columns-with-tidyr
  tidyr::gather(variable, value, -subject_number, -experiment) %>%
  tidyr::separate(variable, into = c("response_type", "output_position")) %>%
  # separate sublist into its own column
  # from http://stackoverflow.com/questions/28027577/split-camelcase-column-names
  # this will throw an error ("Too few values") b/c misc errors don't have a sublist
  # but it's ok b/c this gets represented as `NA`
  tidyr::separate(response_type, into = c("response_type", "sublist"), sep = "(?<=.)(?=[A-C])") %>%
  # convert output_position from character to numeric
  dplyr::mutate(output_position = as.numeric(output_position))

head(by_output_tidy)


#' ### Collapse output position
by_sublist_tidy <-
  by_output %>%
  # remove "AVERAGE" subject
  dplyr::filter(Subject.Number != "AVERAGE") %>%
  # remove misc responses b/c these can't be divvied up by sublist
  dplyr::select(-contains("Misc_")) %>%
  # rename columns
  dplyr::rename(subject_number = Subject.Number,
                CorrectA_1 = CorrectA_Output1,
                CorrectB_1 = CorrectB_Output1,
                CorrectC_1 = CorrectC_Output1,
                SemLureA_1 = SemLureA_Output1,
                SemLureB_1 = SemLureB_Output1,
                SemLureC_1 = SemLureC_Output1,
                PhonoA_1 = PhonoA_Output1,
                PhonoB_1 = PhonoB_Output1,
                PhonoC_1 = PhonoC_Output1,
                PhonoA_3 = PHonoA_3,
                PhonoB_3 = PHonoB_3,
                PhonoC_3 = PHonoC_3) %>%
  # remove extraneous columns
  dplyr::select(-starts_with("Avg"),
                -contains("ABC")) %>%
  # split output position into its own column
  tidyr::gather(variable, value, -subject_number, -experiment) %>%
  tidyr::separate(variable, into = c("response_type", "output_position")) %>%
  # separate sublist into its own column
  tidyr::separate(response_type, into = c("response_type", "sublist"), sep = "(?<=.)(?=[A-C])") %>%
  # eliminate position information and average across sublist
  dplyr::select(-output_position) %>%
  dplyr::group_by(sublist, response_type, experiment) %>%
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE),
                   sd_value = sd(value, na.rm = TRUE),
                   num_subj = n_distinct(subject_number),
                   sem_value = sd_value/sqrt(num_subj),
                   upper = mean_value + sem_value,
                   lower = mean_value - sem_value) %>%
  dplyr::ungroup() %>%
  # re-order responses
  dplyr::mutate(response_type = factor(response_type, levels = c("Correct", "SemLure", "Phono")))

head(by_sublist_tidy)

#' ## Graph data split by output position
#' ### SerialFM
by_output_tidy %>%
  # let's just look at the first 12 responses
  dplyr::filter(output_position < 13,
                experiment == "SerialFM",
                response_type %in% c("Correct", "SemLure")) %>%
  dplyr::group_by(sublist, response_type, output_position) %>%
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  graph_stacked_by_output_position()

if(SAVE_FLAG){
  ggplot2::ggsave(file = paste0(dropbox_graphs_dir, 'abc_stacked_serial_fm.pdf'),
                  width = 10,
                  height = 6)
}

# Figure 3a
by_output_tidy %>%
  # let's just look at the first 12 responses
  dplyr::filter(output_position < 13,
                experiment == "SerialFM",
                response_type %in% c("Correct", "SemLure", "Misc")) %>%
  dplyr::group_by(sublist, response_type, output_position) %>%
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  graph_stacked_by_output_position_w_misc()

if(SAVE_FLAG){
  ggplot2::ggsave(file = paste0(dropbox_graphs_dir, 'abc_stacked_serial_fm_w_misc.pdf'),
                  width = 10,
                  height = 6)
}

# ensure that proportions sum to one if plot just one subject
# 80% should be list C 20% list B correct
by_output_tidy %>%
  dplyr::filter(subject_number == 108) %>%
  # let's just look at the first 12 responses
  dplyr::filter(output_position < 13,
                experiment == "SerialFM",
                response_type %in% c("Correct", "SemLure", "Misc")) %>%
  dplyr::group_by(sublist, response_type, output_position) %>%
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  graph_stacked_by_output_position_w_misc()

# check to see if sublist C really has the greatest (numerical) number of semFM at positions 1-4
by_output_tidy %>%
  # let's just look at the first 12 responses
  dplyr::filter(output_position < 13,
                experiment == "SerialFM",
                response_type %in% c("Correct", "SemLure", "Misc")) %>%
  dplyr::group_by(sublist, response_type, output_position) %>%
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  dplyr::filter(response_type == "SemLure",
                output_position < 5)

#' ### SerialFM - misc only (Figure 1a)
by_output_tidy %>%
  # let's just look at the first 12 responses
  dplyr::filter(output_position < 13,
                experiment == "SerialFM",
                response_type == "Misc") %>%
  dplyr::group_by(response_type, output_position) %>%
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  graph_stacked_by_output_position_no_sublist()

if(SAVE_FLAG){
  ggplot2::ggsave(file = paste0(dropbox_graphs_dir, 'misc_serial_fm.pdf'),
                  width = 10,
                  height = 6)
}

#' ### SerialFM_M
by_output_tidy %>%
  dplyr::filter(output_position < 13,
                experiment == "SerialFM_M",
                response_type %in% c("Correct", "SemLure")) %>%
  dplyr::group_by(sublist, response_type, output_position) %>%
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  graph_stacked_by_output_position()

if(SAVE_FLAG){
  ggplot2::ggsave(file = paste0(dropbox_graphs_dir, 'abc_stacked_serial_fm_m.pdf'),
                  width = 10,
                  height = 6)
}

# Figure 3b
by_output_tidy %>%
  # let's just look at the first 12 responses
  dplyr::filter(output_position < 13,
                experiment == "SerialFM_M",
                response_type %in% c("Correct", "SemLure", "Misc")) %>%
  dplyr::group_by(sublist, response_type, output_position) %>%
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  graph_stacked_by_output_position_w_misc()

if(SAVE_FLAG){
  ggplot2::ggsave(file = paste0(dropbox_graphs_dir, 'abc_stacked_serial_fm_m_w_misc.pdf'),
                  width = 10,
                  height = 6)
}

# check to see if sublist C really has the greatest (numerical) number of semFM at positions 1-4
by_output_tidy %>%
  # let's just look at the first 12 responses
  dplyr::filter(output_position < 13,
                experiment == "SerialFM_M",
                response_type %in% c("Correct", "SemLure", "Misc")) %>%
  dplyr::group_by(sublist, response_type, output_position) %>%
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  dplyr::filter(response_type == "SemLure",
                output_position < 5)

#' ### SerialFM_M - misc only (Figure 1b)
by_output_tidy %>%
  # let's just look at the first 12 responses
  dplyr::filter(output_position < 13,
                experiment == "SerialFM_M",
                response_type == "Misc") %>%
  dplyr::group_by(response_type, output_position) %>%
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  graph_stacked_by_output_position_no_sublist()

if(SAVE_FLAG){
  ggplot2::ggsave(file = paste0(dropbox_graphs_dir, 'misc_serial_fm_m.pdf'),
                  width = 10,
                  height = 6)
}

#' # Graph data split by sublist and response type (Figure 4)
#' ### SerialFM
by_sublist_tidy %>%
  dplyr::filter(experiment == "SerialFM") %>%
  graph_split_by_sublist()

if(SAVE_FLAG){
  ggplot2::ggsave(file = paste0(dropbox_graphs_dir, 'resp_by_sublist_serial_fm.pdf'),
                  width = 6,
                  height = 6)
}

by_sublist_tidy %>%
  dplyr::filter(experiment == "SerialFM_M") %>%
  graph_split_by_sublist()

if(SAVE_FLAG){
  ggplot2::ggsave(file = paste0(dropbox_graphs_dir, 'resp_by_sublist_serial_fm_m.pdf'),
                  width = 6,
                  height = 6)
}

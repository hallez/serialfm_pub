#' ---
#' title: "Load data"
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

#' # Setup stuff
# Load all of the functions in the /R folder
# Also ensures all packages listed as "imports" are installed
#+ warning = FALSE, message = FALSE
devtools::load_all()
# add dplyr with library()
# NB: this is non-standard
# correct way would be to make this script into a function,
# store in the R/ directory,
# and use @importFrom dplyr "%>%",
# but this is incompatible with getting in-line results with code in knitr output
library(dplyr)

#' ## Load in config file
# use packge::function syntax to avoid confusion
project_dir <- ("../../")
config <- yaml::yaml.load_file(paste0(project_dir,"config.yml"))

#' ## Setup paths
raw_behavioral_dir <- paste0(project_dir,halle::ensure_trailing_slash(config$directories$raw_behavioral))
analyzed_behavioral_dir <- paste0(project_dir,halle::ensure_trailing_slash(config$directories$analyzed_behavioral))
dropbox_dir <- halle::ensure_trailing_slash(config$directories$dropbox)
dropbox_graphs_dir <- paste0(dropbox_dir, halle::ensure_trailing_slash("writeups"), halle::ensure_trailing_slash("serialfm"), halle::ensure_trailing_slash("graphs"))

#' ## Flags
SAVE_FLAG <- 1

#' ## Other settings
# turn off scientific notation
# from: http://stackoverflow.com/questions/5352099/how-to-disable-scientific-notation-in-r
options(scipen=999)

#' # Load in data
fname <- "group_data.csv"
fpath <- paste0(halle::ensure_trailing_slash(raw_behavioral_dir),
                fname)
group_data <- read.csv(fpath)

#' # Tidy data
tidy_group_data_long_names <- group_data %>%
  # select only columns that are needed
  # will try to start from "raw" output where possible
  # and compute transformations, proportion scores, etc.
  dplyr::select(SubjectNumber,
         Gender,
         Age,
         Experiment,
         TotalRecalled,
         RecalledA,
         ANoMispro,
         RecalledB,
         BNoMispro,
         RecalledC,
         CNoMispro,
         LureA,
         LureB,
         LureC,
         SemanticA,
         SemanticB,
         SemanticC,
         AcousticA,
         AcousticB,
         AcousticC,
         Repeated,
         AWithinRepeats,
         BWithinRepeats,
         CWithinRepeats,
         Unrelated,
         NonWord,
         NotSure,
         MispronunciationA,
         MispronunciationB,
         MispronunciationC,
         ALureSem_ArcSin,
         BLureSem_ArcSin,
         CLureSem_ArcSin) %>%
  # clear up variable names
  dplyr::rename(veridical_a = RecalledA,
         veridical_b = RecalledB,
         veridical_c = RecalledC,
         veridical_a_no_mispro = ANoMispro,
         veridical_b_no_mispro = BNoMispro,
         veridical_c_no_mispro = CNoMispro,
         lure_a = LureA,
         lure_b = LureB,
         lure_c = LureC,
         semantic_fm_a = SemanticA,
         semantic_fm_b = SemanticB,
         semantic_fm_c = SemanticC,
         phono_fm_a = AcousticA,
         phono_fm_b = AcousticB,
         phono_fm_c = AcousticC,
         between_list_repeats = Repeated,
         sublist_a_repeats = AWithinRepeats,
         sublist_b_repeats = BWithinRepeats,
         sublist_c_repeats = CWithinRepeats) %>%
  # re-compute total recalled and compare w/ original from SPSS as a sanity check
  dplyr::mutate(total_recalled = veridical_a + veridical_b + veridical_c +
           lure_a + lure_b + lure_c +
           semantic_fm_a + semantic_fm_b + semantic_fm_c +
           phono_fm_a + phono_fm_b + phono_fm_c +
           between_list_repeats +
           sublist_a_repeats + sublist_b_repeats + sublist_c_repeats +
           Unrelated + NonWord + NotSure,
         total_recalled_no_mispro = veridical_a_no_mispro + veridical_b_no_mispro + veridical_c_no_mispro +
           lure_a + lure_b + lure_c +
           semantic_fm_a + semantic_fm_b + semantic_fm_c +
           phono_fm_a + phono_fm_b + phono_fm_c +
           between_list_repeats +
           sublist_a_repeats + sublist_b_repeats + sublist_c_repeats +
           Unrelated + NonWord + NotSure,
         total_non_sem_or_phono_errors = between_list_repeats +
           sublist_a_repeats + sublist_b_repeats + sublist_c_repeats +
           Unrelated + NonWord + NotSure) %>%
  # compute total number of veridical responses
  dplyr::mutate(total_correct = veridical_a + veridical_b + veridical_c) %>%
  # compute lure + sem variables
  dplyr::mutate(lure_sem_a = lure_a + semantic_fm_a,
                lure_sem_b = lure_b + semantic_fm_b,
                lure_sem_c = lure_c + semantic_fm_c) %>%
  # compute miscellaneous composite variable
  # from the manuscript, "miscellaneous: mispronunciations, repeats, and unintelligible utterances"
  dplyr::mutate(misc_responses = (MispronunciationA +
                                    MispronunciationB +
                                    MispronunciationC +
                                    sublist_a_repeats +
                                    sublist_b_repeats +
                                    sublist_c_repeats +
                                    between_list_repeats +
                                    Unrelated +
                                    NonWord),
                within_list_repeats = sublist_a_repeats + sublist_b_repeats + sublist_c_repeats,
                within_list_repeats_propn_misc = within_list_repeats / misc_responses) %>%
  # make sure subject number is a factor
  dplyr::mutate(subj_num_factor = factor(SubjectNumber)) %>%
  # combine ab and bc phono FM
  dplyr::mutate(phono_fm_ab = phono_fm_a + phono_fm_b,
                phono_fm_bc = phono_fm_b + phono_fm_c) %>%
  # compute the overall number of semantic FM responses
  dplyr::mutate(total_sem_fm_responses = semantic_fm_a + semantic_fm_b + semantic_fm_c)

mean(tidy_group_data_long_names$TotalRecalled, na.rm = TRUE)
mean(tidy_group_data_long_names$total_recalled, na.rm = TRUE)
mean(tidy_group_data_long_names$total_recalled_no_mispro, na.rm = TRUE)

# Can now use no_mispro options now that have confirmed columns all essentially the same
colnames(tidy_group_data_long_names)

tidy_group_data_renamed <- tidy_group_data_long_names %>%
  dplyr::select(-TotalRecalled,
         -veridical_a, -veridical_b, -veridical_c,
         -total_recalled) %>%
  dplyr::rename(veridical_a = veridical_a_no_mispro,
         veridical_b = veridical_b_no_mispro,
         veridical_c = veridical_c_no_mispro,
         total_recalled = total_recalled_no_mispro)

colnames(tidy_group_data_renamed)

# Compute mean again just to make sure haven't screwed anything up by renaming
mean(tidy_group_data_renamed$total_recalled, na.rm = TRUE)

# Continue tidying by giving character names to numeric factors
# 1 = serial_fm, 2 = serial_fm_m
# 1 = male, 2 = female
tidy_group_data <- tidy_group_data_renamed
head(tidy_group_data)
tidy_group_data$Experiment <- car::recode(tidy_group_data$Experiment, "1='SerialFM'; 2='SerialFM_M'")
tidy_group_data$Gender <- car::recode(tidy_group_data$Gender, "1='male'; 2='female'")
head(tidy_group_data)

#' # Calculate descriptives about participants
#' ## Number of participants (total, w/ exclusions)
#' ## **NB: cannot calculate number of excluded subjects because they were removed from the original .sav data file**
#' ## Mean age
#' ## SD age
#' ## Gender split
tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::summarise(total_subjects = n(),
            mean_age = mean(Age),
            sd_age = sd(Age))

# seems like there should be some way to chain group_by with summarise
# a la chaining n() %>% arrange here:
# http://www.markhneedham.com/blog/2014/11/09/r-dplyr-ordering-by-count-after-multiple-column-group_by/,
# but, for now will do it this way
tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::filter(Gender == "female") %>%
  dplyr::summarise(number_female = n())

#' # Data transformations
#' ## Correct for total number of responses
tidy_group_data <-
  tidy_group_data %>%
  # calulate total number of responses for each sublist position
  dplyr::mutate(a_total_responses = veridical_a + lure_sem_a + phono_fm_a,
         b_total_responses = veridical_b + lure_sem_b + phono_fm_b,
         c_total_responses = veridical_c + lure_sem_c + phono_fm_c) %>%
  # calculate responses corrected by total number of responses in that position
  dplyr::mutate(veridical_propn_all_resp = ((veridical_a + veridical_b + veridical_c)/(a_total_responses + b_total_responses + c_total_responses)),
                a_veridical_propn_a = veridical_a/a_total_responses,
                b_veridical_propn_b = veridical_b/b_total_responses,
                c_veridical_propn_c = veridical_c/c_total_responses,
                a_lure_sem_propn_a = lure_sem_a/a_total_responses,
                b_lure_sem_propn_b = lure_sem_b/b_total_responses,
                c_lure_sem_propn_c = lure_sem_c/c_total_responses,
                # proportion score semantic FM w/o lures (Memory, rev1)
                a_sem_propn_a = semantic_fm_a/a_total_responses,
                b_sem_propn_b = semantic_fm_b/b_total_responses,
                c_sem_propn_c = semantic_fm_c/c_total_responses,
                a_lure_propn_a = lure_a/a_total_responses,
                b_lure_propn_b = lure_b/b_total_responses,
                c_lure_propn_c = lure_c/c_total_responses,
                a_phono_fm_propn_a = phono_fm_a/a_total_responses,
                b_phono_fm_propn_b = phono_fm_b/b_total_responses,
                c_phono_fm_propn_c = phono_fm_c/c_total_responses,
                ab_phono_fm_propn_ab = (phono_fm_a + phono_fm_b)/(a_total_responses + b_total_responses),
                bc_phono_fm_propn_bc = (phono_fm_b + phono_fm_c)/(b_total_responses + c_total_responses))

# summarize raw versus proportion-corrected responses
options(dplyr.width = Inf)
# summarize veridical responses
tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::select(veridical_a, veridical_b, veridical_c,
                a_veridical_propn_a, b_veridical_propn_b, c_veridical_propn_c) %>%
  dplyr::summarise_each(funs(mean))

# summarize lure_semantic_fm responses
tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::select(lure_sem_a, lure_sem_b, lure_sem_c,
                a_lure_sem_propn_a, b_lure_sem_propn_b, c_lure_sem_propn_c) %>%
  dplyr::summarise_each(funs(mean))

# summarize phono_fm responses
tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::select(phono_fm_a, phono_fm_b, phono_fm_c,
                a_phono_fm_propn_a, b_phono_fm_propn_b, c_phono_fm_propn_c) %>%
  dplyr::summarise_each(funs(mean))

# summarise between list intrusions
tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::select(SubjectNumber, between_list_repeats, total_recalled) %>%
  dplyr::summarize(all_btwn = sum(between_list_repeats, na.rm = TRUE),
                   all_total = sum(total_recalled, na.rm = TRUE))

tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::select(SubjectNumber, between_list_repeats) %>%
  dplyr::filter(between_list_repeats > 0) %>%
  dplyr::group_by(Experiment) %>%
  dplyr::summarize(nsub = length(SubjectNumber))

#' # Compute total number of items recalled and total number correct
tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::select(total_recalled, total_correct) %>%
  dplyr::summarise_each(funs(mean(.,na.rm = TRUE),
                             sd(., na.rm = TRUE),
                             min(., na.rm = TRUE),
                             max(., na.rm = TRUE)))

#' # Compute total number of errors that are not semantic or phono
tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::select(total_non_sem_or_phono_errors) %>%
  dplyr::summarise_each(funs(mean(.,na.rm = TRUE),
                             sd(., na.rm = TRUE)))

#' ## Arc sine transformation (Relevant for Footnote #4)
# See Howell's Statistical Methods for Psychology 4th ed page 328 re arcsine:
# The usual form of this transformation is Y =  2 arcsine sqrt(p)
# where p is the proportion correct and Y will be twice the angle whose sine equals the sqrt of p.
arcsine_transform <- function(x){
 2*asin(sqrt(x))
}

tidy_group_data <-
  tidy_group_data %>%
  dplyr::mutate(asin_lure_sem_a = arcsine_transform(lure_sem_a),
                asin_lure_sem_b = arcsine_transform(lure_sem_b),
                asin_lure_sem_c = arcsine_transform(lure_sem_c),
                asin_phono_fm_a = arcsine_transform(phono_fm_a),
                asin_phono_fm_b = arcsine_transform(phono_fm_b),
                asin_phono_fm_c = arcsine_transform(phono_fm_c),
                asin_a_phono_fm_propn_a = arcsine_transform(a_phono_fm_propn_a),
                asin_b_phono_fm_propn_b = arcsine_transform(b_phono_fm_propn_b),
                asin_c_phono_fm_propn_c = arcsine_transform(c_phono_fm_propn_c),
                asin_phono_fm_ab = arcsine_transform(phono_fm_ab),
                asin_phono_fm_bc = arcsine_transform(phono_fm_bc),
                asin_ab_phono_fm_propn_ab = arcsine_transform(ab_phono_fm_propn_ab),
                asin_bc_phono_fm_propn_bc = arcsine_transform(bc_phono_fm_propn_bc))

#' # Correct responses
#' ## Means and SD: Overall and split by sublist
tidy_group_data %>%
  group_by(Experiment) %>%
  dplyr::select(veridical_a, veridical_b, veridical_c,
                veridical_propn_all_resp, a_veridical_propn_a, b_veridical_propn_b, c_veridical_propn_c) %>%
  dplyr::summarise_each(funs(mean(.,na.rm = TRUE),
                             sd(., na.rm = TRUE)))

#' ## ANOVAs on proportion scored responses
#' ### Expt 1
expt1_aov <- NULL

tidy_group_data %>%
  tidyr::gather(sublist, responses, c(a_veridical_propn_a,b_veridical_propn_b,c_veridical_propn_c)) %>%
  dplyr::filter(Experiment == "SerialFM") %>%
  ez::ezANOVA(., dv=.(responses),
              wid=.(subj_num_factor),
              within=.(sublist),
              type = 2,
              detailed = TRUE) -> expt1_aov

expt1_aov

#' ### Expt 2
expt2_aov <- NULL

tidy_group_data %>%
  tidyr::gather(sublist, responses, c(a_veridical_propn_a,b_veridical_propn_b,c_veridical_propn_c)) %>%
  dplyr::filter(Experiment == "SerialFM_M") %>%
  ez::ezANOVA(., dv=.(responses), wid=.(subj_num_factor), within=.(sublist), type = 2, detailed = TRUE) -> expt2_aov

expt2_aov

#' ## Paired t-tests between sublists w/ Bonferroni correction
#' ### Compute bonferroni-corrected t-test value
# let's just assume we're using p < .05 to determine significance
# simply divide that p-value by the number of tests that are done from any given ANOVA
# compare significance against this corrected value
# (instead of what's reported as significant from the t.test)

# in this case, this is the corrected value for both Expt1 and Expt2
# since each has the same number of posthoc t-tests
# and can be used for veridical, semantic FM, and phono FM
# since for each we care about the same 3 sublist comparisons
cur_bonf_pval <- NULL
cur_bonf_pval <- 0.05 / 3
cur_bonf_pval

#' ### Expt 1
# compute the paired t-tests between sublists
tidy_group_data %>%
  dplyr::select(a_veridical_propn_a, b_veridical_propn_b, c_veridical_propn_c, Experiment) %>%
  dplyr::filter(Experiment == "SerialFM") -> e1_tt1_paired

t.test(e1_tt1_paired$a_veridical_propn_a, e1_tt1_paired$b_veridical_propn_b, paired = TRUE)
e1_tt1_paired %>%
  dplyr::select(a_veridical_propn_a, b_veridical_propn_b) %>%
  dplyr::rename(var1 = a_veridical_propn_a,
                var2 = b_veridical_propn_b) ->
  e1_tt1_paired_ab_renamed
halle::compute_cohens_d(e1_tt1_paired_ab_renamed)

t.test(e1_tt1_paired$a_veridical_propn_a, e1_tt1_paired$c_veridical_propn_c, paired = TRUE)
e1_tt1_paired %>%
  dplyr::select(a_veridical_propn_a, c_veridical_propn_c) %>%
  dplyr::rename(var1 = a_veridical_propn_a,
                var2 = c_veridical_propn_c) ->
  e1_tt1_paired_ac_renamed
halle::compute_cohens_d(e1_tt1_paired_ac_renamed)

t.test(e1_tt1_paired$b_veridical_propn_b, e1_tt1_paired$c_veridical_propn_c, paired = TRUE)
e1_tt1_paired %>%
  dplyr::select(b_veridical_propn_b, c_veridical_propn_c) %>%
  dplyr::rename(var1 = b_veridical_propn_b,
                var2 = c_veridical_propn_c) ->
  e1_tt1_paired_bc_renamed
halle::compute_cohens_d(e1_tt1_paired_bc_renamed)

#' ### Expt 2
# t-tests
tidy_group_data %>%
  dplyr::select(a_veridical_propn_a, b_veridical_propn_b, c_veridical_propn_c, Experiment) %>%
  dplyr::filter(Experiment == "SerialFM_M") -> e2_tt1_paired

t.test(e2_tt1_paired$a_veridical_propn_a, e2_tt1_paired$b_veridical_propn_b, paired = TRUE)
e2_tt1_paired %>%
  dplyr::select(a_veridical_propn_a, b_veridical_propn_b) %>%
  dplyr::rename(var1 = a_veridical_propn_a,
                var2 = b_veridical_propn_b) ->
  e2_tt1_paired_ab_renamed
halle::compute_cohens_d(e2_tt1_paired_ab_renamed)

t.test(e2_tt1_paired$a_veridical_propn_a, e2_tt1_paired$c_veridical_propn_c, paired = TRUE)
e2_tt1_paired %>%
  dplyr::select(a_veridical_propn_a, c_veridical_propn_c) %>%
  dplyr::rename(var1 = a_veridical_propn_a,
                var2 = c_veridical_propn_c) ->
  e2_tt1_paired_ac_renamed
halle::compute_cohens_d(e2_tt1_paired_ac_renamed)

t.test(e2_tt1_paired$b_veridical_propn_b, e2_tt1_paired$c_veridical_propn_c, paired = TRUE)
e2_tt1_paired %>%
  dplyr::select(b_veridical_propn_b, c_veridical_propn_c) %>%
  dplyr::rename(var1 = b_veridical_propn_b,
                var2 = c_veridical_propn_c) ->
  e2_tt1_paired_bc_renamed
halle::compute_cohens_d(e2_tt1_paired_bc_renamed)

#' # Miscellaneous error responses
# percentage of errors that were "miscellaneous"
# capitalize on the totals that were computed for the proportion scoring
tidy_group_data <-
  tidy_group_data %>%
  # calculate total number of errors
  dplyr::mutate(a_errors = lure_sem_a + phono_fm_a,
                b_errors = lure_sem_b + phono_fm_b,
                c_errors = lure_sem_c + phono_fm_c,
                all_semfm_errors = lure_sem_a + lure_sem_b + lure_sem_c,
                all_phonofm_errors = phono_fm_a + phono_fm_b + phono_fm_c,
                all_errors = a_errors + b_errors + c_errors + misc_responses,
                misc_propn_all_errors = misc_responses / all_errors,
                misc_propn_all_responses = misc_responses / total_recalled,
                sem_fm_propn_all_errors = all_semfm_errors / all_errors,
                sem_fm_propn_all_responses = all_semfm_errors / total_recalled,
                phono_fm_propn_all_errors = all_phonofm_errors / all_errors,
                phono_fm_propn_all_responses = all_phonofm_errors / total_recalled,
                between_list_repeats_propn_all_responses = between_list_repeats / total_recalled)

head(tidy_group_data$all_errors)
head(tidy_group_data$misc_responses)
head(tidy_group_data$misc_propn_all_errors)

#' ## Proportion of total errors, all responses
tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::select(misc_propn_all_errors, misc_propn_all_responses,
                within_list_repeats, within_list_repeats_propn_misc) %>%
  dplyr::summarise_each(funs(mean))

#' ## SemFM, PhonoFM, Misc as proportion of total errors, all responses (for comparison)
tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::select(one_of(c("misc_propn_all_errors", "misc_propn_all_responses",
                         "sem_fm_propn_all_errors", "sem_fm_propn_all_responses",
                         "phono_fm_propn_all_errors", "phono_fm_propn_all_responses",
                         "between_list_repeats_propn_all_responses"))) %>%
  dplyr::summarise_each(funs(mean))

#' # Within experiment: Direct comparisons between semantic and phono errors by sublist
#' ## Expt 1
#' ### ANOVA
tidy_group_data %>%
  dplyr::select(a_lure_sem_propn_a,
                b_lure_sem_propn_b,
                c_lure_sem_propn_c,
                a_phono_fm_propn_a,
                b_phono_fm_propn_b,
                c_phono_fm_propn_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(fm_by_sublist, responses,
                c(a_lure_sem_propn_a,
                  b_lure_sem_propn_b,
                  c_lure_sem_propn_c,
                  a_phono_fm_propn_a,
                  b_phono_fm_propn_b,
                  c_phono_fm_propn_c)) %>%
  tidyr::separate(fm_by_sublist, into = c("sublist", "fm_type"),
                  sep = "_",
                  extra = "merge") %>%
  # get rid of redundant sublist label on `fm_type`
  # throws an error
  # `In mutate_impl(.data, dots) : NAs introduced by coercion`
  dplyr::mutate(fm_type_no_sublist = car::recode(fm_type, "c('lure_sem_propn_a','lure_sem_propn_b','lure_sem_propn_c') = 'lure_sem_propn'; c('phono_fm_propn_a','phono_fm_propn_b','phono_fm_propn_c') = 'phono_fm_propn'")) -> sem_phono_fm

expt1_aov <- NULL

sem_phono_fm %>%
  dplyr::filter(Experiment == "SerialFM") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist, fm_type_no_sublist),
              type = 2,
              detailed = TRUE) -> expt1_aov

expt1_aov

#' ### Paired t-tests between sublists
# compute t-values
# compare between FM types at each sublist position
tt1 <- NULL
tidy_group_data %>%
  dplyr::filter(Experiment == "SerialFM") -> tt1

t.test(tt1$a_lure_sem_propn_a, tt1$a_phono_fm_propn_a, paired = TRUE)
tt1 %>%
  dplyr::select(a_lure_sem_propn_a, a_phono_fm_propn_a) %>%
  dplyr::rename(var1 = a_lure_sem_propn_a,
                var2 = a_phono_fm_propn_a) ->
  e1_a_sem_phono
halle::compute_cohens_d(e1_a_sem_phono)

t.test(tt1$b_lure_sem_propn_b, tt1$b_phono_fm_propn_b, paired = TRUE)
tt1 %>%
  dplyr::select(b_lure_sem_propn_b, b_phono_fm_propn_b) %>%
  dplyr::rename(var1 = b_lure_sem_propn_b,
                var2 = b_phono_fm_propn_b) ->
  e1_b_sem_phono
halle::compute_cohens_d(e1_b_sem_phono)

t.test(tt1$c_lure_sem_propn_c, tt1$c_phono_fm_propn_c, paired = TRUE)
tt1 %>%
  dplyr::select(c_lure_sem_propn_c, c_phono_fm_propn_c) %>%
  dplyr::rename(var1 = c_lure_sem_propn_c,
                var2 = c_phono_fm_propn_c) ->
  e1_c_sem_phono
halle::compute_cohens_d(e1_c_sem_phono)

#' ### Paired t-tests, collapsing FM type, between sublists
# create new variables that collapse sem & phono FM
fm_collapsed_e1 <-
  tt1 %>%
  dplyr::mutate(a_fm = a_lure_sem_propn_a + a_phono_fm_propn_a,
                b_fm = b_lure_sem_propn_b + b_phono_fm_propn_b,
                c_fm = c_lure_sem_propn_c + c_phono_fm_propn_c)

# run t-tests between sublists, collapsing FM type
t.test(fm_collapsed_e1$a_fm, fm_collapsed_e1$b_fm, paired = TRUE)
fm_collapsed_e1 %>%
  dplyr::select(a_fm, b_fm) %>%
  dplyr::rename(var1 = a_fm, var2 = b_fm) -> e1_ab_fm
halle::compute_cohens_d(e1_ab_fm)

t.test(fm_collapsed_e1$a_fm, fm_collapsed_e1$c_fm, paired = TRUE)
fm_collapsed_e1 %>%
  dplyr::select(a_fm, c_fm) %>%
  dplyr::rename(var1 = a_fm, var2 = c_fm) -> e1_ac_fm
halle::compute_cohens_d(e1_ac_fm)

t.test(fm_collapsed_e1$b_fm, fm_collapsed_e1$c_fm, paired = TRUE)
fm_collapsed_e1 %>%
  dplyr::select(b_fm, c_fm) %>%
  dplyr::rename(var1 = b_fm, var2 = c_fm) -> e1_bc_fm
halle::compute_cohens_d(e1_bc_fm)

#' ## Expt 1 - correct vs. lure vs. semFM
#' ### ANOVA
all_resp_propn <- tidy_group_data %>%
  dplyr::select(a_veridical_propn_a,
                b_veridical_propn_b,
                c_veridical_propn_c,
                a_lure_propn_a,
                b_lure_propn_b,
                c_lure_propn_c,
                a_sem_propn_a,
                b_sem_propn_b,
                c_sem_propn_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(mem_by_sublist, responses,
                c(a_veridical_propn_a,
                  b_veridical_propn_b,
                  c_veridical_propn_c,
                  a_lure_propn_a,
                  b_lure_propn_b,
                  c_lure_propn_c,
                  a_sem_propn_a,
                  b_sem_propn_b,
                  c_sem_propn_c)) %>%
  tidyr::separate(mem_by_sublist, into = c("sublist", "mem_type"),
                  sep = "_",
                  extra = "merge") %>%
  # get rid of redundant sublist label on `fm_type`
  # throws an error
  # `In mutate_impl(.data, dots) : NAs introduced by coercion`
  dplyr::mutate(mem_type_no_sublist = car::recode(mem_type, "c('lure_propn_a','lure_propn_b','lure_propn_c') = 'lure_propn';
                                                  c('sem_propn_a','sem_propn_b','sem_propn_c') = 'sem_propn';
                                                  c('veridical_propn_a','veridical_propn_b','veridical_propn_c') = 'veridical_propn'"))

expt1_aov_all_resp <- NULL

all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist, mem_type_no_sublist),
              type = 2,
              detailed = TRUE) -> expt1_aov_all_resp

expt1_aov_all_resp

#' #### break it up for follow-up tests
# start w/ "family-wise" ANOVAs for each response type by sublist separately
# (including veridical for completness, but that's what's already reported in the paper)
expt1_aov_verid_resp <- all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM") %>%
  dplyr::filter(mem_type_no_sublist == "veridical_propn") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist),
              type = 2,
              detailed = TRUE)

expt1_aov_verid_resp

# then do paired t-tests between each sublist
expt1_verid <- all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM") %>%
  dplyr::filter(mem_type_no_sublist == "veridical_propn")

t.test(expt1_verid$responses[expt1_verid$mem_type == "veridical_propn_a"], expt1_verid$responses[expt1_verid$mem_type == "veridical_propn_b"], paired = TRUE)
e1_ab_verid <- expt1_verid %>%
  dplyr::filter(mem_type %in% c("veridical_propn_a", "veridical_propn_b")) %>%
  # drop any columns that will lead to NA values when spread
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  # for the cohen's D, need to have columns
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = veridical_propn_a,
                var2 = veridical_propn_b)
halle::compute_cohens_d(e1_ab_verid)

t.test(expt1_verid$responses[expt1_verid$mem_type == "veridical_propn_a"], expt1_verid$responses[expt1_verid$mem_type == "veridical_propn_c"], paired = TRUE)
e1_ac_verid <- expt1_verid %>%
  dplyr::filter(mem_type %in% c("veridical_propn_a", "veridical_propn_c")) %>%
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = veridical_propn_a,
                var2 = veridical_propn_c)
halle::compute_cohens_d(e1_ac_verid)

t.test(expt1_verid$responses[expt1_verid$mem_type == "veridical_propn_b"], expt1_verid$responses[expt1_verid$mem_type == "veridical_propn_c"], paired = TRUE)
e1_bc_verid <- expt1_verid %>%
  dplyr::filter(mem_type %in% c("veridical_propn_b", "veridical_propn_c")) %>%
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = veridical_propn_b,
                var2 = veridical_propn_c)
halle::compute_cohens_d(e1_bc_verid)

# rinse and repeat for semFM and lureFM
expt1_aov_semFM_resp <- all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM") %>%
  dplyr::filter(mem_type_no_sublist == "sem_propn") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist),
              type = 2,
              detailed = TRUE)

expt1_aov_semFM_resp

expt1_semFM <- all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM") %>%
  dplyr::filter(mem_type_no_sublist == "sem_propn")

t.test(expt1_semFM$responses[expt1_semFM$mem_type == "sem_propn_a"], expt1_semFM$responses[expt1_semFM$mem_type == "sem_propn_b"], paired = TRUE)
e1_ab_semFM <- expt1_semFM %>%
  dplyr::filter(mem_type %in% c("sem_propn_a", "sem_propn_b")) %>%
  # drop any columns that will lead to NA values when spread
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  # for the cohen's D, need to have columns
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = sem_propn_a,
                var2 = sem_propn_b)
halle::compute_cohens_d(e1_ab_semFM)

t.test(expt1_semFM$responses[expt1_semFM$mem_type == "sem_propn_a"], expt1_semFM$responses[expt1_semFM$mem_type == "sem_propn_c"], paired = TRUE)
e1_ac_semFM <- expt1_semFM %>%
  dplyr::filter(mem_type %in% c("sem_propn_a", "sem_propn_c")) %>%
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = sem_propn_a,
                var2 = sem_propn_c)
halle::compute_cohens_d(e1_ac_semFM)

t.test(expt1_semFM$responses[expt1_semFM$mem_type == "sem_propn_b"], expt1_semFM$responses[expt1_semFM$mem_type == "sem_propn_c"], paired = TRUE)
e1_bc_semFM <- expt1_semFM %>%
  dplyr::filter(mem_type %in% c("sem_propn_b", "sem_propn_c")) %>%
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = sem_propn_b,
                var2 = sem_propn_c)
halle::compute_cohens_d(e1_bc_semFM)

# lures alone
expt1_aov_lureFM_resp <- all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM") %>%
  dplyr::filter(mem_type_no_sublist == "lure_propn") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist),
              type = 2,
              detailed = TRUE)

expt1_aov_lureFM_resp

expt1_lureFM <- all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM") %>%
  dplyr::filter(mem_type_no_sublist == "lure_propn")

t.test(expt1_lureFM$responses[expt1_lureFM$mem_type == "lure_propn_a"], expt1_lureFM$responses[expt1_lureFM$mem_type == "lure_propn_b"], paired = TRUE)
e1_ab_lureFM <- expt1_lureFM %>%
  dplyr::filter(mem_type %in% c("lure_propn_a", "lure_propn_b")) %>%
  # drop any columns that will lead to NA values when spread
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  # for the cohen's D, need to have columns
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = lure_propn_a,
                var2 = lure_propn_b)
halle::compute_cohens_d(e1_ab_lureFM)

t.test(expt1_lureFM$responses[expt1_lureFM$mem_type == "lure_propn_a"], expt1_lureFM$responses[expt1_lureFM$mem_type == "lure_propn_c"], paired = TRUE)
e1_ac_lureFM <- expt1_lureFM %>%
  dplyr::filter(mem_type %in% c("lure_propn_a", "lure_propn_c")) %>%
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = lure_propn_a,
                var2 = lure_propn_c)
halle::compute_cohens_d(e1_ac_lureFM)

t.test(expt1_lureFM$responses[expt1_lureFM$mem_type == "lure_propn_b"], expt1_lureFM$responses[expt1_lureFM$mem_type == "lure_propn_c"], paired = TRUE)
e1_bc_lureFM <- expt1_lureFM %>%
  dplyr::filter(mem_type %in% c("lure_propn_b", "lure_propn_c")) %>%
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = lure_propn_b,
                var2 = lure_propn_c)
halle::compute_cohens_d(e1_bc_lureFM)

#' ## Expt 2
#' ### ANOVA
tidy_group_data %>%
  dplyr::select(a_lure_sem_propn_a,
                b_lure_sem_propn_b,
                c_lure_sem_propn_c,
                a_phono_fm_propn_a,
                b_phono_fm_propn_b,
                c_phono_fm_propn_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(fm_by_sublist, responses,
                c(a_lure_sem_propn_a,
                  b_lure_sem_propn_b,
                  c_lure_sem_propn_c,
                  a_phono_fm_propn_a,
                  b_phono_fm_propn_b,
                  c_phono_fm_propn_c)) %>%
  tidyr::separate(fm_by_sublist, into = c("sublist", "fm_type"),
                  sep = "_",
                  extra = "merge") %>%
  # get rid of redundant sublist label on `fm_type`
  # throws an error
  # `In mutate_impl(.data, dots) : NAs introduced by coercion`
  dplyr::mutate(fm_type_no_sublist = car::recode(fm_type, "c('lure_sem_propn_a','lure_sem_propn_b','lure_sem_propn_c') = 'lure_sem_propn'; c('phono_fm_propn_a','phono_fm_propn_b','phono_fm_propn_c') = 'phono_fm_propn'")) -> sem_phono_fm

expt2_aov <- NULL

sem_phono_fm %>%
  dplyr::filter(Experiment == "SerialFM_M") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist, fm_type_no_sublist),
              type = 2,
              detailed = TRUE) -> expt2_aov

expt2_aov

#' ### Paired t-tests between sublists
# compute t-values
# compare between FM types at each sublist position
tt2 <- NULL
tidy_group_data %>%
  dplyr::filter(Experiment == "SerialFM_M") -> tt2

t.test(tt2$a_lure_sem_propn_a, tt2$a_phono_fm_propn_a, paired = TRUE)
tt2 %>%
  dplyr::select(a_lure_sem_propn_a, a_phono_fm_propn_a) %>%
  dplyr::rename(var1 = a_lure_sem_propn_a,
                var2 = a_phono_fm_propn_a) ->
  e2_a_sem_phono
halle::compute_cohens_d(e2_a_sem_phono)

t.test(tt2$b_lure_sem_propn_b, tt2$b_phono_fm_propn_b, paired = TRUE)
tt2 %>%
  dplyr::select(b_lure_sem_propn_b, b_phono_fm_propn_b) %>%
  dplyr::rename(var1 = b_lure_sem_propn_b,
                var2 = b_phono_fm_propn_b) ->
  e2_b_sem_phono
halle::compute_cohens_d(e2_b_sem_phono)

t.test(tt2$c_lure_sem_propn_c, tt2$c_phono_fm_propn_c, paired = TRUE)
tt2 %>%
  dplyr::select(c_lure_sem_propn_c, c_phono_fm_propn_c) %>%
  dplyr::rename(var1 = c_lure_sem_propn_c,
                var2 = c_phono_fm_propn_c) ->
  e2_c_sem_phono
halle::compute_cohens_d(e2_c_sem_phono)

#' ### Paired t-tests, collapsing FM type, between sublists
# create new variables that collapse sem & phono FM
fm_collapsed_e2 <-
  tt2 %>%
  dplyr::mutate(a_fm = a_lure_sem_propn_a + a_phono_fm_propn_a,
                b_fm = b_lure_sem_propn_b + b_phono_fm_propn_b,
                c_fm = c_lure_sem_propn_c + c_phono_fm_propn_c)

# run t-tests between sublists, collapsing FM type
t.test(fm_collapsed_e2$a_fm, fm_collapsed_e2$b_fm, paired = TRUE)
fm_collapsed_e2 %>%
  dplyr::select(a_fm, b_fm) %>%
  dplyr::rename(var1 = a_fm, var2 = b_fm) -> e2_ab_fm
halle::compute_cohens_d(e2_ab_fm)

t.test(fm_collapsed_e2$a_fm, fm_collapsed_e2$c_fm, paired = TRUE)
fm_collapsed_e2 %>%
  dplyr::select(a_fm, c_fm) %>%
  dplyr::rename(var1 = a_fm, var2 = c_fm) -> e2_ac_fm
halle::compute_cohens_d(e2_ac_fm)

t.test(fm_collapsed_e2$b_fm, fm_collapsed_e2$c_fm, paired = TRUE)
fm_collapsed_e2 %>%
  dplyr::select(b_fm, c_fm) %>%
  dplyr::rename(var1 = b_fm, var2 = c_fm) -> e2_bc_fm
halle::compute_cohens_d(e2_bc_fm)

#' ## Expt 2 - correct vs. lure vs. semFM
#' ### ANOVA
# all_resp_propn is created above
expt2_aov_all_resp <- NULL

all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM_M") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist, mem_type_no_sublist),
              type = 2,
              detailed = TRUE) -> expt2_aov_all_resp

expt2_aov_all_resp

#' #### break it up for follow-up tests
# start w/ "family-wise" ANOVAs for each response type by sublist separately
# (including veridical for completness, but that's what's already reported in the paper)
expt2_aov_verid_resp <- all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM_M") %>%
  dplyr::filter(mem_type_no_sublist == "veridical_propn") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist),
              type = 2,
              detailed = TRUE)

expt2_aov_verid_resp

# then do paired t-tests between each sublist
expt2_verid <- all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM_M") %>%
  dplyr::filter(mem_type_no_sublist == "veridical_propn")

t.test(expt2_verid$responses[expt2_verid$mem_type == "veridical_propn_a"], expt2_verid$responses[expt2_verid$mem_type == "veridical_propn_b"], paired = TRUE)
e1_ab_verid <- expt2_verid %>%
  dplyr::filter(mem_type %in% c("veridical_propn_a", "veridical_propn_b")) %>%
  # drop any columns that will lead to NA values when spread
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  # for the cohen's D, need to have columns
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = veridical_propn_a,
                var2 = veridical_propn_b)
halle::compute_cohens_d(e1_ab_verid)

t.test(expt2_verid$responses[expt2_verid$mem_type == "veridical_propn_a"], expt2_verid$responses[expt2_verid$mem_type == "veridical_propn_c"], paired = TRUE)
e1_ac_verid <- expt2_verid %>%
  dplyr::filter(mem_type %in% c("veridical_propn_a", "veridical_propn_c")) %>%
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = veridical_propn_a,
                var2 = veridical_propn_c)
halle::compute_cohens_d(e1_ac_verid)

t.test(expt2_verid$responses[expt2_verid$mem_type == "veridical_propn_b"], expt2_verid$responses[expt2_verid$mem_type == "veridical_propn_c"], paired = TRUE)
e1_bc_verid <- expt2_verid %>%
  dplyr::filter(mem_type %in% c("veridical_propn_b", "veridical_propn_c")) %>%
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = veridical_propn_b,
                var2 = veridical_propn_c)
halle::compute_cohens_d(e1_bc_verid)

# rinse and repeat for semFM and lureFM
expt2_aov_semFM_resp <- all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM_M") %>%
  dplyr::filter(mem_type_no_sublist == "sem_propn") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist),
              type = 2,
              detailed = TRUE)

expt2_aov_semFM_resp

expt2_semFM <- all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM_M") %>%
  dplyr::filter(mem_type_no_sublist == "sem_propn")

t.test(expt2_semFM$responses[expt2_semFM$mem_type == "sem_propn_a"], expt2_semFM$responses[expt2_semFM$mem_type == "sem_propn_b"], paired = TRUE)
e1_ab_semFM <- expt2_semFM %>%
  dplyr::filter(mem_type %in% c("sem_propn_a", "sem_propn_b")) %>%
  # drop any columns that will lead to NA values when spread
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  # for the cohen's D, need to have columns
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = sem_propn_a,
                var2 = sem_propn_b)
halle::compute_cohens_d(e1_ab_semFM)

t.test(expt2_semFM$responses[expt2_semFM$mem_type == "sem_propn_a"], expt2_semFM$responses[expt2_semFM$mem_type == "sem_propn_c"], paired = TRUE)
e1_ac_semFM <- expt2_semFM %>%
  dplyr::filter(mem_type %in% c("sem_propn_a", "sem_propn_c")) %>%
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = sem_propn_a,
                var2 = sem_propn_c)
halle::compute_cohens_d(e1_ac_semFM)

t.test(expt2_semFM$responses[expt2_semFM$mem_type == "sem_propn_b"], expt2_semFM$responses[expt2_semFM$mem_type == "sem_propn_c"], paired = TRUE)
e1_bc_semFM <- expt2_semFM %>%
  dplyr::filter(mem_type %in% c("sem_propn_b", "sem_propn_c")) %>%
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = sem_propn_b,
                var2 = sem_propn_c)
halle::compute_cohens_d(e1_bc_semFM)

# lures alone
expt2_aov_lureFM_resp <- all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM_M") %>%
  dplyr::filter(mem_type_no_sublist == "lure_propn") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist),
              type = 2,
              detailed = TRUE)

expt2_aov_lureFM_resp

expt2_lureFM <- all_resp_propn %>%
  dplyr::filter(Experiment == "SerialFM_M") %>%
  dplyr::filter(mem_type_no_sublist == "lure_propn")

t.test(expt2_lureFM$responses[expt2_lureFM$mem_type == "lure_propn_a"], expt2_lureFM$responses[expt2_lureFM$mem_type == "lure_propn_b"], paired = TRUE)
e1_ab_lureFM <- expt2_lureFM %>%
  dplyr::filter(mem_type %in% c("lure_propn_a", "lure_propn_b")) %>%
  # drop any columns that will lead to NA values when spread
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  # for the cohen's D, need to have columns
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = lure_propn_a,
                var2 = lure_propn_b)
halle::compute_cohens_d(e1_ab_lureFM)

t.test(expt2_lureFM$responses[expt2_lureFM$mem_type == "lure_propn_a"], expt2_lureFM$responses[expt2_lureFM$mem_type == "lure_propn_c"], paired = TRUE)
e1_ac_lureFM <- expt2_lureFM %>%
  dplyr::filter(mem_type %in% c("lure_propn_a", "lure_propn_c")) %>%
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = lure_propn_a,
                var2 = lure_propn_c)
halle::compute_cohens_d(e1_ac_lureFM)

t.test(expt2_lureFM$responses[expt2_lureFM$mem_type == "lure_propn_b"], expt2_lureFM$responses[expt2_lureFM$mem_type == "lure_propn_c"], paired = TRUE)
e1_bc_lureFM <- expt2_lureFM %>%
  dplyr::filter(mem_type %in% c("lure_propn_b", "lure_propn_c")) %>%
  dplyr::select(subj_num_factor, Experiment, mem_type, responses) %>%
  tidyr::spread(mem_type, responses) %>%
  dplyr::rename(var1 = lure_propn_b,
                var2 = lure_propn_c)
halle::compute_cohens_d(e1_bc_lureFM)

#' # Semantic FM responses
#' ## Mean & SD number of semantic errors, by sublist
tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::select(contains("lure_sem")) %>%
  dplyr::summarise_each(funs(mean(., na.rm = TRUE),
                            sd(., na.rm = TRUE)))

#' ## Breakdown of lures related to all semantic FM
# Table 1 in the paper reports overall means (ie, not proportion corrected)
tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::select(a_lure_sem_propn_a, b_lure_sem_propn_b, c_lure_sem_propn_c,
                a_lure_propn_a, b_lure_propn_b, c_lure_propn_c,
                lure_a, lure_b, lure_c,
                Experiment) %>%
  dplyr::summarise_each(funs(mean(., na.rm = TRUE),
                             sd(., na.rm = TRUE)))

tidy_group_data %>%
  dplyr::mutate(a_lure_propn_a_luresem = lure_a / lure_sem_a,
                b_lure_propn_b_luresem = lure_b / lure_sem_b,
                c_lure_propn_c_luresem = lure_c / lure_sem_c) %>%
  dplyr::select(a_lure_propn_a_luresem, b_lure_propn_b_luresem, c_lure_propn_c_luresem,
                Experiment) %>%
  dplyr::group_by(Experiment) %>%
  dplyr::summarise_each(funs(mean(., na.rm = TRUE),
                        sd(., na.rm = TRUE)))

#' ## Expt 1
#' ### ANOVA
expt1_aov <- NULL

tidy_group_data %>%
  dplyr::select(a_lure_sem_propn_a,
                b_lure_sem_propn_b,
                c_lure_sem_propn_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(sublist, responses, c(a_lure_sem_propn_a,
                                      b_lure_sem_propn_b,
                                      c_lure_sem_propn_c)) %>%
  dplyr::filter(Experiment == "SerialFM") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist),
              type = 2,
              detailed = TRUE) -> expt1_aov

expt1_aov

#' #### Arc sine corrected (relevant to footnote #4)
expt1_aov <- NULL
tidy_group_data %>%
  dplyr::select(asin_lure_sem_a,
                asin_lure_sem_b,
                asin_lure_sem_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(sublist, responses, c(asin_lure_sem_a,
                                      asin_lure_sem_b,
                                      asin_lure_sem_c)) %>%
  dplyr::filter(Experiment == "SerialFM") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist),
              type = 2,
              detailed = TRUE) -> expt1_aov

expt1_aov

#' ### Paired t-tests by sublist
# compute paired t-tests
tidy_group_data %>%
  dplyr::select(a_lure_sem_propn_a,
                b_lure_sem_propn_b,
                c_lure_sem_propn_c,
                subj_num_factor, Experiment) %>%
  dplyr::filter(Experiment == "SerialFM") -> tt1_semfm_paired

t.test(tt1_semfm_paired$a_lure_sem_propn_a, tt1_semfm_paired$b_lure_sem_propn_b, paired = TRUE)
tt1_semfm_paired %>%
  dplyr::select(a_lure_sem_propn_a, b_lure_sem_propn_b) %>%
  dplyr::rename(var1 = a_lure_sem_propn_a,
                var2 = b_lure_sem_propn_b) ->
  e1_ab_sem
halle::compute_cohens_d(e1_ab_sem)

t.test(tt1_semfm_paired$a_lure_sem_propn_a, tt1_semfm_paired$c_lure_sem_propn_c, paired = TRUE)
tt1_semfm_paired %>%
  dplyr::select(a_lure_sem_propn_a, c_lure_sem_propn_c) %>%
  dplyr::rename(var1 = a_lure_sem_propn_a,
                var2 = c_lure_sem_propn_c) ->
  e1_ac_sem
halle::compute_cohens_d(e1_ac_sem)

t.test(tt1_semfm_paired$b_lure_sem_propn_b, tt1_semfm_paired$c_lure_sem_propn_c, paired = TRUE)
tt1_semfm_paired %>%
  dplyr::select(b_lure_sem_propn_b, c_lure_sem_propn_c) %>%
  dplyr::rename(var1 = b_lure_sem_propn_b,
                var2 = c_lure_sem_propn_c) ->
  e1_bc_sem
halle::compute_cohens_d(e1_bc_sem)

#' ### Paired t-tests by sublist, just lure responses
tidy_group_data %>%
  dplyr::select(a_lure_propn_a,
                b_lure_propn_b,
                c_lure_propn_c,
                subj_num_factor, Experiment) %>%
  dplyr::filter(Experiment == "SerialFM") -> tt1_lurefm_paired

t.test(tt1_lurefm_paired$a_lure_propn_a, tt1_lurefm_paired$b_lure_propn_b, paired = TRUE)
tt1_lurefm_paired %>%
  dplyr::select(a_lure_propn_a, b_lure_propn_b) %>%
  dplyr::rename(var1 = a_lure_propn_a,
                var2 = b_lure_propn_b) ->
  e1_ab_lure
halle::compute_cohens_d(e1_ab_lure)

t.test(tt1_lurefm_paired$a_lure_propn_a, tt1_lurefm_paired$c_lure_propn_c, paired = TRUE)
tt1_lurefm_paired %>%
  dplyr::select(a_lure_propn_a, c_lure_propn_c) %>%
  dplyr::rename(var1 = a_lure_propn_a,
                var2 = c_lure_propn_c) ->
  e1_ac_lure
halle::compute_cohens_d(e1_ac_lure)

t.test(tt1_lurefm_paired$b_lure_propn_b, tt1_lurefm_paired$c_lure_propn_c, paired = TRUE)
tt1_lurefm_paired %>%
  dplyr::select(b_lure_propn_b, c_lure_propn_c) %>%
  dplyr::rename(var1 = b_lure_propn_b,
                var2 = c_lure_propn_c) ->
  e1_bc_lure
halle::compute_cohens_d(e1_bc_lure)

#' ### Verify that lure FMs differ from zero
t.test(tt1_lurefm_paired$a_lure_propn_a, mu = 0)
t.test(tt1_lurefm_paired$b_lure_propn_b, mu = 0)
t.test(tt1_lurefm_paired$c_lure_propn_c, mu = 0)

#' ## Expt 2
#' ### ANOVA
expt2_aov <- NULL

tidy_group_data %>%
  dplyr::select(a_lure_sem_propn_a,
                b_lure_sem_propn_b,
                c_lure_sem_propn_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(sublist, responses, c(a_lure_sem_propn_a,
                                      b_lure_sem_propn_b,
                                      c_lure_sem_propn_c)) %>%
  dplyr::filter(Experiment == "SerialFM_M") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist),
              type = 2,
              detailed = TRUE) -> expt2_aov

expt2_aov

#' #### Arc sine corrected (relevant to footnote #4)
expt2_aov <- NULL
tidy_group_data %>%
  dplyr::select(asin_lure_sem_a,
                asin_lure_sem_b,
                asin_lure_sem_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(sublist, responses, c(asin_lure_sem_a,
                                      asin_lure_sem_b,
                                      asin_lure_sem_c)) %>%
  dplyr::filter(Experiment == "SerialFM_M") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist),
              type = 2,
              detailed = TRUE) -> expt2_aov

expt2_aov

#' ### Paired t-tests by sublist
# compute paired t-tests
tidy_group_data %>%
  dplyr::select(a_lure_sem_propn_a,
                b_lure_sem_propn_b,
                c_lure_sem_propn_c,
                subj_num_factor, Experiment) %>%
  dplyr::filter(Experiment == "SerialFM_M") -> tt2_semfm_paired

t.test(tt2_semfm_paired$a_lure_sem_propn_a, tt2_semfm_paired$b_lure_sem_propn_b, paired = TRUE)
tt2_semfm_paired %>%
  dplyr::select(a_lure_sem_propn_a, b_lure_sem_propn_b) %>%
  dplyr::rename(var1 = a_lure_sem_propn_a,
                var2 = b_lure_sem_propn_b) ->
  e2_ab_sem
halle::compute_cohens_d(e2_ab_sem)

t.test(tt2_semfm_paired$a_lure_sem_propn_a, tt2_semfm_paired$c_lure_sem_propn_c, paired = TRUE)
tt2_semfm_paired %>%
  dplyr::select(a_lure_sem_propn_a, c_lure_sem_propn_c) %>%
  dplyr::rename(var1 = a_lure_sem_propn_a,
                var2 = c_lure_sem_propn_c) ->
  e2_ac_sem
halle::compute_cohens_d(e2_ac_sem)

t.test(tt2_semfm_paired$b_lure_sem_propn_b, tt2_semfm_paired$c_lure_sem_propn_c, paired = TRUE)
tt2_semfm_paired %>%
  dplyr::select(b_lure_sem_propn_b, c_lure_sem_propn_c) %>%
  dplyr::rename(var1 = b_lure_sem_propn_b,
                var2 = c_lure_sem_propn_c) ->
  e2_bc_sem
halle::compute_cohens_d(e2_bc_sem)

#' ### Paired t-tests by sublist, just lure responses
tidy_group_data %>%
  dplyr::select(a_lure_propn_a,
                b_lure_propn_b,
                c_lure_propn_c,
                subj_num_factor, Experiment) %>%
  dplyr::filter(Experiment == "SerialFM_M") -> tt2_lurefm_paired

t.test(tt2_lurefm_paired$a_lure_propn_a, tt2_lurefm_paired$b_lure_propn_b, paired = TRUE)
tt2_lurefm_paired %>%
  dplyr::select(a_lure_propn_a, b_lure_propn_b) %>%
  dplyr::rename(var1 = a_lure_propn_a,
                var2 = b_lure_propn_b) ->
  e2_ab_lure
halle::compute_cohens_d(e2_ab_lure)

t.test(tt2_lurefm_paired$a_lure_propn_a, tt2_lurefm_paired$c_lure_propn_c, paired = TRUE)
tt2_lurefm_paired %>%
  dplyr::select(a_lure_propn_a, c_lure_propn_c) %>%
  dplyr::rename(var1 = a_lure_propn_a,
                var2 = c_lure_propn_c) ->
  e2_ac_lure
halle::compute_cohens_d(e2_ac_lure)

t.test(tt2_lurefm_paired$b_lure_propn_b, tt2_lurefm_paired$c_lure_propn_c, paired = TRUE)
tt2_lurefm_paired %>%
  dplyr::select(b_lure_propn_b, c_lure_propn_c) %>%
  dplyr::rename(var1 = b_lure_propn_b,
                var2 = c_lure_propn_c) ->
  e2_bc_lure
halle::compute_cohens_d(e2_bc_lure)

#' ### Verify that lure FMs differ from zero
t.test(tt2_lurefm_paired$a_lure_propn_a, mu = 0)
t.test(tt2_lurefm_paired$b_lure_propn_b, mu = 0)
t.test(tt2_lurefm_paired$c_lure_propn_c, mu = 0)

#' # Phonological FM responses
#' ## Mean & SD number of phono errors, by sublist
tidy_group_data %>%
  dplyr::group_by(Experiment) %>%
  dplyr::select(contains("phono_fm")) %>%
  dplyr::summarise_each(funs(mean(., na.rm = TRUE),
                             sd(., na.rm = TRUE)))
#' ## Expt 1
#' ### ANOVA
expt1_aov <- NULL

tidy_group_data %>%
  dplyr::select(a_phono_fm_propn_a,
                b_phono_fm_propn_b,
                c_phono_fm_propn_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(sublist, responses,
                c(a_phono_fm_propn_a,
                  b_phono_fm_propn_b,
                  c_phono_fm_propn_c)) %>%
  dplyr::filter(Experiment == "SerialFM") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist),
              type = 2,
              detailed = TRUE) -> expt1_aov

expt1_aov

#' #### Arcsine corrected, proportion scored, t-test vs 0 (relevant for footnote #4)
e1_asin_phono_propn_scored <- tidy_group_data %>%
  dplyr::filter(Experiment == "SerialFM") %>%
  dplyr::select(subj_num_factor,
                asin_a_phono_fm_propn_a, asin_b_phono_fm_propn_b, asin_c_phono_fm_propn_c,
                asin_ab_phono_fm_propn_ab)

t.test(e1_asin_phono_propn_scored$asin_a_phono_fm_propn_a, mu = 0, na.rm = TRUE)
t.test(e1_asin_phono_propn_scored$asin_b_phono_fm_propn_b, mu = 0, na.rm = TRUE)
t.test(e1_asin_phono_propn_scored$asin_c_phono_fm_propn_c, mu = 0, na.rm = TRUE)

#' #### Arcsine corrected, proportion scored, paired t-tests between sublists
t.test(e1_asin_phono_propn_scored$asin_a_phono_fm_propn_a, e1_asin_phono_propn_scored$asin_b_phono_fm_propn_b, paired = TRUE)
t.test(e1_asin_phono_propn_scored$asin_a_phono_fm_propn_a, e1_asin_phono_propn_scored$asin_c_phono_fm_propn_c, paired = TRUE)
t.test(e1_asin_phono_propn_scored$asin_b_phono_fm_propn_b, e1_asin_phono_propn_scored$asin_c_phono_fm_propn_c, paired = TRUE)

# compare combined A and B vs. C
t.test(e1_asin_phono_propn_scored$asin_ab_phono_fm_propn_ab, e1_asin_phono_propn_scored$asin_c_phono_fm_propn_c, paired = TRUE)
e1_asin_phono_propn_scored %>%
  dplyr::rename(var1 = asin_ab_phono_fm_propn_ab,
                var2 = asin_c_phono_fm_propn_c) ->
  e1_ab_c_asin_phono_propn_scored
halle::compute_cohens_d(e1_ab_c_asin_phono_propn_scored)

#' ### Paired t-tests by sublist
# compute paired t-tests between sublists
tidy_group_data %>%
  dplyr::select(a_phono_fm_propn_a,
                b_phono_fm_propn_b,
                c_phono_fm_propn_c,
                subj_num_factor, Experiment) %>%
  dplyr::filter(Experiment == "SerialFM") -> tt1_phonofm_paired

t.test(tt1_phonofm_paired$a_phono_fm_propn_a, tt1_phonofm_paired$b_phono_fm_propn_b, paired = TRUE)
tt1_phonofm_paired %>%
  dplyr::select(a_phono_fm_propn_a, b_phono_fm_propn_b) %>%
  dplyr::rename(var1 = a_phono_fm_propn_a,
                var2 = b_phono_fm_propn_b) ->
  e1_ab_phono
halle::compute_cohens_d(e1_ab_phono)

t.test(tt1_phonofm_paired$a_phono_fm_propn_a, tt1_phonofm_paired$c_phono_fm_propn_c, paired = TRUE)
tt1_phonofm_paired %>%
  dplyr::select(a_phono_fm_propn_a, c_phono_fm_propn_c) %>%
  dplyr::rename(var1 = a_phono_fm_propn_a,
                var2 = c_phono_fm_propn_c) ->
  e1_ac_phono
halle::compute_cohens_d(e1_ac_phono)

t.test(tt1_phonofm_paired$b_phono_fm_propn_b, tt1_phonofm_paired$c_phono_fm_propn_c, paired = TRUE)
tt1_phonofm_paired %>%
  dplyr::select(b_phono_fm_propn_b, c_phono_fm_propn_c) %>%
  dplyr::rename(var1 = b_phono_fm_propn_b,
                var2 = c_phono_fm_propn_c) ->
  e1_bc_phono
halle::compute_cohens_d(e1_bc_phono)

# compute t-tests against zero
# (since phono FM are so rare)
t.test(tt1_phonofm_paired$a_phono_fm_propn_a, mu = 0, na.rm = TRUE)
tt1_phonofm_paired %>%
  dplyr::select(a_phono_fm_propn_a) %>%
  dplyr::rename(var1 = a_phono_fm_propn_a) ->
  e1_a_phono
halle::compute_cohens_d_vs_0(e1_a_phono)

t.test(tt1_phonofm_paired$b_phono_fm_propn_b, mu = 0, na.rm = TRUE)
tt1_phonofm_paired %>%
  dplyr::select(b_phono_fm_propn_b) %>%
  dplyr::rename(var1 = b_phono_fm_propn_b) ->
  e1_b_phono
halle::compute_cohens_d_vs_0(e1_b_phono)

t.test(tt1_phonofm_paired$c_phono_fm_propn_c, mu = 0, na.rm = TRUE)
tt1_phonofm_paired %>%
  dplyr::select(c_phono_fm_propn_c) %>%
  dplyr::rename(var1 = c_phono_fm_propn_c) ->
  e1_c_phono
halle::compute_cohens_d_vs_0(e1_c_phono)

# combine sublists A and B
tt1_phonofm_paired <- NULL

tidy_group_data %>%
  dplyr::select(a_phono_fm_propn_a,
                b_phono_fm_propn_b,
                c_phono_fm_propn_c,
                ab_phono_fm_propn_ab,
                subj_num_factor, Experiment) %>%
  dplyr::filter(Experiment == "SerialFM") -> tt1_phonofm_paired

# confirm that ab phono combined FM differs from zero
t.test(tt1_phonofm_paired$ab_phono_fm_propn_ab, mu = 0, na.rm = TRUE)

# compare ab w/ c
t.test(tt1_phonofm_paired$ab_phono_fm_propn_ab, tt1_phonofm_paired$c_phono_fm_propn_c, paired = TRUE)

tt1_phonofm_paired %>%
  dplyr::select(ab_phono_fm_propn_ab, c_phono_fm_propn_c) %>%
  dplyr::rename(var1 = ab_phono_fm_propn_ab, var2 = c_phono_fm_propn_c) -> tt1_phonofm_paired_renamed
halle::compute_cohens_d(tt1_phonofm_paired_renamed)

#' ## Expt 2
#' ### ANOVA
expt2_aov <- NULL

tidy_group_data %>%
  dplyr::select(a_phono_fm_propn_a,
                b_phono_fm_propn_b,
                c_phono_fm_propn_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(sublist, responses,
                c(a_phono_fm_propn_a,
                  b_phono_fm_propn_b,
                  c_phono_fm_propn_c)) %>%
  dplyr::filter(Experiment == "SerialFM_M") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              within = .(sublist),
              type = 2,
              detailed = TRUE) -> expt2_aov

expt2_aov

#' #### Arcsine corrected, proportion scored, t-test vs 0
e2_asin_phono_propn_scored <- tidy_group_data %>%
  dplyr::filter(Experiment == "SerialFM_M") %>%
  dplyr::select(subj_num_factor,
                asin_a_phono_fm_propn_a, asin_b_phono_fm_propn_b, asin_c_phono_fm_propn_c,
                asin_bc_phono_fm_propn_bc)

t.test(e2_asin_phono_propn_scored$asin_a_phono_fm_propn_a, mu = 0, na.rm = TRUE)
t.test(e2_asin_phono_propn_scored$asin_b_phono_fm_propn_b, mu = 0, na.rm = TRUE)
t.test(e2_asin_phono_propn_scored$asin_c_phono_fm_propn_c, mu = 0, na.rm = TRUE)

#' #### Arcsine corrected, proportion scored, paired t-tests between sublists
t.test(e2_asin_phono_propn_scored$asin_a_phono_fm_propn_a, e2_asin_phono_propn_scored$asin_b_phono_fm_propn_b, paired = TRUE)
t.test(e2_asin_phono_propn_scored$asin_a_phono_fm_propn_a, e2_asin_phono_propn_scored$asin_c_phono_fm_propn_c, paired = TRUE)
t.test(e2_asin_phono_propn_scored$asin_b_phono_fm_propn_b, e2_asin_phono_propn_scored$asin_c_phono_fm_propn_c, paired = TRUE)

# compare A vs. combined BC
t.test(e2_asin_phono_propn_scored$asin_a_phono_fm_propn_a, e2_asin_phono_propn_scored$asin_bc_phono_fm_propn_bc, paired = TRUE)
e2_asin_phono_propn_scored %>%
  dplyr::rename(var1 = asin_a_phono_fm_propn_a,
                var2 = asin_bc_phono_fm_propn_bc) ->
  e2_a_bc_asin_phono_propn_scored
halle::compute_cohens_d(e2_a_bc_asin_phono_propn_scored)

#' ### Paired t-tests by sublist
# compute paired t-tests
tidy_group_data %>%
  dplyr::select(a_phono_fm_propn_a,
                b_phono_fm_propn_b,
                c_phono_fm_propn_c,
                subj_num_factor, Experiment) %>%
  dplyr::filter(Experiment == "SerialFM_M") -> tt2_phonofm_paired

t.test(tt2_phonofm_paired$a_phono_fm_propn_a, tt2_phonofm_paired$b_phono_fm_propn_b, paired = TRUE)
tt2_phonofm_paired %>%
  dplyr::select(a_phono_fm_propn_a, b_phono_fm_propn_b) %>%
  dplyr::rename(var1 = a_phono_fm_propn_a,
                var2 = b_phono_fm_propn_b) ->
  e2_ab_phono
halle::compute_cohens_d(e2_ab_phono)

t.test(tt2_phonofm_paired$a_phono_fm_propn_a, tt2_phonofm_paired$c_phono_fm_propn_c, paired = TRUE)
tt2_phonofm_paired %>%
  dplyr::select(a_phono_fm_propn_a, c_phono_fm_propn_c) %>%
  dplyr::rename(var1 = a_phono_fm_propn_a,
                var2 = c_phono_fm_propn_c) ->
  e2_ac_phono
halle::compute_cohens_d(e2_ac_phono)

t.test(tt2_phonofm_paired$b_phono_fm_propn_b, tt2_phonofm_paired$c_phono_fm_propn_c, paired = TRUE)
tt2_phonofm_paired %>%
  dplyr::select(b_phono_fm_propn_b, c_phono_fm_propn_c) %>%
  dplyr::rename(var1 = b_phono_fm_propn_b,
                var2 = c_phono_fm_propn_c) ->
  e2_bc_phono
halle::compute_cohens_d(e2_bc_phono)

# compute t-tests against zero
# (since phono FM are so rare)
t.test(tt2_phonofm_paired$a_phono_fm_propn_a, mu = 0, na.rm = TRUE)
tt2_phonofm_paired %>%
  dplyr::select(a_phono_fm_propn_a) %>%
  dplyr::rename(var1 = a_phono_fm_propn_a) ->
  e2_a_phono
halle::compute_cohens_d_vs_0(e2_a_phono)

t.test(tt2_phonofm_paired$b_phono_fm_propn_b, mu = 0, na.rm = TRUE)
tt2_phonofm_paired %>%
  dplyr::select(b_phono_fm_propn_b) %>%
  dplyr::rename(var1 = b_phono_fm_propn_b) ->
  e2_b_phono
halle::compute_cohens_d_vs_0(e2_b_phono)

t.test(tt2_phonofm_paired$c_phono_fm_propn_c, mu = 0, na.rm = TRUE)
tt2_phonofm_paired %>%
  dplyr::select(c_phono_fm_propn_c) %>%
  dplyr::rename(var1 = c_phono_fm_propn_c) ->
  e2_c_phono
halle::compute_cohens_d_vs_0(e2_c_phono)

# combine sublists B and C
# although technically could do any
# since none of the pairwise t-tests are significant
# trying combining B and C because these are n/s diff vs. 0 (w/ Bonf correction)
tt2_phonofm_paired <- NULL

tidy_group_data %>%
  dplyr::select(a_phono_fm_propn_a,
                b_phono_fm_propn_b,
                c_phono_fm_propn_c,
                bc_phono_fm_propn_bc,
                subj_num_factor, Experiment) %>%
  dplyr::filter(Experiment == "SerialFM_M") -> tt2_phonofm_paired

# confirm that bc phono combined FM differs from zero
t.test(tt2_phonofm_paired$bc_phono_fm_propn_bc, mu = 0, na.rm = TRUE)

# compare bc w/ a
t.test(tt2_phonofm_paired$bc_phono_fm_propn_bc, tt2_phonofm_paired$a_phono_fm_propn_a, paired = TRUE)

tt2_phonofm_paired %>%
  dplyr::select(bc_phono_fm_propn_bc, a_phono_fm_propn_a) %>%
  dplyr::rename(var1 = bc_phono_fm_propn_bc, var2 = a_phono_fm_propn_a) -> tt2_phonofm_paired_renamed
halle::compute_cohens_d(tt2_phonofm_paired_renamed)

#' # Between experiment comparisons
#' ## Bonferroni-corrected pvalues
# since here we care about the between experiment/within sublist
# ie, a/a, b/b, c/c
# and also the between sublist, experiment collapsed
# ie, a/b, a/c, b/c
# comparisons, need to divide by 6
cur_bonf_pval <- NULL
cur_bonf_pval <- 0.05 / 6
cur_bonf_pval

#' ## Veridical, Semantic, and Phono responses in a mega ANOVA: Proportion corrected responses
btwn_expt <- NULL

tidy_group_data %>%
  dplyr::select(a_veridical_propn_a, b_veridical_propn_b, c_veridical_propn_c,
                a_lure_sem_propn_a, b_lure_sem_propn_b, c_lure_sem_propn_c,
                a_phono_fm_propn_a, b_phono_fm_propn_b, c_phono_fm_propn_c,
                subj_num_factor, Experiment) %>%
  # remove the "_" before sublist for sem and phono FM
  dplyr::rename(veridical_propna = a_veridical_propn_a,
                veridical_propnb = b_veridical_propn_b,
                veridical_propnc = c_veridical_propn_c,
                luresem_propna = a_lure_sem_propn_a,
                luresem_propnb = b_lure_sem_propn_b,
                luresem_propnc = c_lure_sem_propn_c,
                phonofm_propna = a_phono_fm_propn_a,
                phonofm_propnb = b_phono_fm_propn_b,
                phonofm_propnc = c_phono_fm_propn_c) %>%
  # group together responses into a single column
  tidyr::gather(sublist, responses,
                c(veridical_propna, veridical_propnb, veridical_propnc,
                  luresem_propna, luresem_propnb, luresem_propnc,
                  phonofm_propna, phonofm_propnb, phonofm_propnc)) %>%
  # split sublist label from memory type
  tidyr::separate(sublist, into = c("sublist", "fm_type"),
                  sep = "_",
                  extra = "merge") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              between = .(Experiment, sublist, fm_type),
              type = 2,
              detailed = TRUE) -> btwn_expt

btwn_expt

#' ## Veridical responses
#' ### Proportion corrected data: ANOVA & post hoc tests
btwn_expt <- NULL

tidy_group_data %>%
  dplyr::select(a_veridical_propn_a, b_veridical_propn_b, c_veridical_propn_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(sublist, responses,
                c(a_veridical_propn_a, b_veridical_propn_b, c_veridical_propn_c)) %>%
  # also get a column for sublist
  tidyr::separate(sublist, into = c("sublist", "fm_type"),
                sep = "_",
                extra = "merge") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              between = .(Experiment, sublist),
              type = 2,
              detailed = TRUE) -> btwn_expt

btwn_expt

#' ## Semantic FM responses
btwn_expt <- NULL

tidy_group_data %>%
  dplyr::select(a_lure_sem_propn_a, b_lure_sem_propn_b, c_lure_sem_propn_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(sublist, responses,
                c(a_lure_sem_propn_a, b_lure_sem_propn_b, c_lure_sem_propn_c)) %>%
  # split off sublist label
  tidyr::separate(sublist, into = c("sublist", "fm_type"),
                  sep = "_",
                  extra = "merge") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              between = .(Experiment, sublist),
              type = 2,
              detailed = TRUE) -> btwn_expt
btwn_expt

# paired t-tests
tt_sem <- NULL

tidy_group_data %>%
  dplyr::select(a_lure_sem_propn_a, b_lure_sem_propn_b, c_lure_sem_propn_c,
                subj_num_factor, Experiment) %>%
  # put all sublists into a single column
  tidyr::gather(sublist, responses,
                c(a_lure_sem_propn_a, b_lure_sem_propn_b, c_lure_sem_propn_c)) %>%
  # group expt and sublist label
  tidyr::unite(sublist_expt, sublist, Experiment) %>%
  # columns for each sublist/expt combo
  tidyr::spread(sublist_expt, responses) -> tt_sem

t.test(tt_sem$a_lure_sem_propn_a_SerialFM, tt_sem$a_lure_sem_propn_a_SerialFM_M)
t.test(tt_sem$b_lure_sem_propn_b_SerialFM, tt_sem$b_lure_sem_propn_b_SerialFM_M)
t.test(tt_sem$c_lure_sem_propn_c_SerialFM, tt_sem$c_lure_sem_propn_c_SerialFM_M)

# also want t-tests collapsing across experiment but looking at sublist diffs
t.test(tidy_group_data$a_lure_sem_propn_a, tidy_group_data$b_lure_sem_propn_b)
t.test(tidy_group_data$a_lure_sem_propn_a, tidy_group_data$c_lure_sem_propn_c)
t.test(tidy_group_data$b_lure_sem_propn_b, tidy_group_data$c_lure_sem_propn_c)

#' ### Proportion scored data: ANOVA, just lure FM
btwn_expt <- NULL

tidy_group_data %>%
  dplyr::select(a_lure_propn_a, b_lure_propn_b, c_lure_propn_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(sublist, responses,
                c(a_lure_propn_a, b_lure_propn_b, c_lure_propn_c)) %>%
  # split off sublist label
  tidyr::separate(sublist, into = c("sublist", "fm_type"),
                  sep = "_",
                  extra = "merge") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              between = .(Experiment, sublist),
              type = 2,
              detailed = TRUE) -> btwn_expt
btwn_expt

# paired t-tests
tt_lure <- NULL

tidy_group_data %>%
  dplyr::select(a_lure_propn_a, b_lure_propn_b, c_lure_propn_c,
                subj_num_factor, Experiment) %>%
  # put all sublists into a single column
  tidyr::gather(sublist, responses,
                c(a_lure_propn_a, b_lure_propn_b, c_lure_propn_c)) %>%
  # group expt and sublist label
  tidyr::unite(sublist_expt, sublist, Experiment) %>%
  # columns for each sublist/expt combo
  tidyr::spread(sublist_expt, responses) -> tt_lure

t.test(tt_lure$a_lure_propn_a_SerialFM, tt_lure$a_lure_propn_a_SerialFM_M)
t.test(tt_lure$b_lure_propn_b_SerialFM, tt_lure$b_lure_propn_b_SerialFM_M)
t.test(tt_lure$c_lure_propn_c_SerialFM, tt_lure$c_lure_propn_c_SerialFM_M)

# also want t-tests collapsing across experiment but looking at sublist diffs
t.test(tidy_group_data$a_lure_propn_a, tidy_group_data$b_lure_propn_b)
t.test(tidy_group_data$a_lure_propn_a, tidy_group_data$c_lure_propn_c)
t.test(tidy_group_data$b_lure_propn_b, tidy_group_data$c_lure_propn_c)

#' ## Phono FM responses
btwn_expt <- NULL
tidy_group_data %>%
  dplyr::select(a_phono_fm_propn_a, b_phono_fm_propn_b, c_phono_fm_propn_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(sublist, responses,
                c(a_phono_fm_propn_a, b_phono_fm_propn_b, c_phono_fm_propn_c)) %>%
  # split into a separate sublist column
  tidyr::separate(sublist, into = c("sublist", "fm_type"),
                  sep = "_",
                  extra = "merge") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              between = .(Experiment, sublist),
              type = 2,
              detailed = TRUE) -> btwn_expt

btwn_expt

# paired t-tests
# even though ANOVA n/s, these are planned comparisons
tt_phono <- NULL

tidy_group_data %>%
  dplyr::select(a_phono_fm_propn_a, b_phono_fm_propn_b, c_phono_fm_propn_c,
                subj_num_factor, Experiment) %>%
  # put all sublists into a single column
  tidyr::gather(sublist, responses,
                c(a_phono_fm_propn_a, b_phono_fm_propn_b, c_phono_fm_propn_c)) %>%
  # group expt and sublist label
  tidyr::unite(sublist_expt, sublist, Experiment) %>%
  # columns for each sublist/expt combo
  tidyr::spread(sublist_expt, responses) -> tt_phono

t.test(tt_phono$a_phono_fm_propn_a_SerialFM, tt_phono$a_phono_fm_propn_a_SerialFM_M)
t.test(tt_phono$b_phono_fm_propn_b_SerialFM, tt_phono$b_phono_fm_propn_b_SerialFM_M)
t.test(tt_phono$c_phono_fm_propn_c_SerialFM, tt_phono$c_phono_fm_propn_c_SerialFM_M)

# also want t-tests collapsing across experiment but looking at sublist diffs
t.test(tidy_group_data$a_phono_fm_propn_a, tidy_group_data$b_phono_fm_propn_b)
t.test(tidy_group_data$a_phono_fm_propn_a, tidy_group_data$c_phono_fm_propn_c)
t.test(tidy_group_data$b_phono_fm_propn_b, tidy_group_data$c_phono_fm_propn_c)

#' ### Arc Sine corrected data, proportion scored: ANOVA
btwn_expt <- NULL
tidy_group_data %>%
  dplyr::select(asin_a_phono_fm_propn_a, asin_b_phono_fm_propn_b, asin_c_phono_fm_propn_c,
                subj_num_factor, Experiment) %>%
  tidyr::gather(sublist, responses,
                c(asin_a_phono_fm_propn_a, asin_b_phono_fm_propn_b, asin_c_phono_fm_propn_c)) %>%
  # split off sublist column
  tidyr::separate(sublist, into = c("fm_type", "sublist"),
                  sep = "fm_") %>%
  ez::ezANOVA(.,
              dv = .(responses),
              wid = .(subj_num_factor),
              between = .(Experiment, sublist),
              type = 2,
              detailed = TRUE) -> btwn_expt

btwn_expt

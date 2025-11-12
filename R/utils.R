#' @importFrom stats na.omit var cor
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when
#' @importFrom stringr str_to_upper str_trim
#' @importFrom tibble tibble
#' @importFrom htmltools span
NULL

#-------------------------------------------------------------------------------
# Operators and Core Calculations (Exported for Shiny App)
#-------------------------------------------------------------------------------

#' Null-coalescing operator
#'
#' Returns `a` unless it is `NULL`; otherwise returns `b`.
#' @name grapes-or-or-grapes
#' @rdname grapes-or-or-grapes
#' @param a Left-hand side value.
#' @param b Right-hand side fallback.
#' @return `a` if not `NULL`, else `b`.
#' @keywords internal
#' @export
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

#' Calculate the mode
#'
#' Finds the most frequent value (the mode) in a vector.
#'
#' @param x A vector.
#' @return The mode of the vector. Returns NA if the vector is empty.
#' @export
#' @examples
#' d_mode(c(1, 2, 2, 3, 3, 3, 4))
#' d_mode(c("a", "b", "a"))
d_mode <- function(x){ z <- table(x); if(length(z)==0) return(NA); as.numeric(names(z)[which.max(z)]) }

#' Calculate KR-20 reliability coefficient
#'
#' Calculates the Kuder-Richardson 20 (KR-20) reliability coefficient
#' for a data.frame or matrix of dichotomous (0/1) items.
#'
#' @param m A data.frame or matrix where rows are subjects and columns are
#'   dichotomously scored (0/1) items.
#'
#' @return A numeric value for the KR-20 coefficient, or NA_real_ if
#'   calculation is not possible.
#' @export
#' @examples
#' item_matrix <- data.frame(
#'   m1 = c(1, 1, 0, 1),
#'   m2 = c(1, 0, 1, 1),
#'   m3 = c(0, 1, 0, 0)
#' )
#' kr20(item_matrix)
kr20 <- function(m){
  if(is.null(m)||ncol(m)<2) return(NA_real_)
  k<-ncol(m); tot<-rowSums(m,na.rm=TRUE)
  vt<-stats::var(tot,na.rm=TRUE); p<-colMeans(m,na.rm=TRUE); q<-1-p; s2<-sum(p*q,na.rm=TRUE)
  if(is.na(vt)||vt==0) return(NA_real_)
  (k/(k-1))*(1 - s2/vt)
}

#' Point-biserial correlation for item analysis
#'
#' Calculates the correlation between a single item's score and the
#' rest of the total score.
#'
#' @param item A numeric vector of dichotomous item scores (0/1).
#' @param rest A numeric vector of the total scores, excluding the item.
#' @return The point-biserial correlation coefficient.
#' @export
#' @examples
#' item1 <- c(1, 0, 1, 0, 1, 1)
#' rest_score <- c(10, 8, 12, 5, 9, 11)
#' pbiserial_rest(item1, rest_score)
pbiserial_rest <- function(item, rest){
  if(all(is.na(item)) || length(unique(na.omit(item)))<2) return(NA_real_)
  tryCatch(stats::cor(item, rest, use="pairwise.complete.obs"), error=function(e) NA_real_)
}

#' Normalize letter grades
#'
#' Cleans and validates a vector of characters, keeping only
#' standard letter grades (A, B, C, D, E).
#'
#' @param x A vector, typically character.
#' @return A character vector of normalized grades (A-E) or NA.
#' @export
#' @examples
#' norm_letter(c(" a ", "B", "c", "F", "d", NA))
norm_letter <- function(x){
  v <- as.character(x) %>%
    stringr::str_to_upper() %>%
    stringr::str_trim()
  ifelse(v%in%c("A","B","C","D","E"), v, NA)
}

#' Calculate q-index (1 - p)
#'
#' A simple helper to calculate the inverse of the p-value (difficulty index).
#'
#' @param p A numeric value or vector (item difficulty).
#' @return A numeric value or vector (1 - p).
#' @export
#' @examples
#' q_index(0.8)
#' q_index(c(0.2, 0.5, 0.7))
q_index <- function(p) 1-p

#-------------------------------------------------------------------------------
# Helper Functions for Shiny UI/Server (All Exported)
#-------------------------------------------------------------------------------

#' Student-level counts (Correct/Incorrect/Missing)
#'
#' Calculates the total number of correct, incorrect, and missing
#' answers for each student (row).
#'
#' @param sc A data.frame of scored items (0=wrong, 1=correct, NA=missing).
#' @return A tibble with columns `Dogru`, `Yanlis`, and `Bos`.
#' @export
#' @examples
#' score_df <- data.frame(m1 = c(1, 0, 1), m2 = c(0, 1, NA), m3 = c(1, 1, 1))
#' student_counts(score_df)
student_counts <- function(sc){
  df <- as.data.frame(sc)
  if (ncol(df)==0) return(tibble::tibble(Dogru=rep(0, nrow(df)), Yanlis=rep(0, nrow(df)), Bos=rep(0, nrow(df))))
  tibble::tibble(
    Dogru = unname(rowSums(df == 1, na.rm = TRUE)),
    Yanlis = unname(rowSums(df == 0, na.rm = TRUE)),
    Bos    = unname(rowSums(is.na(df)))
  )
}

#' Parse raw continuous/Likert scores
#'
#' Cleans a vector of potential scores, converting to numeric
#' and removing invalid or out-of-range values.
#'
#' @param x A vector (typically character) of raw scores.
#' @return A numeric vector of cleaned scores.
#' @export
#' @examples
#' parse_lc_raw(c("10", "5.5", "0", "-2", "ikiyüz", NA, "1000001"))
parse_lc_raw <- function(x){
  v <- suppressWarnings(as.numeric(as.character(x)))
  v[is.na(v)] <- NA_real_
  v[v < 0 | v > 1e6] <- NA_real_
  v
}

#' Normalize Column Names
#'
#' A helper function that takes a mixed character
#' vector or a single semi-colon/comma-separated string and returns a
#' clean character vector of column names.
#'
#' @param x A character vector or a single string containing column names.
#' @return A character vector of trimmed, non-empty column names.
#' @export
#' @examples
#' norm_cols(" m1 , m2;m3")
#' norm_cols(c(" m1 ", "m2", "", " m3 "))
norm_cols <- function(x){
  if (is.null(x)) return(character(0))

  if (length(x) == 1) {
    y <- unlist(strsplit(as.character(x), "[;,]+"))
  } else {
    y <- as.character(x)
  }

  y <- trimws(y)
  y[y != ""]
}

#' Safely get quantile from 'psychometric' package
#'
#' Tries to get the default `quant` argument from `psychometric::item.exam`.
#' Returns 0.27 if the package is not installed or an error occurs.
#'
#' @return A numeric value for the quantile (default 0.27).
#' @export
#' @examples
#' get_itemexam_quant()
get_itemexam_quant <- function(){
  q <- 0.27
  if (requireNamespace("psychometric", quietly = TRUE)) {
    res <- tryCatch(as.numeric(formals(psychometric::item.exam)$quant), error = function(e) NULL)
    if (!is.null(res) && length(res) == 1 && !is.na(res)) q <- res
  }
  return(q)
}

#' Create a colored HTML span badge (UI helper)
#'
#' Generates a colored HTML badge for displaying values
#' (like p-values or correlations) in the Shiny UI.
#'
#' @param v The numeric value to display.
#' @param type The type of value ('generic', 'p', 'r') for coloring rules.
#' @return An `htmltools::span()` object.
#' @export
#' @examples
#' if (interactive()) {
#'   # Example for difficulty (p)
#'   color_badge(0.5, "p")
#'   # Example for discrimination (r)
#'   color_badge(0.2, "r")
#' }
color_badge <- function(v, type = c("generic", "p", "r")){
  type <- match.arg(type)
  col <- "#888"
  if(!is.na(v)){
    if(type == "p"){
      col <- if(v < .40 || v > .80) "#d62728" else "#2ca02c"
    } else if(type == "r"){
      col <- if(v < .30) "#d62728" else "#2ca02c"
    } else {
      col <- if(v < .50) "#d62728" else "#2ca02c"
    }
  }
  htmltools::span(style = paste0("padding:4px 8px;border-radius:8px;background:", col, ";color:white;"),
                  sprintf("%.3f", v))
}

#' Generate translation keys for overall comments
#'
#' Creates a vector of translation keys based on average
#' difficulty (ap) and discrimination (ar).
#'
#' @param ap Average difficulty (p-value).
#' @param ar Average discrimination (r-value).
#' @return A character vector of translation keys.
#' @export
#' @examples
#' comment_overall_keys(0.6, 0.35) # Medium, Keep
#' comment_overall_keys(0.2, 0.15) # Hard, Remove
comment_overall_keys <- function(ap, ar){
  pkey <- if (is.na(ap)) NULL else if (ap < .4)
    "comment_overall.difficulty.low"
  else if (ap > .8)
    "comment_overall.difficulty.high"
  else
    "comment_overall.difficulty.mid"

  rkey <- if (is.na(ar)) NULL else if (ar < .2)
    "comment_overall.discrimination.low"
  else if (ar > .4)
    "comment_overall.discrimination.high"
  else
    "comment_overall.discrimination.mid"

  c(pkey, rkey)
}

#' Generate label key for difficulty (p)
#'
#' Returns a specific translation key based on an item's difficulty value.
#'
#' @param p A numeric item difficulty value.
#' @return A character string (translation key).
#' @export
#' @examples
#' difficulty_label_key(0.3) # Hard
#' difficulty_label_key(0.7) # Medium
#' difficulty_label_key(0.9) # Easy
difficulty_label_key <- function(p){
  if (is.na(p)) return(NA_character_)
  if (p < .40)      "labels.difficulty.hard"
  else if (p <= .80)"labels.difficulty.medium"
  else              "labels.difficulty.easy"
}

#' Generate label key for discrimination (r)
#'
#' Returns a specific translation key based on an item's discrimination value.
#'
#' @param r A numeric item discrimination value.
#' @return A character string (translation key).
#' @export
#' @examples
#' discrimination_decision_key(0.15) # Remove
#' discrimination_decision_key(0.25) # Consider
#' discrimination_decision_key(0.4)  # Keep
discrimination_decision_key <- function(r){
  if (is.na(r)) return(NA_character_)
  if (r < .20)      "labels.discrimination.remove"
  else if (r < .30) "labels.discrimination.consider_remove"
  else              "labels.discrimination.keep"
}

#' Detect ID columns using regex
#'
#' Searches a vector of column names for common ID-related patterns.
#'
#' @param cols A character vector of column names.
#' @return A character vector of names that matched the ID pattern.
#' @export
#' @examples
#' detect_id_cols(c("Ad", "Soyad", "ogrenci no", "Madde1", "StudentID"))
detect_id_cols <- function(cols){
  if(length(cols)==0) return(character(0))
  rx <- "(^id$|kimlik|ogrenci|\u00f6\u011frenci|no$|numara$|student\\s*id)"
  cols[grepl(rx, cols, ignore.case = TRUE, perl = TRUE)]
}

#' Score True/False items as 1/0
#'
#' Scores a True/False (Doğru/Yanlış) response vector against a key.
#'
#' @param x A vector of student responses.
#' @param key The correct answer key (e.g., "D", "Y", "TRUE", "FALSE").
#' @return An integer vector (1=correct, 0=wrong, NA=invalid).
#' @export
#' @examples
#' parse_tf_bin(c("D", "Y", "DOGRU", "False", "Belki"), key = "D")
parse_tf_bin <- function(x, key){
  norm <- function(v){
    v <- as.character(v) %>% stringr::str_to_upper() %>% stringr::str_trim()
    dplyr::case_when(v%in%c("D","DOGRU","DO\u011eRU","TRUE","T")~"D",
                     v%in%c("Y","YANLIS","YANLI\u015e","FALSE","F")~"Y",
                     TRUE~NA_character_)
  }
  as.integer(norm(x) == norm(key))
}

#' Score Multiple Choice items as 1/0
#'
#' Scores a Multiple Choice (A-E) response vector against a key.
#'
#' @param x A vector of student responses.
#' @param key The correct answer key (e.g., "A", "B").
#' @return An integer vector (1=correct, 0=wrong, NA=invalid).
#' @export
#' @examples
#' parse_mc_bin(c("a", "B", "c", "F", " b "), key = "B")
parse_mc_bin <- function(x, key){
  xN<-as.character(x) %>% stringr::str_to_upper() %>% stringr::str_trim()
  kN<-as.character(key) %>% stringr::str_to_upper() %>% stringr::str_trim()
  xN<-ifelse(xN%in%c("A","B","C","D","E"),xN,NA); kN<-ifelse(kN%in%c("A","B","C","D","E"),kN,NA)
  as.integer(xN==kN)
}

#' Parse 1/0 coded data
#'
#' Validates that a vector contains only 1s, 0s, or NAs.
#'
#' @param x A vector of potential 1/0 scores.
#' @return An integer vector (1, 0, or NA).
#' @export
#' @examples
#' parse_lc_bin(c(1, 0, "1", "0", 2, "A", NA))
parse_lc_bin <- function(x){
  v <- suppressWarnings(as.numeric(as.character(x)))
  v[!(v %in% c(0,1))] <- NA
  as.integer(v)
}

#' Check if a vector is scored 0/1
#'
#' Detects if a vector (after removing NAs) contains only 0 and 1.
#'
#' @param vec The vector to check.
#' @return TRUE if the vector is 0/1 scored, FALSE otherwise.
#' @export
#' @examples
#' is_scored_01(c(1, 0, 1, 0, NA))
#' is_scored_01(c(1, 0, 2, 0))
#' is_scored_01(c("A", "B", "C"))
is_scored_01 <- function(vec){
  u <- unique(na.omit(suppressWarnings(as.numeric(as.character(vec)))))
  length(u) > 0 && all(u %in% c(0,1))
}

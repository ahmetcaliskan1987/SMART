#' @importFrom stats na.omit var cor
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when
#' @importFrom stringr str_to_upper str_trim
#' @importFrom tibble tibble
#' @importFrom htmltools span

#-------------------------------------------------------------------------------
# Operators and Core Calculations (Internal)
#-------------------------------------------------------------------------------

#' Null-coalescing operator (internal)
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Mode calculation (internal)
#' @noRd
d_mode <- function(x){ z <- table(x); if(length(z)==0) return(NA); as.numeric(names(z)[which.max(z)]) }

#' KR-20 reliability coefficient (internal)
#' @noRd
kr20 <- function(m){
  if(is.null(m)||ncol(m)<2) return(NA_real_)
  k<-ncol(m); tot<-rowSums(m,na.rm=TRUE)
  vt<-stats::var(tot,na.rm=TRUE); p<-colMeans(m,na.rm=TRUE); q<-1-p; s2<-sum(p*q,na.rm=TRUE)
  if(is.na(vt)||vt==0) return(NA_real_)
  (k/(k-1))*(1 - s2/vt)
}

#' Point-biserial correlation (internal)
#' @noRd
pbiserial_rest <- function(item, rest){
  if(all(is.na(item)) || length(unique(na.omit(item)))<2) return(NA_real_)
  tryCatch(stats::cor(item, rest, use="pairwise.complete.obs"), error=function(e) NA_real_)
}

#' Normalize letter grades (internal)
#' @noRd
norm_letter <- function(x){
  v <- as.character(x) %>%
    stringr::str_to_upper() %>%
    stringr::str_trim()
  ifelse(v%in%c("A","B","C","D","E"), v, NA)
}

#' Calculate q index (1-p) (internal)
#' @noRd
q_index <- function(p) 1-p

#-------------------------------------------------------------------------------
# Helper Functions for Shiny UI/Server (All Internal - @noRd)
#-------------------------------------------------------------------------------

#' Student-level counts (Correct/Incorrect/Missing) (internal)
#' @noRd
student_counts <- function(sc){
  df <- as.data.frame(sc)
  if (ncol(df)==0) return(tibble::tibble(Dogru=rep(0, nrow(df)), Yanlis=rep(0, nrow(df)), Bos=rep(0, nrow(df))))
  tibble::tibble(
    Dogru = unname(rowSums(df == 1, na.rm = TRUE)),
    Yanlis = unname(rowSums(df == 0, na.rm = TRUE)),
    Bos    = unname(rowSums(is.na(df)))
  )
}

#' Parse raw likert/continuous scores (internal)
#' @noRd
parse_lc_raw <- function(x){
  v <- suppressWarnings(as.numeric(as.character(x)))
  v[is.na(v)] <- NA_real_
  v[v < 0 | v > 1e6] <- NA_real_
  v
}

#' Normalize column names (split by comma/semicolon, trim) (internal)
#' @noRd
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

#' Safely get quantile from psychometric::item.exam (internal)
#' @noRd
get_itemexam_quant <- function(){
  q <- 0.27
  if (requireNamespace("psychometric", quietly = TRUE)) {
    res <- tryCatch(as.numeric(formals(psychometric::item.exam)$quant), error = function(e) NULL)
    if (!is.null(res) && length(res) == 1 && !is.na(res)) q <- res
  }
  return(q)
}

#' Create a colored HTML span badge (UI helper) (internal)
#' @noRd
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

#' Generate translation keys for overall comments (internal)
#' @noRd
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

#' Generate label key for difficulty (p) (internal)
#' @noRd
difficulty_label_key <- function(p){
  if (is.na(p)) return(NA_character_)
  if (p < .40)      "labels.difficulty.hard"
  else if (p <= .80)"labels.difficulty.medium"
  else              "labels.difficulty.easy"
}

#' Generate label key for discrimination (r) (internal)
#' @noRd
discrimination_decision_key <- function(r){
  if (is.na(r)) return(NA_character_)
  if (r < .20)      "labels.discrimination.remove"
  else if (r < .30) "labels.discrimination.consider_remove"
  else              "labels.discrimination.keep"
}

#' Detect ID columns using regex (internal)
#' @noRd
detect_id_cols <- function(cols){
  if(length(cols)==0) return(character(0))
  rx <- "(^id$|kimlik|ogrenci|\u00f6\u011frenci|no$|numara$|student\\s*id)"
  cols[grepl(rx, cols, ignore.case = TRUE, perl = TRUE)]
}

#' Score True/False items as 1/0 (internal)
#' @noRd
parse_tf_bin <- function(x, key){
  norm <- function(v){
    v <- as.character(v) %>% stringr::str_to_upper() %>% stringr::str_trim()
    dplyr::case_when(v%in%c("D","DOGRU","DO\u011eRU","TRUE","T")~"D",
                     v%in%c("Y","YANLIS","YANLI\u015e","FALSE","F")~"Y",
                     TRUE~NA_character_)
  }
  as.integer(norm(x) == norm(key))
}

#' Score Multiple Choice items as 1/0 (internal)
#' @noRd
parse_mc_bin <- function(x, key){
  xN<-as.character(x) %>% stringr::str_to_upper() %>% stringr::str_trim()
  kN<-as.character(key) %>% stringr::str_to_upper() %>% stringr::str_trim()
  xN<-ifelse(xN%in%c("A","B","C","D","E"),xN,NA); kN<-ifelse(kN%in%c("A","B","C","D","E"),kN,NA)
  as.integer(xN==kN)
}

#' Parse 1/0 coded data (internal)
#' @noRd
parse_lc_bin <- function(x){
  v <- suppressWarnings(as.numeric(as.character(x)))
  v[!(v %in% c(0,1))] <- NA
  as.integer(v)
}

#' Check if a vector is scored 0/1 (internal)
#' @noRd
is_scored_01 <- function(vec){
  u <- unique(na.omit(suppressWarnings(as.numeric(as.character(vec)))))
  length(u) > 0 && all(u %in% c(0,1))
}


<!-- README.md is generated from README.Rmd. Please edit that file -->

# SMART

<!-- badges: start -->

<!-- badges: end -->

The goal of SMART is to provide a Shiny-based toolkit for comprehensive
item/test analysis. It is designed to handle multiple-choice,
true-false, and open-ended questions from datasets in 1-0 or other
formats, delivering key analyses such as difficulty, discrimination,
distractor analysis, student reports, and DOCX exports.

## Installation

Install the package directly from GitHub using the `devtools` package:

``` r
# First install devtools if not already installed:
install.packages("devtools")

# Then install SMART from GitHub:
devtools::install_github("ahmetcaliskan1987/SMART")
```

## Usage

After installation, load the library and launch the Shiny application:

``` r
library(SMART)
run_app()
```

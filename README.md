# readcamp

**readcamp** is a simple R package to munge DataCamp's group exports into a tidy
format. It is useful for group administrators who wish to understand how their
students are doing, and prefer to perform this analysis in R.

## Installation

This package is not yet available on CRAN. You can install the development
version from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("atheriel/readcamp")
```

## Usage

**readcamp** exposes one main function that reads all course & chapter grades
and completion information from the `.zip` archive downloaded from the DataCamp
website. For example:

```r
x <- readcamp::read_group_export("group_export_XXXX.zip")
x
#> A DataCamp Group Export containing the following course dataset:
#> # A tibble: ?? x 10
#>     user_id                                   course course_id  chapter
#>       <chr>                                    <chr>     <int>    <chr>
#>  1 93ea1f54                        introduction to r        58 Overview
#>  2 93ea1f54 data visualization with ggplot2 (part 1)       774 Overview
#>  3 93ea1f54                           intermediate r       672 Overview
#>  4 93ea1f54        data manipulation in r with dplyr       625 Overview
#>  5 93ea1f54             importing data in r (part 1)      1477 Overview
#>  6 93ea1f54                intermediate r - practice       753 Overview
#>  7 93ea1f54             importing data in r (part 2)      1478 Overview
#>  8 93ea1f54                     introduction to data      1800 Overview
#>  9 93ea1f54                  data visualization in r      1498 Overview
#> 10 93ea1f54       data visualization in r with ggvis       638 Overview
#> # ... with ?? more rows, and 6 more variables: free <lgl>,
#> #   status <fctr>, start_date <chr>, completed_date <chr>, grade <dbl>,
#> #   completion <dbl>
```

As you can see, this will return an object containing course- and chapter-level
start and end dates, grades, and completion, in a tidy data format. To extract
the course data frame itself, use the `readcamp::courses()` method:

```r
readcamp::courses(x)
#> # A tibble: ?? x 10
#>     user_id                                   course course_id  chapter
#>       <chr>                                    <chr>     <int>    <chr>
#>  1 93ea1f54                        introduction to r        58 Overview
#>  2 93ea1f54 data visualization with ggplot2 (part 1)       774 Overview
#>  3 93ea1f54                           intermediate r       672 Overview
#>  4 93ea1f54        data manipulation in r with dplyr       625 Overview
#>  5 93ea1f54             importing data in r (part 1)      1477 Overview
#>  6 93ea1f54                intermediate r - practice       753 Overview
#>  7 93ea1f54             importing data in r (part 2)      1478 Overview
#>  8 93ea1f54                     introduction to data      1800 Overview
#>  9 93ea1f54                  data visualization in r      1498 Overview
#> 10 93ea1f54       data visualization in r with ggvis       638 Overview
#> # ... with ?? more rows, and 6 more variables: free <lgl>,
#> #   status <fctr>, start_date <chr>, completed_date <chr>, grade <dbl>,
#> #   completion <dbl>
```

By default, user names and emails are hashed into the `user_id` column, but
should you wish to retrieve the underlying account information, use the
`readcamp::users()` method to view the data frame mapping users to their ids.

## Course Metadata

Since the package uses a metadata file (see `inst/course_metadata.csv`) to map
course names to IDs and to identify free courses, it is possible that this file
will become out-of-date. The `read_group_export()` function will warn you if
there are courses with no IDs or names, and you can diagnose these with the
`missing_courses()` method.

If you do find errors or needed updates in the course metadata, please contact
me on Slack, or submit a PR making the change directly.

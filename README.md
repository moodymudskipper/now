
<!-- README.md is generated from README.Rmd. Please edit that file -->

# now

Experimental!

Forget about lifecycles, live in the {now}.

`now::clean_up()` removes functions of given life cycles from the
namespace’s exports so they’re not attached with `library()` and not
available or suggested with `::`.

Why would we want that ?

- You might not want to use deprecated functions, to use best current
  practice
- You might not want to use experimental functions, because you need to
  write stable code
- You might not want to run into defunct functions, that exist only to
  fail
- You might want to explore a package without being distracted by
  outdated or unstable features.

In short:

- `clean_up(pkgs)` is
  `clean_up(pkgs, remove = getOption("now.lifecycles", c("superseded", "deprecated", "defunct")))`.
- Additional possible options are `"experimental"`, `"questioning"`, and
  `"reexports"`, the latter is not a life cycle, but allows you to drop
  reexports so you’re forced to use the source package when programming.
- For instance use`clean_up(pkg, lifecycle = "defunct")` to cleanup only
  defunct functions, and not break anything.
- Use`clean_up_tidyverse()` to do the above on all tidyverse packages at
  once.
- `clean_attach()` and `clean_attach_tidyverse()` are wrappers that call
  library

> **It’s not meant to be used in production, because it will break some
> dependencies that rely on the features you’ve removed**

Obviously life cycles are important and there are reasons why they
exist, they just sometimes get in the way.

It works by looking at the doc and the code of a function, but we might
miss some “life-cycled” functions if the doc doesn’t clearly use life
cycle tags and the code doesn’t call a {lifecycle} function directly.

## Installation

Install with

``` r
remotes::install_github("moodymudskipper/now")
```

## Example

``` r
library(tidyverse)
before <- sapply(tidyverse_packages(), \(pkg) length(getNamespaceExports(pkg)))
do
#> function (.data, ...) 
#> {
#>     lifecycle::signal_stage("superseded", "do()")
#>     UseMethod("do")
#> }
#> <bytecode: 0x11a1adc80>
#> <environment: namespace:dplyr>
location
#> function (df) 
#> {
#>     lifecycle::deprecate_stop("1.0.0", "location()", "lobstr::ref()")
#> }
#> <bytecode: 0x12e2df648>
#> <environment: namespace:dplyr>

now::clean_up_tidyverse()
after <- sapply(tidyverse_packages(), \(pkg) length(getNamespaceExports(pkg)))
do
#> Error in eval(expr, envir, enclos): object 'do' not found
location
#> Error in eval(expr, envir, enclos): object 'location' not found
dplyr::do
#> Error: 'do' is not an exported object from 'namespace:dplyr'

# expect some failures! Many packages use superseded functions in particular
library(dm)
#> Error: package or namespace load failed for 'dm':
#>  object 'separate' not found whilst loading namespace 'dm'
```

To avoid the latter, either :

- call `library(dm)` before cleaning up (these dependencies would be
  copied in dm’s imports env so `dm::separate()` would work but
  `tidyr::separate()` wouldn’t)
- call `clean_up_tidyverse(force_keep = c("separate", "transmute"))`
  above to keep the necessary dependencies around.
- call `clean_up("dm")` or `clean_attach("dm")` before
  `clean_up_tidyverse()` and don’t be bothered with {dm}’s re-exports

Let’s take a look at how much we can expect to trim off

``` r
# removing only superseded/deprecated/defunct
left_join(enframe(before, value = "before"), enframe(after, value = "after"), by = "name") |> 
  filter(after != before) |>
  mutate(keep_ratio = sprintf("%2.0f%%", 100*after/before)) |>
  arrange(keep_ratio) |>
  knitr::kable()
```

| name          | before | after | keep_ratio |
|:--------------|-------:|------:|:-----------|
| rvest         |     40 |    26 | 65%        |
| purrr         |    189 |   124 | 66%        |
| dplyr         |    293 |   196 | 67%        |
| tidyr         |     70 |    49 | 70%        |
| tibble        |     46 |    34 | 74%        |
| hms           |     12 |    10 | 83%        |
| rlang         |    440 |   384 | 87%        |
| googledrive   |     87 |    77 | 89%        |
| pillar        |     45 |    40 | 89%        |
| readr         |    115 |   107 | 93%        |
| forcats       |     39 |    38 | 97%        |
| stringr       |     59 |    57 | 97%        |
| googlesheets4 |     53 |    52 | 98%        |
| dbplyr        |    162 |   161 | 99%        |
| ggplot2       |    536 |   529 | 99%        |

``` r

# removing additionally all experimental and questioning features, along with reexports
now::clean_up_tidyverse(c("experimental", "superseded", "deprecated", "defunct", "questioning", "reexports"))
after <- sapply(tidyverse_packages(), \(pkg) length(getNamespaceExports(pkg)))
left_join(enframe(before, value = "before"), enframe(after, value = "after"), by = "name") |> 
  filter(after != before) |>
  mutate(keep_ratio = sprintf("%2.0f%%", 100*after/before)) |>
  arrange(keep_ratio) |>
  knitr::kable()
```

| name          | before | after | keep_ratio |
|:--------------|-------:|------:|:-----------|
| tidyr         |     70 |    30 | 43%        |
| purrr         |    189 |    91 | 48%        |
| tibble        |     46 |    22 | 48%        |
| dplyr         |    293 |   148 | 51%        |
| rvest         |     40 |    23 | 58%        |
| ragg          |     10 |     6 | 60%        |
| pillar        |     45 |    28 | 62%        |
| broom         |      9 |     6 | 67%        |
| readxl        |     13 |     9 | 69%        |
| rlang         |    440 |   340 | 77%        |
| jsonlite      |     28 |    23 | 82%        |
| hms           |     12 |    10 | 83%        |
| googledrive   |     87 |    76 | 87%        |
| googlesheets4 |     53 |    47 | 89%        |
| readr         |    115 |   107 | 93%        |
| dbplyr        |    162 |   153 | 94%        |
| forcats       |     39 |    37 | 95%        |
| stringr       |     59 |    56 | 95%        |
| ggplot2       |    536 |   512 | 96%        |
| haven         |     28 |    27 | 96%        |
| lubridate     |    205 |   198 | 97%        |
| modelr        |     34 |    33 | 97%        |

functions
================
Xuehan Yang
2021/11/16

``` r
library(tidyverse)
library(rvest)

set.seed(1)
```

## My first function

``` r
x_vec = rnorm(50, mean = 5, sd = 3)

(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1] -0.87431729  0.10006694 -1.12591259  1.79798344  0.27551259 -1.10767788
    ##  [7]  0.46546018  0.76723729  0.57173026 -0.48814004  1.69755015  0.34808404
    ## [13] -0.86804682 -2.78465849  1.23224695 -0.17486523 -0.14029275  1.01442637
    ## [19]  0.86694513  0.59352495  0.98452618  0.81993384 -0.03113241 -2.51360985
    ## [25]  0.62470683 -0.18833072 -0.30820984 -1.88983899 -0.69593765  0.38188068
    ## [31]  1.51339964 -0.24445212  0.34547201 -0.18553578 -1.77714533 -0.61997427
    ## [37] -0.59507078 -0.19216123  1.20229058  0.79712811 -0.31870798 -0.42556236
    ## [43]  0.71748791  0.54873496 -0.94925395 -0.97179376  0.31769980  0.80357171
    ## [49] -0.25594906  0.93897660

``` r
z_score = function(x){
  z = (x - mean(x))/sd(x)
  return(z)
}
```

``` r
z_score(x_vec)
```

    ##  [1] -0.87431729  0.10006694 -1.12591259  1.79798344  0.27551259 -1.10767788
    ##  [7]  0.46546018  0.76723729  0.57173026 -0.48814004  1.69755015  0.34808404
    ## [13] -0.86804682 -2.78465849  1.23224695 -0.17486523 -0.14029275  1.01442637
    ## [19]  0.86694513  0.59352495  0.98452618  0.81993384 -0.03113241 -2.51360985
    ## [25]  0.62470683 -0.18833072 -0.30820984 -1.88983899 -0.69593765  0.38188068
    ## [31]  1.51339964 -0.24445212  0.34547201 -0.18553578 -1.77714533 -0.61997427
    ## [37] -0.59507078 -0.19216123  1.20229058  0.79712811 -0.31870798 -0.42556236
    ## [43]  0.71748791  0.54873496 -0.94925395 -0.97179376  0.31769980  0.80357171
    ## [49] -0.25594906  0.93897660

``` r
z_score(3)
```

    ## [1] NA

``` r
z_score("Sean")
```

    ## Error in x - mean(x): 二进列运算符中有非数值参数

``` r
z_score(iris)
```

    ## Error in is.data.frame(x): 'list' object cannot be coerced to type 'double'

``` r
z_score(sample(c(TRUE, FALSE), 25, replace = TRUE))
```

    ##  [1] -0.9413574 -0.9413574 -0.9413574  1.0198039  1.0198039  1.0198039
    ##  [7]  1.0198039 -0.9413574  1.0198039  1.0198039  1.0198039  1.0198039
    ## [13]  1.0198039 -0.9413574 -0.9413574 -0.9413574 -0.9413574  1.0198039
    ## [19] -0.9413574 -0.9413574 -0.9413574 -0.9413574  1.0198039  1.0198039
    ## [25] -0.9413574

check the argument calues using conditional statements.

``` r
z_scores = function(x){
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric!")
  } else if (length(x) == 1) {
    stop("Z score cannot be computed for length 1 vectors")
  }

z = mean(x) / sd(x)

z
}
```

## Multiple outputs

``` r
mean_and_sd = function(x) {
  
   if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  list(mean = mean_x, sd = sd_x)
  
  #tibble(mean = mean_x, sd = sd_x)
}
```

``` r
mean_and_sd(x_vec)
```

    ## $mean
    ## [1] 5.301345
    ## 
    ## $sd
    ## [1] 2.494182

## Multiple inputs

``` r
sim_data = tibble(
  x = rnorm(30, mean = 2, sd = 3)
)

sim_data %>% 
  summarise(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.07      2.28

``` r
sim_mean_sd = function(n, mu=2, sigma=2) {
  sim_data = tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )
  
  sim_data %>% 
    summarise(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
}

sim_mean_sd(1000,2,2)
```

    ## # A tibble: 1 x 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   1.94      2.02

## Scraping Amazon

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=10"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

``` r
read_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(vec_urls[1]),
  read_page_reviews(vec_urls[2]),
  read_page_reviews(vec_urls[3]),
  read_page_reviews(vec_urls[4]),
  read_page_reviews(vec_urls[5])
)

dynamite_reviews
```

    ## # A tibble: 50 x 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 Yeah., it was pretty good.                                5 "Yeah, it was pr~
    ##  2 Love it                                                   5 "Didn't like thi~
    ##  3 it was                                                    5 "mad good yo"    
    ##  4 Fun!                                                      4 "Fun and enterta~
    ##  5 Vintage                                                   5 "Easy to order. ~
    ##  6 too many commercials                                      1 "5 minutes into ~
    ##  7 this film is so good!                                     5 "VOTE FOR PEDRO!"
    ##  8 Good movie                                                5 "Weird story, go~
    ##  9 I Just everyone to know this....                          5 "VOTE FOR PEDRO ~
    ## 10 the cobweb in his hair during the bike ramp scene lol     5 "5 stars for bei~
    ## # ... with 40 more rows

## Functions as arguments

``` r
x_vec = rnorm(25, 0, 1)

my_summary = function(x, summ_func) {
  summ_func(x)
}

my_summary(x_vec, sd)
```

    ## [1] 1.051502

``` r
my_summary(x_vec, IQR)
```

    ## [1] 1.591179

## Scoping and names

``` r
f = function(x) {
  #y = 80
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

cross validation
================
nz2333
12/4/2021

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-36. For overview type 'help("mgcv-package")'.

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## simulate data and look at the data

``` r
nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, .3)
  )

nonlin_df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
```

<img src="Cross-validation_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

## CV by hand, split training data and test data

``` r
train_df = sample_n(nonlin_df, 80)
test_df = anti_join(nonlin_df, train_df, by = "id")

ggplot(train_df, aes(x = x, y = y)) + 
  geom_point() + 
  geom_point(data = test_df, color = "red")
```

<img src="Cross-validation_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

## Fit 3 models

``` r
linear_mod = lm(y ~ x, data = train_df)
smooth_mod = mgcv::gam(y ~ s(x), data = train_df)
wiggly_mod = mgcv::gam(y ~ s(x, k = 30), sp = 10e-6, data = train_df)
```

look at what I did.

``` r
train_df %>% 
  add_predictions(smooth_mod) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + 
  geom_line(aes(y = pred), color = "red")
```

<img src="Cross-validation_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

``` r
train_df %>% 
  add_predictions(wiggly_mod) %>% 
  ggplot(aes(x = x, y = y)) + geom_point() + 
  geom_line(aes(y = pred), color = "red")
```

<img src="Cross-validation_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

``` r
train_df %>% 
  gather_predictions(linear_mod, smooth_mod, wiggly_mod) %>% 
  mutate(model = fct_inorder(model)) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + 
  geom_line(aes(y = pred), color = "red") + 
  facet_wrap(~model)
```

<img src="Cross-validation_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

Look at prediction accuracy

``` r
rmse(linear_mod, test_df)
```

    ## [1] 0.9166625

``` r
rmse(smooth_mod, test_df)
```

    ## [1] 0.2650594

``` r
rmse(wiggly_mod, test_df)
```

    ## [1] 0.2626697

## CV by modelr

``` r
cv_df = 
  crossv_mc(nonlin_df, 100) 
```

what is happening

``` r
cv_df %>% pull(train) %>% .[[1]] %>% as_tibble
```

    ## # A tibble: 79 x 3
    ##       id      x      y
    ##    <int>  <dbl>  <dbl>
    ##  1     2 0.139   0.839
    ##  2     4 0.218   1.33 
    ##  3     5 0.714  -0.691
    ##  4     7 0.794  -1.09 
    ##  5     8 0.230   1.18 
    ##  6     9 0.900  -2.62 
    ##  7    10 0.256   0.510
    ##  8    12 0.0363  0.742
    ##  9    13 0.920  -2.78 
    ## 10    14 0.0678  0.350
    ## # ... with 69 more rows

``` r
cv_df %>% pull(test) %>% .[[1]] %>% as_tibble
```

    ## # A tibble: 21 x 3
    ##       id     x       y
    ##    <int> <dbl>   <dbl>
    ##  1     1 0.638 -0.282 
    ##  2     3 0.177  0.852 
    ##  3     6 0.846 -1.97  
    ##  4    11 0.751 -0.701 
    ##  5    20 0.364  1.04  
    ##  6    26 0.116  0.0391
    ##  7    28 0.699 -0.303 
    ##  8    31 0.612  0.189 
    ##  9    38 0.776 -1.53  
    ## 10    44 0.535  1.42  
    ## # ... with 11 more rows

``` r
cv_df =
  cv_df %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble))
```

fir models and get RMSEs for them.

``` r
cv_df = 
  cv_df %>% 
  mutate(
    linear_mod  = map(train, ~lm(y ~ x, data = .x)),
    smooth_mod  = map(train, ~mgcv::gam(y ~ s(x), data = .x)),
    wiggly_mod  = map(train, ~gam(y ~ s(x, k = 30), sp = 10e-6, data = .x))) %>% 
  mutate(
    rmse_linear = map2_dbl(linear_mod, test, ~rmse(model = .x, data = .y)),
    rmse_smooth = map2_dbl(smooth_mod, test, ~rmse(model = .x, data = .y)),
    rmse_wiggly = map2_dbl(wiggly_mod, test, ~rmse(model = .x, data = .y)))
```

making plot for RMSEs across different models.

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model", 
    values_to = "rmse",
    names_prefix = "rmse_") %>% 
  mutate(model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()
```

<img src="Cross-validation_files/figure-gfm/unnamed-chunk-13-1.png" width="90%" />

Practice Exam
================
Karl Rohe
2/27/2020

Practice Exam
=============

This practice exam asks you to do several code wrangling tasks that we have done in class so far.

Clone this repo into Rstudio and fill in the necessary code. Then, commit and push to github. Finally, turn in a link to canvas.

    ## Warning: package 'tidyverse' was built under R version 3.6.2

    ## -- Attaching packages -------------------------------------------------------------------- tidyverse 1.3.0 --

    ## <U+2713> ggplot2 3.2.1     <U+2713> purrr   0.3.3
    ## <U+2713> tibble  2.1.3     <U+2713> dplyr   0.8.3
    ## <U+2713> tidyr   1.0.0     <U+2713> stringr 1.4.0
    ## <U+2713> readr   1.3.1     <U+2713> forcats 0.4.0

    ## Warning: package 'ggplot2' was built under R version 3.6.2

    ## Warning: package 'tidyr' was built under R version 3.6.2

    ## Warning: package 'dplyr' was built under R version 3.6.2

    ## -- Conflicts ----------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## Warning: package 'nycflights13' was built under R version 3.6.2

Make a plot with three facets, one for each airport in the weather data. The x-axis should be the day of the year (1:365) and the y-axis should be the mean temperature recorded on that day, at that airport.

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 3.6.2

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
weather %>% mutate(day_of_year = yday(time_hour)) %>% 
  group_by(origin, day_of_year) %>% 
  summarise(temp = mean(temp), na.rm = T) %>% 
  ggplot(aes(x = day_of_year, y = temp)) + 
    geom_point() + facet_wrap(~origin)
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

Make a non-tidy matrix of that data where each row is an airport and each column is a day of the year.

``` r
weather %>% mutate(day_of_year = yday(time_hour)) %>% 
  group_by(origin, day_of_year) %>% 
  summarise(temp = mean(temp), na.rm = T) %>% 
  pivot_wider(names_from = day_of_year, values_from = temp) %>% 
  select(-na.rm)
```

    ## # A tibble: 3 x 365
    ## # Groups:   origin [3]
    ##   origin   `1`   `2`   `3`   `4`   `5`   `6`   `7`   `8`   `9`  `10`  `11`  `12`
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 EWR     36.8  28.7  29.6  34.3  36.6  39.9  40.3  38.6  42.1  43.6  42.0  46.0
    ## 2 JFK     36.9  28.6  30.1  34.7  36.8  39.3  40.1  39.4  42.7  43.6  41.3  45.0
    ## 3 LGA     37.2  28.8  30.3  35.8  38.3  41.0  41.4  42.3  44.9  44.3  40.3  43.9
    ## # … with 352 more variables: `13` <dbl>, `14` <dbl>, `15` <dbl>, `16` <dbl>,
    ## #   `17` <dbl>, `18` <dbl>, `19` <dbl>, `20` <dbl>, `21` <dbl>, `22` <dbl>,
    ## #   `23` <dbl>, `24` <dbl>, `25` <dbl>, `26` <dbl>, `27` <dbl>, `28` <dbl>,
    ## #   `29` <dbl>, `30` <dbl>, `31` <dbl>, `32` <dbl>, `33` <dbl>, `34` <dbl>,
    ## #   `35` <dbl>, `36` <dbl>, `37` <dbl>, `38` <dbl>, `39` <dbl>, `40` <dbl>,
    ## #   `41` <dbl>, `42` <dbl>, `43` <dbl>, `44` <dbl>, `45` <dbl>, `46` <dbl>,
    ## #   `47` <dbl>, `48` <dbl>, `49` <dbl>, `50` <dbl>, `51` <dbl>, `52` <dbl>,
    ## #   `53` <dbl>, `54` <dbl>, `55` <dbl>, `56` <dbl>, `57` <dbl>, `58` <dbl>,
    ## #   `59` <dbl>, `60` <dbl>, `61` <dbl>, `62` <dbl>, `63` <dbl>, `64` <dbl>,
    ## #   `65` <dbl>, `66` <dbl>, `67` <dbl>, `68` <dbl>, `69` <dbl>, `70` <dbl>,
    ## #   `71` <dbl>, `72` <dbl>, `73` <dbl>, `74` <dbl>, `75` <dbl>, `76` <dbl>,
    ## #   `77` <dbl>, `78` <dbl>, `79` <dbl>, `80` <dbl>, `81` <dbl>, `82` <dbl>,
    ## #   `83` <dbl>, `84` <dbl>, `85` <dbl>, `86` <dbl>, `87` <dbl>, `88` <dbl>,
    ## #   `89` <dbl>, `90` <dbl>, `91` <dbl>, `92` <dbl>, `93` <dbl>, `94` <dbl>,
    ## #   `95` <dbl>, `96` <dbl>, `97` <dbl>, `98` <dbl>, `99` <dbl>, `100` <dbl>,
    ## #   `101` <dbl>, `102` <dbl>, `103` <dbl>, `104` <dbl>, `105` <dbl>,
    ## #   `106` <dbl>, `107` <dbl>, `108` <dbl>, `109` <dbl>, `110` <dbl>,
    ## #   `111` <dbl>, `112` <dbl>, …

For each (airport, day) contruct a tidy data set of the airport's "performance" as the proportion of flights that departed less than an hour late.

``` r
late.performance = 
  flights %>% mutate(day_of_year = yday(time_hour)) %>% 
  group_by(origin, day_of_year) %>% 
  summarize(performance = mean((dep_delay < 60), na.rm = T))

late.performance
```

    ## # A tibble: 1,095 x 3
    ## # Groups:   origin [3]
    ##    origin day_of_year performance
    ##    <chr>        <dbl>       <dbl>
    ##  1 EWR              1       0.918
    ##  2 EWR              2       0.837
    ##  3 EWR              3       0.979
    ##  4 EWR              4       0.935
    ##  5 EWR              5       0.966
    ##  6 EWR              6       0.95 
    ##  7 EWR              7       0.921
    ##  8 EWR              8       0.982
    ##  9 EWR              9       0.976
    ## 10 EWR             10       0.980
    ## # … with 1,085 more rows

Construct a tidy data set to that give weather summaries for each (airport, day). Use the total precipitation, minimum visibility, maximum wind\_gust, and average wind\_speed.

``` r
weather.final = weather %>% mutate(day_of_year = yday(time_hour)) %>% 
  group_by(origin, day_of_year) %>% 
  summarise(precipitation = sum(precip, na.rm = T),
            visibility = min(visib, na.rm = T),
            max_wind_gust = max(wind_gust, na.rm = T),
            wind_speed = mean(wind_speed, na.rm = T))
```

Construct a linear model to predict the performance of each (airport,day) using the weather summaries and a "fixed effect" for each airport. Display the summaries.

``` r
final.data = left_join(weather.final, late.performance)
```

    ## Joining, by = c("origin", "day_of_year")

``` r
summary(lm(performance ~ origin+precipitation+visibility+wind_speed, data = final.data))
```

    ## 
    ## Call:
    ## lm(formula = performance ~ origin + precipitation + visibility + 
    ##     wind_speed, data = final.data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.45572 -0.01734  0.01755  0.03704  0.18800 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    0.8605210  0.0082091 104.825  < 2e-16 ***
    ## originJFK      0.0200180  0.0052986   3.778 0.000167 ***
    ## originLGA      0.0209328  0.0052113   4.017 6.31e-05 ***
    ## precipitation -0.0531217  0.0077869  -6.822 1.49e-11 ***
    ## visibility     0.0081510  0.0006977  11.682  < 2e-16 ***
    ## wind_speed    -0.0010823  0.0005174  -2.092 0.036684 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0698 on 1086 degrees of freedom
    ## Multiple R-squared:  0.2532, Adjusted R-squared:  0.2498 
    ## F-statistic: 73.65 on 5 and 1086 DF,  p-value: < 2.2e-16

Repeat the above, but only for EWR. Obviously, exclude the fixed effect for each airport.

``` r
final.data %>% 
  filter(origin == 'EWR')
```

    ## # A tibble: 364 x 7
    ## # Groups:   origin [1]
    ##    origin day_of_year precipitation visibility max_wind_gust wind_speed
    ##    <chr>        <dbl>         <dbl>      <dbl>         <dbl>      <dbl>
    ##  1 EWR              1             0         10          26.5      13.2 
    ##  2 EWR              2             0         10          26.5      10.9 
    ##  3 EWR              3             0         10        -Inf         8.58
    ##  4 EWR              4             0         10          31.1      14.0 
    ##  5 EWR              5             0         10          26.5       9.40
    ##  6 EWR              6             0          6          19.6       9.11
    ##  7 EWR              7             0         10          24.2       7.34
    ##  8 EWR              8             0          8          25.3       7.19
    ##  9 EWR              9             0          6          20.7       5.99
    ## 10 EWR             10             0         10          26.5       8.92
    ## # … with 354 more rows, and 1 more variable: performance <dbl>

``` r
summary(lm(performance ~ origin+precipitation+visibility+wind_speed, data = final.data))
```

    ## 
    ## Call:
    ## lm(formula = performance ~ origin + precipitation + visibility + 
    ##     wind_speed, data = final.data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.45572 -0.01734  0.01755  0.03704  0.18800 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    0.8605210  0.0082091 104.825  < 2e-16 ***
    ## originJFK      0.0200180  0.0052986   3.778 0.000167 ***
    ## originLGA      0.0209328  0.0052113   4.017 6.31e-05 ***
    ## precipitation -0.0531217  0.0077869  -6.822 1.49e-11 ***
    ## visibility     0.0081510  0.0006977  11.682  < 2e-16 ***
    ## wind_speed    -0.0010823  0.0005174  -2.092 0.036684 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0698 on 1086 degrees of freedom
    ## Multiple R-squared:  0.2532, Adjusted R-squared:  0.2498 
    ## F-statistic: 73.65 on 5 and 1086 DF,  p-value: < 2.2e-16

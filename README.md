Modleing fish weight with multiple linear regression
================

# Objective

This notebook explores using multiple linear regression to model fish
weight.

[Journal of Stats Edu data
source](http://jse.amstat.org/datasets/fishcatch.txt).

###### Species Breakdowns

| Code | Finnish | Swedish   | English      | Latin             |
| :--- | :------ | :-------- | :----------- | :---------------- |
| 1    | Lahna   | Braxen    | Bream        | Abramis brama     |
| 2    | Siika   | Iiden     | Whitefish    | Leusiscus idus    |
| 3    | Saerki  | Moerten   | Roach        | Leuciscus rutilus |
| 4    | Parkki  | Bjoerknan | Silver Bream | Abramis bjrkna    |
| 5    | Norssi  | Norssen   | Smelt        | Osmerus eperlanus |
| 6    | Hauki   | Jaedda    | Pike         | Esox lucius       |
| 7    | Ahven   | Abborre   | Perch        | Perca fluviatilis |

###### Additional Variables

| Data Variable | Description                                               |
| :------------ | :-------------------------------------------------------- |
| Weight        | Weight of the fish (in grams)                             |
| Length1       | Length from the nose to the beginning of the tail (in cm) |
| Length2       | Length from the nose to the notch of the tail (in cm)     |
| Length3       | Length from the nose to the end of the tail (in cm)       |
| Height        | Maximal height as % of Length3                            |
| Width         | Maximal width as % of Length3                             |
| Sex           | 1 = male 0 = female                                       |

<!-- | Fish Dim |  -->

<!-- |:------:| -->

<!-- |    ___/////___                   _| -->

<!-- |    /           \    ___          || -->

<!-- |  /\             \_ /  /          H| -->

<!-- |<   )            __)  \           || -->

<!-- |  \/_\\_________/   \__\          _| -->

<!-- | -->

<!-- ||------- L1 -------|| -->

<!-- ||------- L2 ----------|| -->

<!-- ||------- L3 ------------|| -->

# Load data, clean, sanity check

Warning: running this code chunk installs packages that aren’t already
installed.

``` r
required_packages <- c('MASS', 'tidyverse', 'GGally', 'faraway', 'corrplot', 
                       'broom', 'corrr', 'modelr', 'gridExtra',
                       'caret', 'leaps', 'ggfortify', 'gvlma')
for(p in required_packages) {
  if(!require(p,character.only = TRUE)) 
        install.packages(p, repos = "http://cran.us.r-project.org")
  library(p,character.only = TRUE)
}
pct_formater_1 <- scales::label_percent(accuracy = 1)
```

``` r
url <- "http://jse.amstat.org/datasets/fishcatch.dat.txt"
df_fish <- read_table(file = url, 
           col_names = c("Obs", "Species", "Weight", 
                         "Length1", "Length2", "Length3",
                         "Height", "Width", "Sex"))
glimpse(df_fish)
```

    ## Rows: 159
    ## Columns: 9
    ## $ Obs     <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18…
    ## $ Species <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ Weight  <dbl> 242, 290, 340, 363, 430, 450, 500, 390, 450, 500, 475, 500, 5…
    ## $ Length1 <dbl> 23.2, 24.0, 23.9, 26.3, 26.5, 26.8, 26.8, 27.6, 27.6, 28.5, 2…
    ## $ Length2 <dbl> 25.4, 26.3, 26.5, 29.0, 29.0, 29.7, 29.7, 30.0, 30.0, 30.7, 3…
    ## $ Length3 <dbl> 30.0, 31.2, 31.1, 33.5, 34.0, 34.7, 34.5, 35.0, 35.1, 36.2, 3…
    ## $ Height  <dbl> 38.4, 40.0, 39.8, 38.0, 36.6, 39.2, 41.1, 36.2, 39.9, 39.3, 3…
    ## $ Width   <dbl> 13.4, 13.8, 15.1, 13.3, 15.1, 14.2, 15.3, 13.4, 13.8, 13.7, 1…
    ## $ Sex     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, NA,…

Check NAs count per variable

``` r
df_fish %>% 
  map_df(~sum(is.na(.))) %>%
  gather(key="variable", value="NA_count") %>%
  filter(NA_count>=1) %>%
  mutate(percent_total_rows_NA = pct_formater_1(NA_count/nrow(df_fish)))
```

    ## # A tibble: 2 x 3
    ##   variable NA_count percent_total_rows_NA
    ##   <chr>       <int> <chr>                
    ## 1 Weight          1 1%                   
    ## 2 Sex            87 55%

Drop sex/Obs variable + drop observation with missing weight.  
Could impute the missing weight variable but since it’s the target var
deciding to drop

``` r
df_fish_clean <- df_fish %>% 
  dplyr::select(-Sex, -Obs) %>%
  filter(!is.na(Weight) & Weight != 0)

### convert Species var to factor Species name
df_fish_clean <- df_fish_clean %>% 
 mutate(Species = case_when(
                     Species == 1 ~ "Bream",
                     Species == 2 ~ "Whitefish",
                     Species == 3 ~ "Roach",
                     Species == 4 ~ "Silver Bream",
                     Species == 5 ~ "Smelt",
                     Species == 6 ~ "Pike",
                     Species == 7 ~ "Perch"),
        Species = factor(Species, ordered = FALSE)
)
```

# Data Exploration

View summary stats by species

``` r
df_fish_clean %>% split(.$Species) %>% map(summary)
```

    ## $Bream
    ##          Species       Weight          Length1         Length2     
    ##  Bream       :34   Min.   : 242.0   Min.   :23.20   Min.   :25.40  
    ##  Perch       : 0   1st Qu.: 481.2   1st Qu.:27.80   1st Qu.:30.18  
    ##  Pike        : 0   Median : 615.0   Median :30.65   Median :33.25  
    ##  Roach       : 0   Mean   : 626.0   Mean   :30.33   Mean   :33.14  
    ##  Silver Bream: 0   3rd Qu.: 718.5   3rd Qu.:31.98   3rd Qu.:35.00  
    ##  Smelt       : 0   Max.   :1000.0   Max.   :38.00   Max.   :41.00  
    ##  Whitefish   : 0                                                   
    ##     Length3          Height          Width      
    ##  Min.   :30.00   Min.   :36.20   Min.   :12.00  
    ##  1st Qu.:35.38   1st Qu.:38.50   1st Qu.:13.70  
    ##  Median :38.55   Median :39.80   Median :14.10  
    ##  Mean   :38.39   Mean   :39.59   Mean   :14.15  
    ##  3rd Qu.:40.83   3rd Qu.:40.58   3rd Qu.:14.88  
    ##  Max.   :46.50   Max.   :44.50   Max.   :15.50  
    ##                                                 
    ## 
    ## $Perch
    ##          Species       Weight          Length1         Length2     
    ##  Bream       : 0   Min.   :   5.9   Min.   : 7.50   Min.   : 8.40  
    ##  Perch       :56   1st Qu.: 120.0   1st Qu.:19.82   1st Qu.:21.82  
    ##  Pike        : 0   Median : 207.5   Median :23.25   Median :25.30  
    ##  Roach       : 0   Mean   : 382.2   Mean   :25.74   Mean   :27.89  
    ##  Silver Bream: 0   3rd Qu.: 692.5   3rd Qu.:34.12   3rd Qu.:36.62  
    ##  Smelt       : 0   Max.   :1100.0   Max.   :41.10   Max.   :44.00  
    ##  Whitefish   : 0                                                   
    ##     Length3          Height          Width      
    ##  Min.   : 8.80   Min.   :21.30   Min.   :13.20  
    ##  1st Qu.:23.32   1st Qu.:24.77   1st Qu.:15.00  
    ##  Median :26.75   Median :26.25   Median :15.55  
    ##  Mean   :29.57   Mean   :26.26   Mean   :15.84  
    ##  3rd Qu.:39.08   3rd Qu.:27.62   3rd Qu.:16.43  
    ##  Max.   :46.60   Max.   :30.80   Max.   :20.90  
    ##                                                 
    ## 
    ## $Pike
    ##          Species       Weight          Length1         Length2     
    ##  Bream       : 0   Min.   : 200.0   Min.   :30.00   Min.   :32.30  
    ##  Perch       : 0   1st Qu.: 345.0   1st Qu.:35.50   1st Qu.:38.00  
    ##  Pike        :17   Median : 510.0   Median :40.10   Median :43.00  
    ##  Roach       : 0   Mean   : 718.7   Mean   :42.48   Mean   :45.48  
    ##  Silver Bream: 0   3rd Qu.: 950.0   3rd Qu.:48.30   3rd Qu.:51.70  
    ##  Smelt       : 0   Max.   :1650.0   Max.   :59.00   Max.   :63.40  
    ##  Whitefish   : 0                                                   
    ##     Length3          Height          Width      
    ##  Min.   :34.80   Min.   :14.50   Min.   : 9.50  
    ##  1st Qu.:40.50   1st Qu.:15.00   1st Qu.: 9.70  
    ##  Median :45.80   Median :15.80   Median :10.20  
    ##  Mean   :48.72   Mean   :15.84   Mean   :10.44  
    ##  3rd Qu.:55.10   3rd Qu.:16.00   3rd Qu.:11.20  
    ##  Max.   :68.00   Max.   :18.00   Max.   :11.70  
    ##                                                 
    ## 
    ## $Roach
    ##          Species       Weight         Length1         Length2     
    ##  Bream       : 0   Min.   : 40.0   Min.   :12.90   Min.   :14.10  
    ##  Perch       : 0   1st Qu.:115.0   1st Qu.:18.85   1st Qu.:20.40  
    ##  Pike        : 0   Median :150.0   Median :20.50   Median :22.50  
    ##  Roach       :19   Mean   :160.1   Mean   :20.73   Mean   :22.37  
    ##  Silver Bream: 0   3rd Qu.:174.5   3rd Qu.:22.05   3rd Qu.:23.75  
    ##  Smelt       : 0   Max.   :390.0   Max.   :29.50   Max.   :31.70  
    ##  Whitefish   : 0                                                  
    ##     Length3          Height          Width      
    ##  Min.   :16.20   Min.   :23.50   Min.   :13.30  
    ##  1st Qu.:22.65   1st Qu.:25.70   1st Qu.:13.95  
    ##  Median :25.00   Median :26.30   Median :14.60  
    ##  Mean   :25.08   Mean   :26.65   Mean   :14.60  
    ##  3rd Qu.:27.00   3rd Qu.:27.65   3rd Qu.:15.25  
    ##  Max.   :35.00   Max.   :30.40   Max.   :16.10  
    ##                                                 
    ## 
    ## $`Silver Bream`
    ##          Species       Weight         Length1         Length2     
    ##  Bream       : 0   Min.   : 55.0   Min.   :13.50   Min.   :14.70  
    ##  Perch       : 0   1st Qu.:105.0   1st Qu.:16.90   1st Qu.:18.35  
    ##  Pike        : 0   Median :145.0   Median :19.00   Median :20.70  
    ##  Roach       : 0   Mean   :154.8   Mean   :18.73   Mean   :20.35  
    ##  Silver Bream:11   3rd Qu.:185.0   3rd Qu.:20.50   3rd Qu.:22.25  
    ##  Smelt       : 0   Max.   :300.0   Max.   :24.00   Max.   :26.00  
    ##  Whitefish   : 0                                                  
    ##     Length3          Height          Width      
    ##  Min.   :16.50   Min.   :36.80   Min.   :13.10  
    ##  1st Qu.:20.55   1st Qu.:38.50   1st Qu.:13.60  
    ##  Median :23.20   Median :39.60   Median :14.20  
    ##  Mean   :22.79   Mean   :39.31   Mean   :14.08  
    ##  3rd Qu.:24.95   3rd Qu.:40.25   3rd Qu.:14.65  
    ##  Max.   :29.00   Max.   :41.50   Max.   :14.80  
    ##                                                 
    ## 
    ## $Smelt
    ##          Species       Weight         Length1         Length2     
    ##  Bream       : 0   Min.   : 6.70   Min.   : 9.30   Min.   : 9.80  
    ##  Perch       : 0   1st Qu.: 8.95   1st Qu.:10.47   1st Qu.:11.05  
    ##  Pike        : 0   Median : 9.85   Median :11.30   Median :11.80  
    ##  Roach       : 0   Mean   :11.18   Mean   :11.26   Mean   :11.92  
    ##  Silver Bream: 0   3rd Qu.:12.20   3rd Qu.:11.65   3rd Qu.:12.35  
    ##  Smelt       :14   Max.   :19.90   Max.   :13.80   Max.   :15.00  
    ##  Whitefish   : 0                                                  
    ##     Length3          Height          Width       
    ##  Min.   :10.80   Min.   :14.90   Min.   : 8.700  
    ##  1st Qu.:12.10   1st Qu.:16.20   1st Qu.: 9.475  
    ##  Median :13.10   Median :16.85   Median : 9.950  
    ##  Mean   :13.04   Mean   :16.89   Mean   :10.221  
    ##  3rd Qu.:13.47   3rd Qu.:17.75   3rd Qu.:10.375  
    ##  Max.   :16.20   Max.   :18.90   Max.   :13.600  
    ##                                                  
    ## 
    ## $Whitefish
    ##          Species      Weight        Length1         Length2     
    ##  Bream       :0   Min.   : 270   Min.   :23.60   Min.   :26.00  
    ##  Perch       :0   1st Qu.: 279   1st Qu.:24.48   1st Qu.:26.88  
    ##  Pike        :0   Median : 423   Median :27.05   Median :29.50  
    ##  Roach       :0   Mean   : 531   Mean   :28.80   Mean   :31.32  
    ##  Silver Bream:0   3rd Qu.: 735   3rd Qu.:32.40   3rd Qu.:35.05  
    ##  Smelt       :0   Max.   :1000   Max.   :37.30   Max.   :40.00  
    ##  Whitefish   :6                                                 
    ##     Length3          Height          Width      
    ##  Min.   :28.70   Min.   :27.80   Min.   :14.50  
    ##  1st Qu.:29.68   1st Qu.:28.43   1st Qu.:14.85  
    ##  Median :32.40   Median :28.85   Median :15.10  
    ##  Mean   :34.32   Mean   :29.20   Mean   :15.90  
    ##  3rd Qu.:38.20   3rd Qu.:29.57   3rd Qu.:16.25  
    ##  Max.   :43.50   Max.   :31.60   Max.   :19.30  
    ## 

``` r
df_fish_clean %>%
  ### transformation to make the data easier to plot
  gather(key="metric", value="value", -Species) %>%
  ggplot(aes(x=value, fill=Species, group=Species)) +
  geom_histogram(bins = 40) +
  facet_grid(Species ~ metric, scale="free") +
  theme(legend.position = "none") +
  labs(title = "Histograms by species metric")
```

![](code_notebook_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
df_fish_clean %>%
  ggplot(aes(x=Species, y=Weight, fill=Species, group=Species)) +
  geom_boxplot() +
  facet_grid(. ~ Species, scale="free") +
  labs(title="Boxplots to compare weight by species")
```

![](code_notebook_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Looks to be a curved relationship between length variables and weight.  
Could be useful to add polynomial features to fit the curves.

``` r
### check to see if there looks to be linear trend between weight and other variables
df_fish_clean %>%
  gather(key="metric", value="value", -Species, -Weight) %>%
  ggplot(aes(x=value, y=Weight, color=Species)) +
  geom_smooth(aes(group=1), method="lm", se=F, size=0.5, color="grey40") +
  geom_point(size=1, alpha=0.5) +
  facet_wrap(. ~ metric, scale="free") +
  labs(title="Scatterplot of predictor variable and weight",
       x="")
```

![](code_notebook_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Length variables have high correlation.  
We can use length 1 only: to represent the 3 length variables.  
Length 1 is the measurement of nose to tip of tail which should
represent where the bulk of weight lands.  
Dropping highly correlated features can help prevent multicollinearity
which makes coefficients unreliable.

``` r
predictors_cor <- cor(df_fish_clean %>% dplyr::select(-Species))

### Correlations all Species
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(predictors_cor, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE )
```

![](code_notebook_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
df_fish_clean %>%
  group_by(Species) %>%
  nest() %>%
  mutate(cor_data = map(data, function(df_fish_clean) correlate(df_fish_clean, diagonal = 1))) %>%
  select(-data) %>%
  unnest(cor_data) %>%
  gather(key="Variable", value="value", -Species, -rowname) %>%
  ggplot(aes(x=rowname, y=Variable, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value,2)), angle=90) +
  facet_grid(. ~ Species) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Metric correlation by Species", x="", y="")
```

![](code_notebook_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Check for outlier points by species.  
Diagnostic plots can be used to determine if these points have leverage
on model fit.

``` r
df_fish_clean %>% 
      ungroup() %>%
      mutate(row_id = row_number()) %>%
      group_by(Species) %>%
      mutate(mod_zscore_by_species = (Weight - median(Weight))/ sd(Weight)) %>%
      filter(abs(mod_zscore_by_species)>=2) %>%
      arrange(-abs(mod_zscore_by_species))
```

    ## # A tibble: 12 x 9
    ## # Groups:   Species [4]
    ##    Species Weight Length1 Length2 Length3 Height Width row_id mod_zscore_by_spe…
    ##    <fct>    <dbl>   <dbl>   <dbl>   <dbl>  <dbl> <dbl>  <int>              <dbl>
    ##  1 Roach    390      29.5    31.7    35     27.1  15.3     59               2.87
    ##  2 Perch   1100      39      42      44.6   28.7  15.4    153               2.57
    ##  3 Perch   1100      40.1    43      45.5   27.5  16.3    155               2.57
    ##  4 Smelt     19.9    13.8    15      16.2   18.1  11.6     84               2.43
    ##  5 Smelt     19.7    13.2    14.3    15.2   18.9  13.6     83               2.38
    ##  6 Perch   1015      37      40      42.4   29.2  17.6    151               2.32
    ##  7 Pike    1650      59      63.4    68     15.9  11      101               2.31
    ##  8 Perch   1000      39.8    43      45.2   26.4  16.1    154               2.28
    ##  9 Perch   1000      40.2    43.5    46     27.4  17.7    156               2.28
    ## 10 Perch   1000      41.1    44      46.6   26.8  16.3    157               2.28
    ## 11 Pike    1600      56      60      64     15     9.6     99               2.21
    ## 12 Pike    1550      56      60      64     15     9.6    100               2.10

# Modeling

### Multiple linear regression assumptions

  - Linearity
  - Errors are normally distributed (needed to get reliable coefficient
    estimates + overall test sig)
  - Errors have constant variance (aka homoscedasticity)
  - Errors are indepenent
  - Perfect multicolinearity doesn’t exist
  - Independent variables don’t correlate with error term

### We’ll take the log of the target weight variable

  - Taking the log helps to not over-penalize largest weight
    observations
  - Taking the log can also help transform a exponential relationships
    into a linear relationship
  - Can help meet constant variance assumption
  - Callout: taking the log of the target variable is not to make the
    target more normally distributed. Normality of target variables or
    predictor variables is not an assumption for linear regression.

### Interpreting model output when only target is log transformed

  - Coefficients give the percent change in response for every one unit
    increase in x.
  - If needed, use this formula to convert coefficient values:
    (exp(coefficient\_value) - 1)\*100 = % change in Y for every one
    unit increase in x.

### We’ll use feature sub selection to find simple model which performs well and has low multicolinearity impact

##### Metrics callouts when using feature sub selection

  - \[R^2 / RSS / MSE (RSS/n)\] all decrease with more features added.

  - Below metrics indirectly estimate test error by adding an adjustment
    to number of features included in the model.
    
      - Adjust R squared: percent of variation explained by the model
        which penalizes for additional predictors. Will not increase if
        the added predictors don’t help the model performance. R^2a: 1 -
        ((n-1)/(n-p)) x (1-R^2)
      - BIC (Bayesian Information Criterion): measure of model quality.
        Tends to have larger penalty than AIC and tends to recommend
        smaller model than AIC.
      - AIC (Akaike Information Criterion): measure of model quality.
        Smaller the better when comparing models.
      - Mallows’s Cp: gets at how much error is left unexplained by the
        partial model. Smaller CP is better. Consider selecting smallest
        model where CP \<= p or CP near p. P number of features.

##### Common methods for linear regression predictor subset selection

  - Exhaustive: test all model predictor combinations. 2^p combinations
  - Forward Selection: add one predictor at a time if pvalue below X.
    Pick lowest pvalue predictor.
  - Backward Selection: build full model. remove one predictor at a time
    if pvalue above X till stopping criteria is hit.
  - Hybrid Forward and Backward Stepwise Selection: forward selection to
    determine predictors to add. Looks back as predictors are added to
    see if predictors should be removed.

Given reasonable number of features (2^p) we can use exhaustive
selection.  
Not recommended when number of features is large.

``` r
### Using exhaustive search given reasonable number of feature combinations 2^P
# methods backward and forward can also be used via the leaps package
model_exhaustive_select <- regsubsets(log(Weight) ~ Species + poly(Length1, 3) + Width * Height, data=df_fish_clean, 
                                      nbest=1, nvmax=NULL, method="exhaustive")
summary_model_exhaustive_select <- summary(model_exhaustive_select)
```

Not much benefit including more than 5 variables in the model.  
For the various metrics, we can see top model performance by number of
variables included.

``` r
tibble(adjr2=summary_model_exhaustive_select$adjr2,
       Cp=summary_model_exhaustive_select$cp,
       BIC=summary_model_exhaustive_select$bic) %>%
      mutate(num_of_vars = row_number()) %>%
      gather(key="metric", value="value", -num_of_vars) %>%
      ggplot(aes(x=num_of_vars, y=value, color=factor(num_of_vars))) +
      geom_line(aes(group=1), alpha=0.4, color="grey40") +  
      geom_point() +
      scale_x_continuous(breaks = 1:20) +
      facet_wrap(. ~ metric, scales="free") +
      theme(legend.position = "none")
```

![](code_notebook_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Visualizing which variables were included for each metric variable count
iteration.  
Metrics are in agreement when model includes intercept and one variable
(i.e. use first order length 1 variable).  
We see some disagreement arise when more variables are added to the
model.

``` r
### Function inspiration source: https://gist.github.com/dkahle/7942a7eba8aaa026d0bab6a1e9d88580
ggregsubsets <- function(x){
  if(inherits(x, "regsubsets")) x <- summary(x)
  if(!inherits(x, "summary.regsubsets"))
    stop("The input to ggregsubsets() should be the result of regsubsets().")
  df <- bind_cols(
    as.data.frame(x$which), 
    as.data.frame(x[c("adjr2","cp","bic")]),
    data.frame(nvars = 1:nrow(x$which))
  )
  names(df)[1] <- "Int"
  df %>% 
    mutate(adjr2 = 100*adjr2) %>% 
    gather(variable, is_in, -adjr2, -cp, -bic, -nvars) %>% 
    gather(measure, value, -nvars, -variable, -is_in) %>% 
    ggplot(aes(variable, factor(round(value)))) +
    geom_tile(aes(fill = is_in), alpha=0.8) +
    facet_wrap(. ~ measure, scales = "free_y", ncol=5) +
    scale_fill_manual("", values = c("TRUE" = "black", "FALSE" = "white"), guide = FALSE) +
    labs(x = "", y = "") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

ggregsubsets(model_exhaustive_select)
```

![](code_notebook_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Let’s explore a final model using Species and the poly(Length1, 3)
variables.

``` r
# Bream is the baseline Species 
model <- lm(log(Weight) ~ Species + poly(Length1, 3), data=df_fish_clean)
```

Checking diagnostic plots to see if model assumptions look to be
violated or if model statistics will be skewed.

``` r
autoplot(model, which = 1:4)
```

![](code_notebook_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

If we want to use model statistics then it’s important to confirm linear
model assumptions are not violated.  
Diagnostic plots are used to assess linear model assumptions.

##### Fitted vs residuals

  - Checks
      - Linearity: does the fit line track closely to the dashed line or
        is the line curved.
      - Can spot potential outliers.
  - Observations
      - Looks like linearity assumption is met and constant variance is
        met.
      - Outliers look to exist.

##### Normal Q-Q

  - Checks
      - Are residuals normally distributed.
  - Observations
      - Points fall close to normal distribution line (outliers look to
        be present in the tails).

##### Scale location

  - Checks
      - Errors have constant variance: errors should look random around
        straight line vs following a curved shape or funnel.
  - Observations
      - Homoscedasticity assumption looks to be met.

##### Cook’s distance

  - Checks
      - Cook’s distance is used to find influential data points which
        impact model fit.
      - Removing or including influential data points alter model fit.
      - Points that land far away from the norm should be investigated.
      - Points above 4/(\# of observations − \# of predictors − 1) are
        often considered influential.
  - Observations
      - Several observations look to have high leverage on the model
        fit.
      - High leverage points might have data errors. More investigation
        would be needed on the data collection process and domain
        knowledge could be used to better assess if the observations
        should be included in the model.

### Checking multicolinearity impact

  - Doesn’t impact overall model fit.
  - Impacts interpretability of individual coefficients & pvalues.
  - Linear regression model attempts to hold all other predictors
    constant when deriving coefficients. When variables are correlated
    the model isn’t able to hold the other variable constant due to the
    correlation.
  - We can use VIF (variance inflation factor) of each variable to
    determine if the model is impacted by multicolinearity.
  - VIF measures how much the variance of a regression coefficient is
    inflated due to multicollinearity in the model.
  - If VIF is above 5 (or 10) then there’s a multicolinearity issue.

VIF is below 5 for all the variables included in the model. Doesn’t look
like multicolinearity is a major issue.

``` r
data.frame(vif(model)) 
```

    ##                     vif.model.
    ## SpeciesPerch          1.973599
    ## SpeciesPike           2.084594
    ## SpeciesRoach          1.712698
    ## SpeciesSilver Bream   1.555279
    ## SpeciesSmelt          3.691742
    ## SpeciesWhitefish      1.135184
    ## poly(Length1, 3)1     2.656093
    ## poly(Length1, 3)2     2.011505
    ## poly(Length1, 3)3     1.461540

### Assessing model performance

Investigate prediction results by species.  
Plot prediction results by species.

``` r
df_fish_clean %>%
      mutate(predicted_log_Weight = predict(model),
             actual_log_Weight = log(Weight)) %>%
      group_by(Species) %>%
      mutate(log_rmse = paste0("Log RMSE: ", round(RMSE(predicted_log_Weight, actual_log_Weight),3))) %>%
      ggplot(aes(x=predicted_log_Weight, y=actual_log_Weight, color=Species)) +
      geom_abline(intercept=0, slope=1) +
      geom_point(alpha=0.6) +
      facet_wrap(. ~ Species + log_rmse, scale="free", nrow=2) + 
      theme(legend.position = "none") +
  labs(title="Predict vs Actual Log Weight by Species")
```

![](code_notebook_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

### Use cross validation to assess model performance on unseen data

``` r
train_control <- trainControl(method="cv", number=5)
model_cv <- train(log(Weight) ~ Species + poly(Length1, 3), 
                  data=df_fish_clean, 
                  trControl=train_control, 
                  method="lm")

summary(model_cv)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.31830 -0.08098  0.00155  0.06780  0.42218 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            5.70653    0.02263 252.126  < 2e-16 ***
    ## SpeciesPerch          -0.23780    0.02681  -8.870 2.27e-15 ***
    ## SpeciesPike           -0.95973    0.04247 -22.596  < 2e-16 ***
    ## SpeciesRoach          -0.22183    0.03668  -6.048 1.16e-08 ***
    ## `SpeciesSilver Bream`  0.07005    0.04466   1.569    0.119    
    ## SpeciesSmelt          -0.96697    0.06163 -15.691  < 2e-16 ***
    ## SpeciesWhitefish      -0.06029    0.05080  -1.187    0.237    
    ## `poly(Length1, 3)1`   14.99530    0.18667  80.332  < 2e-16 ***
    ## `poly(Length1, 3)2`   -3.71571    0.16244 -22.874  < 2e-16 ***
    ## `poly(Length1, 3)3`    1.38564    0.13847  10.007  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1145 on 147 degrees of freedom
    ## Multiple R-squared:  0.993,  Adjusted R-squared:  0.9926 
    ## F-statistic:  2321 on 9 and 147 DF,  p-value: < 2.2e-16

``` r
print(model_cv)
```

    ## Linear Regression 
    ## 
    ## 157 samples
    ##   2 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 125, 127, 125, 126, 125 
    ## Resampling results:
    ## 
    ##   RMSE       Rsquared   MAE       
    ##   0.1207367  0.9909328  0.09507761
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

# Conclusions

  - The multiple linear regression model looks to do a good job at
    modeling fish weight.
      - Large F stat and low pvalue provide evidence that predictor
        variables in the model are significant.
      - We see a high adjusted R squared (e.g. variables in the model
        explain a large portion of the variance in log weight).
      - We’ll hold off on commenting on the predictor variable
        coefficients as the diagnostic plots indicate potential problems
        that might skew model parameters.
      - The Root Mean Square Log Error from cross validation indicates
        that we could expect the model to be on average off by 1.13
        times bigger than the actual weight or 1/1.13 smaller than than
        the actual weight.
  - How this could be applied to the real world?
      - Imagine scientists had a computer vision model which detected
        fish species and length.
      - The linear regression model above could be used to predict fish
        weight without putting the fish on a scale.

---
title: "Bioinformática - Seminário"
subtitle: "Introdução a Modelagem"
output: 
  html_document: 
    toc: true
    toc_depth: 4
    keep_md: true
    code_folding: "hide"    
editor_options: 
  chunk_output_type: console
---

Every analysis above were execute using the language R version 4.4.0 (2024-04-24 ucrt) and o software IDE (Integrated Development Environment) RStudio. Also the reports in format PDF was development for library `rmarkdown`.

Welinton Yoshio Hirai^[wyhirai@gmail.com]



# Modelo 1


``` r
# Simulation data 1 -------------------------------------------------------
set.seed(2025)
pred1_simu <- runif(100, min = 0, max = 100)
error_simu <- rnorm(100, mean = 0, sd = 25)
beta0 = 20
beta1 = -37

resp_calc <- beta0 + beta1*pred1_simu + error_simu

datafull <- data.frame(
  resp_calc, 
  pred1_simu
)
```

## Descritivo


``` r
summary(datafull)
```

```
##    resp_calc          pred1_simu    
##  Min.   :-3685.78   Min.   : 2.384  
##  1st Qu.:-2816.21   1st Qu.:26.839  
##  Median :-1912.33   Median :51.931  
##  Mean   :-1916.32   Mean   :52.191  
##  3rd Qu.: -972.92   3rd Qu.:77.080  
##  Max.   :  -33.87   Max.   :99.783
```


``` r
datafull %>% 
  ggplot(aes(x = pred1_simu, y = resp_calc)) +
  geom_point()
```

![](Study1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Modelagem 


``` r
model1_fit <- lm(resp_calc ~ pred1_simu, data = datafull)

summary(model1_fit)
```

```
## 
## Call:
## lm(formula = resp_calc ~ pred1_simu, data = datafull)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -54.504 -18.234  -0.351  18.300  49.263 
## 
## Coefficients:
##              Estimate Std. Error  t value Pr(>|t|)    
## (Intercept)  17.12390    5.03185    3.403 0.000966 ***
## pred1_simu  -37.04535    0.08445 -438.649  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 24.27 on 98 degrees of freedom
## Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
## F-statistic: 1.924e+05 on 1 and 98 DF,  p-value: < 2.2e-16
```

## Calculado


``` r
#calculo para beta1
num_cal = sum(datafull$resp_calc * datafull$pred1_simu) - (sum(datafull$resp_calc) * sum(datafull$pred1_simu))/100
den_cal = sum(datafull$pred1_simu^2) - (sum(datafull$pred1_simu)^2/100)
beta1_cal = num_cal/den_cal
paste('Beta 1 calculado')
```

```
## [1] "Beta 1 calculado"
```

``` r
beta1_cal
```

```
## [1] -37.04535
```


``` r
#calculo para beta0
beta0_cal = mean(datafull$resp_calc) - beta1_cal * mean(datafull$pred1_simu)
paste('Beta 0 calculado')
```

```
## [1] "Beta 0 calculado"
```

``` r
beta0_cal
```

```
## [1] 17.1239
```


``` r
#desvios
# datafull$resp_calc - predict(model1_fit)
# residuals(model1_fit)

#estimando a variância dos resíduos
sum(residuals(model1_fit)^2) / (100 - 2)
```

```
## [1] 589.1498
```

``` r
sqrt(sum(residuals(model1_fit)^2) / (100 - 2))
```

```
## [1] 24.27241
```

---

\newpage 

# Modelo 2


``` r
# Simulation data 2 -------------------------------------------------------
set.seed(2025)
pred1_simu <- runif(100, min = 0, max = 100)
pred2_simu <- rpois(100, lambda = 30)
error_simu <- rnorm(100, mean = 0, sd = 25)
beta0 = 20
beta1 = -37
beta2 = 16

resp_calc <- beta0 + beta1*pred1_simu + beta2*pred2_simu + error_simu

datafull <- data.frame(
  resp_calc, 
  pred1_simu,
  pred2_simu,
  error_simu
)
```

## Descritivo


``` r
summary(datafull)
```

```
##    resp_calc         pred1_simu       pred2_simu      error_simu     
##  Min.   :-3226.8   Min.   : 2.384   Min.   :17.00   Min.   :-71.231  
##  1st Qu.:-2367.0   1st Qu.:26.839   1st Qu.:25.00   1st Qu.:-10.987  
##  Median :-1523.5   Median :51.931   Median :29.00   Median :  6.376  
##  Mean   :-1436.7   Mean   :52.191   Mean   :29.46   Mean   :  2.977  
##  3rd Qu.: -442.3   3rd Qu.:77.080   3rd Qu.:33.00   3rd Qu.: 18.150  
##  Max.   :  406.5   Max.   :99.783   Max.   :40.00   Max.   : 62.240
```


``` r
datafull %>% 
  ggplot(aes(x = pred1_simu, 
             y = pred2_simu,
             size = resp_calc)) +
  geom_point(alpha = .8)
```

![](Study1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

## Modelagem


``` r
model2_fit <- lm(resp_calc ~ pred1_simu + pred2_simu, data = datafull)

summary(model2_fit)
```

```
## 
## Call:
## lm(formula = resp_calc ~ pred1_simu + pred2_simu, data = datafull)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -73.658 -14.704   3.627  16.277  58.641 
## 
## Coefficients:
##              Estimate Std. Error  t value Pr(>|t|)    
## (Intercept)  34.49524   14.77913    2.334   0.0217 *  
## pred1_simu  -36.98980    0.08788 -420.919   <2e-16 ***
## pred2_simu   15.59096    0.47320   32.948   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.25 on 97 degrees of freedom
## Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9994 
## F-statistic: 8.883e+04 on 2 and 97 DF,  p-value: < 2.2e-16
```

## Calculado


``` r
XMatriz <- data.frame(
  X0 = rep(1, 100),
  X1 = datafull$pred1_simu,
  X2 = datafull$pred2_simu
) %>% as.matrix()

head(XMatriz)
```

```
##      X0       X1 X2
## [1,]  1 73.26202 32
## [2,]  1 47.57614 23
## [3,]  1 51.42159 34
## [4,]  1 49.84323 29
## [5,]  1 78.02845 38
## [6,]  1 50.42522 23
```

``` r
YMatrix = as.vector(datafull$resp_calc)
```


``` r
#calculado os parâmetros
t(XMatriz) %*% XMatriz
```

```
##          X0         X1       X2
## X0  100.000   5219.128   2946.0
## X1 5219.128 354995.360 154154.5
## X2 2946.000 154154.500  89638.0
```

``` r
solve(t(XMatriz) %*% XMatriz)
```

```
##               X0            X1            X2
## X0  0.3426347559 -5.822798e-04 -1.025950e-02
## X1 -0.0005822798  1.211438e-05 -1.696706e-06
## X2 -0.0102594986 -1.696706e-06  3.512577e-04
```

``` r
solve(t(XMatriz) %*% XMatriz) %*% t(XMatriz) %*% YMatrix
```

```
##         [,1]
## X0  34.49524
## X1 -36.98980
## X2  15.59096
```


``` r
#calculado os valores preditos
Y_Pred = XMatriz %*% solve(t(XMatriz) %*% XMatriz) %*% t(XMatriz) %*% YMatrix
Y_Pred
```

```
##               [,1]
##   [1,] -2176.54110
##   [2,] -1366.74455
##   [3,] -1337.48615
##   [4,] -1357.05773
##   [5,] -2259.30488
##   [6,] -1472.13116
##   [7,] -2898.89515
##   [8,]    76.07235
##   [9,] -1975.95063
##  [10,] -1557.68690
##  [11,] -1076.82118
##  [12,] -2911.27551
##  [13,] -1793.50351
##  [14,]   398.46538
##  [15,] -1213.76203
##  [16,] -2778.20753
##  [17,]  -863.71751
##  [18,]  -927.98510
##  [19,] -2273.03809
##  [20,]    57.51725
##  [21,]  -700.27669
##  [22,] -2557.94225
##  [23,]   324.46985
##  [24,] -2346.40113
##  [25,]  -853.06482
##  [26,] -2957.29109
##  [27,] -2468.52611
##  [28,]  -234.45005
##  [29,] -2667.15351
##  [30,]  -411.30621
##  [31,] -1032.02754
##  [32,]  -138.04655
##  [33,] -1223.57554
##  [34,] -1593.83843
##  [35,] -1899.38029
##  [36,]   220.70590
##  [37,]    74.18648
##  [38,]   -11.07169
##  [39,] -3246.70956
##  [40,] -2877.69037
##  [41,]  -911.96906
##  [42,] -2923.96187
##  [43,] -2465.02238
##  [44,] -2402.14056
##  [45,] -3110.78540
##  [46,] -2396.81857
##  [47,] -2346.47745
##  [48,]   -91.75730
##  [49,] -1176.69112
##  [50,] -1576.10663
##  [51,]   129.42624
##  [52,] -2126.68856
##  [53,]  -301.55002
##  [54,]   155.76441
##  [55,] -1609.16586
##  [56,]  -886.03528
##  [57,]  -705.98774
##  [58,] -1743.05126
##  [59,] -2312.07045
##  [60,] -1735.63381
##  [61,]  -186.60894
##  [62,] -2226.97665
##  [63,] -3163.09577
##  [64,]  -239.61392
##  [65,]   109.45554
##  [66,] -2400.24565
##  [67,] -2137.28535
##  [68,]  -759.81296
##  [69,]  -486.04235
##  [70,]  -231.82911
##  [71,]    28.46163
##  [72,] -3212.12573
##  [73,] -1666.89104
##  [74,] -1104.90615
##  [75,]   293.02028
##  [76,] -1331.97131
##  [77,] -2589.91467
##  [78,] -1846.74482
##  [79,] -2459.26442
##  [80,] -1678.72903
##  [81,] -2790.17722
##  [82,]  -351.99869
##  [83,] -2962.22564
##  [84,]    76.16702
##  [85,]  -389.48104
##  [86,]  -694.69094
##  [87,]  -779.73853
##  [88,]   -87.04012
##  [89,]   284.35967
##  [90,] -1259.38156
##  [91,] -2736.83623
##  [92,] -3014.92149
##  [93,]  -795.28782
##  [94,] -2866.36656
##  [95,] -1584.75972
##  [96,] -2110.67212
##  [97,] -2424.87724
##  [98,] -2366.14467
##  [99,] -1984.69857
## [100,]  -699.92133
```

``` r
predict(model2_fit)
```

```
##           1           2           3           4           5           6 
## -2176.54110 -1366.74455 -1337.48615 -1357.05773 -2259.30488 -1472.13116 
##           7           8           9          10          11          12 
## -2898.89515    76.07235 -1975.95063 -1557.68690 -1076.82118 -2911.27551 
##          13          14          15          16          17          18 
## -1793.50351   398.46538 -1213.76203 -2778.20753  -863.71751  -927.98510 
##          19          20          21          22          23          24 
## -2273.03809    57.51725  -700.27669 -2557.94225   324.46985 -2346.40113 
##          25          26          27          28          29          30 
##  -853.06482 -2957.29109 -2468.52611  -234.45005 -2667.15351  -411.30621 
##          31          32          33          34          35          36 
## -1032.02754  -138.04655 -1223.57554 -1593.83843 -1899.38029   220.70590 
##          37          38          39          40          41          42 
##    74.18648   -11.07169 -3246.70956 -2877.69037  -911.96906 -2923.96187 
##          43          44          45          46          47          48 
## -2465.02238 -2402.14056 -3110.78540 -2396.81857 -2346.47745   -91.75730 
##          49          50          51          52          53          54 
## -1176.69112 -1576.10663   129.42624 -2126.68856  -301.55002   155.76441 
##          55          56          57          58          59          60 
## -1609.16586  -886.03528  -705.98774 -1743.05126 -2312.07045 -1735.63381 
##          61          62          63          64          65          66 
##  -186.60894 -2226.97665 -3163.09577  -239.61392   109.45554 -2400.24565 
##          67          68          69          70          71          72 
## -2137.28535  -759.81296  -486.04235  -231.82911    28.46163 -3212.12573 
##          73          74          75          76          77          78 
## -1666.89104 -1104.90615   293.02028 -1331.97131 -2589.91467 -1846.74482 
##          79          80          81          82          83          84 
## -2459.26442 -1678.72903 -2790.17722  -351.99869 -2962.22564    76.16702 
##          85          86          87          88          89          90 
##  -389.48104  -694.69094  -779.73853   -87.04012   284.35967 -1259.38156 
##          91          92          93          94          95          96 
## -2736.83623 -3014.92149  -795.28782 -2866.36656 -1584.75972 -2110.67212 
##          97          98          99         100 
## -2424.87724 -2366.14467 -1984.69857  -699.92133
```


``` r
#estimando a variância dos resíduos
desvio_cal <- datafull$resp_calc - predict(model2_fit)

residuals(model2_fit)
```

```
##           1           2           3           4           5           6 
##  54.0976927  24.2739775 -17.6638484  19.2473770 -44.9108950 -31.1180184 
##           7           8           9          10          11          12 
##   8.3208188  21.0088324  30.7271068  13.8828470 -39.7909564 -27.0939217 
##          13          14          15          16          17          18 
##  16.2692641   8.0721513  18.7750928 -13.0025673   7.5007825  12.2079599 
##          19          20          21          22          23          24 
##  23.1635356   3.4218278   7.3565521   6.9976690  27.9214312  26.0603211 
##          25          26          27          28          29          30 
##   1.4567308  -3.2740796 -10.3741713 -73.6582424 -20.7233318  -3.5084752 
##          31          32          33          34          35          36 
##  -6.1878426 -30.6189432 -47.0065873  25.8930777 -31.4772979  22.5414662 
##          37          38          39          40          41          42 
##  24.5754596  -1.2566160  23.8625319 -21.6103318   7.7400983  27.5078548 
##          43          44          45          46          47          48 
##   9.6965915  14.2236158  -3.2432895  22.7454776  -2.2273358 -14.7983799 
##          49          50          51          52          53          54 
##   3.1768941  -1.7080769 -13.3164149   3.8318673 -27.6410782   8.9279856 
##          55          56          57          58          59          60 
## -11.8306650 -11.7880449  14.5429884 -60.3307425  13.2431395 -19.7773262 
##          61          62          63          64          65          66 
## -32.7362264 -21.7045330 -38.1505696  16.3018171   4.4841305   9.8249088 
##          67          68          69          70          71          72 
##   0.5508145 -13.5323690  34.6261847 -10.2262324 -25.7558727 -14.6938599 
##          73          74          75          76          77          78 
##  20.4852453   8.4827728 -46.4386577  44.4258406  12.5663540   9.3141708 
##          79          80          81          82          83          84 
##  13.6000312  19.4492996 -14.3636966  48.9281185 -19.5218045   6.3580056 
##          85          86          87          88          89          90 
##  14.4119213  39.9625614  -7.1783597   4.5207829 -15.2607469 -64.2667223 
##          91          92          93          94          95          96 
## -35.8790018  58.6414198  27.5633587  -6.2936389  21.3231613  -2.2307489 
##          97          98          99         100 
## -10.6750600   1.4902256 -14.7329108  12.9963476
```

``` r
sum(desvio_cal^2) / (100 - 2)
```

```
## [1] 630.9743
```

``` r
sqrt(sum(desvio_cal^2) / (100 - 3))
```

```
## [1] 25.24835
```


``` r
data.frame(
  datafull,
  predict = predict(model2_fit)
) %>% 
  tidyr::pivot_longer(cols = c(resp_calc, predict)) %>% 
  ggplot(aes(x = pred1_simu, 
             y = pred2_simu,
             col = name,
             size = value,
             shape = name)) +
  geom_point(alpha = .8) 
```

![](Study1_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

---

\newpage 

<!-- # Modelo 3 -->

<!-- ```{r} -->
<!-- # Simulation data 2 ------------------------------------------------------- -->
<!-- set.seed(2025) -->
<!-- pred1_simu <- rep(c('Case', 'Control'), each = 10) -->
<!-- resp_case <- rnorm(10, mean = 20, sd = 10) -->
<!-- resp_control <- rnorm(10, mean = 40, sd = 10) -->

<!-- datafull <- data.frame( -->
<!--   resp_calc = c(resp_case, resp_control),  -->
<!--   pred1_simu -->
<!-- ) -->
<!-- ``` -->

<!-- ## Descritivo -->

<!-- ```{r} -->
<!-- aggregate(. ~ pred1_simu, datafull, mean) -->
<!-- ``` -->

<!-- ## Modelagem -->

<!-- ```{r} -->
<!-- model3_fit <- lm(resp_calc ~ pred1_simu, data = datafull) -->
<!-- summary(model3_fit) -->
<!-- ``` -->

<!-- ## Calculado -->

<!-- ```{r} -->
<!-- #calculado os parâmetros -->
<!-- XMatriz <- model.matrix(~ pred1_simu, data = datafull) -->
<!-- YMatrix <- datafull$resp_calc -->

<!-- t(XMatriz) %*% XMatriz -->
<!-- solve(t(XMatriz) %*% XMatriz) -->
<!-- solve(t(XMatriz) %*% XMatriz) %*% t(XMatriz) %*% YMatrix -->

<!-- 40.5 - 23.6 -->
<!-- ``` -->

<!-- ## Modelagem -->

<!-- ```{r} -->
<!-- model4_fit <- lm(resp_calc ~ 0 + pred1_simu, data = datafull) -->
<!-- summary(model4_fit) -->
<!-- ``` -->

<!-- ```{r} -->
<!-- #calculado os parâmetros -->
<!-- XMatriz <- model.matrix(~ 0 + pred1_simu, data = datafull) -->
<!-- YMatrix <- datafull$resp_calc -->

<!-- t(XMatriz) %*% XMatriz -->
<!-- solve(t(XMatriz) %*% XMatriz) -->
<!-- solve(t(XMatriz) %*% XMatriz) %*% t(XMatriz) %*% YMatrix -->
<!-- ``` -->



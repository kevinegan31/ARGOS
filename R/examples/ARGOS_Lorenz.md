---
title: "Lorenz_ARGOS_Identification"
author: "Kevin Egan"
date: "2023-03-01"
output: 
  html_document:
    toc: true
    toc_float: true
    toc_depth: 6
    theme: spacelab
    keep_md: true
---



# Import Packages and Functions



# Generate Lorenz System

$$\begin{equation}
\dot{x}_1 =\sigma(x_2 - x_1)\\
\dot{x}_2 =x_1(\rho - x_3) - x_2\\
\dot{x}_3 =x_1x_2 - \zeta x_3\\
\end{equation}$$


```r
n_obs <- 5000
x <- runif(1, min = -15, max = 15) # x1 initial condition bounds
y <- runif(1, min = -15, max = 15) # x2 initial condition bounds
z <- runif(1, min = 10, max = 40) # x3 initial condition bounds
init_coniditons <- c(x, y, z)
monomial_degree <- 5 # transform theta
dt <- 0.001 # time-step
snr <- 61
lorenz_system <-
  lorenz_ode(
    n_obs = n_obs,
    dt = dt,
    init_conditions = init_coniditons,
    snr = snr
  )
# 80/20 split
x_t_train <- lorenz_system[1:round((nrow(lorenz_system)*0.8), 0), ]

# Identify SG filtering paramters
xt_1 <- as.matrix(x_t_train[, 1])
xt_2 <- as.matrix(x_t_train[, 2])
xt_3 <- as.matrix(x_t_train[, 3])
lorenz_porder_wl_combos_x1 <-
  sg_optimal_combination(xt_1, dt, polyorder = 4)[[2]]
lorenz_porder_wl_combos_x2 <-
  sg_optimal_combination(xt_2, dt, polyorder = 4)[[2]]
lorenz_porder_wl_combos_x3 <-
  sg_optimal_combination(xt_3, dt, polyorder = 4)[[2]]
```

## ARGOS-Lasso Identification


```r
lasso_method <- lasso
bs_method <- list(statistic = lasso_method,
                  ols_ps = TRUE)
alpha <- 0.05 # 95% confidence intervals
num_samples <- 2000 # 2000 BS samples in real example
# xdot
start_time_xdot_lasso <- Sys.time()
xdot_argos_lasso <- boot_lasso(
  x_t = x_t_train,
  monomial_degree = monomial_degree,
  dt = dt,
  alpha = alpha,
  num_samples = num_samples,
  sg_combinations_x1 = lorenz_porder_wl_combos_x1,
  sg_combinations_x2 = lorenz_porder_wl_combos_x2,
  sg_combinations_x3 = lorenz_porder_wl_combos_x3,
  sr_method = "lasso",
  bs_method = bs_method,
  derivative = "xdot"
)
end_time_xdot_lasso <- Sys.time()
print(end_time_xdot_lasso - start_time_xdot_lasso)
```

```
## Time difference of 7.370209 mins
```

```r
# ydot
start_time_ydot_lasso <- Sys.time()
ydot_argos_lasso <- boot_lasso(
  x_t = x_t_train,
  monomial_degree = monomial_degree,
  dt = dt,
  alpha = alpha,
  num_samples = num_samples,
  sg_combinations_x1 = lorenz_porder_wl_combos_x1,
  sg_combinations_x2 = lorenz_porder_wl_combos_x2,
  sg_combinations_x3 = lorenz_porder_wl_combos_x3,
  sr_method = "lasso",
  bs_method = bs_method,
  derivative = "ydot"
)
end_time_ydot_lasso <- Sys.time()
print(end_time_ydot_lasso - start_time_ydot_lasso)
```

```
## Time difference of 13.47311 mins
```

```r
# zdot
start_time_zdot_lasso <- Sys.time()
zdot_argos_lasso <- boot_lasso(
  x_t = x_t_train,
  monomial_degree = monomial_degree,
  dt = dt,
  alpha = alpha,
  num_samples = num_samples,
  sg_combinations_x1 = lorenz_porder_wl_combos_x1,
  sg_combinations_x2 = lorenz_porder_wl_combos_x2,
  sg_combinations_x3 = lorenz_porder_wl_combos_x3,
  sr_method = "lasso",
  bs_method = bs_method,
  derivative = "zdot"
)
end_time_zdot_lasso <- Sys.time()
print(end_time_zdot_lasso - start_time_zdot_lasso)
```

```
## Time difference of 7.72482 mins
```

### Confidence Intervals


```r
# xdot
xdot_ci_df <- xdot_argos_lasso$ci
colnames(xdot_ci_df) <- c("1", xdot_argos_lasso$theta_colnames[-1])
rownames(xdot_ci_df) <- c("lo", "up")
xdot_ci_df
```

```
##              1          x         y z
## lo -0.04091999 -10.008297  9.995562 0
## up  0.04631766  -9.994354 10.006137 0
```

```r
# ydot
ydot_ci_df <- ydot_argos_lasso$ci
colnames(ydot_ci_df) <- c("1", ydot_argos_lasso$theta_colnames[-1])
rownames(ydot_ci_df) <- c("lo", "up")
ydot_ci_df
```

```
##            1         x          y         z xx xy xz          yy yz        zz
## lo -6151.518 -2.894284  0.7477451 -1415.663  0  0  0 -0.01027071  0 -82.98784
## up  7472.589 10.978018 14.5335630  1132.960  0  0  0  0.28415491  0 107.83504
##    xxx xxy xxz xyy xyz xzz yyy yyz yzz       zzz         xxxx xxxy xxxz xxyy
## lo   0   0   0   0   0   0   0   0   0 -4.120552 -0.006005005    0    0    0
## up   0   0   0   0   0   0   0   0   0  3.056329  0.000000000    0    0    0
##    xxyz xxzz xyyy xyyz xyzz          xzzz yyyy yyyz yyzz          yzzz
## lo    0    0    0    0    0 -0.0005155165    0    0    0 -0.0013928662
## up    0    0    0    0    0  0.0000000000    0    0    0 -0.0004240722
##           zzzz         xxxxx xxxxy xxxxz xxxyy xxxyz xxxzz xxyyy xxyyz xxyzz
## lo -0.05633205 -0.0002562709     0     0     0     0     0     0     0     0
## up  0.07838264  0.0000627227     0     0     0     0     0     0     0     0
##    xxzzz xyyyy xyyyz xyyzz xyzzz xzzzz yyyyy yyyyz yyyzz yyzzz        yzzzz
## lo     0     0     0     0     0     0     0     0     0     0 1.228992e-05
## up     0     0     0     0     0     0     0     0     0     0 2.729445e-05
##            zzzzz
## lo -0.0005886702
## up  0.0004142374
```

```r
# zdot
zdot_ci_df <- zdot_argos_lasso$ci
colnames(zdot_ci_df) <- c("1", zdot_argos_lasso$theta_colnames[-1])
rownames(zdot_ci_df) <- c("lo", "up")
zdot_ci_df
```

```
##            1            x          y         z          xx        xy xz
## lo -1.743653 -0.009352537 -0.1720337 -3.073914 -0.02705164 0.9856745  0
## up  4.835903  0.000000000  0.1114483 -2.616296  0.00000000 1.0194473  0
##             yy yz          zz
## lo 0.000000000  0 0.000000000
## up 0.005643848  0 0.009469461
```

### Point Estimates


```r
# xdot
xdot_pe_df <- t(data.frame(xdot_argos_lasso$point_estimates))
rownames(xdot_pe_df) <- "pe"
colnames(xdot_pe_df) <- c("1", xdot_argos_lasso$theta_colnames[-1])
xdot_pe_df
```

```
##               1         x        y z
## pe -0.001649865 -10.00078 10.00058 0
```

```r
# ydot
ydot_pe_df <- t(data.frame(ydot_argos_lasso$point_estimates))
rownames(ydot_pe_df) <- "pe"
colnames(ydot_pe_df) <- c("1", ydot_argos_lasso$theta_colnames[-1])
ydot_pe_df
```

```
##            1        x        y        z xx xy xz         yy yz        zz xxx
## pe -1483.566 10.13939 1.692035 273.9118  0  0  0 0.02078038  0 -20.32589   0
##    xxy xxz xyy xyz xzz yyy yyz yzz       zzz          xxxx xxxy xxxz xxyy xxyz
## pe   0   0   0   0   0   0   0   0 0.7530788 -0.0004264199    0    0    0    0
##    xxzz xyyy xyyz xyzz          xzzz yyyy yyyz yyzz          yzzz        zzzz
## pe    0    0    0    0 -0.0004830386    0    0    0 -0.0004896973 -0.01383356
##            xxxxx xxxxy xxxxz xxxyy xxxyz xxxzz xxyyy xxyyz xxyzz xxzzz xyyyy
## pe -1.611459e-05     0     0     0     0     0     0     0     0     0     0
##    xyyyz xyyzz xyzzz xzzzz yyyyy yyyyz yyyzz yyzzz        yzzzz        zzzzz
## pe     0     0     0     0     0     0     0     0 1.348609e-05 9.987856e-05
```

```r
# zdot
zdot_pe_df <- t(data.frame(zdot_argos_lasso$point_estimates))
rownames(zdot_pe_df) <- "pe"
colnames(zdot_pe_df) <- c("1", zdot_argos_lasso$theta_colnames[-1])
zdot_pe_df
```

```
##             1 x           y         z xx        xy xz yy yz zz
## pe -0.5677046 0 -0.05863099 -2.651728  0 0.9955011  0  0  0  0
```

### Identified Models


```r
# xdot
xdot_identified_model <- t(data.frame(xdot_argos_lasso$identified_model))
xdot_identified_model
```

```
##                                   Intercept         x        y z
## xdot_argos_lasso.identified_model         0 -10.00078 10.00058 0
```

```r
# ydot
ydot_identified_model <- t(data.frame(ydot_argos_lasso$identified_model))
ydot_identified_model
```

```
##                                   Intercept x        y z xx xy xz yy yz zz xxx
## ydot_argos_lasso.identified_model         0 0 1.692035 0  0  0  0  0  0  0   0
##                                   xxy xxz xyy xyz xzz yyy yyz yzz zzz xxxx xxxy
## ydot_argos_lasso.identified_model   0   0   0   0   0   0   0   0   0    0    0
##                                   xxxz xxyy xxyz xxzz xyyy xyyz xyzz xzzz yyyy
## ydot_argos_lasso.identified_model    0    0    0    0    0    0    0    0    0
##                                   yyyz yyzz          yzzz zzzz xxxxx xxxxy
## ydot_argos_lasso.identified_model    0    0 -0.0004896973    0     0     0
##                                   xxxxz xxxyy xxxyz xxxzz xxyyy xxyyz xxyzz
## ydot_argos_lasso.identified_model     0     0     0     0     0     0     0
##                                   xxzzz xyyyy xyyyz xyyzz xyzzz xzzzz yyyyy
## ydot_argos_lasso.identified_model     0     0     0     0     0     0     0
##                                   yyyyz yyyzz yyzzz        yzzzz zzzzz
## ydot_argos_lasso.identified_model     0     0     0 1.348609e-05     0
```

```r
# zdot
zdot_identified_model <- t(data.frame(zdot_argos_lasso$identified_model))
zdot_identified_model
```

```
##                                   Intercept x y         z xx        xy xz yy yz
## zdot_argos_lasso.identified_model         0 0 0 -2.651728  0 0.9955011  0  0  0
##                                   zz
## zdot_argos_lasso.identified_model  0
```

## ARGOS-Adaptive lasso Identification


```r
alasso_weights_method <- "ridge"
lasso_method <- alasso
bs_method <- list(
  statistic = match.fun(lasso_method),
  weights_method = alasso_weights_method,
  ols_ps = TRUE
)

alpha <- 0.05 # 95% confidence intervals
num_samples <- 2000 # 2000 BS samples in real example
# xdot
start_time_xdot_alasso <- Sys.time()
xdot_argos_alasso <- boot_lasso(
  x_t = x_t_train,
  monomial_degree = monomial_degree,
  dt = dt,
  alpha = alpha,
  num_samples = num_samples,
  sg_combinations_x1 = lorenz_porder_wl_combos_x1,
  sg_combinations_x2 = lorenz_porder_wl_combos_x2,
  sg_combinations_x3 = lorenz_porder_wl_combos_x3,
  sr_method = "alasso",
  bs_method = bs_method,
  derivative = "xdot"
)
end_time_xdot_alasso <- Sys.time()
print(end_time_xdot_alasso - start_time_xdot_alasso)
```

```
## Time difference of 13.89352 mins
```

```r
# ydot
start_time_ydot_alasso <- Sys.time()
ydot_argos_alasso <- boot_lasso(
  x_t = x_t_train,
  monomial_degree = monomial_degree,
  dt = dt,
  alpha = alpha,
  num_samples = num_samples,
  sg_combinations_x1 = lorenz_porder_wl_combos_x1,
  sg_combinations_x2 = lorenz_porder_wl_combos_x2,
  sg_combinations_x3 = lorenz_porder_wl_combos_x3,
  sr_method = "alasso",
  bs_method = bs_method,
  derivative = "ydot"
)
end_time_ydot_alasso <- Sys.time()
print(end_time_ydot_alasso - start_time_ydot_alasso)
```

```
## Time difference of 14.77546 mins
```

```r
# zdot
start_time_zdot_alasso <- Sys.time()
zdot_argos_alasso <- boot_lasso(
  x_t = x_t_train,
  monomial_degree = monomial_degree,
  dt = dt,
  alpha = alpha,
  num_samples = num_samples,
  sg_combinations_x1 = lorenz_porder_wl_combos_x1,
  sg_combinations_x2 = lorenz_porder_wl_combos_x2,
  sg_combinations_x3 = lorenz_porder_wl_combos_x3,
  sr_method = "alasso",
  bs_method = bs_method,
  derivative = "zdot"
)
end_time_zdot_alasso <- Sys.time()
print(end_time_zdot_alasso - start_time_zdot_alasso)
```

```
## Time difference of 14.80053 mins
```

### Confidence Intervals


```r
# xdot
xdot_ci_df <- xdot_argos_alasso$ci
colnames(xdot_ci_df) <- c("1", xdot_argos_alasso$theta_colnames[-1])
rownames(xdot_ci_df) <- c("lo", "up")
xdot_ci_df
```

```
##    1          x         y z
## lo 0 -10.005425  9.995584 0
## up 0  -9.995512 10.005311 0
```

```r
# ydot
ydot_ci_df <- ydot_argos_alasso$ci
colnames(ydot_ci_df) <- c("1", ydot_argos_alasso$theta_colnames[-1])
rownames(ydot_ci_df) <- c("lo", "up")
ydot_ci_df
```

```
##            1         x          y        z xx xy xz yy yz zz
## lo -225.5307 0.9857959 -1.0759726 8.336937  0  0  0  0  0  0
## up -222.1544 1.3387029 -0.8626169 8.499207  0  0  0  0  0  0
```

```r
# zdot
zdot_ci_df <- zdot_argos_alasso$ci
colnames(zdot_ci_df) <- c("1", zdot_argos_alasso$theta_colnames[-1])
rownames(zdot_ci_df) <- c("lo", "up")
zdot_ci_df
```

```
##             1          x           y         z xx        xy xz yy yz zz
## lo -2.0688965 -0.1003121 -0.18517514 -2.691321  0 0.9891117  0  0  0  0
## up  0.6181841  0.1741348  0.05974007 -2.574696  0 1.0060699  0  0  0  0
```

### Point Estimates


```r
# xdot
xdot_pe_df <- t(data.frame(xdot_argos_alasso$point_estimates))
rownames(xdot_pe_df) <- "pe"
colnames(xdot_pe_df) <- c("1", xdot_argos_alasso$theta_colnames[-1])
xdot_pe_df
```

```
##    1        x        y z
## pe 0 -10.0006 10.00059 0
```

```r
# ydot
ydot_pe_df <- t(data.frame(ydot_argos_alasso$point_estimates))
rownames(ydot_pe_df) <- "pe"
colnames(ydot_pe_df) <- c("1", ydot_argos_alasso$theta_colnames[-1])
ydot_pe_df
```

```
##            1        x          y        z xx xy xz yy yz zz
## pe -223.8423 1.157707 -0.9700623 8.417595  0  0  0  0  0  0
```

```r
# zdot
zdot_pe_df <- t(data.frame(zdot_argos_alasso$point_estimates))
rownames(zdot_pe_df) <- "pe"
colnames(zdot_pe_df) <- c("1", zdot_argos_alasso$theta_colnames[-1])
zdot_pe_df
```

```
##             1          x          y         z xx        xy xz yy yz zz
## pe -0.5277028 0.05599579 -0.0458528 -2.640567  0 0.9987953  0  0  0  0
```

### Identified Models


```r
# xdot
xdot_identified_model <- t(data.frame(xdot_argos_alasso$identified_model))
xdot_identified_model
```

```
##                                    Intercept        x        y z
## xdot_argos_alasso.identified_model         0 -10.0006 10.00059 0
```

```r
# ydot
ydot_identified_model <- t(data.frame(ydot_argos_alasso$identified_model))
ydot_identified_model
```

```
##                                    Intercept        x          y        z xx xy
## ydot_argos_alasso.identified_model -223.8423 1.157707 -0.9700623 8.417595  0  0
##                                    xz yy yz zz
## ydot_argos_alasso.identified_model  0  0  0  0
```

```r
# zdot
zdot_identified_model <- t(data.frame(zdot_argos_alasso$identified_model))
zdot_identified_model
```

```
##                                    Intercept x y         z xx        xy xz yy
## zdot_argos_alasso.identified_model         0 0 0 -2.640567  0 0.9987953  0  0
##                                    yz zz
## zdot_argos_alasso.identified_model  0  0
```




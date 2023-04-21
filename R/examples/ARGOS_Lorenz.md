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
snr <- 49
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
## Time difference of 7.581495 mins
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
## Time difference of 14.49926 mins
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
## Time difference of 9.944773 mins
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
## lo -0.12606784 -10.022962  9.982457 0
## up  0.07948886  -9.981166 10.019820 0
```

```r
# ydot
ydot_ci_df <- ydot_argos_lasso$ci
colnames(ydot_ci_df) <- c("1", ydot_argos_lasso$theta_colnames[-1])
rownames(ydot_ci_df) <- c("lo", "up")
ydot_ci_df
```

```
##            1         x         y         z       xx xy        xz         yy
## lo -19467.47 -1894.104 -436.9667 -6463.626 -116.336  0 -417.9717 -132.92265
## up  30048.47  2630.282  187.4883  3626.301  437.901  0  330.0606   12.54023
##           yz        zz       xxx xxy       xxz       xyy xyz       xzz
## lo -95.41838 -197.5438 -49.52875   0 -73.07075 -4.603644   0 -37.04583
## up 163.13612  572.2341  82.65690   0  94.08769  2.001244   0  31.09190
##            yyy yyz        yzz        zzz       xxxx xxxy      xxxz xxyy xxyz
## lo -18.5489452   0 -12.696480 -25.414204 -8.1064249    0 -13.63886    0    0
## up   0.3758551   0   8.632602   1.896885  0.1604806    0   0.00000    0    0
##         xxzz       xyyy xyyz xyzz      xzzz        yyyy yyyz yyzz       yzzz
## lo -9.679513 -0.6474298    0    0 -2.322287 -1.27183592    0    0 -0.3018287
## up  0.000000  0.0000000    0    0  1.068005  0.00183738    0    0  0.3029794
##            zzzz         xxxxx xxxxy         xxxxz xxxyy xxxyz    xxxzz xxyyy
## lo -0.002918286 -0.0003135381     0 -7.621362e-05     0     0 0.000000     0
## up  0.480086411  1.3262417534     0  2.197529e+00     0     0 1.455209     0
##    xxyyz xxyzz     xxzzz xyyyy xyyyz xyyzz xyzzz      xzzzz         yyyyy
## lo     0     0 0.0000000     0     0     0     0 0.00000000 -0.0347140827
## up     0     0 0.5021074     0     0     0     0 0.09083445  0.0001596347
##          yyyyz yyyzz yyzzz        yzzzz        zzzzz
## lo 0.000000000     0     0 -0.002744475 0.0000000000
## up 0.007426805     0     0  0.004598174 0.0007985947
```

```r
# zdot
zdot_ci_df <- zdot_argos_lasso$ci
colnames(zdot_ci_df) <- c("1", zdot_argos_lasso$theta_colnames[-1])
rownames(zdot_ci_df) <- c("lo", "up")
zdot_ci_df
```

```
##            1          x          y         z          xx        xy xz yy yz zz
## lo -4.584256 -0.8793619 -0.4275108 -2.788865 -0.06538658 0.9508661  0  0  0  0
## up  4.202247  0.5467150  0.3412243 -2.595295  0.05567838 1.0356410  0  0  0  0
##    xxx xxy xxz xyy xyz xzz yyy yyz yzz zzz
## lo   0   0   0   0   0   0   0   0   0   0
## up   0   0   0   0   0   0   0   0   0   0
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
##    1         x        y z
## pe 0 -10.00104 10.00107 0
```

```r
# ydot
ydot_pe_df <- t(data.frame(ydot_argos_lasso$point_estimates))
rownames(ydot_pe_df) <- "pe"
colnames(ydot_pe_df) <- c("1", ydot_argos_lasso$theta_colnames[-1])
ydot_pe_df
```

```
##           1        x        y       z xx xy xz         yy yz         zz xxx xxy
## pe 5.797536 3.007343 10.52701 3.79403  0  0  0 0.09206813  0 -0.4251225   0   0
##    xxz xyy xyz xzz         yyy yyz         yzz        zzz xxxx xxxy xxxz xxyy
## pe   0   0   0   0 0.002379009   0 -0.02784551 0.01054497    0    0    0    0
##    xxyz xxzz xyyy xyyz xyzz          xzzz yyyy yyyz yyzz         yzzz zzzz
## pe    0    0    0    0    0 -4.795096e-05    0    0    0 0.0004499205    0
##           xxxxx xxxxy xxxxz xxxyy xxxyz xxxzz xxyyy xxyyz xxyzz xxzzz xyyyy
## pe 2.915688e-05     0     0     0     0     0     0     0     0     0     0
##    xyyyz xyyzz xyzzz xzzzz yyyyy yyyyz yyyzz yyzzz yzzzz zzzzz
## pe     0     0     0     0     0     0     0     0     0     0
```

```r
# zdot
zdot_pe_df <- t(data.frame(zdot_argos_lasso$point_estimates))
rownames(zdot_pe_df) <- "pe"
colnames(zdot_pe_df) <- c("1", zdot_argos_lasso$theta_colnames[-1])
zdot_pe_df
```

```
##            1           x          y         z          xx        xy xz yy yz zz
## pe 0.4351693 -0.07341804 0.02259727 -2.696167 0.001012413 0.9980162  0  0  0  0
##    xxx xxy xxz xyy xyz xzz yyy yyz yzz zzz
## pe   0   0   0   0   0   0   0   0   0   0
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
## Time difference of 14.23916 mins
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
## Time difference of 15.84679 mins
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
## Time difference of 14.84244 mins
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
## lo 0 -10.020910  9.982519 0
## up 0  -9.981183 10.019837 0
```

```r
# ydot
ydot_ci_df <- ydot_argos_alasso$ci
colnames(ydot_ci_df) <- c("1", ydot_argos_alasso$theta_colnames[-1])
rownames(ydot_ci_df) <- c("lo", "up")
ydot_ci_df
```

```
##            1        x          y        z xx xy xz yy yz zz
## lo -229.2260 0.000000 -1.3761990 7.951243  0  0  0  0  0  0
## up -215.7221 1.910602 -0.3379833 8.730526  0  0  0  0  0  0
```

```r
# zdot
zdot_ci_df <- zdot_argos_alasso$ci
colnames(zdot_ci_df) <- c("1", zdot_argos_alasso$theta_colnames[-1])
rownames(zdot_ci_df) <- c("lo", "up")
zdot_ci_df
```

```
##             1          x          y         z xx       xy xz yy yz zz
## lo -85.072306 -6.4126450 -9.5878080 -2.780754  0 0.000000  0  0  0  0
## up   3.211956  0.1862119  0.3172373 -1.752677  0 1.026724  0  0  0  0
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
##    1         x        y z
## pe 0 -10.00104 10.00107 0
```

```r
# ydot
ydot_pe_df <- t(data.frame(ydot_argos_alasso$point_estimates))
rownames(ydot_pe_df) <- "pe"
colnames(ydot_pe_df) <- c("1", ydot_argos_alasso$theta_colnames[-1])
ydot_pe_df
```

```
##            1        x         y        z xx xy xz yy yz zz
## pe -225.9044 1.468482 -1.130111 8.541453  0  0  0  0  0  0
```

```r
# zdot
zdot_pe_df <- t(data.frame(zdot_argos_alasso$point_estimates))
rownames(zdot_pe_df) <- "pe"
colnames(zdot_pe_df) <- c("1", zdot_argos_alasso$theta_colnames[-1])
zdot_pe_df
```

```
##            1           x          y         z xx        xy xz yy yz zz
## pe 0.3832569 -0.08429495 0.02541526 -2.695293  0 0.9984839  0  0  0  0
```

### Identified Models


```r
# xdot
xdot_identified_model <- t(data.frame(xdot_argos_alasso$identified_model))
xdot_identified_model
```

```
##                                    Intercept         x        y z
## xdot_argos_alasso.identified_model         0 -10.00104 10.00107 0
```

```r
# ydot
ydot_identified_model <- t(data.frame(ydot_argos_alasso$identified_model))
ydot_identified_model
```

```
##                                    Intercept x         y        z xx xy xz yy
## ydot_argos_alasso.identified_model -225.9044 0 -1.130111 8.541453  0  0  0  0
##                                    yz zz
## ydot_argos_alasso.identified_model  0  0
```

```r
# zdot
zdot_identified_model <- t(data.frame(zdot_argos_alasso$identified_model))
zdot_identified_model
```

```
##                                    Intercept x y         z xx xy xz yy yz zz
## zdot_argos_alasso.identified_model         0 0 0 -2.695293  0  0  0  0  0  0
```




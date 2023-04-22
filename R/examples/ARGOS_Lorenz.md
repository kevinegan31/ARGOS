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
init_coniditions <- c(x = -5,
                      y = -12,
                      z = 16)
monomial_degree <- 5 # transform theta
dt <- 0.001 # time-step
snr <- 61
lorenz_system <-
  lorenz_ode(
    n_obs = n_obs,
    dt = dt,
    init_conditions = init_coniditions,
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
xdot_argos_lasso <- argos_3d(
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
## Time difference of 6.93677 mins
```

```r
# ydot
start_time_ydot_lasso <- Sys.time()
ydot_argos_lasso <- argos_3d(
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
## Time difference of 8.222913 mins
```

```r
# zdot
start_time_zdot_lasso <- Sys.time()
zdot_argos_lasso <- argos_3d(
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
## Time difference of 7.003687 mins
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
##    1          x         y z
## lo 0 -10.008556  9.991524 0
## up 0  -9.989817 10.006888 0
```

```r
# ydot
ydot_ci_df <- ydot_argos_lasso$ci
colnames(ydot_ci_df) <- c("1", ydot_argos_lasso$theta_colnames[-1])
rownames(ydot_ci_df) <- c("lo", "up")
ydot_ci_df
```

```
##              1        x          y z xx xy         xz yy            yz zz
## lo -0.03015307 27.93024 -1.0152439 0  0  0 -1.0009669  0 -0.0013651079  0
## up  0.04111421 28.04106 -0.9554675 0  0  0 -0.9980399  0  0.0002459916  0
```

```r
# zdot
zdot_ci_df <- zdot_argos_lasso$ci
colnames(zdot_ci_df) <- c("1", zdot_argos_lasso$theta_colnames[-1])
rownames(zdot_ci_df) <- c("lo", "up")
zdot_ci_df
```

```
##             1 x y         z xx        xy xz yy yz zz
## lo -0.1251631 0 0 -2.671133  0 0.9995418  0  0  0  0
## up  0.1074504 0 0 -2.661810  0 1.0005684  0  0  0  0
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
##    1         x       y z
## pe 0 -9.999108 9.99919 0
```

```r
# ydot
ydot_pe_df <- t(data.frame(ydot_argos_lasso$point_estimates))
rownames(ydot_pe_df) <- "pe"
colnames(ydot_pe_df) <- c("1", ydot_argos_lasso$theta_colnames[-1])
ydot_pe_df
```

```
##    1        x        y z xx xy        xz yy yz zz
## pe 0 28.00235 -1.00112 0  0  0 -1.000056  0  0  0
```

```r
# zdot
zdot_pe_df <- t(data.frame(zdot_argos_lasso$point_estimates))
rownames(zdot_pe_df) <- "pe"
colnames(zdot_pe_df) <- c("1", zdot_argos_lasso$theta_colnames[-1])
zdot_pe_df
```

```
##               1 x y         z xx       xy xz yy yz zz
## pe -0.008806913 0 0 -2.666345  0 1.000032  0  0  0  0
```

### Identified Models


```r
# xdot
xdot_identified_model <- t(data.frame(xdot_argos_lasso$identified_model))
xdot_identified_model
```

```
##                                   Intercept         x       y z
## xdot_argos_lasso.identified_model         0 -9.999108 9.99919 0
```

```r
# ydot
ydot_identified_model <- t(data.frame(ydot_argos_lasso$identified_model))
ydot_identified_model
```

```
##                                   Intercept        x        y z xx xy        xz
## ydot_argos_lasso.identified_model         0 28.00235 -1.00112 0  0  0 -1.000056
##                                   yy yz zz
## ydot_argos_lasso.identified_model  0  0  0
```

```r
# zdot
zdot_identified_model <- t(data.frame(zdot_argos_lasso$identified_model))
zdot_identified_model
```

```
##                                   Intercept x y         z xx       xy xz yy yz
## zdot_argos_lasso.identified_model         0 0 0 -2.666345  0 1.000032  0  0  0
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
xdot_argos_alasso <- argos_3d(
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
## Time difference of 14.00894 mins
```

```r
# ydot
start_time_ydot_alasso <- Sys.time()
ydot_argos_alasso <- argos_3d(
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
## Time difference of 15.97792 mins
```

```r
# zdot
start_time_zdot_alasso <- Sys.time()
zdot_argos_alasso <- argos_3d(
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
## Time difference of 14.39719 mins
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
##    1         x         y z
## lo 0 -10.00848  9.991735 0
## up 0  -9.98983 10.006495 0
```

```r
# ydot
ydot_ci_df <- ydot_argos_alasso$ci
colnames(ydot_ci_df) <- c("1", ydot_argos_alasso$theta_colnames[-1])
rownames(ydot_ci_df) <- c("lo", "up")
ydot_ci_df
```

```
##    1        x          y z xx xy         xz yy yz zz
## lo 0 27.96271 -1.0144124 0  0  0 -1.0008949  0  0  0
## up 0 28.03961 -0.9872505 0  0  0 -0.9991494  0  0  0
```

```r
# zdot
zdot_ci_df <- zdot_argos_alasso$ci
colnames(zdot_ci_df) <- c("1", zdot_argos_alasso$theta_colnames[-1])
rownames(zdot_ci_df) <- c("lo", "up")
zdot_ci_df
```

```
##              1 x y         z xx        xy xz yy yz zz
## lo -0.12733438 0 0 -2.670681  0 0.9995379  0  0  0  0
## up  0.09671692 0 0 -2.661637  0 1.0005245  0  0  0  0
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
##    1         x       y z
## pe 0 -9.999108 9.99919 0
```

```r
# ydot
ydot_pe_df <- t(data.frame(ydot_argos_alasso$point_estimates))
rownames(ydot_pe_df) <- "pe"
colnames(ydot_pe_df) <- c("1", ydot_argos_alasso$theta_colnames[-1])
ydot_pe_df
```

```
##    1        x        y z xx xy        xz yy yz zz
## pe 0 28.00235 -1.00112 0  0  0 -1.000056  0  0  0
```

```r
# zdot
zdot_pe_df <- t(data.frame(zdot_argos_alasso$point_estimates))
rownames(zdot_pe_df) <- "pe"
colnames(zdot_pe_df) <- c("1", zdot_argos_alasso$theta_colnames[-1])
zdot_pe_df
```

```
##               1 x y         z xx       xy xz yy yz zz
## pe -0.008806913 0 0 -2.666345  0 1.000032  0  0  0  0
```

### Identified Models


```r
# xdot
xdot_identified_model <- t(data.frame(xdot_argos_alasso$identified_model))
xdot_identified_model
```

```
##                                    Intercept         x       y z
## xdot_argos_alasso.identified_model         0 -9.999108 9.99919 0
```

```r
# ydot
ydot_identified_model <- t(data.frame(ydot_argos_alasso$identified_model))
ydot_identified_model
```

```
##                                    Intercept        x        y z xx xy
## ydot_argos_alasso.identified_model         0 28.00235 -1.00112 0  0  0
##                                           xz yy yz zz
## ydot_argos_alasso.identified_model -1.000056  0  0  0
```

```r
# zdot
zdot_identified_model <- t(data.frame(zdot_argos_alasso$identified_model))
zdot_identified_model
```

```
##                                    Intercept x y         z xx       xy xz yy yz
## zdot_argos_alasso.identified_model         0 0 0 -2.666345  0 1.000032  0  0  0
##                                    zz
## zdot_argos_alasso.identified_model  0
```




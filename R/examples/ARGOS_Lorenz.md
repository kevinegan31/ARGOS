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
    n = n_obs,
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
## Time difference of 7.54683 mins
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
## Time difference of 13.84031 mins
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
## Time difference of 9.447675 mins
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
##             1          x         y z
## lo -0.1215687 -10.024299  9.981784 0
## up  0.1026217  -9.979109 10.020094 0
```

```r
# ydot
ydot_ci_df <- ydot_argos_lasso$ci
colnames(ydot_ci_df) <- c("1", ydot_argos_lasso$theta_colnames[-1])
rownames(ydot_ci_df) <- c("lo", "up")
ydot_ci_df
```

```
##            1         x         y         z xx xy        xz        yy        yz
## lo -2164.528 -4.321953 -12.34734 -232.5497  0  0 -1.023624 -1.673062 -7.992269
## up  1521.716 32.043614  75.00502  366.1304  0  0  0.000000  1.690300  1.569840
##           zz xxx xxy xxz xyy xyz         xzz         yyy yyz        yzz
## lo -24.15585   0   0   0   0   0 -0.03294063 -0.09203942   0 -0.2091023
## up  11.26304   0   0   0   0   0  0.06842158  0.10352487   0  0.2877750
##           zzz         xxxx xxxy xxxz xxyy xxyz xxzz xyyy xyyz xyzz         xzzz
## lo -0.2703509 -0.016266972    0    0    0    0    0    0    0    0 -0.002031304
## up  0.6794228  0.002324193    0    0    0    0    0    0    0    0  0.000000000
##    yyyy yyyz yyzz        yzzz         zzzz         xxxxx        xxxxy
## lo    0    0    0 -0.00422784 -0.007985563 -0.0014651076 0.0000000000
## up    0    0    0  0.00919804  0.002468146  0.0002872366 0.0005505999
##            xxxxz xxxyy xxxyz xxxzz xxyyy xxyyz xxyzz xxzzz xyyyy xyyyz xyyzz
## lo -4.413720e-06     0     0     0     0     0     0     0     0     0     0
## up  3.264858e-05     0     0     0     0     0     0     0     0     0     0
##    xyzzz xzzzz         yyyyy yyyyz yyyzz yyzzz         yzzzz zzzzz
## lo     0     0 -0.0001368170     0     0     0 -1.166816e-04     0
## up     0     0  0.0001200714     0     0     0  4.579797e-05     0
```

```r
# zdot
zdot_ci_df <- zdot_argos_lasso$ci
colnames(zdot_ci_df) <- c("1", zdot_argos_lasso$theta_colnames[-1])
rownames(zdot_ci_df) <- c("lo", "up")
zdot_ci_df
```

```
##            1          x          y         z          xx         xy xz yy yz zz
## lo -5.160595 -0.9247501 -3.7352904 -2.899516 -0.24209566 0.07910777  0  0  0  0
## up  6.443528  0.9862844  0.4722972 -2.525139  0.06490658 1.04259853  0  0  0  0
##    xxx         xxy xxz xyy xyz xzz yyy yyz yzz zzz
## lo   0 -0.03997833   0   0   0   0   0   0   0   0
## up   0  0.00000000   0   0   0   0   0   0   0   0
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
## pe 0 -10.00029 10.00061 0
```

```r
# ydot
ydot_pe_df <- t(data.frame(ydot_argos_lasso$point_estimates))
rownames(ydot_pe_df) <- "pe"
colnames(ydot_pe_df) <- c("1", ydot_argos_lasso$theta_colnames[-1])
ydot_pe_df
```

```
##           1        x y       z xx xy xz         yy        yz      zz xxx xxy
## pe 183.6141 12.54877 0 -27.676  0  0  0 0.06649409 0.1910356 1.33645   0   0
##    xxz xyy xyz xzz         yyy yyz         yzz         zzz xxxx xxxy xxxz xxyy
## pe   0   0   0   0 0.006053078   0 -0.02385398 -0.02086066    0    0    0    0
##    xxyz xxzz xyyy xyyz xyzz          xzzz yyyy yyyz yyzz         yzzz zzzz
## pe    0    0    0    0    0 -0.0005920378    0    0    0 0.0005902289    0
##            xxxxx xxxxy xxxxz xxxyy xxxyz xxxzz xxyyy xxyyz xxyzz xxzzz xyyyy
## pe -2.117919e-05     0     0     0     0     0     0     0     0     0     0
##    xyyyz xyyzz xyzzz xzzzz         yyyyy yyyyz yyyzz yyzzz yzzzz zzzzz
## pe     0     0     0     0 -8.355355e-06     0     0     0     0     0
```

```r
# zdot
zdot_pe_df <- t(data.frame(zdot_argos_lasso$point_estimates))
rownames(zdot_pe_df) <- "pe"
colnames(zdot_pe_df) <- c("1", zdot_argos_lasso$theta_colnames[-1])
zdot_pe_df
```

```
##            1           x        y         z          xx        xy xz yy yz zz
## pe 0.8906814 0.001939962 0.029838 -2.701481 0.006076659 0.9982641  0  0  0  0
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
## Time difference of 14.08839 mins
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
## Time difference of 14.84103 mins
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
## Time difference of 14.93555 mins
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
## lo 0 -10.019930  9.981048 0
## up 0  -9.980376 10.020370 0
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
## lo -225.8070 0.8324606 -1.1020449 8.279385  0  0  0  0  0  0
## up -221.3109 1.3919036 -0.7912223 8.520379  0  0  0  0  0  0
```

```r
# zdot
zdot_ci_df <- zdot_argos_alasso$ci
colnames(zdot_ci_df) <- c("1", zdot_argos_alasso$theta_colnames[-1])
rownames(zdot_ci_df) <- c("lo", "up")
zdot_ci_df
```

```
##             1         x          y         z xx       xy xz yy yz zz
## lo -84.631210 -6.401952 -9.5575755 -2.876165  0 0.000000  0  0  0  0
## up   4.763995  0.340840  0.4313367 -1.779823  0 1.029728  0  0  0  0
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
## pe 0 -10.00029 10.00061 0
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
## pe -223.6015 1.121981 -0.9515123 8.403269  0  0  0  0  0  0
```

```r
# zdot
zdot_pe_df <- t(data.frame(zdot_argos_alasso$point_estimates))
rownames(zdot_pe_df) <- "pe"
colnames(zdot_pe_df) <- c("1", zdot_argos_alasso$theta_colnames[-1])
zdot_pe_df
```

```
##            1           x          y         z xx       xy xz yy yz zz
## pe 0.5790711 -0.06333582 0.04675173 -2.696229  0 1.001072  0  0  0  0
```

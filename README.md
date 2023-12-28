# Automatically identifying dynamical systems from data

This Github repository contains codes for the paper _Automatically discovering ordinary differential equations from data with sparse regression_.

## Motivation

The purpose of this code is to enable users to perform system identification of linear and nonlinear systems automatically.
System identification refers to the process of building mathematical models of dynamical systems using measurements of the input and output signals of the system.
A dynamical system is a system whose state varies over time and follows sets of ordinary or partial differential equations.
We propose a framework that applies sparse regression algorithms with model selection methods to identify the governing equations that describe the data automatically.
Upon identification, users can apply the resulting models to generalize and predict the systems existing in their data.

## Folders

[**R_code**](https://github.com/kevinegan31/AutomaticSparseRegression/tree/master/R_Code) contains all codes used for implementing our method in `R`.
We provide code for the bootstrap, lasso and adaptive lasso, and Savitzky-Golay filter.
We also provide all tests run for the _Automatically discovering ordinary differential equations from data with sparse regression_ paper.

[**Python_code**](https://github.com/kevinegan31/AutomaticSparseRegression/tree/master/Python_Code) contains all code and tests we used for SINDy-AIC.

[**Data**](https://github.com/kevinegan31/AutomaticSparseRegression/tree/master/Data) contains raw outputs (in .csv format) from test results for the _Automatically discovering ordinary differential equations from data with sparse regression_ paper.

[**Plot_code**](https://github.com/kevinegan31/AutomaticSparseRegression/tree/master/Plot_Code) contains code for figures shown in the paper.

[**RData**](https://github.com/kevinegan31/AutomaticSparseRegression/tree/master/RData) contains cleaned results data developed for the paper and implemented for the plots.

## Notice

When using Windows system, please clone to the drive directly like 'C:\\' or 'D:\\', not the folders in 'C:\\' or 'D:\\', because some file names are too long to be cloned in Windows.
MAC OS and Linux do not have this problem.

# Automatic Regression for Governing Equations (ARGOS)

This Github repository contains codes for the paper _Automatically identifying dynamical systems from data_.

## Motivation

The purpose of this repository is to enable users to perform system identification of linear and nonlinear systems automatically.
System identification refers to the process of building mathematical models of dynamical systems using measurements of the input and output signals.
Dynamical systems can either be discrete or continuous and use ordinary or partial differential equations to describe their time-varying behavior.
The Automatic Regression for Governing Equations (ARGOS) algorithm combines optimal numerical derivatives with formal variable selection procedures for automatic discovery of dynamical systems from data.
Upon identification, users can apply the resulting models to generalize and predict the systems existing in their data.

## Folders

The [**R**](https://github.com/kevinegan31/ARGOS/tree/main/R) folder contains all code used for implementing our method in `R`.
We provide code for ARGOS, including the bootstrap, lasso and adaptive lasso, and Savitzky-Golay filter.
We also provide all tests run for the _Automatically identifying dynamical systems from data_ paper.

The [**Python**](https://github.com/kevinegan31/ARGOS/tree/main/Python_Code) folder contains all code and tests we used for SINDy-AIC.

The [**Data**](https://github.com/kevinegan31/ARGOS/tree/main/Data) folder contains raw outputs (in .csv format) from test results for the _Automatically identifying dynamical systems from data_ paper.

The [**Create_data**](https://github.com/kevinegan31/ARGOS/tree/main/Create_data) folder enables users to create each file necessary for plotting the results.

The [**Plot_code**](https://github.com/kevinegan31/ARGOS/tree/main/Plot_code) folder contains code for figures shown in the paper.

The [**Additional_Functions**](https://github.com/kevinegan31/ARGOS/tree/main/additional_functions/) folder provides code to run each dynamical system examined in the paper.

## Notice

For users running Windows, please clone to the drive directly to a drive (e.g., 'C:\\' or 'D:\\') rather than a folder within the drive, as some file names may be too long for Windows to handle.
This issue does not affect macOS and Linux users.

To access the data, users need to unzip the compressed `Data.zip` file.

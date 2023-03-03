# Automatic Regression for Governing Equations (ARGOS)

This Github repository contains codes for the paper *Automatically identifying dynamical systems from data*. 

## Motivation
The purpose of this code is to enable users to perform system identification of linear and nonlinear systems automatically.
System identification refers to the process of building mathematical models of dynamical systems using measurements of the input and output signals of the system.
Dynamical systems can either be discrete or continous, and use ordinary or partial differential equations to describe its behavior whose state varies over time.
The automatic regression for governing equations (ARGOS) provides a framework that leverages the optimal numerical derivative calculation with formal variable selection procedures to identify governing equations from data automatically.
Upon identification, users can apply the resulting models to generalize and predict the systems existing in their data.

## Folders
[**R**](https://github.com/kevinegan31/ARGOS/tree/main/R) contains all code used for implementing our method in `R`.
We provide code for ARGOS, including the bootstrap, lasso and adaptive lasso, and Savitzky-Golay filter.
We also provide all tests run for the *Automatically identifying dynamical systems from data* paper. 

[**Python**](https://github.com/kevinegan31/ARGOS/tree/main/Python_Code) contains all code and tests we used for SINDy-AIC.

[**Data**](https://github.com/kevinegan31/ARGOS/tree/main/Data) contains raw outputs (in .csv format) from test results for the *Automatically identifying dynamical systems from data* paper.

[**Create_data**](https://github.com/kevinegan31/ARGOS/tree/main/Create_data) enables users to create each file necessary for plotting the results.

[**Plot_code**](https://github.com/kevinegan31/ARGOS/tree/main/Plot_code) contains code for figures shown in the paper.


## Notice
When using Windows system, please clone to the drive directly like 'C:\\' or 'D:\\', not the folders in 'C:\\' or 'D:\\',  because some file names are too long to be cloned in Windows.
MAC OS and Linux do not have this problem.

Table of Contents
Installation
Usage
Contributing
License
Installation
Instructions on how to install the project, including any dependencies that need to be installed.

Usage
Instructions on how to use the project, including any command line options or configuration settings.

Contributing
Guidelines for contributing to the project, including information on how to submit bug reports or feature requests.

License
Information on the license under which the project is released. Include any relevant attribution or copyright notices.
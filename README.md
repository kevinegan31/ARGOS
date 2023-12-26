# ARGOS: Automatic Regression for Governing Equations 
An R package for the identification of dynamical systems.

## Overview

**ARGOS** (Automatic Regression for Governing Equations) provides tools to perform system identification of both linear and nonlinear dynamical systems directly from data.
Inspired by the research presented in the paper,[_"Automatically discovering ordinary differential equations from data"_](https://arxiv.org/abs/2304.11182), this package automates the complex task of building mathematical models of dynamical systems using observed input and output signals.

## Code for Research Article

For those interested in the original code accompanying the research article, it can be found at [this GitHub repository](https://github.com/kevinegan31/ARGOS).

## Key Features

- **System Identification**: Offers tools to derive mathematical models from real-world observations.
- **Support for Various Systems**: Handles both discrete and continuous dynamical systems, using either ordinary differential equations.
- **Optimal Numerical Derivatives**: Enhances accuracy and reliability in system identification.
- **Formal Variable Selection**: Enables automatic discovery of dynamical systems, helping users discern the most relevant variables.
- **Predictive Capabilities**: Once models are identified, they can be used to generalize and predict behaviors of systems within the data set.

## Motivation

Modern research and industries demand accurate models of dynamical systems, yet building these models from raw data can be challenging.
**ARGOS** streamlines this process, automating the identification of both linear and nonlinear systems.
Whether dealing with discrete or continuous systems described by ordinary differential equations, this package provides the tools needed.
By combining optimal numerical derivatives with formal variable selection procedures, ARGOS not only aids in the discovery of these systems but also empowers users to apply the resulting models to predict and understand the behaviors present in their data.

## Getting Started

1. **Installation**: Install the package as such: ```r install.packages("ARGOS")```.

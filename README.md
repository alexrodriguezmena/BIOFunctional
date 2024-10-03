# BioFunctional Analysis Program

## Overview

This repository contains an R script (`BioFunctional_AA.R`) that automates functional analysis on biological data. The script also incorporates a Shiny application for an interactive experience and includes necessary functions and packages in supporting files. You only need to run the main file, and it will handle the rest.

## Prerequisites

To run this program, ensure that **R** is installed on your computer. You can download it from the official CRAN website:

- [Download R](https://cran.r-project.org/)

### Additional Packages

The program will automatically load all required packages. If any are missing, R will prompt you to install them.

## Folder Structure

- **`BioFunctional_AA.R`**: The main script that orchestrates the entire analysis, including loading the Shiny application and the necessary functions.
- **`aplicacion_biofunctional.R`**: Contains the code for the Shiny application, which provides an interactive user interface.
- **`funciones_analisis_datos.R`**: Includes the packages and functions required for data analysis.

> **Note**: There is no need to run `aplicacion_biofunctional.R` or `funciones_analisis_datos.R` separately. These files are automatically sourced and executed when you run `BioFunctional_AA.R`.

## How to Run the Program

1. **Install R** (if you haven't already).
2. Download or clone the repository containing the R script and supporting files.
3. Open the R environment (e.g., RStudio, or run directly from the command line using `Rscript`).
4. Run the `BioFunctional_AA.R` script.

   - In RStudio: Open `BioFunctional_AA.R` and click "Run."
   - From the terminal/command line: Navigate to the directory where the script is located and run:

     ```bash
     Rscript BioFunctional_AA.R
     ```

5. The script will automatically source `aplicacion_biofunctional.R` and `funciones_analisis_datos.R` for the Shiny app and required functions.

## Sample Files

We have provided sample files that you can use to test the functionality of the program. The files are categorized as follows:

- **Files without the "network" suffix**: These files are used for **functional analysis**.
- **Files with the "network" suffix**: These files are intended for **network and heatmap generation**.

## Questions and Support

If you have any questions or encounter any issues, feel free to:
- Open an issue in the repository.
- Contact the project maintainer for additional support.


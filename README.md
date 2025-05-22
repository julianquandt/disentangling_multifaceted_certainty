# How to use this repository

This folder can be used to produce all reported analyses, perform data preprocessing (the output of which is in the data_codebook folder), and generate figures and tables for the paper.

In principle to reproduce this paper you have to follow the steps below:

1. If you are using Rstudio, select File > New Project > Existing Directory and select the folder "reproduction_environment" for initializing the project. If you are not using Rstudio, but for instance VSCode, you should not need to create a project but can open the folder as is.
2. Run `renv::restore()` in the console to install all required packages. This will install the `renv` package if you do not have it yet.
3. Run `renv::restore()` again to install all required packages. 
4. Should you run in to problems (likely with the `StanHeaders` package), follow these steps:
    - Restart R
    - Run `renv::install("devtools")`
    - Run `devtools::install_github("paul-buerkner/brms")`
    - Run `renv::restore()` again
5. Open the file `00_manuscript_jepg.Rmd` file in the `00_manuscript` folder.
6. In the first code chunk, set `fit_models = TRUE` to fit the models.
7. Run the code chunks in the file sequentially or use the "Knit" button to produce the manuscript. This will produce the model fits in the `01_analyses/fitted_models` folder. The code that are used for fitting the models are in the `01_analyses/model_fits` folder. The indicated numbers clearly identify which model is used for which claim in the manuscript.

# Manuscript Repository

This is the repository for the manuscript *Spike-and-Slab Generalized Additive Models and Scalable Algorithms for High-Dimensional Data*. The repository is set up using the R workflow package [`targets`](https://cran.r-project.org/web/packages/targets/index.html), and can be reproduced easily via `targets` syntax.

## How to run

1.  Install the necessary workflow packages [`targets`](https://cran.r-project.org/web/packages/targets/index.html) and [`renv`](https://rstudio.github.io/renv/articles/renv.html) if you don't already have
2.  Open the R console and call `renv::restore()` to install the required R packages. Please give permission to install the necessary packages. This will mirror the version of packages used in the creation of the manuscript exactly.
3.  call the `targets::tar_make()` function to run the pipeline; for example `targets::tar_make("manu")` to create the manuscript, and `targets::tar_make("manu_app")` to create the supporting information.

## Remarks

-   The raw/unaggregated the simulation results can be accessed [here](https://www.dropbox.com/sh/9f8kvnduuqxwj7o/AADSULXkTpO5-DskQo0CaHMia?dl=0). The simulation computation was conducted separately on [a high-performance super computer](https://www.uab.edu/it/home/research-computing/cheaha), and hence not included in the current workflow. You can find the code to replicate the simulation process in the folder [`Simulation\Code`](https://github.com/boyiguo1/Manuscript-Spike_Slab_HD_GAM/tree/main/Simulation/Code) and the instruction to deploy the simulation on a cluster with the Slurm scheduling system via the repo [`boyiguo1/
Tutorial-Sim_Cluster_Composer`](https://github.com/boyiguo1/Tutorial-Sim_Cluster_Composer).

-   The two datasets used for real data analyses are included in the repo for reproducibility purpose (placed in the folder [`Real_Data`](https://github.com/boyiguo1/Manuscript-Spike_Slab_HD_GAM/tree/main/Real_Data)). The owner of this repository does not own the datasets, and suggest readers to check out the datasets on [Dryad](https://datadryad.org/stash/) and [Zendo](https://zenodo.org/)

    -   Emory Cardiovascular Biobank: <https://datadryad.org/stash/dataset/doi:10.5061/dryad.866t1g1mt>
    -   Weight Loss Maintenance Cohort: <https://zenodo.org/record/4767969>

-   The `sparseGAM` package (v1.0.99) included in the `renv` environment differs from the current CRAN version (v1.0 as on Oct. 22, 2021). The difference being the cross-validation function `cv.SBGAM` will return prevalid response for out-of-sample statistics calculation.

    -   To root down the difference, check out `cv.SSGL.output` in the function `cv.SSGL`
    -   The modified version (v1.0.99) is located at `renv\local\sparseGAM_1.0.99.tar.gz`

-   For Windows users with limited choices of compilers, there might be packages needs compilation from source, e.g. `xfun` package. In this case, I encourage you to install the binary version of the package locally and use `renv::hydrate()` before `renv::restore()`to cache the necessary packages from your library.

    ```
    renv::hydrate()
    renv::restore()
    ```

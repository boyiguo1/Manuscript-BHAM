# Manuscript Repository

This is the repository for the manuscript _Spike-and-Slab Generalized Additive Models and Scalable Algorithms for High-Dimensional Data_. The repository is set up using the R workflow package [`targets`](https://cran.r-project.org/web/packages/targets/index.html), and can be reproduced easily via `targets` syntax. 

## Reproducibility

To reproduce the manuscript, including real data analyses, tables and figures, simply run
```
targets::tar_make()
``` 

## Notes
  * The repo contains the raw results for the simulations described in the manuscript. The simulation computation was conducted separately on [a high-performance super computer](https://www.uab.edu/it/home/research-computing/cheaha), and hence not included in the current workflow. You can find the code to replicate the simulation process in the folder [`Simulation\Code`](https://github.com/boyiguo1/Manuscript-Spike_Slab_HD_GAM/tree/main/Simulation/Code).
  * The two datasets used for real data analyses are included in the repo for reproducibility purpose (placed in the folder [`Real_Data`](https://github.com/boyiguo1/Manuscript-Spike_Slab_HD_GAM/tree/main/Real_Data)). The owner of this repository does not own the datasets, and suggest readers to check out the datasets on [Dryad](https://datadryad.org/stash/) and [Zendo](https://zenodo.org/)

    * Emory Cardiovascular Biobank: [https://datadryad.org/stash/dataset/doi:10.5061/dryad.866t1g1mt](https://datadryad.org/stash/dataset/doi:10.5061/dryad.866t1g1mt)
    * Weight Loss Maintenance Cohort: [https://zenodo.org/record/4767969](https://zenodo.org/record/4767969)


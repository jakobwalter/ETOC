# Permutation Tests for Assessing Differential Expression in RNA-Seq Data
Accompanying code to my thesis: Permutation Tests for Assessing
Differential Expression in RNA-Seq Data

In the folder "reports" there are three R-Markdown files:
- "InitialDataAnalysis", which contains the code to download the TCGA-LIHC Data. Runnung on all methods on this data and the code to obtain the analysis, visualization and material up to Section 6.2 of the Thesis
- "PerturbationSimulationStudy" which contains the simulation study based on permutations (Section 6.2).
- "ParametricSimulation Study" which contains the simulation study based on the NB distribution (Section 6.3).

Furthermore, some helper functions are in the folder functions. All plots from the thesis are in "plots" and all data - including the results of the simulation study - are in data.

Note that the simulations run for a very long time. Because of this, we included the data. All analyses should thus be reproducible without rerunning the simulation. Lastly, note that the accompanying .html files might not be representative, as we made some changes to the code without rerunning the simulation study.

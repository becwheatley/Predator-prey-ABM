# Predator-prey-ABM

This repository contains the model code, simulation data, and R analysis files for the agent-based model presented in the paper:
Wheatley, R., Pavlic, T.P., Levy, O. & Wilson, R.S. Habitat features and performance interact to determine the outcomes of terrestrial predator–prey pursuits. _Journal of Animal Ecology_, accepted.

Below is a description of the contents of each folder.



**Model code**

Contains two versions of the model NetLogo code (these are nearly identical, the only difference being the parameters that are set in the 'interface' tab). Model code is compatible with NetLogo v6.1. NetLogo is free to run and can be downloaded from: https://ccl.northwestern.edu/netlogo/download.shtml.

1. _predator-prey-model-with_limb_length_input.nlogo_ - the user can specify the limb lengths of the predator and prey, and their performance capabilities are set via scaling relationships with limb length.

2. _predator-prey-model-with_manual_performance_input.nlogo_ - the user can specify the performance capabilities of predator and prey directly.



**Data**

Contains csv files of the simulation data used in the aforementioned paper (note that all raw data files contain the parameter settings for the simulation in addition to the model output).

**1. Sensitivity analysis**

For the global sensitivity analysis, we used a reduced factorial design where each parameter was varied across its range with every other parameter in pairs (a total of 8,558 parameter combinations). We ran 100 simulation replications per parameter combination, resulting in 855,800 simulations in total. This folder contains the raw simulation data (in csv files) for each parameter pair combination, divided into subfolders according to which parameter was consistently varied. 

1. _Global sensitivity analysis-parameter sampling design.xlsx_ - excel file containing the sampling design for the simulations described above. Gives a summary that describes which file corresponds to which parameter pair at a glance (using the PS_XXX codes).

2. _predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv_ - csv file containing the mean and median values of the simulation results (generated using the file _global sensitivity analysis 1-calculate mean and median responses.R_ in the _Analysis_ folder), which was analysed for the global sensitivity analysis (see _global sensitivity analysis 2-multisensi sensitivity analysis.R_ in the _Analysis_ folder).

**2. Obstacle vs refuge experiments**

Contains simulation data presented in the sub-section exploring the interaction between the predator and prey's relative performance, obstacles, and refuges.

1. _predator-prey-model-revised-performance_vs_obstacles.csv_ - raw data from simulations where the predator and prey's relative performance was varied with the proportion of obstacles in the habitat (analysed using the _experiments-obstacles and refuges.R_ file in the _Analysis_ folder).

2. _predator-prey-model-revised-performance_vs_refuges.csv_ - raw data from simulations where the predator and prey's relative performance was varied with the number of refuges in the habitat (analysed using the _experiments-obstacles and refuges.R_ file in the _Analysis_ folder).


**3. Cheetah vs impala experiment**

Simulation data presented for the case study of an impala escaping a cheetah in open savanna and acacia thicket.

1. _predator-prey-model-revised-cheetah_vs_impala-acacia_thicket.csv_ - raw simulation data for the cheetah vs impala in acacia thicket (analysed using the _experiments-cheetah vs impala.R_ file in the _Analysis_ folder).

2. _predator-prey-model-revised-cheetah_vs_impala-open_savanna.csv_ - raw simulation data for the cheetah vs impala in open savanna (analysed using the _experiments-cheetah vs impala.R_ file in the _Analysis_ folder).



**Analysis**

Contains R code used in the analysis of the data contained in the Data folder.

1. _global sensitivity analysis 1-calculate mean and median responses.R_ - takes the raw simulation data contained in the subfolders in the _Sensitivity analysis_ data folder and compiles the summarised data file (_predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv_)

2. _global sensitivity analysis 2-multisensi sensitivity analysis.R_ - conducts the global sensitivity analysis on the summarised data file generated above.

3. _experiments-obstacles and refuges.R_ - analyses the two raw data files in the _Obstacle vs refuge experiments_ data subfolder.

4. _experiments-cheetah vs impala.R_ - analyses the two raw data files in the _Cheetah vs impala experiment_ data subfolder.

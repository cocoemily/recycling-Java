# recycling-Java

Agent-based model of recycling

### OriginalModel

Translation of agent-based model from Coco et al 2020 from R to Java. Replication analysis can be found in the analysis folder.

### ExtendedModel

Agent-based model of recycling in extreme case of no geological processes with additional parameters and outputs

#### Parameters

**args[0]** of (String) *name of folder for output*\
**args[1]** name (String) *name of model run*\
**args[2]** size (int) *number of rows and number of columns*\
**args[3]** startYear (int) *initial data for all layers*\
**args[4]** timestep (int) *number of years each timestep represents*\
**args[5]** maxUI (int) *maximum number of actions agents can take when manufacturing*\
**args[6]** maxAC (int) *maximum number of artifacts agents can carry when moving*\
**args[7]** maxFS (int) *maximum flake size that can be removed from nodules*\
**args[8]** maxNS (int) *maximum nodule surface area*\
**args[9]** bProb (double) *probability of agents removing new flakes from nodules*\
**args[10]** sProb (double) *probability of scavenging previously discarded material*\
**args[11]** overlap (double) *parameter determines order of agent appearance on landscape*\
**args[12]** mu (double) *parameter for Levy walk function*\
**args[13]** sizePref (boolean) *selection is based on size of objects*\
**args[14]** flakePref (boolean) *preference of flakes over nodules*\
**args[15]** minFS (int) *selection for flakes of at least this size*\
**args[16]** strict (boolean) *possibility of scavenging items that do not match selection parameters*\
**args[17]** ED (double) *probability of erosion* **unused in current model code**\
**args[18]** GF (int) *frequency of geological events* **unused in current model code**\
**args[19]** totalSteps (int) *total number of timesteps for each model run*\
**args[20]** totalAgents (int) *total number of agents that will be used for each model run*

#### Model run

1.  Agents look for items to collect from their current location. If there are objects available, they collect based on scavenging probability and selection criteria
2.  If agents have items in hand, they either create new flakes based on a probability of making blanks or retouch flakes
3.  If agents does not have items, they "find"" new nodules
4.  If agents have exhausted items, they discard those at the current location
5.  If agents are carrying too much, they discard unwanted items that do not fit the selection criteria at the current location
6.  Agents move to a new location

#### Running the model

Must create an output folder in the base recycling-java directory for model outputs to be created properly!

1.  Model can be run for testing purposes from a Java IDE using the TestExtendedModel.java script

2.  Model can be run in a command line environment using the call-sbatch.sh script

    -   In this case, filepath names in call-sbatch.sh and run-param.sbatch need to be updated

#### Output

***[model name]\_artifacts-data.csv***\
information for each artifact on the landscape at the end of model run

***[model name]\_layers-data.csv***\
information for each layer on the landscape at the end of model run

***[model name]\_model-data.csv***\
information for the whole model at each timestep of a model run

#### Analysis

-   All output should be left in respective folders for R scripts in analysis folder to run properly

-   A CSV with all of the model-level results needs to be created with the following command:

    `find -name model* -exec cat {} \; > joined_model_data.csv`

-   Each line of all .sbatch scripts in the experiments/model-analysis, experiments/layer-analysis, and experiments/artifact-analysis folders should be run

    -   Any CSV outputs created should be moved to a results folder

-   All R scripts and .sbatch scripts in the experiments/results-analysis folder should be run

    -   This requires creating a figures and figures/supplementary-figures folder

*In most cases, filepaths in the analysis scripts were hardcoded to avoid any issues with producing results and figures. These would need to be refactored to run properly.*

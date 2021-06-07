# recycling-Java

Agent-based model of recycling 

### OriginalModel
Translation of agent-based model from Coco et al 2020 from R to Java


### ExtendedModel
Agent-based model of recycling in extreme case of no geological processes with additional parameters and outputs

#### Parameters
**args[0]** of (String) *name of folder for output*  
**args[1]** name (String) *name of model run*  
**args[2]** size (int) *number of rows and number of columns*  
**args[3]** startYear (int) *initial data for all layers*
**args[4]** timestep (int) *number of years each timestep represents*
**args[5]** maxUI (int) *maximum number of actions agents can take when manufacturing*
**args[6]** maxAC (int) *maximum number of artifacts agents can carry when moving*
**args[7]** maxFS (int) *maximum flake size that can be removed from nodules*
**args[8]** maxNS (int) *maximum nodule surface area*
**args[9]** bProb (double) *probability of agents removing new flakes from nodules*
**args[10]** sProb (double) *probability of scavenging previously discarded material*
**args[11]** numAgents (int) *number of agents*
**args[12]** overlap (double) *parameter determines order of agent appearance on landscape*
**args[13]** mu (double) *parameter for Levy walk function*
**args[14]** sizePref (boolean) *selection is based on size of objects*
**args[15]** flakePref (boolean) *preference of flakes over nodules*
**args[16]** minNS (int) *selection for nodules of at least this size*
**args[17]** minFS (int) *selection for flakes of at least this size*
**args[18]** strict (boolean) *possibility of scavenging items that do not match selection parameters*
**args[19]** ED (double) *probability of erosion* **unused in current model code**
**args[20]** GF (int) *frequency of geological events* **unused in current model code**
**args[21]** totalSteps (int) *total number of timesteps for each model run*


# recycling-Java

Agent-based model of recycling 

### OriginalModel
Translation of agent-based model from Coco et al 2020 from R to Java. Replication analysis can be found in the analysis folder.


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
**args[11]** overlap (double) *parameter determines order of agent appearance on landscape*  
**args[12]** mu (double) *parameter for Levy walk function*  
**args[13]** sizePref (boolean) *selection is based on size of objects*  
**args[14]** flakePref (boolean) *preference of flakes over nodules*  
**args[15]** minFS (int) *selection for flakes of at least this size*  
**args[16]** minNS (int) *selection for nodules of at least this size*  
**args[17]** strict (boolean) *possibility of scavenging items that do not match selection parameters*  
**args[18]** ED (double) *probability of erosion* **unused in current model code**  
**args[19]** GF (int) *frequency of geological events* **unused in current model code**  
**args[20]** totalSteps (int) *total number of timesteps for each model run*  

#### Model run
1. Agents look for items to collect from their current location. If there are objects available, they collect based on scavenging probability and selection criteria
2. If agents have items in hand, they either create new flakes based on a probability of making blanks or retouch flakes 
3. If agents does not have items, they find new nodules
4. If agents have exhausted items, they discard those at the current location
5. If agents are carrying too much, they discard unwanted items that do not fit the selection criteria at the current location
6. Agents move to a new location

#### Output 
**[model name]_artifacts-data.csv**   
	information for each artifact on the landscape at each timestep  

**[model name]_layers-data.csv**  
	information for each layer on the landscape at each timestep  

**[model name]_model-data.csv**  
	information for the whole model at each timestep  




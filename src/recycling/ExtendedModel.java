package recycling;

import java.util.*;

public class ExtendedModel {

	public String outputFile;
	public String name;

	public int totalSteps;

	public int size;
	public int startYear;
	public int currentYear;
	public int timestep;
	public double overlap;

	public int nextId;
	public ArrayList<Agent> agents;
	public int totalAgents;
	public double groupPerc;

	public int maxUseIntensity;
	public int maxArtifactCarry;
	public int maxFlakeSize;
	public int maxNoduleSize;

	public double blankProb;
	public double discardProb;
	public double scavengeProb;

	public Grid landscape;
	public double EDratio;
	public int geoFreq;

	public double mu;

	public double noduleV = 100000;
	public double noduleSA = 11091.8;
	public double avgFlakesOnNodule;

	public boolean sizePref;
	public int minAcceptableFlakeSize;
	public int minAcceptableNoduleSize;
	public boolean flakePref;
	public boolean strictSelect; //determine whether or not random collection after things meeting selection criteria are selected

	public int numberScavengingEvents; 

	//output data 
	StringBuilder layerdata;
	StringBuilder modeldata;
	StringBuilder artifactdata;


	public ExtendedModel(
			String of, String name, int size, int startYear, int timestep, //run parameters
			int maxUI, int maxAC, int maxFS, int maxNS, 
			double bProb, double sProb, //action probability parameters
			double overlap, double mu, //agent creation and movement parameters
			boolean sizePref, boolean flakePref, int minFS, int minNS, boolean strict, //selection parameters
			double ED, int GF, //geology parameters
			int totalSteps) {

		this.outputFile = of;
		this.name = name;

		this.size = size;
		this.startYear  = startYear;
		this.currentYear = startYear;
		this.timestep = -timestep;

		this.nextId = 1;
		this.agents = new ArrayList<Agent>();
		this.totalAgents = (int) Math.ceil((0.315 * totalSteps) + 3); //equation based on linear regression fit to timesteps and last agent number
		this.groupPerc = 0.5;

		this.maxArtifactCarry = maxAC;
		this.maxUseIntensity = maxUI;
		this.maxFlakeSize = maxFS;
		this.maxNoduleSize = maxNS;
		if(maxFS == 1) { //values based on creating nodules with varying maxFlakeSize 
			this.avgFlakesOnNodule = 20;
		} else if(maxFS == 2) {
			this.avgFlakesOnNodule = 13.5;
		} else if(maxFS == 3) {
			this.avgFlakesOnNodule = 10.3;
		} else {
			this.avgFlakesOnNodule = 5.5 + 25.2*Math.pow(Math.E, -0.56); //exponential function fit to output from creating random nodules
		}

		//this.discardProb = 1; //not being tested in this model
		this.blankProb = bProb;
		this.scavengeProb = sProb;

		this.overlap = overlap;
		this.mu = mu;

		this.sizePref = sizePref;
		this.minAcceptableFlakeSize = minFS;
		this.minAcceptableNoduleSize = minNS;
		this.flakePref = flakePref;
		this.strictSelect = strict;

		this.landscape = new Grid(size, startYear);
		this.EDratio = ED;
		this.geoFreq = GF;

		this.totalSteps = totalSteps;

		this.numberScavengingEvents = 0;

		this.layerdata = new StringBuilder();
		String cols = this.getParameterData().get(0) + ",row,col,model_year,year,nodule.count,flake.count,cortex.ratio,"
				+ "recycling.intensity,num.discards,num.encounters,num.manufacture,num.retouch,num.occupation";
		this.layerdata.append(cols + "\n");
		this.modeldata = new StringBuilder();
		String mcols = this.getParameterData().get(0) + ",model_year,num.scav.events,total.recycled,num.deposits,total.encounters,total.discards,total.manu.events,total.retouches,total.CR,total.RI"; 
		this.modeldata.append(mcols + "\n");
		this.artifactdata = new StringBuilder();
		this.artifactdata.append("row,col,model_year,layer_year,obj_type,size,volume,cortex,stage,numgroups,first_tech,last_tech,recycled\n");

	}

	/**
	 * Creates a new agent and updates variable for providing agents unique id numbers
	 * @param techNum integer specifying the technology type of an agent
	 */
	public void createAgent(int techNum) {
		int counter = nextId;
		agents.add(new Agent(counter, techNum));
		counter++;
		this.nextId = counter;
	}

	/**
	 * Creates multiple new agents as specified by numAgents parameter
	 * @param techNum integer specifying the technology type of an agent
	 * @param numAgents integer for number of agents to be created
	 */
	public void createAgents(int techNum, int numAgents) {
		int counter = nextId;
		for(int i=0; i < numAgents; i++) {
			agents.add(new Agent(counter, techNum));
			counter++;
			this.nextId = counter;
		}

	}

	/**
	 * Removes agents from the model agent list if they are of a specific technology type
	 * @param techNum integer specifying the technology type of an agent
	 */
//	public void removeAgents(int techNum) {
//		this.agents.removeIf(Agent -> (Agent.getTech() == techNum));
//	}

	/**
	 * Initialize some squares within the landscape with new nodules
	 * @param sourceProb probability that a square will be initialized with new nodules
	 * @param numArtifacts number of nodules added to a square
	 */
	public void createRandomSources(double sourceProb, int numArtifacts) { 
		//initialize landscape with some artifacts
		for(int i=0; i < this.landscape.getNumRows(); i++) {
			for(int j=0; j < this.landscape.getNumCols(); j++) {
				double artifactProb = Math.random();
				if(artifactProb < sourceProb) { //chance for each grid square to start with artifacts
					ArrayList<Nodule> initArtifacts = new ArrayList<Nodule>();
					for(int a = 0; a < numArtifacts; a++) { 
						//int nSize = (int) ((Math.random() * (this.maxNoduleSize / 10)) + 1) * 10;
						Nodule toAdd = new Nodule(this.maxNoduleSize, this.noduleV, this.maxFlakeSize);
						initArtifacts.add(toAdd);
					}
					this.landscape.getElement(i, j).addNodules(initArtifacts);
				}
			}
		}

	}


	/**
	 * Calls functions for geological events within the Square class for each grid square in the model
	 */
	public void geologicalEvents() {
		int year = currentYear + timestep;
		for(int j=0; j < landscape.getNumRows(); j++) { //each row of the Grid
			for(int k=0; k < landscape.getNumCols(); k++) { //each element of the Grid row
				//System.out.println(j + ", " + k);

				double erodeOrDeposit = Math.random();
				if(erodeOrDeposit > EDratio) {
					landscape.getElement(j, k).erode();
					//System.out.println("erode");
				}
				else {
					landscape.getElement(j, k).deposit(year);
					//System.out.println("deposit");
				}

			}
		}
	}


	//Agent methods
	/**
	 * Moves all agents in the agent list, either randomly or according to the Levy walk function
	 * @param randomMove if true agents move randomly; if false agents use Levy walks
	 */
	public void moveAgents(boolean randomMove) { //for when there are multiple agents on the landscape
		for(int i=0; i < agents.size(); i++) {
			if(randomMove) {
				agents.get(i).randomMove(this.landscape.getNumRows(), this.landscape.getNumCols());
			} else {
				agents.get(i).randomWalk(this.mu);
				if(agents.get(i).getCurrentX() >= 0 && agents.get(i).getCurrentX() < this.landscape.getNumRows() &&
						agents.get(i).getCurrentY() >= 0 && agents.get(i).getCurrentY() < this.landscape.getNumCols()) {
					agents.get(i).setInWindow(true);
				} else {
					agents.get(i).setInWindow(false);
				}
			}
		}
	}
	
	/**
	 * Moves a single agent, either randomly or according to the Levy walk function 
	 * @param agent to be moved
	 * @param randomMove if true agents move randomly; if false agents use Levy walks
	 */
	public void moveAgent(Agent agent, boolean randomMove) { //for when there is a single agent on the landscape
		if(randomMove) {
			agent.randomMove(this.landscape.getNumRows(), this.landscape.getNumCols());
		} else {
			agent.randomWalk(this.mu);
			if(agent.getCurrentX() >= 0 && agent.getCurrentX() < this.landscape.getNumRows() &&
					agent.getCurrentY() >= 0 && agent.getCurrentY() < this.landscape.getNumCols()) {
				agent.setInWindow(true);
			} else {
				agent.setInWindow(false);
			}
		}
	}

	/**
	 * Agent "finds" new nodules and adds them to its nodule list
	 * @param agent 
	 * @param numNodules number of nodules agent "finds"
	 */
	public void findNodules(Agent agent, int numNodules) {
		agent.initializeNodules(numNodules, this.maxNoduleSize, this.noduleV, this.maxFlakeSize);
	}
	
	/**
	 * Agent scavenges artifacts from a grid square randomly (using no selection parameters) 
	 * and adds them to its artifact lists
	 * @param agent 
	 */
	public void collectRandomArtifacts(Agent agent) { //completely random
		for(int i=agent.getAgentArtifacts().size(); i < this.maxUseIntensity; i++) {
			ArrayList<Object> all = new ArrayList<Object>();
			all.addAll(this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().getNodules());
			all.addAll(this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().getFlakes());
			Object choice = null;

			if(all.size() != 0) {
				int index = (int) (Math.random() * all.size());
				choice = all.get(index);

				if(choice instanceof Nodule) {
					agent.collectNodule((Nodule) choice);
					this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().removeNodule((Nodule) choice);
					this.numberScavengingEvents++; //tracks number of scavenging events

				} else if(choice instanceof Flake) {
					agent.collectFlake((Flake) choice);
					this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().removeFlake((Flake) choice);
					this.numberScavengingEvents++; //tracks number of scavenging events
				}
			}
		}
	}

	/**
	 * Function to provide a selection of flakes/nodules from provided lists of flakes and nodules 
	 * that fit the selection criteria parameters of the model instance
	 * 
	 * @param nodules list of Nodule objects to select from
	 * @param flakes list of Flake objects to select from
	 * @param numNeeded number of objects to select
	 * @return list of Objects selected based on combinations of selection parameters
	 */
	public ArrayList<Object> select(ArrayList<Nodule> nodules, ArrayList<Flake> flakes, int numNeeded) {
		ArrayList<Object> selection = new ArrayList<Object>();

		if(this.flakePref) { //flake preference
			ArrayList<Flake> possFlakes = new ArrayList<Flake>();
			if(this.sizePref) { //size preference
				for(int i=0; i < flakes.size(); i++) {
					if(flakes.get(i).getSize() >= this.minAcceptableFlakeSize) {
						possFlakes.add(flakes.get(i));
					}
				}
			} else {
				possFlakes = flakes;
			}

			if(possFlakes.size() <= numNeeded) { //if there are fewer flakes than number needed or exactly the number needed, return all
				for(int i=0; i < possFlakes.size(); i++) {
					selection.add(possFlakes.get(i));
					flakes.remove(possFlakes.get(i));
				}
			} else if(possFlakes.size() > numNeeded) {
				for(int i=numNeeded; i < possFlakes.size(); i++) {//if there are more flakes than number needed, randomly select from possible flakes
					int index = (int) (Math.random() * possFlakes.size());
					selection.add(possFlakes.get(index));
					flakes.remove(possFlakes.get(index));
				}
			}



		} else { //nodule preference
			ArrayList<Nodule> possNods = new ArrayList<Nodule>();
			if(this.sizePref) { //size preference
				for(int i=0; i < nodules.size(); i++) {
					//current selection based on how many flakes are left to take off
					if(nodules.get(i).getFlakes().size() >= this.minAcceptableNoduleSize) { 
						possNods.add(nodules.get(i));
					}
				}
			} else {
				possNods = nodules;
			}

			if(possNods.size() <= numNeeded) { //if there are fewer nodules than number needed or exactly the number needed, return all
				for(int i=0; i < possNods.size(); i++) {
					selection.add(possNods.get(i));
					nodules.remove(possNods.get(i));
				}
			} else if(possNods.size() > numNeeded){
				for(int i=numNeeded; i < possNods.size(); i++) {//if there are more nodules than number needed, randomly select from possible nodules
					int index = (int) (Math.random() * possNods.size());
					selection.add(possNods.get(index));
					nodules.remove(possNods.get(index));
				}
			}

		}

		if(!this.strictSelect) { //if selection is not strictly limited to the above parameters, add additional objects
			if(selection.size() < numNeeded) {
				ArrayList<Object> possible = new ArrayList<Object>();
				if(flakes.size() > 0) {
					for(int f=0; f<flakes.size(); f++) {
						possible.add(flakes.get(f));
					}
				}
				if(nodules.size() > 0) {
					for(int n=0; n<nodules.size(); n++) {
						possible.add(nodules.get(n));
					}
				}

				if(possible.size() != 0) {
					int iter = Math.min(possible.size(), (numNeeded - selection.size()));
					for(int i=0; i < iter; i++) {
						int index = (int) (Math.random() * possible.size());
						selection.add(possible.get(index));
						possible.remove(index);
					}
				}
			}
		}

		return selection;
	}

	/**
	 * Agent scavenges artifacts from a grid square using the selection parameters  
	 * and adds them to its artifact lists
	 * @param agent
	 */
	public void collectSelectedArtifacts(Agent agent) {
		ArrayList<Object> selection  = select(
				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().getNodules(),
				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().getFlakes(),
				(this.maxUseIntensity - agent.getAgentArtifacts().size())
				);

		for(int i=0; i < selection.size(); i++) {
			if(selection.get(i) instanceof Nodule) {
				agent.collectNodule((Nodule) selection.get(i));
				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().removeNodule((Nodule) selection.get(i));
				this.numberScavengingEvents++; //tracks scavenging events
			} else if(selection.get(i) instanceof Flake) {
				agent.collectFlake((Flake) selection.get(i));
				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().removeFlake((Flake) selection.get(i));
				this.numberScavengingEvents++; //tracks scavenging events
			}
		}

	}

	/**
	 * Resets scavenging event counter to zero to track number of events per model step
	 */
	public void resetScavengeEventCounter() {
		this.numberScavengingEvents = 0;
	}

	/**
	 * Agent removes a flake from a random nodule it is holding
	 * @param agent
	 */
	public void produceBlank(Agent agent) { //create new flake 
		int index = (int) (Math.random() * agent.getAgentNodules().size());
		if(agent.getAgentNodules().get(index).getFlakes().size() != 0) {
			Flake f = agent.getAgentNodules().get(index).removeFlake(agent);
			agent.collectFlake(f);
		}
	}

	/**
	 * Agent does work on a random flake it is holding 
	 * @param agent
	 */
	public void retouchFlake(Agent agent) {
		int index = (int) (Math.random() * agent.getAgentFlakes().size());
		Flake f = agent.getAgentFlakes().get(index);
		f.retouch();
		f.addGroup(agent.getGroup());
		f.addTech(agent.getTech());
		if(f.getFirstTech() != f.getLastTech()) {
			f.setRecycled();
		}
	}

	/**
	 * Agent drops any exhausted objects (i.e. nodules with no more flakes to remove 
	 * & flakes that cannot be retouched anymore)
	 * @param agent
	 */
	public void dropExhaustedArifacts(Agent agent) {
		ArrayList<Object> all = new ArrayList<Object>();
		all.addAll(agent.getAgentNodules());
		all.addAll(agent.getAgentFlakes());

		ArrayList<Nodule> dropN = new ArrayList<Nodule>();
		ArrayList<Flake> dropF = new ArrayList<Flake>();

		for(int i=0; i < all.size(); i++) {
			if(all.get(i) instanceof Nodule) {
				if(((Nodule) all.get(i)).getFlakes().size() == 0) {
					dropN.add((Nodule) all.get(i));
					agent.getAgentNodules().remove((Nodule) all.get(i));
				}
			} else if(all.get(i) instanceof Flake) {
				if(!((Flake) all.get(i)).checkFlakeUsable()) {
					dropF.add((Flake) all.get(i));
					agent.getAgentFlakes().remove((Flake) all.get(i));
				}
			}
		}

		if(dropN.size() != 0) {
			this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).discards(dropN.size());
			this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().discards(dropN.size());
			this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().depositNodules(dropN);
			System.out.println("\t agent dropped nodules");
		}
		if(dropF.size() != 0) {
			this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).discards(dropF.size());
			this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().discards(dropF.size());
			this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().depositFlakes(dropF);
			System.out.println("\t agent dropped flakes");
		}
	}

	/**
	 * Agent drops objects when it is carrying too many according to the selection parameters.
	 * Agents will keep objects that fit the selection parameters over other objects. 
	 * @param agent
	 */
	public void dropArtifacts(Agent agent) { 
		//select flakes/nodules to keep
		if(agent.getAgentFlakes().size() != 0 || agent.getAgentNodules().size() != 0 ) {
			ArrayList<Object> toKeep = select(
					agent.getAgentNodules(),
					agent.getAgentFlakes(),
					this.maxArtifactCarry
					);

			//drop other objects from Agent's lists
			ArrayList<Object> all = new ArrayList<Object>();
			all.addAll(agent.getAgentNodules());
			all.addAll(agent.getAgentFlakes());

			ArrayList<Nodule> dropN = new ArrayList<Nodule>();
			ArrayList<Flake> dropF = new ArrayList<Flake>();

			for(int i=0; i < all.size(); i++) {
				if(!toKeep.contains(all.get(i))) {
					if(all.get(i) instanceof Nodule) {
						dropN.add((Nodule) all.get(i));
						agent.getAgentNodules().remove((Nodule) all.get(i));

					} else if(all.get(i) instanceof Flake) {
						dropF.add((Flake) all.get(i));
						agent.getAgentFlakes().remove((Flake) all.get(i));

					}
				}
			}

			if(dropN.size() != 0) {
				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).discards(dropN.size());
				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().discards(dropN.size());
				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().depositNodules(dropN);
				System.out.println("\t agent dropped nodules");
			}
			if(dropF.size() != 0) {
				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).discards(dropF.size());
				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().discards(dropF.size());
				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().depositFlakes(dropF);
				System.out.println("\t agent dropped flakes");

			}
		}
	}

	/**
	 * Function for calculating the Cortex Ratio of all the artifacts left on the landscape for a given model step
	 * @param year
	 * @return Cortex Ratio 
	 */
	//need to rethink how this would work with geological events happening, 
	//because layers on "surface" would not necessarily have the same age
	public double calculateTotalCortexRatio(int year) {
		int numNods = 0;
		int numFlks = 0;
		int flksOnNods = 0;
		
		for(int i=0; i < this.landscape.getNumRows(); i++) {
			for(int j=0; j < this.landscape.getNumCols(); j++) {
				//find layer matching the year provided
				ArrayList<Layer> layers = this.landscape.getElement(i, j).getLayers();
				for(int l=0; l < layers.size(); l++) {
					if(layers.get(l).getYear() == year) {
						numNods += layers.get(l).getNodules().size();
						numFlks += layers.get(l).getFlakes().size();
						for(int n=0; n<layers.get(l).getNodules().size(); n++) {
							flksOnNods += layers.get(l).getNodules().get(n).getFlakes().size();
						}
					}
				}
			}
		}
		
		double flakevol = numFlks * ((noduleV * 0.05) / avgFlakesOnNodule);
		double nodvol = numNods * (noduleV * (1.0 - 0.05));
		double fnvol = flksOnNods * ((noduleV * 0.05) / avgFlakesOnNodule);
		
		double modeledNodNum = (flakevol + nodvol + fnvol) / noduleV;
		
		double expSA = noduleSA * modeledNodNum;
		double obsSA = ((numFlks + flksOnNods) / avgFlakesOnNodule) * noduleSA;
		
		double CR = obsSA / expSA;
		
		return CR;
	}

	/**
	 * Function for calculating recycling intensity as the recycled proportion of all the artifacts 
	 * left on the landscape for a given model step
	 * @param year
	 * @return recycling intensity
	 */
	//need to rethink how this would work with geological events happening, 
	//because layers on "surface" would not necessarily have the same age
	public double calculateTotalRecyclingIntensity(int year) {
		double recycledItems = 0.0;
		double aSize = 0;
		
		for(int i=0; i < this.landscape.getNumRows(); i++) {
			for(int j=0; j < this.landscape.getNumCols(); j++) {
				//find layer matching the year provided
				ArrayList<Layer> layers = this.landscape.getElement(i, j).getLayers();
				for(int l=0; l < layers.size(); l++) {
					if(layers.get(l).getYear() == year) {
						aSize += layers.get(l).getNodules().size() + layers.get(l).getFlakes().size();
						
						for(int f=0; f < layers.get(l).getFlakes().size(); f++) {
							if(layers.get(l).getFlakes().get(f).checkWasRecycled()) {
								recycledItems += 1;
							}
						}
						for(int n=0; n < layers.get(l).getNodules().size(); n++) {
							Set<Integer> uniqueGroups = new HashSet<Integer>(layers.get(l).getNodules().get(n).getGroups());
							if(uniqueGroups.size() > 1) {
								recycledItems += 1;
							}
						}
					}
				}
			}
		}
		
		return (recycledItems / aSize);
	}

	/**
	 * Function for outputting artifact information for each layer 
	 */
	public void getArtifactData() {
		StringBuilder toAddData = new StringBuilder();

		for(int i=0; i < this.landscape.getNumRows(); i++) {
			for(int j=0; j < this.landscape.getNumCols(); j++) {
				ArrayList<Layer> layers = this.landscape.getElement(i, j).getLayers();
				for(int l=0; l < layers.size(); l++) {
					//create entry for each nodule
					ArrayList<Nodule> nods = layers.get(l).getNodules();
					if(nods.size() != 0 ) {
						for(int n=0; n<nods.size(); n++) {
							//int nsize = nods.get(n).getSize();
							int fleft = 0;
							for(int y=0; y<nods.get(n).getFlakes().size(); y++) {
								fleft += nods.get(n).getFlakes().get(y).getSize();
							}
							boolean nodRecycled = nods.get(n).getFirstTech() != nods.get(n).getLastTech();

							toAddData.append(i + "," + j + "," + this.currentYear + "," + layers.get(l).getYear() + ","	//row,col,model_year,layer_year
									+ "nodule" + ","  									//obj_type
									+ nods.get(n).getSize() + "," 						//size
									+ nods.get(n).getVolume() + ","						//volume
									+ fleft + ","										//cortex (equal to size of flakes left)
									+ "NA" + ","										//stage
									+ nods.get(n).getGroups().size() + ","				//numgroups
									+ nods.get(n).getFirstTech() + ","					//first_tech
									+ nods.get(n).getLastTech() + "," 					//last_tech
									+ nodRecycled + "\n"									//recycled
									);

						}
					}
					//create entry for each flake
					ArrayList<Flake> flakes = layers.get(l).getFlakes();
					if(flakes.size() != 0) {
						for(int f=0; f<flakes.size(); f++) {
							toAddData.append(i + "," + j + "," + this.currentYear + "," + layers.get(l).getYear() + ","	//row,col,model_yar,layer_year
									+ "flake" + ","  									//obj_type
									+ flakes.get(f).getSize() + "," 					//size
									+ flakes.get(f).getVolume() + ","					//volume
									+ flakes.get(f).getSize() + ","						//cortex (equal to size of flake)
									+ flakes.get(f).getStage() + ","					//stage
									+ flakes.get(f).getGroups().size() + ","			//numgroups
									+ flakes.get(f).getFirstTech() + ","				//first_tech
									+ flakes.get(f).getLastTech() + "," 				//last_tech
									+ flakes.get(f).checkWasRecycled() + "\n"			//recycled
									);
						}
					}
				}
			}
		}
		this.artifactdata.append(toAddData.toString());
		
	}

	/**
	 * Function for outputting information for each layer 
	 */
	public void getLayerData() {
		StringBuilder data = new StringBuilder();
		ArrayList<String> params = this.getParameterData();

		for(int i=0; i < this.landscape.getNumRows(); i++) {
			for(int j=0; j < this.landscape.getNumCols(); j++) {
				for(int l=0; l < this.landscape.getElement(i, j).getLayers().size(); l++) {
					String datastring = params.get(1);
					Square square = this.landscape.getElement(i, j);
					Layer layer = this.landscape.getElement(i, j).getLayers().get(l);
					datastring += i + "," + j + "," + this.currentYear + ",";
					datastring += layer.getYear() + ",";
					//nodule count
					datastring += layer.getNodules().size() + ",";
					//flake count
					datastring += layer.getFlakes().size() + ",";
					//cortex ratio
					datastring += layer.calculateCortexRatio(this.noduleV, this.noduleSA, this.avgFlakesOnNodule) + ",";
					// recycling intensity
					datastring += layer.calculateRecyclingIntensity() + ",";
					// discard count
					datastring += layer.getDiscardEvents() + ",";
					//encounter count
					datastring += layer.getEncounters() + ",";
					//manufacture event count
					datastring += layer.getManufactureEvents() + ",";
					//retouch event count
					datastring += layer.getRetouchEvents() + ",";
					//occupation event count
					datastring += square.getOccupationEvents();

					data.append(datastring + "\n");
				}
			}
		}
		this.layerdata.append(data.toString());
	}

	/**
	 * Function for outputting model data at necessary intervals
	 */
	public void getModelData() {
		StringBuilder data = new StringBuilder();
		ArrayList<String> params = this.getParameterData();

		String datastring = params.get(1);
		datastring += this.currentYear + "," + this.numberScavengingEvents + "," ;

		int totalRecycledItems = 0;
		int numDeposits = 0;
		int totalEncounters = 0;
		int totalDiscards = 0;
		int totalManufactures = 0;
		int totalRetouches = 0;
		double totalCR = 0;
		double totalRI = 0;

		for(int i=0; i < this.landscape.getNumRows(); i++) {
			for(int j=0; j < this.landscape.getNumCols(); j++) {
				for(int l=0; l < this.landscape.getElement(i, j).getLayers().size(); l++) {
					Layer layer = this.landscape.getElement(i, j).getLayers().get(l);

					//counter for recycled flakes
					ArrayList<Flake> flakes = layer.getFlakes();
					for(int f=0; f<flakes.size(); f++) {
						if(flakes.get(f).checkWasRecycled()) {
							totalRecycledItems++;
						}
					}
					//counter for recycled nodules
					ArrayList<Nodule> nods = layer.getNodules();
					for(int n=0; n<nods.size(); n++) {
						if(nods.get(n).getFirstTech() != nods.get(n).getLastTech()){
							totalRecycledItems++;
						}
					}

					if(layer.hasFlakes() || layer.hasNodules()) {
						numDeposits++;
					}
					totalEncounters += layer.getEncounters();
					totalDiscards += layer.getDiscardEvents();
					totalManufactures += layer.getManufactureEvents();
					totalRetouches += layer.getRetouchEvents();

					totalCR = this.calculateTotalCortexRatio(layer.getYear());
					totalRI = this.calculateTotalRecyclingIntensity(layer.getYear());
				}
			}
		}
		datastring += totalRecycledItems + "," + numDeposits + "," + totalEncounters + "," + totalDiscards + "," + totalManufactures + "," + totalRetouches + ",";
		datastring += totalCR + "," + totalRI;
		data.append(datastring + "\n");
		this.modeldata.append(data.toString());
	}

	/**
	 * Function for tracking parameter data of the model
	 * @return two Strings with column names and parameter values
	 */
	public ArrayList<String> getParameterData() {
		ArrayList<String> data = new ArrayList<String>();
		data.add("size," +
				"start_year," +
				"timestep," +
				"max_use_intensity," +
				"max_artifact_carry," +
				"max_flake_size," +
				"max_nodules_size," +
				"blank_prob," +
				"scavenge_prob," +
				"num_agents," +
				"overlap," +
				"mu," +
				"size_preference," +
				"flake_preference," +
				"min_suitable_nodule_size," +
				"min_suitable_flake_size," +
				"strict_selection," +
				"erosion_deposition_ratio," +
				"geo_event_freq," +
				"total_steps");

		data.add(this.size*this.size + "," + 
				this.startYear + "," +
				this.timestep + "," +
				this.maxUseIntensity + "," +
				this.maxArtifactCarry + "," +
				this.maxFlakeSize + "," +
				this.maxNoduleSize + "," +
				this.blankProb + "," +
				this.scavengeProb + "," +
				this.totalAgents + "," +
				this.overlap + "," +
				this.mu + "," +
				this.sizePref + "," +
				this.flakePref + "," +
				this.minAcceptableFlakeSize + "," +
				this.minAcceptableNoduleSize + "," +
				this.strictSelect + "," +
				this.EDratio + "," +
				this.geoFreq + "," +
				this.totalSteps + ",");


		return data;
	}

	/**
	 * @return Strings with model data
	 */
	public StringBuilder modelOutput() {
		return this.modeldata;
	}

	/**
	 * @return Strings with layer data
	 */
	public StringBuilder layersOutput() {
		return this.layerdata;
	}

	/**
	 * @return Strings with artifact data
	 */
	public StringBuilder artifactsOutput() {
		return this.artifactdata;
	}

	public void print() { //need to update as deciding what factors we are most interested in 
		System.out.println(this.outputFile + " " + this.name + " parameters:");
		System.out.println("\t time steps: " + this.totalSteps);
		System.out.println("\t squares: " + this.size*this.size);
		System.out.println("\t agents: " + this.totalAgents);
		System.out.println("\t overlap: " + this.overlap);
		System.out.println("\t Levy mu: " + this.mu);

		System.out.println("\t selection parameters: " + "flake preference " + this.flakePref + " size preference " + this.sizePref + " strict preferences " + this.strictSelect);
		
		System.out.println("\t blank probablity: " + this.blankProb);
		System.out.println("\t scavenging probablity: " + this.scavengeProb);
		System.out.println("\t max use intensity: " + this.maxUseIntensity);
		System.out.println("\t max artifact carry: " + this.maxArtifactCarry);
		
		//System.out.println("\t ED ratio: " + this.EDratio);
		//System.out.println("\t geo frequency: " + this.geoFreq);

	}

	public void printAgents() {
		for(int i=0; i < this.agents.size(); i++) {
			this.agents.get(i).print();
		}
	}

}
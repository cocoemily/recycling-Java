package recycling;

import java.util.*;

public class ExtendedModel {

	public String outputFile;
	public String name;

	public int size;
	public int startYear;
	public int currentYear;
	public int timestep;
	public double overlap;

	public int nextId;
	public ArrayList<Agent> agents;
	public int totalAgents;
	public int maxUseIntensity;
	public int maxArtifactCarry;
	public int maxFlakeSize;
	public int maxNoduleSize;
	public double groupPerc;
	public boolean interaction;
	public int interactionRadius;
	public boolean oneWayLearn;
	public double learningProb;
	public double inventProb;
	public double blankProb;

	public Grid landscape;
	public double EDratio;
	public int geoFreq;

	public double mu;

	public double noduleV = 100000;
	public double noduleSA = 11091.8;
	public double avgFlakesOnNodule;

	//selection variables -- need to figure out how to do this


	public ExtendedModel(String of, String name, int size, int startYear, int timestep, int maxUI, int maxAC, int maxFS, int maxNS, int numAgents, double ED, int GF, double overlap, int mu) {
		this.outputFile = of;
		this.name = name;

		this.nextId = 1;
		this.agents = new ArrayList<Agent>();
		this.totalAgents = numAgents;
		this.maxArtifactCarry = maxAC;
		this.maxUseIntensity = maxUI;
		this.groupPerc = 0.5;
		this.interaction = false;
		this.interactionRadius = 2;
		this.oneWayLearn = false;
		this.learningProb = 0.5;
		this.inventProb = 0.5;
		this.blankProb = 0.25;

		this.size = size;
		this.startYear  = startYear;
		this.currentYear = startYear;
		this.timestep = -timestep;
		this.overlap = overlap;

		this.landscape = new Grid(size, startYear);
		this.EDratio = ED;
		this.geoFreq = GF;

		this.mu = mu;
		this.maxFlakeSize = maxFS;
		this.maxNoduleSize = maxNS;

	}

	public void createAgents(int techNum, int numAgents) {
		int counter = nextId;
		for(int i=0; i < numAgents; i++) {
			agents.add(new Agent(counter, techNum, this.interactionRadius));
			counter++;
			this.nextId = counter;
		}

	}

	public void removeAgents(int techNum) {
		this.agents.removeIf(Agent -> Agent.getTech() == techNum);
	}

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
	public void moveAgents(boolean randomMove) {
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

	public void interact(Agent agent) {
		//check for agents in vicinity
		//learn
	}

	public void invent(Agent agent) {
		//change technology type
	}

	public void findNodules(Agent agent, int numNodules) {
		agent.initializeNodules(numNodules, this.maxNoduleSize, this.noduleV, this.maxFlakeSize);
	}

	public void collectRandomArtifacts(Agent agent) { //need to update for collecting both flakes and nodules
		//		//randomly pick up artifacts from agent's current location and add to agent's artifact list
		//		for(int i=agent.getAgentArtifacts().size(); i < this.maxUseIntensity; i++) {
		//			ArrayList<Artifact> artifacts = this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().getArtifacts();
		//			Artifact choice = null; 
		//			if(artifacts.size() != 0) {
		//				double minStage = Double.MAX_VALUE; //picking lowest stage artifacts first
		//				for(int j=0; j < artifacts.size(); j++) {
		//					int index = (int) (Math.random() * artifacts.size());
		//					if(artifacts.get(index).getStage() < minStage) {
		//						minStage = artifacts.get(index).getStage();
		//						choice = artifacts.get(index);
		//					}
		//				}
		//				agent.collectArtifact(choice); //add artifact to agent's artifact list
		//				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().removeArtifact(choice);
		//			}
		//		}
		//		//agent.printArtifactList();
	}

	public void collectSelectedArtifacts(Agent agent) { //need to add in information for selection criteria + flakes vs nodules
		//pick up artifacts based on selection criteria
	}

	public void produceBlanks(Agent agent) { //create new flakes 
		int index = (int) (Math.random() * agent.getAgentNodules().size());
		for(int i=0; i < this.maxUseIntensity; i++) {
			Flake f = agent.getAgentNodules().get(index).removeFlake(agent);
			agent.collectFlake(f);
		}
	}

	public void retouchFlakes(Agent agent) {
		//retouch (use retouch method from Artifact class) artifacts up to maxUI from agent's artifact list
		for(int i=0; i < agent.getAgentFlakes().size(); i++) {
			agent.getAgentFlakes().get(i).retouch();
			agent.getAgentFlakes().get(i).addGroup(agent.getGroup());
			agent.getAgentFlakes().get(i).addTech(agent.getTech());
		}
	}

	public void dropArtifacts(Agent agent) { //need to update to drop flakes and nodules
		//		//remove artifacts from agent's artifact list and add to current layer at current location
		//		int numDrop = 0;
		//		if(agent.getAgentArtifacts().size() > this.maxArtifactCarry) {
		//			numDrop = agent.getAgentArtifacts().size() - this.maxArtifactCarry;
		//		} else {
		//			numDrop = 1;
		//		}
		//		
		//		if(numDrop != 0) {
		//			for(int i=0; i < numDrop; i++) {
		//				int index = (int) (agent.getAgentArtifacts().size() * Math.random());
		//				ArrayList<Artifact> depositList = new ArrayList<Artifact>();
		//				depositList.add(agent.getAgentArtifacts().get(index));
		//				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().depositArtifacts(depositList);
		//				agent.getAgentArtifacts().remove(index);
		//			}
		//		}
	}

	public double calculateTotalCortexRatio(int year) {
		double CR = 0.0;
		for(int i=0; i < this.landscape.getNumRows(); i++) {
			for(int j=0; j < this.landscape.getNumCols(); j++) {
				//find layer matching the year provided
				ArrayList<Layer> layers = this.landscape.getElement(i, j).getLayers();
				for(int l=0; l < layers.size(); l++) {
					if(layers.get(l).getYear() == year) {
						CR += layers.get(l).calculateCortexRatio(this.noduleV, this.noduleSA);
					}
				}
			}
		}
		return CR;
	}

	public double calculateTotalRecyclingIntensity(int year) {
		double RI = 0.0;
		for(int i=0; i < this.landscape.getNumRows(); i++) {
			for(int j=0; j < this.landscape.getNumCols(); j++) {
				//find layer matching the year provided
				ArrayList<Layer> layers = this.landscape.getElement(i, j).getLayers();
				for(int l=0; l < layers.size(); l++) {
					if(layers.get(l).getYear() == year) {
						RI += layers.get(l).calculateRecyclingIntensity();
					}
				}
			}
		}
		return RI;
	}

	public ArrayList<String> gridToSpatial() {
		ArrayList<String> data = new ArrayList<String>();
		//create row names
		data.add("row,col,layer_year,obj_type,size,volume,cortex,stage,numgroups,first_tech,last_tech,recycled"); 

		for(int i=0; i < this.landscape.getNumRows(); i++) {
			for(int j=0; j < this.landscape.getNumCols(); j++) {
				ArrayList<Layer> layers = this.landscape.getElement(i, j).getLayers();
				for(int l=0; l < layers.size(); l++) {
					//create entry for each nodule
					ArrayList<Nodule> nods = layers.get(l).getNodules();
					for(int n=0; n<nods.size(); n++) {
						int nsize = nods.get(n).getSize();
						int fleft = 0;
						for(int y=0; y<nods.get(n).getFlakes().size(); y++) {
							fleft += nods.get(n).getFlakes().get(y).getSize();
						}
						
						data.add(i + "," + j + "," + layers.get(l).getYear() + ","	//row,col,layer_year
								+ "nodule" + ","  									//obj_type
								+ nods.get(n).getSize() + "," 						//size
								+ nods.get(n).getVolume() + ","						//volume
								+ fleft + ","										//cortex (equal to size of flakes left)
								+ "NA" + ","										//stage
								+ nods.get(n).getGroups().size() + ","				//numgroups
								+ nods.get(n).getFirstTech() + ","					//first_tech
								+ nods.get(n).getLastTech() + "," 					//last_tech
								+ "NA" + ","										//recycled
 								);

					}
					//create entry for each flake
					ArrayList<Flake> flakes = layers.get(l).getFlakes();
					for(int f=0; f<flakes.size(); f++) {
						data.add(i + "," + j + "," + layers.get(l).getYear() + ","	//row,col,layer_year
								+ "flake" + ","  									//obj_type
								+ flakes.get(f).getSize() + "," 					//size
								+ flakes.get(f).getVolume() + ","					//volume
								+ flakes.get(f).getSize() + ","						//cortex (equal to size of flake)
								+ flakes.get(f).getStage() + ","					//stage
								+ flakes.get(f).getGroups().size() + ","			//numgroups
								+ flakes.get(f).getFirstTech() + ","				//first_tech
								+ flakes.get(f).getLastTech() + "," 				//last_tech
								+ flakes.get(f).checkWasRecycled()					//recycled
 								);
					}
				}
			}
		}
		return data;
	}

	public void print() {
		System.out.println(this.outputFile + " " + this.name + " parameters:");
		System.out.println("\t squares: " + this.size*this.size);
		System.out.println("\t agents: " + this.totalAgents);
		System.out.println("\t geo frequency: " + this.geoFreq);
		System.out.println("\t ED ratio: " + this.EDratio);
		System.out.println("\t overlap: " + this.overlap);
	}

	public void printAgents() {
		for(int i=0; i < this.agents.size(); i++) {
			this.agents.get(i).print();
		}
	}

}
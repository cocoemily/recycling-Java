package recycling;

import java.util.*;


public class OriginalModel {
	
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
	
	public OriginalModel(String of, String name, int size, int startYear, int timestep, int maxUI, int maxAC, int numAgents, double ED, int GF, double overlap) {
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
		
		this.mu = 1; //need to turn this into a parameter
	
	}
	
	public void createAgents(int techNum, int numAgents) {
		int counter = nextId;
		for(int i=0; i < numAgents; i++) {
			Agent a = new Agent(counter, techNum);
			a.setInteractionRadius(interactionRadius);
			agents.add(a);
			counter++;
			this.nextId = counter;
		}
		
	}
	
	//need to rewrite this function -- something is not working with commented out line
	public void removeAgents(int techNum) {
		//this.agents.removeIf(Agent -> Agent.getTech() == techNum);
		for(int i=0; i<this.agents.size(); i++) {
			if(this.agents.get(i).getTech() == techNum) {
				
			}
		}
	}
	
	public void createRandomSources(double sourceProb, int numArtifacts, int techType) { 
		//initialize landscape with some artifacts
		for(int i=0; i < this.landscape.getNumRows(); i++) {
			for(int j=0; j < this.landscape.getNumCols(); j++) {
				double artifactProb = Math.random();
				if(artifactProb < sourceProb) { //chance for each grid square to start with artifacts
					ArrayList<Artifact> initArtifacts = new ArrayList<Artifact>();
					for(int a = 0; a < numArtifacts; a++) { //make 30 artifacts at each location
						Artifact toAdd = new Artifact();
						toAdd.addTech(techType);
						initArtifacts.add(toAdd);
					}
					this.landscape.getElement(i, j).addArtifacts(initArtifacts);
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
				//add in functionality to set inWindow parameter for agent
				
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
	
	public void produceBlanks(Agent agent, int numBlanks) {
		//create 20 blanks of agent's technology type and add to landscape at current location
		int tech = agent.getTech();
		int group = agent.getGroup();
		
		ArrayList<Artifact> blanks = new ArrayList<Artifact>();
		for(int i=0; i < numBlanks; i++) {
			Artifact a = new Artifact();
			a.addTech(tech);
			a.addGroup(group);
			blanks.add(a);
		}
		this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).addArtifacts(blanks);
	}
	
	public void collectRandomArtifacts(Agent agent) {
		//randomly pick up artifacts from agent's current location and add to agent's artifact list
		for(int i=agent.getAgentArtifacts().size(); i < this.maxUseIntensity; i++) {
			ArrayList<Artifact> artifacts = this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().getArtifacts();
			Artifact choice = null; 
			if(artifacts.size() != 0) {
				double minStage = Double.MAX_VALUE; //picking lowest stage artifacts first
				for(int j=0; j < artifacts.size(); j++) {
					int index = (int) (Math.random() * artifacts.size());
					if(artifacts.get(index).getStage() < minStage) {
						minStage = artifacts.get(index).getStage();
						choice = artifacts.get(index);
					}
				}
				agent.collectArtifact(choice); //add artifact to agent's artifact list
				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().removeArtifact(choice);
			}
		}
		//agent.printArtifactList();
	}
	
	public void collectSelectedArtifacts(Agent agent) { //need to add in information for selection criteria
		//pick up artifacts based on selection criteria
	}
	
	public void retouchArtifacts(Agent agent) {
		//retouch (use retouch method from Artifact class) artifacts up to maxUI from agent's artifact list
		for(int i=0; i < agent.getAgentArtifacts().size(); i++) {
			agent.getAgentArtifacts().get(i).retouch();
			agent.getAgentArtifacts().get(i).addGroup(agent.getGroup());
			agent.getAgentArtifacts().get(i).addTech(agent.getTech());
		}
		//agent.printArtifactList();
	}
	
	public void dropArtifacts(Agent agent) {
		//remove artifacts from agent's artifact list and add to current layer at current location
		int numDrop = 0;
		if(agent.getAgentArtifacts().size() > this.maxArtifactCarry) {
			numDrop = agent.getAgentArtifacts().size() - this.maxArtifactCarry;
		}
		
		if(numDrop != 0) {
			for(int i=0; i < numDrop; i++) {
				int index = (int) (agent.getAgentArtifacts().size() * Math.random());
				ArrayList<Artifact> depositList = new ArrayList<Artifact>();
				depositList.add(agent.getAgentArtifacts().get(index));
				this.landscape.getElement(agent.getCurrentX(), agent.getCurrentY()).getTopLayer().depositArtifacts(depositList);
				agent.getAgentArtifacts().remove(index);
			}
		}
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
package recycling;

import java.util.*;

public class Agent {
	
	private int group;
	private int tech;
	private int curX;
	private int curY;
	private boolean inWindow;
	private ArrayList<Artifact> artifacts;
	private ArrayList<Nodule> nodules;
	private ArrayList<Flake> flakes;
	private int interactionRadius;
	
	public Agent(int group, int tech, int iR) {
		this.group = group;
		this.tech = tech;
		interactionRadius = iR;
		
		artifacts = new ArrayList<Artifact>();
		nodules = new ArrayList<Nodule>();
		flakes = new ArrayList<Flake>();
		
	}
	
	public void initializeFlakes(int f, int maxFS) {
		for(int i=0; i<f; i++) {
			int fSize = (int) ((Math.random() * maxFS) + 1);
			flakes.add(new Flake(fSize));
		}
	}
	
	public void initializeNodules(int n, int maxNS, double nV, int maxFS) {
		for(int i=0; i<n; i++) {
			//int nSize = (int) (20 * ((Math.random() * 2) + 1));
			nodules.add(new Nodule(maxNS, nV, maxFS));
		}
	}
	
	public void setLocation(int x, int y) {
		curX = x;
		curY = y;
	}
	
	public void setInWindow(boolean inside) {
		inWindow = inside;
	}
	
	public boolean checkInWindow() {
		return inWindow;
	}
	
	public int getCurrentX() {
		return curX;
	}
	
	public int getCurrentY() {
		return curY;
	}
	
	public void randomMove(int rows, int cols) {
		this.setLocation((int) (Math.random()* rows), (int) (Math.random()* cols));
		
	}
	
	public void randomWalk(double mu) {
		int[] pHeadings = new int[]{ 45, 90, 135, 180, 225, 270, 315, 360 };
		int index = (int) (Math.random() * pHeadings.length);
		int heading = pHeadings[index];
		System.out.println("Heading: " + heading);
		
		double steplength = Math.pow(Math.random(), (-1/mu));
		int sl = (int) steplength;
		System.out.println("Step length: " + steplength + " -> " + sl);
		
		int newX = 0;
		int newY = 0;
		
		if(heading <= 135) { //heading = 45, 90, 135
			newX = curX - sl;
		} else if (heading >= 225 && heading < 360) { //heading = 225, 270, 315
			newX = curX + sl;
		} else { //heading = 180, 360
			newX = curX;
		}
		
		if(heading >= 135 && heading <= 225) { //heading = 135, 180, 225
			newY = curY - sl;
		} else if(heading <= 45 || heading >= 315) { //heading = 45, 315, 360
			newY = curY + sl;
		} else { //heading = 90, 270
			newY = curY;
		}
		
		this.setLocation(newX, newY);
		
	}
	
	public int getTech() {
		return this.tech;
	}
	
	public int getGroup() {
		return this.group;
	}
	
	public int getInteractionRadius() {
		return this.interactionRadius;
	}
	
	public ArrayList<Artifact> getAgentArtifacts() {
		return this.artifacts;
	}
	
	public ArrayList<Nodule> getAgentNodules() {
		return this.nodules;
	}
	
	public ArrayList<Flake> getAgentFlakes() {
		return this.flakes;
	}
	
	public void collectArtifact(Artifact a) {
		this.artifacts.add(a);
	}
	
	public void collectFlake(Flake f) {
		this.flakes.add(f);
	}
	
	public void print() {
		System.out.println("Agent " + this.group + " of type " + this.tech + " at (" + this.curX + "," + this.curY + ")");
	}
	
	public void printArtifactList() {
		for(int i=0; i < this.artifacts.size(); i++) {
			artifacts.get(i).print();
		}
	}
	
}
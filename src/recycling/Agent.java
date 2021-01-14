package recycling;

import java.util.*;

public class Agent {
	
	private int group;
	private int tech;
	private int curX;
	private int curY;
	private int destX;
	private int destY;
	private ArrayList<Artifact> artifacts;
	private int interactionRadius;
	
	public Agent(int group, int tech, int iR) {
		this.group = group;
		this.tech = tech;
		interactionRadius = iR;
		
		artifacts = new ArrayList<Artifact>();
	}
	
	public void setLocation(int x, int y) {
		curX = x;
		curY = y;
	}
	
	public int getCurrentX() {
		return curX;
	}
	
	public int getCurrentY() {
		return curY;
	}
	
	public void setDestination(int x, int y) {
		destX = x;
		destY = y;
	}
	
	public void randomMove(int rows, int cols) {
		this.setLocation((int) (Math.random()* rows), (int) (Math.random()* cols));
		
	}
	
	public void randomWalk() {
		
	}
	
	public int getTech() {
		return this.tech;
	}
	
	public int getGroup() {
		return this.group;
	}
	
	public ArrayList<Artifact> getAgentArtifacts() {
		return this.artifacts;
	}
	
	public void collectArtifact(Artifact a) {
		this.artifacts.add(a);
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
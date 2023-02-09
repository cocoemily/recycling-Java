package recycling;

import java.util.*;
import java.awt.Point;

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

	private ArrayList<Point> locationList;

	public Agent(int group, int tech) {
		this.group = group;
		this.tech = tech;
		this.interactionRadius = 0;

		artifacts = new ArrayList<Artifact>();
		nodules = new ArrayList<Nodule>();
		flakes = new ArrayList<Flake>();

		locationList = new ArrayList<Point>();

	}

	/**
	 * Create new flakes not from nodules
	 * @param f number of flakes to make
	 * @param maxFS maximum size flakes can be
	 */
//	public void initializeFlakes(int f, int maxFS) {
//		for(int i=0; i<f; i++) {
//			int fSize = (int) ((Math.random() * maxFS) + 1);
//			flakes.add(new Flake(fSize));
//		}
//	}

	/**
	 * Create new nodules
	 * @param n number of nodules to make
	 * @param maxNS maximum size nodules can be
	 * @param nV nodule volume
	 * @param maxFS maximum size flakes on nodules can be
	 */
	public void initializeNodules(int n, int maxNS, double nV, double nSA, int maxFS) {
		for(int i=0; i<n; i++) {
			nodules.add(new Nodule(maxNS, nV, nSA, maxFS));
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

	/** 
	 * Move to a random grid square
	 * @param rows number of possible grid rows
	 * @param cols number of possible grid columns
	 */
	public void randomMove(int rows, int cols) {
		this.setLocation((int) (Math.random()* rows), (int) (Math.random()* cols));
		this.locationList.add(new Point(this.curX, this.curY));
		this.setInWindow(true);

	}

	/**
	 * Move via Levy walk 
	 * @param mu Levy walk function parameter
	 */
	public void randomWalk(double mu) {
		int[] pHeadings = new int[]{ 0, 45, 90, 135, 180, 225, 270, 315, 360 };
		int index = (int) (Math.random() * pHeadings.length);
		int heading = pHeadings[index];
		//System.out.println("Heading: " + heading);

		double steplength = Math.pow(Math.random(), (-1/mu));
		int sl = (int) steplength;
		//System.out.println("Step length: " + steplength + " -> " + sl);

		int newX = 0;
		int newY = 0;

		if(heading == 0) { //stay in same location
			newX = curX;
			newY = curY;
			
		} else {
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
		}

		this.setLocation(newX, newY);
		this.locationList.add(new Point(this.curX, this.curY));

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

	public ArrayList<Nodule> getAgentNodules() {
		return this.nodules;
	}

	public ArrayList<Flake> getAgentFlakes() {
		return this.flakes;
	}

	public boolean hasObjects() {
		if(this.flakes.size() != 0 || this.nodules.size() != 0) {
			return true;
		} else {
			return false;
		}
	}

	public int numberCurrentObjects() {
		return this.flakes.size() + this.nodules.size();
	}

	public void collectArtifact(Artifact a) {
		this.artifacts.add(a);
	}

	public void collectNodule(Nodule n) {
		this.nodules.add(n);
	}

	public void collectFlake(Flake f) {
		this.flakes.add(f);
	}

	public void setInteractionRadius(int i) {
		this.interactionRadius = i;
	}

	public int getInteractionRadius() {
		return this.interactionRadius;
	}

	public void print() {
		System.out.println("Agent " + this.group + " of type " + this.tech + " at (" + this.curX + "," + this.curY + ")");
	}

	public void printArtifactList() {
		for(int i=0; i < this.artifacts.size(); i++) {
			artifacts.get(i).print();
		}
	}

	public void printAllObjects() {
		if(this.nodules.size() > 0) {
			for(int i=0; i < this.nodules.size(); i++) {
				nodules.get(i).print();
			}
		}
		if(this.flakes.size() > 0) {
			for(int i=0; i < this.flakes.size(); i++) {
				flakes.get(i).print();
			}
		}
	}

	public ArrayList<String> outputLocations() {
		ArrayList<String> data = new ArrayList<String>();
		data.add("x,y");

		for(int i=0; i < this.locationList.size(); i++) {
			data.add(locationList.get(i).getX() + "," + locationList.get(i).getY());
		}

		return data;
	}

}
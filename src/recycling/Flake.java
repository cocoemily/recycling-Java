package recycling;

import java.util.ArrayList;

public class Flake {
	
	private int size;
	private double volume; 
	private int stage;
	private int shape; //0 is flake, 1 is blade
	private ArrayList<Integer> groups;
	private ArrayList<Integer> techs;
	
	private boolean usable;
	private boolean recycled;
	
	public Flake(int s) {
		this.size = s;
		this.volume = (this.size * 0.05) * 100000; //proportion same as Davies et al 2018
		this.stage = 0;
		this.groups = new ArrayList<Integer>();
		this.techs = new ArrayList<Integer>();
		this.usable = true;
		this.recycled = false;
	}
	
	public void addGroup(int group) {
		this.groups.add(group);
	}
	
	public ArrayList<Integer> getGroups() {
		return groups;
	}
	
	public void addTech(int tech) {
		this.techs.add(tech);
	}
	
	public void retouch() {
		this.stage += 1;
	}
	
	public int getSize() {
		return this.size;
	}
	
	public double getVolume() {
		return this.volume;
	}
	
	public int getStage() {
		return this.stage;
	}
	
	public int getFirstTech() {
		return this.techs.get(0);
	}
	
	public int getLastTech() {
		return this.techs.get(this.techs.size()-1);
	}
	
	public boolean checkFlakeUsable() {
		return this.usable;
	}
	
	public void setUnusable() {
		this.usable = false;
	}
	
	public void setRecycled() {
		this.recycled = true;
	}
	
	public boolean checkWasRecycled() {
		return this.recycled;
	}
	
	public void print() {
		System.out.print("flake: stage(" + stage + ") size(" + size + ")");
		if(this.techs.size() > 0) {
			System.out.print(" blank tech (" + this.getFirstTech() + ")");
		}
		if(this.stage > 0) {
			System.out.print(" retouch tech (" + this.getLastTech() + ")");
		}
		System.out.print("\n");
	}
	
}
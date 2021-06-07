package recycling;

import java.util.*;

public class Nodule {
	
	private int size;
	private double volume;
	private ArrayList<Flake> flakes;
	
	private ArrayList<Integer> groups;
	private ArrayList<Integer> techs;
	
	
	public Nodule(int s, double noduleV, int maxFS) {
		this.size = s;
		this.volume = noduleV;
		this.flakes = new ArrayList<Flake>();
		
		this.groups = new ArrayList<Integer>();
		this.techs = new ArrayList<Integer>();
		
		int SA = s;
		while(SA > 0) {
			if(SA > maxFS) {
				int fSize = (int) ((Math.random() * maxFS) + 1);
				flakes.add(new Flake(fSize));
				SA -= fSize;
			} else {
				int fSize = (int) ((Math.random() * maxFS) + 1);
				flakes.add(new Flake(fSize));
				SA -= fSize;
			}
			
		}
	}
	
	public int getSize() {
		return size;
	}
	
	public double getVolume() {
		return volume;
	}
	
	public ArrayList<Flake> getFlakes() {
		return flakes;
	}
	
	public void addGroup(int group) {
		this.groups.add(group);
	}
	
	public ArrayList<Integer> getGroups() {
		return this.groups;
	}
	
	public void addTech(int tech) {
		this.techs.add(tech);
	}
	
	public ArrayList<Integer> getTechs() {
		return this.techs;
	}
	
	public int getFirstTech() {
		if(this.techs.size() == 0 ) {
			return 0;
		} else {
			return this.techs.get(0);
		}
	}
	
	public int getLastTech() {
		if(this.techs.size() == 0 ) {
			return 0;
		} else {
			return this.techs.get(this.techs.size()-1);
		}
	}
	
	public Flake removeFlake(Agent a) {
		int index = (int) (Math.random() * this.flakes.size());
		Flake f = this.flakes.remove(index);
		f.addGroup(a.getGroup());
		f.addTech(a.getTech());
		this.volume -= (this.volume * f.getVolume());
		this.addGroup(a.getGroup());
		this.addTech(a.getTech());
		return f;
	}
	
	public void print() {
		System.out.print("nodule: size(" + size + ")");
		System.out.print(" flakes left (" + this.flakes.size() + ")");
		System.out.print("\n");
	}
	
	
}

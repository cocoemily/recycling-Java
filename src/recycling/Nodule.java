package recycling;

import java.util.*;

/**
 * Nodule (core) artifacts that have a size and particular number of flakes that can be removed
 * for the ExtendedModel 
 * @author emilycoco
 *
 */
public class Nodule {
	
	private int size;
	private double volume;
	private double surfaceArea;
	private double flakePropVol = 0.04;
	private ArrayList<Flake> flakes;
	
	private ArrayList<Integer> groups;
	private ArrayList<Integer> techs;
	
	private boolean recycled;
	
	private int discardYear;
	
	
	public Nodule(int s, double noduleV, double noduleSA, int maxFS) {
		this.size = s;
		this.volume = noduleV;
		this.surfaceArea = noduleSA;
		this.flakes = new ArrayList<Flake>();
		
		this.groups = new ArrayList<Integer>();
		this.techs = new ArrayList<Integer>();
		
		int SA = s;
		ArrayList<Integer> flakeSizes = new ArrayList<Integer>();
		while(SA > 0) {
			if(SA > maxFS) {
				int fSize = (int) ((Math.random() * maxFS) + 1);
				flakeSizes.add(fSize);
				SA -= fSize;
			} else {
				int fSize = SA;
				flakeSizes.add(fSize);
				SA -= fSize;
			}
			
		}
		
		int flakesOnNod = flakeSizes.size();
		for(int f = 0; f<flakeSizes.size(); f++) {
			int fSize = flakeSizes.get(f);
			double fVol = (this.volume * (fSize * this.flakePropVol));
			double fSA = (this.surfaceArea / this.size) * fSize;
			flakes.add(new Flake(fSize, fVol, fSA));
		}
		
		
		this.recycled = false;
		this.discardYear = 0;
	}
	
	public int getSize() {
		return size;
	}
	
	public double getVolume() {
		return volume;
	}

	public double getSurfaceArea() {
		return this.surfaceArea;
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
	
	public void setRecycled() {
		this.recycled = true;
	}
	
	public boolean checkWasRecycled() {
		return this.recycled;
	}
	
	public Flake removeFlake(Agent a) {
		int index = (int) (Math.random() * this.flakes.size());
		Flake f = this.flakes.remove(index);
		f.addGroup(a.getGroup());
		f.addTech(a.getTech());
		this.volume -= f.getVolume();
		this.surfaceArea -= f.getSurfaceArea();
		if(this.surfaceArea < 0) {
			this.surfaceArea = 0;
		}
		this.addGroup(a.getGroup());
		this.addTech(a.getTech());
		return f;
	}
	
	public int getDiscardYear() {
		return this.discardYear;
	}
	
	public void setDiscardYear(int currentYear) {
		this.discardYear = currentYear;
	}
	
	public void print() {
		System.out.print("nodule: size(" + size + ")");
		System.out.print(" flakes left (" + this.flakes.size() + ")");
		System.out.print("\n");
	}
	
	
}

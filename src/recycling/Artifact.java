package recycling;

import java.util.ArrayList;

/**
 * Simple artifacts that can be retouched indefinitely for Original Model
 * @author emilycoco
 *
 */
public class Artifact {
	
	private int stage;
	private int initialDiscardYear;
	private ArrayList<Integer> groups;
	private ArrayList<Integer> techs;
	
	public Artifact() {
		stage = 0;
		initialDiscardYear = 0;
		groups = new ArrayList<Integer>();
		techs = new ArrayList<Integer>();
	}
	
	public void addGroup(int group) {
		this.groups.add(group);
	}
	
	public void addTech(int tech) {
		this.techs.add(tech);
	}
	
	public void retouch() {
		this.stage += 1;
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
	
	public int getDiscardYear() {
		return this.initialDiscardYear;
	}
	
	public void print() {
		System.out.print("artifact: stage(" + stage + ")");
		System.out.print(" blank tech (" + this.getFirstTech() + ")");
		if(this.stage > 0) {
			System.out.print(" retouch tech (" + this.getLastTech() + ")");
		}
		System.out.print("\n");
	}
	
}
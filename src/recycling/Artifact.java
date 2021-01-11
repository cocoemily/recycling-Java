package recycling;

import java.util.ArrayList;

public class Artifact {
	
	private int stage;
	private ArrayList<Integer> groups;
	private ArrayList<Integer> techs;
	
	//need to add in properties for types of artifacts
	
	public Artifact() {
		stage = 0;
		groups = new ArrayList<Integer>();
		techs = new ArrayList<Integer>();
		
		//initialize other properties
	}
	
	public void addGroup(int group) {
		groups.add(group);
	}
	
	public void addTech(int tech) {
		techs.add(tech);
	}
	
	public void retouch() {
		stage += 1;
	}
	
	
	public void print() {
		System.out.println("artifact: stage(" + stage + ")");
	}
	
}
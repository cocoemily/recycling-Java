package recycling;

import java.util.*;

public class Square {
	
	ArrayList<Layer> yearsBP;
	
	public Square(int startYear) {
		yearsBP = new ArrayList<Layer>();
		yearsBP.add(new Layer(startYear));
	}
	
	public void erode() {
		if(yearsBP.size() == 1) {
			return;
		}
		else {
			int last = yearsBP.size() - 1;
			yearsBP.remove(last);
		}
	}
	
	public void deposit(int currentYear) {
		yearsBP.add(new Layer(currentYear));
	}
	
	public boolean hasArtifacts() {
		return yearsBP.get(yearsBP.size()).hasArtifacts();
	}
	
	public void addArtifacts(ArrayList<Artifact> newArtifacts) {
		yearsBP.get((yearsBP.size()-1)).depositArtifacts(newArtifacts);
	}
	
	public Layer getTopLayer() {
		return yearsBP.get(yearsBP.size()-1);
	}
	
	public ArrayList<Layer> getLayers() {
		return yearsBP;
	}
	
	public void print() {
		for(int i = 0; i < yearsBP.size(); ++i) {
			//System.out.println(yearsBP.get(i).getYear() + ": has artifacts - " +
					//yearsBP.get(i).hasArtifacts()); 
			yearsBP.get(i).print();
		}
	}
	
	

}

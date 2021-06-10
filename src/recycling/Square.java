package recycling;

import java.util.*;

/**
 * Object for storing an Array List of Layer objects that serves to track the geological events 
 * that occur at a particular Grid location 
 * @author emilycoco
 *
 */
public class Square {
	
	ArrayList<Layer> yearsBP;
	
	private int numOccupations; 
	private int numDiscard;
	
	public Square(int startYear) {
		yearsBP = new ArrayList<Layer>();
		yearsBP.add(new Layer(startYear));
		
		numOccupations = 0;
		numDiscard = 0;
	}
	
	/**
	 * Removes a Layer when erosion happens
	 */
	public void erode() {
		if(yearsBP.size() == 1) {
			return;
		}
		else {
			int last = yearsBP.size() - 1;
			yearsBP.remove(last);
		}
	}
	
	/**
	 * Adds a new Layer when deposition happens
	 * @param currentYear current model step
	 */
	public void deposit(int currentYear) {
		yearsBP.add(new Layer(currentYear));
	}
	
	public boolean hasArtifacts() {
		return yearsBP.get(yearsBP.size()).hasArtifacts();
	}
	
	public void addArtifacts(ArrayList<Artifact> newArtifacts) {
		yearsBP.get((yearsBP.size()-1)).depositArtifacts(newArtifacts);
	}
	
	public void addNodules(ArrayList<Nodule> newNodules) {
		yearsBP.get((yearsBP.size()-1)).depositNodules(newNodules);
	}
	
	public void addFlakes(ArrayList<Flake> newFlakes) {
		yearsBP.get((yearsBP.size()-1)).depositFlakes(newFlakes);
	}
	
	public Layer getTopLayer() {
		return yearsBP.get(yearsBP.size()-1);
	}
	
	public ArrayList<Layer> getLayers() {
		return yearsBP;
	}
	
	public void occupied() {
		numOccupations++;
	}
	
	public int getOccupationEvents() {
		return numOccupations;
	}
	
	public void discards(int num) {
		numDiscard += num;
	}
	
	public int getDiscardEvents() {
		return numDiscard;
	}
	
	public void print() {
		for(int i = 0; i < yearsBP.size(); ++i) {
			//System.out.println(yearsBP.get(i).getYear() + ": has artifacts - " +
					//yearsBP.get(i).hasArtifacts()); 
			yearsBP.get(i).print();
		}
	}
	
	

}

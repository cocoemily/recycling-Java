package recycling;

import java.util.*;

/**
 * Object for storing artifact lists associated with a particular model year
 * @author emilycoco
 *
 */
public class Layer {

	private boolean artifacts;
	private boolean nodules;
	private boolean flakes;
	
	private int date;
	
	private ArrayList<Artifact> artifactlist;
	private ArrayList<Nodule> nodulelist;
	private ArrayList<Flake> flakelist;
	
	private int numEncounters; //how many times the layer has been visited
	private int numScavenge;
	private int numDiscard; //how many discard events into the layer
	private int exposureTime; //how long the layer is on the surface //will need to be added with geo event functionality
	private int manufactureEvents; //how many blanks produced + retouch actions at the layer location
	private int retouchEvents; //how many flake retouches happen at the layer location

	public Layer(int d) {
		artifacts = false;
		nodules = false;
		flakes = false;
		date = d;
		artifactlist = new ArrayList<Artifact>();

		nodulelist = new ArrayList<Nodule>();
		flakelist = new ArrayList<Flake>();
		
		numEncounters = 0;
		numDiscard = 0;
		numScavenge = 0;
		exposureTime = 0;
		manufactureEvents = 0;
		retouchEvents = 0;
	}

	public boolean hasArtifacts() {
		return artifacts;
	}
	
	public boolean hasFlakes() {
		return flakes;
	}
	
	public boolean hasNodules() {
		return nodules;
	}

	public ArrayList<Artifact> getArtifacts() {
		return artifactlist;
	}
	
	public ArrayList<Nodule> getNodules() {
		return nodulelist;
	}
	
	public ArrayList<Flake> getFlakes() {
		return flakelist;
	}

	public int getYear() {
		return date;
	}

	public void depositArtifacts(ArrayList<Artifact> newArtifacts) {
		for(int i=0; i < newArtifacts.size(); i++) {
			artifactlist.add(newArtifacts.get(i));
		}
		artifacts = true;

	}

	public void depositNodules(ArrayList<Nodule> newNodules) {
		for(int i=0; i < newNodules.size(); i++) {
			nodulelist.add(newNodules.get(i));
		}
		nodules = true;
	}

	public void depositFlakes(ArrayList<Flake> dropF) {
		for(int i=0; i < dropF.size(); i++) {
			flakelist.add(dropF.get(i));
		}
		flakes = true;
	}

	public void removeArtifact(Artifact a) {
		artifactlist.remove(a);
		if(artifactlist.size() == 0) {
			artifacts = false;
		}
	}

	public void removeNodule(Nodule n) {
		nodulelist.remove(n);
		if(nodulelist.size() == 0) {
			nodules = false;
		}
	}

	public void removeFlake(Flake f) {
		flakelist.remove(f);
		if(flakelist.size() == 0) {
			flakes = false;
		}
	}
	
	public void discards(int num) {
		numDiscard += num;
	}
	
	public int getDiscardEvents() {
		return numDiscard;
	}
	
	public void scavenged() {
		numScavenge ++;
	}
	
	public int getScavengeEvents() {
		return numScavenge;
	}
	
	public void exposed() {
		exposureTime++;
	}
	
	public int getExposureTime() {
		return exposureTime;
	}
	
	public void encounter() {
		numEncounters++;
	}
	
	public int getEncounters() {
		return numEncounters;
	}
	
	public void manufactured() {
		manufactureEvents++;
	}
	
	public int getManufactureEvents() {
		return manufactureEvents;
	}
	
	public void retouched() {
		retouchEvents++;
	}
	
	public int getRetouchEvents() {
		return retouchEvents;
	}
	
	/**
	 * Function for calculating the Cortex Ratio of all the artifacts in layer
	 * @param noduleV nodule volume
	 * @param noduleSA nodule surface area
	 * @param avgFlakesPerNodule average number of flakes per nodule
	 * @return Cortex Ratio
	 */
	public double calculateCortexRatio(double noduleV, double noduleSA, double avgFlakesPerNodule) {
		double flakevol = 0;
		double nodvol = 0;
		double flsa = 0;
		double nodsa = 0;
		
		for(int f=0; f<this.getFlakes().size(); f++ ) {
			flakevol += this.getFlakes().get(f).getVolume();
			flsa += this.getFlakes().get(f).getSurfaceArea();
		}

		for(int n=0; n<this.getNodules().size(); n++) {
			nodvol += this.getNodules().get(n).getVolume();
			nodsa += this.getNodules().get(n).getSurfaceArea();
		
		}
		
		double modeledNodNum = (flakevol + nodvol) / noduleV;

		double expSA = noduleSA * modeledNodNum;
		double obsSA = flsa + nodsa;
		
		double CR = obsSA / expSA;
		
		return CR;
	}
	
	public double calculateAssemblageVol() {
		double vol = 0;
		
		for(int i=0; i < this.flakelist.size(); i++) {
			vol += this.flakelist.get(i).getVolume();
		}
		for(int i=0; i < this.nodulelist.size(); i++) {
			vol += this.nodulelist.get(i).getVolume();
		}
		
		return vol;
	}
	
	public double calculateAssemblageSA() {
		double vol = 0;
		
		for(int i=0; i < this.flakelist.size(); i++) {
			vol += this.flakelist.get(i).getSurfaceArea();
		}
		for(int i=0; i < this.nodulelist.size(); i++) {
			vol += this.nodulelist.get(i).getSurfaceArea();
		}
		
		return vol;
	}
	
	/**
	 * Function for calculating recycling intensity as the recycled proportion of all the artifacts in the layer
	 * @return recycling intensity
	 */
	public double calculateRecyclingIntensity() { //what proportion of artifacts have been recycled
		double recycledItems = 0.0;
		double aSize = this.flakelist.size() + this.nodulelist.size();
		for(int i=0; i < this.flakelist.size(); i++) {
			if(this.flakelist.get(i).checkWasRecycled()) {
				recycledItems += 1;
			}
		}
		for(int i=0; i < this.nodulelist.size(); i++) {
			Set<Integer> uniqueGroups = new HashSet<Integer>(this.nodulelist.get(i).getGroups());
			if(uniqueGroups.size() > 1) {
				recycledItems += 1;
			}
		}
		return (recycledItems / aSize);
	}


	public void print() {
		System.out.println("Age: " + date + " , Artifacts:");
		if(artifacts || nodules || flakes) {
			if(artifacts) {
				for(int i=0; i < artifactlist.size(); i++) {
					System.out.print("\t");
					artifactlist.get(i).print(); 
				}
			} 
			if(nodules) {
				for(int i=0; i < nodulelist.size(); i++) {
					System.out.print("\t");
					nodulelist.get(i).print(); 
				}
			} 
			if(flakes) {
				for(int i=0; i < flakelist.size(); i++) {
					System.out.print("\t");
					flakelist.get(i).print(); 
				}
			} 
		} else {
			System.out.println("\t No artifacts");
		}
	}


}

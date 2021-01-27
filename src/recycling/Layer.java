package recycling;

import java.util.*;

public class Layer {

	private boolean artifacts;
	private boolean nodules;
	private boolean flakes;
	
	private int date;
	
	private ArrayList<Artifact> artifactlist;
	private ArrayList<Nodule> nodulelist;
	private ArrayList<Flake> flakelist;

	public Layer(int d) {
		artifacts = false;
		nodules = false;
		flakes = false;
		date = d;
		artifactlist = new ArrayList<Artifact>();

		nodulelist = new ArrayList<Nodule>();
		flakelist = new ArrayList<Flake>();
	}

	public boolean hasArtifacts() {
		return artifacts;
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

	public void depositFlakes(ArrayList<Nodule> newFlakes) {
		for(int i=0; i < newFlakes.size(); i++) {
			nodulelist.add(newFlakes.get(i));
		}
		flakes = true;
	}

	public void removeArtifact(Artifact a) {
		artifactlist.remove(a);
	}

	public void removeNodule(Nodule n) {
		nodulelist.remove(n);
	}

	public void removeFlake(Artifact f) {
		flakelist.remove(f);
	}
	
	public double calculateCortexRatio(double noduleV, double noduleSA) {
		//flake volume
		double flakeV = 0;
		for(int i=0; i < this.flakelist.size(); i ++) {
			flakeV += this.flakelist.get(i).getVolume();
		}
		
		//core volume
		double coreV = 0;
		for(int i=0; i < this.nodulelist.size(); i++) {
			coreV += this.nodulelist.get(i).getVolume();
		}
		
		//assemblage volume
		double assemV = flakeV + coreV;
		
		//expected surface area
		double eNN = assemV / noduleV;
		double eSA =  noduleSA * eNN;
		
		//observed surface area 
		double oSA = 0;
		int totalFlakeSize = 0;
		for(int i=0; i < this.nodulelist.size(); i++) {
			ArrayList<Flake> flakes = this.nodulelist.get(i).getFlakes();
			for(int f=0; f < flakes.size(); f++) {
				totalFlakeSize += flakes.get(f).getSize();
			}
		}
		for(int i=0; i < this.flakelist.size(); i++) {
			totalFlakeSize += flakelist.get(i).getSize();
		}
		
		double averageFlakesPerNodule = 20.0; //need to figure out a way to update this
		oSA = (totalFlakeSize / averageFlakesPerNodule) * noduleSA;
		
		
		return (oSA / eSA);
	}
	
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
		if(artifacts) {
			for(int i=0; i < artifactlist.size(); i++) {
				System.out.print("\t");
				artifactlist.get(i).print(); 
			}
		} else if(nodules) {
			for(int i=0; i < nodulelist.size(); i++) {
				System.out.print("\t");
				nodulelist.get(i).print(); 
			}
		} else if(flakes) {
			for(int i=0; i < flakelist.size(); i++) {
				System.out.print("\t");
				flakelist.get(i).print(); 
			}
		} else {
			System.out.println("\t No artifacts");
		}
	}

	//add in function to calculate Cortex Ratio per layer assemblage


}

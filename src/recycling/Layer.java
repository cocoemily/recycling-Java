package recycling;

import java.util.ArrayList;

public class Layer {
	
	private boolean artifacts;
	private int date;
	private ArrayList<Artifact> artifactlist;
	
	public Layer(int d) {
		artifacts = false;
		date = d;
		artifactlist = new ArrayList<Artifact>();
		
	}
	
	public boolean hasArtifacts() {
		return artifacts;
	}
	
	public ArrayList<Artifact> getArtifacts() {
		return artifactlist;
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
	
	public void removeArtifact(Artifact a) {
		artifactlist.remove(a);
	}
	
	
	public void print() {
		System.out.println("Age: " + date + " , Artifacts:");
		if(artifacts) {
			for(int i=0; i < artifactlist.size(); i++) {
				System.out.print("\t");
				artifactlist.get(i).print();
			}
		} else {
			System.out.println("\t No artifacts");
		}
	}

}

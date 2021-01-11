package recycling;

import java.util.*;

public class TestSquare {

	public static void main(String[] args) {
		int year = 2000;
		int yearStep = -10;
		Square test = new Square(year);
		for(int i=0; i < 10; i++) {
			year += yearStep;
			double erodeOrDeposit = Math.random();
			if(erodeOrDeposit > 0.8) {
				test.erode();
				System.out.println("erode");
			}
			else {
				test.deposit(year);
				System.out.println("deposit");
			}
		}
		//test.print();
		
		
		Artifact a1 = new Artifact();
		Artifact a2 = new Artifact();
		Artifact a3 = new Artifact();
		ArrayList<Artifact> newArtifacts = new ArrayList<Artifact>();
		newArtifacts.add(a1);
		newArtifacts.add(a2);
		newArtifacts.add(a3);
		
		test.addArtifacts(newArtifacts);
		test.print();
		
		

	}

}

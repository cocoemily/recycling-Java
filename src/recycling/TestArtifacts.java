package recycling;

import java.util.*;

public class TestArtifacts {

	public static void main(String[] args) {

		//		Nodule n = new Nodule(20, 100000, 2);
		//		//n.print();
		//		
		//		for(int i=0; i < n.getFlakes().size(); i++) {
		//			//n.getFlakes().get(i).print();
		//		}
		//		
		//		int test = (int) ((Math.random() * (20 / 10)) + 2) * 10;
		//		
		//		System.out.println(test);


		Agent a = new Agent(1, 1);
		for(int i=0; i < 5; i++) {
			a.collectNodule(new Nodule(20, 100000, 2));
		}
		for(int i=0; i < 4; i++) {
			if((Math.random()) > 0.5) {
				a.collectFlake(new Flake(2));
			} else {
				a.collectFlake(new Flake(1));
			}
		}
		a.printAllObjects();
		
		System.out.println("");
		
		
		ExtendedModel model = new ExtendedModel(
				"run_1",	//outputFile
				"test", 	//name
				5, 			//size 
				250000, 	//startYear
				100, 		//timestep
				30, 		//maxUI
				5, 			//maxAC
				2, 			//maxFS
				10, 		//maxNS
				0.5,		//bProb
				0.5,		//sProb
				0, 			//overlap
				1.0,  		//mu
				true, 		//sizePref
				true, 		//flakePref
				2, 			//minFS
				10, 		//minNS
				true, 		//strict
				0.5, 		//ED
				0, 			//GF
				1000		//totalSteps
				);
		
		ArrayList<Object> selection = model.select(a.getAgentNodules(), a.getAgentFlakes(), 5);
		
		for(int i = 0; i < selection.size(); i++) {
			if(selection.get(i) instanceof Nodule ) {
				((Nodule) selection.get(i)).print();
			} else if(selection.get(i) instanceof Flake) {
				((Flake) selection.get(i)).print();
			}
		}

		

	}

}
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
			a.collectNodule(new Nodule(20, 100000, 1));
		}
		for(int i=0; i < 5; i++) {
			if((Math.random()) > 0.5) {
				a.collectFlake(new Flake(1));
			} else {
				a.collectFlake(new Flake(1));
			}
		}
		a.printAllObjects();
		
		
		ExtendedModel model = new ExtendedModel(
				"run_1",	//outputFile
				"test", 	//name
				5, 			//size 
				250000, 	//startYear
				100, 		//timestep
				30, 		//maxUI
				5, 			//maxAC
				1, 			//maxFS
				20, 		//maxNS
				0.5,		//bProb
				0.5,		//sProb
				1000, 		//numAgents
				0, 			//overlap
				1.0,  		//mu
				true, 		//sizePref
				true, 		//flakePref
				1, 		//minFS
				10, 			//minNS
				false, 		//strict
				0.5, 		//ED
				0, 			//GF
				1000		//totalSteps
				);
		
		model.select(a.getAgentNodules(), a.getAgentFlakes(), 5);

		//		ArrayList<Object> all = new ArrayList<Object>();
		//		all.addAll(a.getAgentFlakes());
		//		all.addAll(a.getAgentNodules());
		//		
		//		for(int i=0; i < all.size(); i++) {
		//			if(all.get(i) instanceof Nodule) {
		//				((Nodule) all.get(i)).print();
		//			}
		//		}

//		ArrayList<Nodule> n = new ArrayList<Nodule>();
//		double totalF = 0;
//
//		for(int i=0; i < 1000; i++) {
//			n.add(new Nodule(20, 100000, 6));
//			totalF += n.get(i).getFlakes().size();
//		}
//		
//		System.out.println(totalF/1000.0);
		

	}

}
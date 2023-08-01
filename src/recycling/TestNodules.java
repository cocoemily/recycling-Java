package recycling;

import java.util.*;

public class TestNodules {

	public static void main(String[] args) {
		
		Agent a  = new Agent(1, 1);
		
		
		int maxNoduleSize = 20;
		int maxFlakeSize = 1;
		double noduleV = 100000;
		double noduleSA = 11091.8;
//		double avgFlakesOnNodule = 20;
//		
//		if(maxFlakeSize == 1) { //values based on creating nodules with varying maxFlakeSize 
//			avgFlakesOnNodule = 20;
//		} else if(maxFlakeSize == 2) {
//			avgFlakesOnNodule = 13.5;
//		} else if(maxFlakeSize == 3) {
//			avgFlakesOnNodule = 10.3;
//		} else {
//			avgFlakesOnNodule = 5.5 + 25.2*Math.pow(Math.E, -0.56); //exponential function fit to output from creating random nodules
//		}
		
		Nodule n1 = new Nodule(maxNoduleSize, noduleV, noduleSA, maxFlakeSize);
		
		System.out.print(n1.getFlakes().size());
		
		Flake f1 = n1.removeFlake(a);
		Flake f2 = n1.removeFlake(a);
		
		System.out.print(n1.getFlakes().size());
		
		while(n1.getFlakes().size() > 0) {
			n1.removeFlake(a);
		}

	}

}

package recycling;

import java.util.*;

public class TestArtifacts {
	
	public static void main(String[] args) {
		
		Nodule n = new Nodule(20, 100000, 2);
		//n.print();
		
		for(int i=0; i < n.getFlakes().size(); i++) {
			//n.getFlakes().get(i).print();
		}
		
		int test = (int) ((Math.random() * (20 / 10)) + 2) * 10;
		
		System.out.println(test);
		
	}
	
}
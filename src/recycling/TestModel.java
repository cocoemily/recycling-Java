package recycling;

import java.util.*;
import java.io.File;
import java.io.IOException;
import java.io.FileWriter;

public class TestModel {

	public static void main(String[] args) {
		
		//System.out.println(System.getProperty("user.dir"));
		
		int totalSteps = 6; //original model number is 2000
		
		ExtendedModel model = new ExtendedModel("run_1", "test", 10, 250000, 100, 
				30, 10, 1, 20, 0.5, 0.5, 0.5, 10,
				0.5, //this is overlap parameter
				1.0, true, true, 10, 1, false, 0.5, 2, 1000);
		//model.print();
		
		if(model.overlap == 1) {
			ArrayList<Integer> techs = new ArrayList<Integer>();
			for(int i=0; i < (int) (model.totalAgents * model.groupPerc); i++) {
				techs.add(1);
			}
			for(int j=0; j < model.totalAgents - ((int) (model.totalAgents * model.groupPerc)); j++) {
				techs.add(2);
			}
			
			Collections.shuffle(techs);
			for(int i=0; i < techs.size(); i++) {
				model.createAgent(techs.get(i));
			}
			
		} else if(model.overlap == 0) {
			model.createAgents(1, (int) (model.totalAgents * model.groupPerc));
			model.createAgents(2, model.totalAgents - ((int) (model.totalAgents * model.groupPerc)));
			
		} else if(model.overlap == 0.5) {
			int oneThird = (int) (model.totalAgents / 3.0);
			int aLeft = model.totalAgents - (oneThird + oneThird);
			
			model.createAgents(1, oneThird);
			
			
			ArrayList<Integer> techs = new ArrayList<Integer>();
			for(int i=0; i < (int) (aLeft * model.groupPerc); i++) {
				techs.add(1);
			}
			for(int j=0; j < aLeft - ((int) (aLeft * model.groupPerc)); j++) {
				techs.add(2);
			}
			
			Collections.shuffle(techs);
			for(int i=0; i < techs.size(); i++) {
				model.createAgent(techs.get(i));
			}
			
			model.createAgents(2, oneThird);
		}
		
		model.printAgents();
	
		
		
				
//		//model run here
//		for(int i=1; i <= totalSteps; i++) {
//			
//			if(i % model.geoFreq == 0) {
//				model.geologicalEvents();
//			}
//			
//			//model.landscape.printGrid();
//			model.currentYear = model.startYear + (i*model.timestep); //update current year of model
//			
//			//agent activities
//			for(int j=0; j < model.agents.size(); j++) {
//				
//				//random placement of agents
//				model.moveAgents(true);
//				model.agents.get(j).print();
//				
//				
//				if(Math.random() < model.blankProb) {
//					//make blanks
//					System.out.println("make blanks");
//					model.produceBlanks(model.agents.get(j), model.maxUseIntensity);
//					model.landscape.getElement(0, 0).getTopLayer().print();
//					
//				} else {
//					//recycle
//					System.out.println("recycle");
//					model.landscape.getElement(0, 0).getTopLayer().print();
//					System.out.println("--------");
//					
//					System.out.println("collection: ");
//					model.collectRandomArtifacts(model.agents.get(j));
//					model.landscape.getElement(0, 0).getTopLayer().print();
//					model.agents.get(j).printArtifactList();
//					
//					System.out.println("retouch: ");
//					model.retouchArtifacts(model.agents.get(j));
//					//model.landscape.getElement(0, 0).getTopLayer().print();
//					model.agents.get(j).printArtifactList();
//					
//					System.out.println("drop: ");
//					model.dropArtifacts(model.agents.get(j));
//					model.landscape.getElement(0, 0).getTopLayer().print();
//					model.agents.get(j).printArtifactList();
//				}
//				
//			}
//			
//		}
		
	}
	
}
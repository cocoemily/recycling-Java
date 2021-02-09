package recycling;

import java.util.*;
import java.io.File;
import java.io.IOException;
import java.io.FileWriter;

public class RunExtendedModel {

	public static void main(String[] args) {
		//arguments
		if(args.length != 22) {
			System.out.println("Missing arguments");
			
			
		} else {
			ExtendedModel model = new ExtendedModel(
					(String) args[0], (String) args[1], 
					Integer.parseInt(args[2]), 
					Integer.parseInt(args[3]), 
					Integer.parseInt(args[4]),
					Integer.parseInt(args[5]), 
					Integer.parseInt(args[6]), 
					Integer.parseInt(args[7]), 
					Integer.parseInt(args[8]), 
					Double.parseDouble(args[9]), 
					Double.parseDouble(args[10]), 
					Integer.parseInt(args[11]), 
					Double.parseDouble(args[12]), 
					Double.parseDouble(args[13]), 
					Boolean.parseBoolean(args[14]), 
					Boolean.parseBoolean(args[15]), 
					Integer.parseInt(args[16]), 
					Integer.parseInt(args[17]), 
					Boolean.parseBoolean(args[18]), 
					Double.parseDouble(args[19]), 
					Integer.parseInt(args[20]), 
					Integer.parseInt(args[21])
					);
			
			
			//create agents per overlap parameter
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
			
			int whichAgent = 0;
			model.agents.get(whichAgent).randomMove(model.landscape.getNumRows(), model.landscape.getNumCols());
			
			for(int i=0; i < model.totalSteps; i++) {
		
				model.currentYear = model.startYear + (i*model.timestep); //update current year of model
				
				//geological events
				if(i % model.geoFreq == 0) {
					model.geologicalEvents();
				}
				
				//agent behavior
				if(model.agents.get(whichAgent).checkInWindow()) {
					
					//
					
					
					model.moveAgent(model.agents.get(whichAgent), false);
					
				} else {
					whichAgent++;
					model.agents.get(whichAgent).randomMove(model.landscape.getNumRows(), model.landscape.getNumCols());
				}
				
			}
			
			
			
//			for(int a=0; a < model.agents.size(); a++) {
//				int agentSteps = 1;
//				model.agents.get(a).randomMove(model.landscape.getNumRows(), model.landscape.getNumCols());
//				
//				while(model.agents.get(a).checkInWindow()) {
//					
//					if(agentSteps % model.geoFreq == 0) {
//						model.geologicalEvents();
//					}
//					
//					model.currentYear = model.startYear + (agentSteps*model.timestep); //update current year of model
//					
//					model.moveAgent(model.agents.get(a), false); //move agent via Levy walk function
//					
//					
//					
//				}
//			}
			
			
			
		}
		
		
		
	}
	
	public static void createFile(String filename, ArrayList<String> data) {
		try {
			FileWriter fw = new FileWriter(System.getProperty("user.dir") + "/output/" + filename + ".csv");
			for(int i=0; i < data.size(); i++) {
				fw.write(data.get(i) + "\n");
			}
			fw.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			System.out.println("Error occurred.");
			e.printStackTrace();
		}

	}
	
}
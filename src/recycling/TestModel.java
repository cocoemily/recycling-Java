package recycling;

import java.util.*;
import java.io.File;
import java.io.IOException;
import java.io.FileWriter;

public class TestModel {

	public static void main(String[] args) {
		
		//System.out.println(System.getProperty("user.dir"));
		
		int totalSteps = 25; //original model number is 2000
		
		ExtendedModel model = new ExtendedModel("run_1", "test", 5, 250000, 100, 
				30, 10, 1, 20, 0.5, 0.5, 10,
				0, //this is overlap parameter
				1.0, true, true, 10, 1, false, 0.5, 2, 1000);
		model.print();
		
		
		//testing agent creation
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
		} else { //if model overlap is anything else, create agents with all different technology types
			int tech = 1;
			for(int i=0; i < model.totalAgents; i++) {
				model.createAgent(tech);
				tech++;
			}
		}
		//model.printAgents();
		
		int whichAgent = 0;
		model.agents.get(whichAgent).randomMove(model.landscape.getNumRows(), model.landscape.getNumCols());
		//model.printAgents();
		
		//model.agents.get(whichAgent).print();
		//System.out.print(" : "+ model.agents.get(whichAgent).checkInWindow());
		
		//int i = 0;
		for(int i=0; i < totalSteps; i++) {

			model.currentYear = model.startYear + (i*model.timestep); //update current year of model

			//geological events
			//				if(i % model.geoFreq == 0) {
			//					model.geologicalEvents();
			//				}

			//agent behavior
			if(model.agents.get(whichAgent).checkInWindow()) {

				Agent a = model.agents.get(whichAgent);
				Square s = model.landscape.getElement(a.getCurrentX(), a.getCurrentY());
				Layer l = s.getTopLayer();

				s.occupied();
				l.encounter();

				if(l.hasFlakes() || l.hasNodules()) { //if there are objects at the current layer, collect with certain probability
					if(Math.random() < model.scavengeProb) {
						model.collectSelectedArtifacts(a);
					}
				}
				
				if(a.hasObjects()) {
					if(Math.random() < model.blankProb) {
						if(a.getAgentNodules().size() != 0 ) {
							for(int ui=0; ui < model.maxUseIntensity; ui++) {
								model.produceBlank(a);
								l.manufactured();
							}
						}
					} else {
						if(a.getAgentFlakes().size() != 0) {
							for(int ui=0; ui < model.maxUseIntensity; ui++) {
								model.retouchFlake(a);
								l.manufactured();
								l.retouched();
							}
						}
					}

				} else {
					model.findNodules(a, model.maxArtifactCarry - a.numberCurrentObjects()); //find nodules until toolkit is full?
				}

				//drop all exhausted objects
				model.dropExhaustedArifacts(a);
				//drop up to maxArtifactCarry
				model.dropArtifacts(a);
				
				
				model.moveAgent(model.agents.get(whichAgent), false);

			} else {
				if(whichAgent < model.agents.size()-1) {
					whichAgent++;
					model.agents.get(whichAgent).randomMove(model.landscape.getNumRows(), model.landscape.getNumCols());
				}
			}
			
			model.getArtifactData();
			model.getLayerData();
			model.getModelData();
			model.resetScavengeEventCounter();
		}
		
		RunExtendedModel.outputModelData(model);
		
	}
	
}
package recycling;

import java.util.*;
import java.io.File;
import java.io.IOException;
import java.io.FileWriter;

/**
 * Class for running the ExtendedModel locally
 * @author emilycoco
 *
 */
public class TestModel {

	public static void main(String[] args) {

		//System.out.println(System.getProperty("user.dir"));

		ExtendedModel model = new ExtendedModel(
				"test",	//outputFile
				"run1", 	//name
				5, 			//size 
				500000, 	//startYear
				100, 		//timestep
				40, 		//maxUI
				25, 			//maxAC
				1, 			//maxFS
				20, 		//maxNS
				0.5,		//bProb
				0.5,		//sProb
				1, 			//overlap
				1.0,  		//mu
				false, 		//sizePref
				false, 		//flakePref
				1, 			//minFS
				10, 		//minNS
				false, 		//strict
				0.5, 		//ED
				0, 			//GF
				100		//totalSteps
				);


		model.print();
		System.out.println("model created.");

		//create agents per overlap parameter
		if(model.overlap == 1) { //complete overlap -> agents randomly added to agent list
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

		} else if(model.overlap == 0) { //no overlap -> agents added in order starting with all type 1 agents
			model.createAgents(1, (int) (model.totalAgents * model.groupPerc));
			model.createAgents(2, model.totalAgents - ((int) (model.totalAgents * model.groupPerc)));

		} else if(model.overlap == 0.5) { //partial overlap -> one third type 1 agents, one third random mix of agents, one third type 2 agents
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
		model.printAgents();
		System.out.println("agents created.");
		

		int whichAgent = 0;
		model.agents.get(whichAgent).randomMove(model.landscape.getNumRows(), model.landscape.getNumCols());

		for(int i=0; i <= model.totalSteps; i++) {

			model.currentYear = model.startYear + (i*model.timestep); //update current year of model
			System.out.println("current year: " + model.currentYear);
			System.out.println("\t current agent: " + model.agents.get(whichAgent).getGroup());

			//geological events are not tested in this model, but would go here in model run
			//geological events
			//				if(i % model.geoFreq == 0) {
			//					model.geologicalEvents();
			//				}

			//agent behavior
			if(model.agents.get(whichAgent).checkInWindow()) {

				Agent a = model.agents.get(whichAgent);
				Square s = model.landscape.getElement(a.getCurrentX(), a.getCurrentY());
				Layer l = s.getTopLayer();

				s.occupied(); //update behavior counters
				l.encounter(); //update behavior counters

				if(l.hasFlakes() || l.hasNodules()) { //if there are objects at the current layer, collect with certain probability
					if(Math.random() < model.scavengeProb) {
						if(!model.flakePref && !model.sizePref) {
							model.collectRandomArtifacts(a);
							//l.scavenged();
							System.out.println("\t agent collected random artifacts");
						} else {
							model.collectSelectedArtifacts(a);
							//l.scavenged();
							System.out.println("\t agent collected selected artifacts");
						}
						
					}
				}
				
				if(a.hasObjects()) { //if agent is holding objects
					if(Math.random() < model.blankProb) { //produce blanks with certain probability
						if(a.getAgentNodules().size() != 0 ) {
							System.out.println("\t agent produced blanks");
							for(int ui=0; ui < model.maxUseIntensity; ui++) {
								model.produceBlank(a);
								l.manufactured();
							}
						}
					} else { //if agent is not making blanks, retouch flakes
						if(a.getAgentFlakes().size() != 0) {
							System.out.println("\t agent retouched flakes");
							for(int ui=0; ui < model.maxUseIntensity; ui++) {
								model.retouchFlake(a);
								l.manufactured();
								l.retouched();
							}
						}
					}

				} else { //if agents is not holding objects
					model.findNodules(a, model.maxArtifactCarry - a.numberCurrentObjects());
					System.out.println("\t agent found new nodules");
				}

				//drop all exhausted objects
				model.dropExhaustedArifacts(a, model.currentYear);
				//drop up to maxArtifactCarry
				model.dropArtifacts(a, model.currentYear);
				
				model.moveAgent(model.agents.get(whichAgent), false);

			} else { //if agent has moved outside of the window of observation, move onto next agent
				if(whichAgent < model.agents.size()-1) {
					whichAgent++;
					model.agents.get(whichAgent).randomMove(model.landscape.getNumRows(), model.landscape.getNumCols());
				}
			}
			
			if(i % (model.totalSteps/2) == 0) {
				model.getArtifactData();
			}
			
			model.getLayerData();
			model.getModelData();
			model.resetScavengeEventCounter();
		}
		
		RunExtendedModel.outputModelData(model);
	}

}
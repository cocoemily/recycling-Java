package recycling;

import java.util.*;

import java.io.File;
import java.io.IOException;
import java.io.FileWriter;

public class RunExtendedModel {

	/**
	 * Class for running the Extended Model on HPC
	 * @param args parameter values
	 */
	public static void main(String[] args) {
		//arguments
		if(args.length != 21) {
			System.out.println("Missing arguments");


		} else {
			ExtendedModel model = new ExtendedModel(
					args[0], 						//outputFile
					args[1], 						//name
					Integer.parseInt(args[2]), 		//size 
					Integer.parseInt(args[3]), 		//startYear
					Integer.parseInt(args[4]), 		//timestep
					Integer.parseInt(args[5]), 		//maxUI
					Integer.parseInt(args[6]), 		//maxAC
					Integer.parseInt(args[7]), 		//maxFS
					Integer.parseInt(args[8]), 		//maxNS
					Double.parseDouble(args[9]),	//bProb
					Double.parseDouble(args[10]),	//sProb
					Double.parseDouble(args[11]), 	//overlap
					Double.parseDouble(args[12]),  	//mu
					Boolean.parseBoolean(args[13]), //sizePref
					Boolean.parseBoolean(args[14]), //flakePref
					Integer.parseInt(args[15]), 	//minFS
					Integer.parseInt(args[16]), 	//minNS
					Boolean.parseBoolean(args[17]), //strict
					Double.parseDouble(args[18]), 	//ED
					Integer.parseInt(args[19]), 	//GF
					Integer.parseInt(args[20])		//totalSteps
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
			//model.printAgents();
			System.out.println("agents created.");


			int whichAgent = 0;
			model.agents.get(whichAgent).randomMove(model.landscape.getNumRows(), model.landscape.getNumCols());

			for(int i=0; i <= model.totalSteps; i++) {

				model.currentYear = model.startYear + (i*model.timestep); //update current year of model
				System.out.println("current year: " + model.currentYear);
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
							if(a.getAgentNodules().size() == 0 ) { //if agent does not have a nodule to make blanks, find new ones
								//agent fills up nodules to max artifact carry or finds one additional nodule if already at max capacity
								model.findNodules(a, Math.max(model.maxArtifactCarry - a.numberCurrentObjects(), 1));
							} 
							for(int ui=0; ui < model.maxUseIntensity; ui++) {
								model.produceBlank(a);
							}
							System.out.println("\t agent produced blanks");

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
					System.out.println("\t agent " + model.agents.get(whichAgent).getGroup() + " has moved outside observation window");
					if(whichAgent < model.agents.size()-1) {
						whichAgent++;
						model.agents.get(whichAgent).randomMove(model.landscape.getNumRows(), model.landscape.getNumCols());
						System.out.println("\t agent " + model.agents.get(whichAgent).getGroup() + " has moved into the window");
					}
				}

				if(i % (model.totalSteps/2) == 0) {
					//model.getArtifactData();
				}

				//model.getLayerData();
				model.getModelData();
				model.resetScavengeEventCounter();
			}

			outputModelData(model);
		}
	}


	public static void outputModelData(ExtendedModel em) { 
		String path = System.getProperty("user.dir") + "/output/" + em.outputFile;
		File file = new File(path);
		file.mkdir();

		//output model data
		createFile2((em.outputFile + "/" + em.name + "_" + "model-data"), em.modelOutput());

		//output layer data
		//createFile2((em.outputFile + "/" + em.name + "_" + "layers-data"), em.layersOutput());

		//output artifact data
		//createFile2((em.outputFile + "/" + em.name + "_" + "artifacts-data"), em.artifactsOutput());
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

	public static void createFile2(String filename, StringBuilder data) {
		try {
			FileWriter fw = new FileWriter(System.getProperty("user.dir") + "/output/" + filename + ".csv");
			fw.write(data.toString());
			fw.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			System.out.println("Error occurred.");
			e.printStackTrace();
		}

	}

}

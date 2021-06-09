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
					(String) args[0], 				//outputFile
					(String) args[1], 				//name
					Integer.parseInt(args[2]), 		//size 
					Integer.parseInt(args[3]), 		//startYear
					Integer.parseInt(args[4]), 		//timestep
					Integer.parseInt(args[5]), 		//maxUI
					Integer.parseInt(args[6]), 		//maxAC
					Integer.parseInt(args[7]), 		//maxFS
					Integer.parseInt(args[8]), 		//maxNS
					Double.parseDouble(args[9]),	//bProb
					Double.parseDouble(args[10]),	//sProb
					Integer.parseInt(args[11]), 	//numAgents
					Double.parseDouble(args[12]), 	//overlap
					Double.parseDouble(args[13]),  	//mu
					Boolean.parseBoolean(args[14]), //sizePref
					Boolean.parseBoolean(args[15]), //flakePref
					Integer.parseInt(args[16]), 	//minFS
					Integer.parseInt(args[17]), 	//minNS
					Boolean.parseBoolean(args[18]), //strict
					Double.parseDouble(args[19]), 	//ED
					Integer.parseInt(args[20]), 	//GF
					Integer.parseInt(args[21])		//totalSteps
					);
			model.print();
			System.out.println("model created.");

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
			model.printAgents();
			System.out.println("agents created.");
			

			int whichAgent = 0;
			model.agents.get(whichAgent).randomMove(model.landscape.getNumRows(), model.landscape.getNumCols());

			for(int i=0; i < model.totalSteps; i++) {

				model.currentYear = model.startYear + (i*model.timestep); //update current year of model
				System.out.println("current year: " + model.currentYear);

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
							System.out.println("\t agent collected artifacts");
						}
					}
					
					if(a.hasObjects()) {
						if(Math.random() < model.blankProb) {
							if(a.getAgentNodules().size() != 0 ) {
								System.out.println("\t agent produced blanks");
								for(int ui=0; ui < model.maxUseIntensity; ui++) {
									model.produceBlank(a);
									l.manufactured();
								}
							}
						} else {
							if(a.getAgentFlakes().size() != 0) {
								System.out.println("\t agent retouched flakes");
								for(int ui=0; ui < model.maxUseIntensity; ui++) {
									model.retouchFlake(a);
									l.manufactured();
									l.retouched();
								}
							}
						}

					} else {
						model.findNodules(a, model.maxArtifactCarry - a.numberCurrentObjects());
						System.out.println("\t agent found new nodules");
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
			
			outputModelData(model);
		}
	}

	
	public static void outputModelData(ExtendedModel em) { 
		String path = System.getProperty("user.dir") + "/output/" + em.outputFile;
		File file = new File(path);
		file.mkdir();
		
		//output model data
		createFile((em.outputFile + "/" + em.name + "_" + "model-data"), em.modelOutput());
		
		//output layer data
		createFile((em.outputFile + "/" + em.name + "_" + "layers-data"), em.layersOutput());
		
		//output artifact data
		createFile((em.outputFile + "/" + em.name + "_" + "artifacts-data"), em.artifactsOutput());
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
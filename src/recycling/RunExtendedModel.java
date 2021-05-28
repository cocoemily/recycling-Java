package recycling;

import java.util.*;

import javax.swing.event.TableColumnModelListener;

import java.io.File;
import java.io.IOException;
import java.io.FileWriter;

public class RunExtendedModel {

	public static void main(String[] args) {
		//arguments
		if(args.length != 23) {
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
					Double.parseDouble(args[11]),
					Integer.parseInt(args[12]), 
					Double.parseDouble(args[13]), 
					Double.parseDouble(args[14]), 
					Boolean.parseBoolean(args[15]), 
					Boolean.parseBoolean(args[16]), 
					Integer.parseInt(args[17]), 
					Integer.parseInt(args[18]), 
					Boolean.parseBoolean(args[19]), 
					Double.parseDouble(args[20]), 
					Integer.parseInt(args[21]), 
					Integer.parseInt(args[22])
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
			} else { //if model overlap is anything else, create agents with all different technology types
				int tech = 1;
				for(int i=0; i < model.totalAgents; i++) {
					model.createAgent(tech);
					tech++;
				}
			}

			int whichAgent = 0;
			model.agents.get(whichAgent).randomMove(model.landscape.getNumRows(), model.landscape.getNumCols());

			for(int i=0; i < model.totalSteps; i++) {

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
					whichAgent++;
					model.agents.get(whichAgent).randomMove(model.landscape.getNumRows(), model.landscape.getNumCols());
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
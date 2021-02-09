package recycling;

import java.util.*;
import java.io.File;
import java.io.IOException;
import java.io.FileWriter;

public class RunOriginalModel {

	public static void main(String[] args) {

		if(args.length != 11) {
			System.out.println("Missing arguments");

		} else {
			String folderName = (String) args[0];
			String modelName = (String) args[1];
			int size = Integer.parseInt(args[2]);
			int startYear = Integer.parseInt(args[3]);
			int timestep = Integer.parseInt(args[4]);
			int maxUI = Integer.parseInt(args[5]);
			int maxAC = Integer.parseInt(args[6]);
			int numAg = Integer.parseInt(args[7]);
			double ed = Double.parseDouble(args[8]);
			int gf = Integer.parseInt(args[9]);
			double overlap = Double.parseDouble(args[10]);

			int totalSteps = 2000; //original model number is 2000
			OriginalModel model = new OriginalModel(folderName, modelName, size, startYear, timestep, maxUI, maxAC, numAg, ed, gf, overlap);
			model.print();

			model.createRandomSources(0.25, maxUI, 1);

			if(model.overlap == 0 || model.overlap == 0.5) {
				model.createAgents(1, (int) (model.totalAgents * model.groupPerc));
			} else if (model.overlap == 1) {
				model.createAgents(1, (int) (model.totalAgents * model.groupPerc));
				model.createAgents(2, model.totalAgents - ((int) (model.totalAgents * model.groupPerc)));
			}

			//model run here
			for(int i=1; i <= totalSteps; i++) {

				if(model.overlap == 0.5) {
					if((int) (totalSteps/3) == i) {
						model.createAgents(2, model.totalAgents - ((int) (model.totalAgents * model.groupPerc)));
						//model.printAgents();
					} else if((int) (2*(totalSteps/3))+1 == i) {
						model.removeAgents(1);
						//model.printAgents();
					}
				} else if(model.overlap == 0) {
					if((int) (totalSteps/2) == i) {
						model.removeAgents(1);
						model.createAgents(2, model.totalAgents - ((int) (model.totalAgents * model.groupPerc)));
						//model.printAgents();
					}
				}
				
				model.currentYear = model.startYear + (i*model.timestep); //update current year of model

				if(i % model.geoFreq == 0) {
					model.geologicalEvents();
				}

				//agent activities
				for(int j=0; j < model.agents.size(); j++) {

					//random placement of agents
					model.moveAgents(true);
					//model.agents.get(j).print();


					if(Math.random() < model.blankProb) {
						//make blanks
						//System.out.println("make blanks");
						model.produceBlanks(model.agents.get(j), model.maxUseIntensity);

					} else {
						//recycle
						//System.out.println("recycle");
						model.collectRandomArtifacts(model.agents.get(j));
						model.retouchArtifacts(model.agents.get(j));
						model.dropArtifacts(model.agents.get(j));
					}

				}


				modelAnalysis(model);

			}
		}

	}

	public static void modelAnalysis(OriginalModel m) {
		for(int i=0; i < m.landscape.getNumRows(); i++) {
			for(int j=0; j < m.landscape.getNumCols(); j ++) {
				ArrayList<String> data = new ArrayList<String>();
				data.add("overlap,total.groups,ed.freq,ed,year,blanks1,blanks2,tools,mixed,retouch.11,retouch.22,retouch.12,retouch.21");
				for(int l=0; l < m.landscape.getElement(i, j).getLayers().size(); l++) {
					String datastring = m.overlap + "," + m.totalAgents + "," + m.geoFreq + "," + m.EDratio + ",";
					Layer layer = m.landscape.getElement(i, j).getLayers().get(l);
					datastring += layer.getYear() + ",";

					ArrayList<Artifact> artifacts = layer.getArtifacts();
					int blanks1 = 0;
					int blanks2 = 0;
					int tools = 0;
					int mixed = 0;
					int retouch11 = 0;
					int retouch22 = 0;
					int retouch12 = 0;
					int retouch21 = 0;

					for(int n=0; n < artifacts.size(); n++) {
						Artifact a = artifacts.get(n);
						int bt = a.getFirstTech();
						int rt = a.getLastTech();
						//blanks tech type 1
						if(bt == 1) {
							blanks1++;
						}
						//blanks tech type 2
						if(bt == 2) {
							blanks2++;
						}
						//tools (artifacts above stage 0)
						if(a.getStage() > 0) {
							tools++;

							//mixed (stage 0 type != last retouch type)
							if(bt != rt) {
								mixed++;
							}
							//blank type 1 retouch type 1
							if(bt == 1 && rt == 1) {
								retouch11++;
							}
							//blank type 2 retouch type 2
							if(bt == 2 && rt == 2) {
								retouch22++;
							}
							//blank type 1 retouch type 2
							if(bt == 1 && rt == 2) {
								retouch12++;
							}
							//blank type 2 retouch type 1
							if(bt == 2 && rt == 1) {
								retouch21++;
							}
						}
					}

					datastring += blanks1 + "," + 
							blanks2 + "," + tools + "," + 
							mixed + "," + 
							retouch11 + "," + 
							retouch22 + "," + 
							retouch12 + "," + 
							retouch21;
					data.add(datastring);
				}
				String path = System.getProperty("user.dir") + "/output/" + m.outputFile;
				File file = new File(path);
				file.mkdir();
				createFile(m.outputFile + "/" + m.name + "_" + i + "_" + j, data);
			}
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
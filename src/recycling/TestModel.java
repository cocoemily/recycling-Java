package recycling;

import java.util.*;
import java.io.File;
import java.io.IOException;
import java.io.FileWriter;

public class TestModel {

	public static void main(String[] args) {
		
		//need to figure out how to do arguments
		
		int totalSteps = 6; //original model number is 2000
		Model model = new Model("test", 1, 250000, 100, 2, 1, 1, 0.5, 2, 0);
		model.print();
		
		model.createRandomSources(1, 5, 1);
		model.landscape.getElement(0, 0).getTopLayer().print();
		
//		if(model.overlap == 0 || model.overlap == 0.5) {
//			model.createAgents(1, (int) (model.totalAgents * model.groupPerc));
//		} else if (model.overlap == 1) {
//			model.createAgents(1, (int) (model.totalAgents * model.groupPerc));
//			model.createAgents(2, model.totalAgents - ((int) (model.totalAgents * model.groupPerc)));
//		}
		
		model.createAgents(2, model.totalAgents); //create all same agents for testing
		
//		for(int i=1; i <= totalSteps; i++) {
//			if(i % model.geoFreq == 0) {
//				model.geologicalEvents();
//			}
//			model.currentYear = model.startYear + (i*model.timestep); //update current year of model
//			//model.landscape.printGrid();
//			//System.out.println("------");
//		}
				
		//model run here
		for(int i=1; i <= totalSteps; i++) {
//			
//			if(model.overlap == 0.5) {
//				if((int) (totalSteps/3) == i) {
//					model.createAgents(2, model.totalAgents - ((int) (model.totalAgents * model.groupPerc)));
//					//model.printAgents();
//				} else if((int) (2*(totalSteps/3))+1 == i) {
//					model.removeAgents(1);
//					//model.printAgents();
//				}
//			} else if(model.overlap == 0) {
//				if((int) (totalSteps/2) == i) {
//					model.removeAgents(1);
//					model.createAgents(2, model.totalAgents - ((int) (model.totalAgents * model.groupPerc)));
//					//model.printAgents();
//				}
//			}
//			
			if(i % model.geoFreq == 0) {
				model.geologicalEvents();
			}
			
			//model.landscape.printGrid();
			model.currentYear = model.startYear + (i*model.timestep); //update current year of model
			
			//agent activities
			for(int j=0; j < model.agents.size(); j++) {
				
				//random placement of agents
				model.moveAgents(true);
				model.agents.get(j).print();
				
				
				if(Math.random() < model.blankProb) {
					//make blanks
					System.out.println("make blanks");
					model.produceBlanks(model.agents.get(j), model.maxUseIntensity);
					model.landscape.getElement(0, 0).getTopLayer().print();
					
				} else {
					//recycle
					System.out.println("recycle");
					model.landscape.getElement(0, 0).getTopLayer().print();
					System.out.println("--------");
					
					System.out.println("collection: ");
					model.collectRandomArtifacts(model.agents.get(j));
					model.landscape.getElement(0, 0).getTopLayer().print();
					model.agents.get(j).printArtifactList();
					
					System.out.println("retouch: ");
					model.retouchArtifacts(model.agents.get(j));
					//model.landscape.getElement(0, 0).getTopLayer().print();
					model.agents.get(j).printArtifactList();
					
					System.out.println("drop: ");
					model.dropArtifacts(model.agents.get(j));
					model.landscape.getElement(0, 0).getTopLayer().print();
					model.agents.get(j).printArtifactList();
				}
				
				//model.moveAgents(true);
				//model.agents.get(j).print();
				
			}
			
			
			//modelAnalysis(model);
			
		}
		
	}
	
	public static void modelAnalysis(Model m) {
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
				String path = "/Users/emilycoco/eclipse-workspace/recycling-Java/output/" + m.name;
				File file = new File(path);
				file.mkdir();
				createFile(m.name + "/" + m.name + "_" + i + "_" + j, data);
			}
		}
		
		
		
		
		
	}
	
	public static void createFile(String filename, ArrayList<String> data) { //first line of data needs to be column names
		try {
			FileWriter fw = new FileWriter("/Users/emilycoco/eclipse-workspace/recycling-Java/output/" + filename + ".csv");
			//fw.write("test1, test2, test3\n");
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
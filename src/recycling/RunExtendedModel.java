package recycling;

import java.util.*;
import java.io.File;
import java.io.IOException;
import java.io.FileWriter;

public class RunExtendedModel {

	public static void main(String[] args) {
		//MODEL RUN HERE
		
		
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
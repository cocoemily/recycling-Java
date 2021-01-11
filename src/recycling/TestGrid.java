package recycling;

public class TestGrid {

	public static void main(String[] args) {
		Grid grid = new Grid(2, 20000);
		grid.printGrid();
		
		int year = 20000;
		int yearStep = -1000;
		
		for(int i=0; i < 2; i++) { //runs
			year += yearStep;
			for(int j=0; j < grid.getNumRows(); j++) { //each row of the Grid
				for(int k=0; k < grid.getNumCols(); k++) { //each element of the Grid row
					System.out.println(j + ", " + k);
					
					double erodeOrDeposit = Math.random();
					if(erodeOrDeposit > 0.8) {
						grid.getElement(j, k).erode();
						System.out.println("erode");
					}
					else {
						grid.getElement(j, k).deposit(year);
						System.out.println("deposit");
					}
					
				}
			}
		}
		grid.printGrid();

	}

}

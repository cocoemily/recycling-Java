package recycling;

public class Grid {
	
	Square[][] grid;
	private int rows;
	private int columns; 

	
	public Grid(int n, int year) {
		grid = new Square[n][n];
		rows = n;
		columns = n;
		for(int i=0; i<n; i++) {
			for(int j=0; j<n; j++) {
				grid[i][j] = new Square(year);
			}
		}
	}
	
	public void printGrid() {
		for(int i=0; i<grid.length; i++) {
			for(int j=0; j<grid.length; j++) {
				System.out.print("[ " + grid[i][j].getTopLayer().getYear() + " BP ]");
			}
			System.out.println();
		}
	}
	
	public int getNumRows() {
		return rows;
	}
	
	public int getNumCols() {
		return columns;
	}
	
	public Square getElement(int row, int col) {
		return grid[row][col];
	}

}

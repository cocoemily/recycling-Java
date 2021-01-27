package recycling;

public class TestRandomWalks {
	
	public static void main(String[] args) {
		
		int rows = 10;
		int cols = 10;
		
		Agent a = new Agent(1, 1, 1);
		a.print();
		
		a.randomMove(rows, cols); //send to a random location in the world to start
		
		if(a.getCurrentX() >= 0 && a.getCurrentY() >= 0 
				&& a.getCurrentX() < rows && a.getCurrentY() < cols) {
			a.setInWindow(true);
		} else {
			a.setInWindow(false);
		}
		
		a.print();
		
		for(int i = 0; i < 5; i++) {
			a.randomWalk(3); //mu = 1, 2, or 3
			
			if(a.getCurrentX() >= 0 && a.getCurrentY() >= 0 
					&& a.getCurrentX() < rows && a.getCurrentY() < cols) {
				a.setInWindow(true);
			} else {
				a.setInWindow(false);
			}
			
			a.print();
		}
		
		
		
	}
	
}
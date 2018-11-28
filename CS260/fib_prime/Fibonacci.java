package question1and2;


public class Fibonacci implements Runnable{
	private int limit;
	private int[] lastFibs = {0, 1};
	CustomBuffer buffer;
	
	public Fibonacci(int limit, CustomBuffer buffer) {
		this.limit = limit;
		this.buffer = buffer;
	}
	
	public void run() {
		int nextFib = 1;
		while(nextFib < limit) {
			try {
				buffer.blockingPut(nextFib);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			}
			
			lastFibs[1] = lastFibs[0];
			lastFibs[0] = nextFib;
			nextFib = lastFibs[0] + lastFibs[1];
				
		}
		
		try {
			buffer.blockingPut(-1);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}
	}

}

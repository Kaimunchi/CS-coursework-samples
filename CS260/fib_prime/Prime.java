package question1and2;


public class Prime implements Runnable{
	private int limit;
	CustomBuffer buffer;
	
	public Prime(int limit, CustomBuffer buffer) {
		this.limit = limit;
		this.buffer = buffer;
	}
	
	public void run() {
		for(int num = 2; num < limit; num++) {
			boolean isPrime = true;
			for(int counter = 2; counter <= num/2; counter++) {
				if (num % counter == 0) {
					isPrime = false;
					break;
				}
			}
		
			if(isPrime) {
				try {
					buffer.blockingPut(num);
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
				}
			}
			
		}
		
		try {
			buffer.blockingPut(-1);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}
		
	}

}

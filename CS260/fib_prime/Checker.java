package question1and2;

import java.util.concurrent.CopyOnWriteArrayList;

public class Checker implements Runnable{
	
	private CustomBuffer primesBuffer;
	private CustomBuffer fibsBuffer;
	private CopyOnWriteArrayList<Integer> checkedValues;
	
	public Checker(CustomBuffer primesBuffer, CustomBuffer fibsBuffer, CopyOnWriteArrayList<Integer> checkedValues) {
		this.primesBuffer = primesBuffer;
		this.fibsBuffer = fibsBuffer;
		this.checkedValues = checkedValues;
	}
	
	public void run() {
		int fib = 0;
		int prime = 0;
		
		while(fib != -1 && prime != -1) {
			try {
				fib = fibsBuffer.blockingGet();
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			}
			
			while(prime != -1 && prime <= fib) {
				if(prime == fib) {
					checkedValues.add(fib);
					break;
				}
				else {
					try {
						prime = primesBuffer.blockingGet();
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
					}
				}
			}
			
		}
		
	}

}

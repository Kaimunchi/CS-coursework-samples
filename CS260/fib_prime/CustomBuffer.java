package question1and2;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class CustomBuffer {
	
	private final Lock accessLock = new ReentrantLock();
	private final Condition canWrite = accessLock.newCondition();
	private final Condition canRead = accessLock.newCondition();
	
	private final int[] buffer;
	private int writeIndex = 0;
	private int readIndex = 0;
	private int occupiedCells = 0;
	
	public CustomBuffer(int bufferSize) {
		buffer = new int[bufferSize];
	}
	
	public void blockingPut(int value) throws InterruptedException{
		accessLock.lock();
		try {
			while(occupiedCells == buffer.length) {
				canWrite.await();
			}
			
			buffer[writeIndex] = value;
			occupiedCells++;
			writeIndex = (writeIndex + 1) % buffer.length;
			canRead.signalAll();
			
		}
		finally {
			accessLock.unlock();
		}
	}
	
	public int blockingGet() throws InterruptedException{
		int toReturn = -1;
		accessLock.lock();
		try {
			while(occupiedCells == 0) {
				canRead.await();
			}
			
			toReturn = buffer[readIndex];
			occupiedCells--;
			readIndex = (readIndex + 1) % buffer.length;
			canWrite.signalAll();
		}
		finally {
			accessLock.unlock();
		}
		
		return toReturn;
	}
	
}

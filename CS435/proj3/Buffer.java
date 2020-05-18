import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * A thread-safe Generic Buffer based around an array.
 * Generic type must be an Object, as per usual.
 * @author Brandon Ingli
 * @version 17 Apr 2020
 */

public class Buffer<T> {
	
	private Lock accessLock = new ReentrantLock();
	private Condition canWrite = accessLock.newCondition();
	private Condition canRead = accessLock.newCondition();
	
	private T[] buffer;
	private int writeIndex = 0;
	private int readIndex = 0;
	private int occupiedCells = 0;
	
	/**
	 * Constructor to create a new Buffer.
	 * Some warnings are suppressed, as this cast is considered by my research 
	 * to be an okay practice if used responsibly and the array is not exposed 
	 * outside the class. Credit for the workaround to the Univ of Washington 
	 * CS Department (and various StackOverflow users suggesting the same): 
	 * https://courses.cs.washington.edu/courses/cse332/10sp/sectionMaterials/week1/genericarrays.html
	 * @param bufferSize Size of the underlying array for the buffer.
	 */
	@SuppressWarnings("unchecked")
	public Buffer(int bufferSize) {
		buffer = (T[]) new Object[bufferSize];
	}

	/**
	 * Helper function to determine if buffer is empty.
	 * Doesn't need a lock() because it will only be accessed if a lock is acquired.
	 * @return true if buffer is empty, false otherwise
	 */
	private boolean isEmpty(){
		return occupiedCells == 0;
	}

	/**
	 * Helper function to determine if buffer is full.
	 * Doesn't need a lock() because it will only be accessed if a lock is acquired.
	 * @return true if buffer is full, false otherwise
	 */
	private boolean isFull(){
		return occupiedCells == buffer.length;
	}
	
	/**
	 * Put a value into the buffer
	 * @param value item to put into the buffer
	 * @throws InterruptedException if thread is interrupted while waiting on lock or condition
	 */
	public void put(T value) throws InterruptedException{
		accessLock.lock();
		try {
			while(isFull()) {
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
	
	/**
	 * Get the next value from the buffer
	 * @return next value in the buffer
	 * @throws InterruptedException if thread is interrupted while waiting on lock or condition
	 */
	public T get() throws InterruptedException{
		T toReturn = null;
		accessLock.lock();
		try {
			while(isEmpty()) {
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

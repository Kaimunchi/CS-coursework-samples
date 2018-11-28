package question1and2;

import java.util.Scanner;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class FibAndPrimeDriver {
	
	public static void main(String[] args) {
		Scanner s = new Scanner(System.in);
		
		System.out.print("What is the limit 'n'? ");
		int limit = s.nextInt();
		
		System.out.print("What is the buffer size 't'? ");
		int bufferSize = s.nextInt();
		
		s.close();
		
		CustomBuffer primesBuffer = new CustomBuffer(bufferSize);
		CustomBuffer fibsBuffer = new CustomBuffer(bufferSize);
		CopyOnWriteArrayList<Integer> checkedValues = new CopyOnWriteArrayList<Integer>();
		
		Prime prime = new Prime(limit, primesBuffer);
		Fibonacci fib = new Fibonacci(limit, fibsBuffer);
		Checker checker = new Checker(primesBuffer, fibsBuffer, checkedValues);
		
		ExecutorService executorService = Executors.newCachedThreadPool();
		
		executorService.execute(prime);
		executorService.execute(fib);
		executorService.execute(checker);
		
		executorService.shutdown();
		
		boolean finished = false;
		while(!finished) {
			try {
				finished = executorService.awaitTermination(500, TimeUnit.MILLISECONDS);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			}
		}
		
		System.out.printf("Found %d number(s) that are both Fibonacci and Prime.%n", checkedValues.size());
		if(checkedValues.size() > 0) {
			for(int index = 0; index < checkedValues.size() - 1; index++) {
				System.out.printf("%d, ", checkedValues.get(index));
			}
			System.out.printf("%d%n", checkedValues.get(checkedValues.size()-1));
		}
		
	}

}

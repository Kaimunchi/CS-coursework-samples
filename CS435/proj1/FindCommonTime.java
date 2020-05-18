import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * Finds a common time among several people.
 * @author Brandon Ingli
 * @version 1.0
 */
public class FindCommonTime {

  public static void main(String[] args) {

    if (args.length != 1) {
      System.out.println("Usage:  FindCommonTime [file name]");
      System.exit(-1);
    }

    // Open the file for reading
    String filename = args[0];
    File inputFile = new File(filename);
    Scanner input = null;
    try {
      input = new Scanner(inputFile);
    } catch (FileNotFoundException e) {
      System.out.println("Oops! File could not be opened.");
      e.printStackTrace();
      System.exit(-1);
    }
    
    ListsData data = new ListsData(); // Shared Memory

    // Read in the first list
    int numFirstList = input.nextInt();
    ArrayList<Integer> firstList = new ArrayList<Integer>();
    for (int i = 0; i < numFirstList; i++) {
      firstList.add(input.nextInt());
    }

    // Add lists as they appear
    while (input.hasNextInt()) {
      ArrayList<Integer> list = new ArrayList<Integer>();
      int numOfInts = input.nextInt();
      for (int j = 0; j < numOfInts; j++) {
        list.add(input.nextInt());
      }
      data.addToLists(list);
    }

    // Create frames and threads
    ArrayList<Thread> threads = new ArrayList<Thread>();
    for (int i = 0; i < firstList.size(); i++) {
      SearchLists sl = new SearchLists(data, firstList.get(i));
      Thread t = new Thread(sl);
      threads.add(t);
    }

    // Start the threads
    for (int i = 0; i < threads.size(); i++) {
      threads.get(i).start();
    }

    // Wait for the threads to finish
    for (int i = 0; i < threads.size(); i++) {
      try {
        threads.get(i).join();
      } catch (InterruptedException e) {
        System.out.println("Oops. An InterruptedException has occurred!");
        e.printStackTrace();
        System.exit(-1);
      }
    }

    // If no common meeting time, announce that fact.
    if(!data.getFound()){
      System.out.println("There is no common meeting time.");
    }
  }
}
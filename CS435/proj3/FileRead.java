import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

/**
 * Reads in a file and puts each line in a buffer
 * @author Brandon Ingli
 * @version 17 Apr 2020
 */
public class FileRead implements Runnable{
  
  private String filename;
  private Buffer<String> output;

  /**
   * Constructor
   * @param filename file to read in
   * @param output buffer to put lines of text in
   */
  public FileRead(String filename, Buffer<String> output){
    this.filename = filename;
    this.output = output;
  }

  /**
   * "Main" of the Thread
   */
  public void run(){
    // Open the file
    File inputFile = new File(filename);
    Scanner input = null;
    try {
      input = new Scanner(inputFile);
    } catch (FileNotFoundException e) {
      System.out.println("Oops! File could not be opened.");
      e.printStackTrace();
      System.exit(-1);
    }

    // Read in the file lines to the buffer
    try {
      while (input.hasNextLine()) {
        output.put(input.nextLine());
      }
      output.put("<<<EOF>>>"); // Signal the end of data
    } catch (InterruptedException e) {
      return;
    }
    
  }
}
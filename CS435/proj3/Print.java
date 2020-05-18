/**
 * Prints the lines in a buffer
 * @author Brandon Ingli
 * @version 17 Apr 2020
 */
public class Print implements Runnable{
  
  private Buffer<String> input;

  /**
   * Constructor
   * @param input Buffer to read from
   */
  public Print(Buffer<String> input){
    this.input = input;
  }

  /**
   * "Main" of the Thread
   */
  public void run() {
    boolean keepGoing = true;
    try {
      while(keepGoing){
        String temp = input.get();
        if(temp == "<<<EOF>>>"){ // This is the String noting the end of data
          keepGoing = false;
        } else {
          // Print the line
          System.out.println(temp);
        }
      }
    } catch (InterruptedException e) {
      return;
    }
  }
}
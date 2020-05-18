/**
 * Replaces digits in Strings from a buffer with their word representation
 * @author Brandon Ingli
 * @version 17 Apr 2020
 */
public class DigitReplace implements Runnable{
  
  private Buffer<String> input;
  private Buffer<String> output;
  private String digits[] = {"zero", "one", "two", "three", "four", "five", 
                            "six", "seven", "eight", "nine"};

  /**
   * Constructor
   * @param input Buffer to read from
   * @param output Buffer to write results to
   */
  public DigitReplace(Buffer<String> input, Buffer<String> output){
    this.input = input;
    this.output = output;
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
          output.put("<<<EOF>>>"); // Tell the next Thread to stop.
        } else {
          // Replace the digits with words
          for(int i = 0; i <= 9; i++){
            temp = temp.replaceAll(Integer.toString(i), digits[i]);
          }
          output.put(temp);
        }
      }
    } catch (InterruptedException e) {
      return;
    }
  }
}

public class Reverse implements Runnable{

  private Buffer<String> input;
  private Buffer<String> output;

   /**
   * Constructor
   * @param input Buffer to read from
   * @param output Buffer to write results to
   */
  public Reverse(Buffer<String> input, Buffer<String> output){
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
          // Reverse the line
          String reversed = "";
          for(int i = temp.length() - 1; i >= 0; i--){
            reversed += temp.charAt(i);
          }
          output.put(reversed);
        }
      }
    } catch (InterruptedException e) {
      return;
    }
  }
}
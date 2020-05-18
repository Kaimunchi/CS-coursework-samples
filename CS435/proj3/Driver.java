/**
 * Driver for Project 3
 * @author Brandon Ingli
 * @version 17 Apr 2020
 */
public class Driver {

  private static Buffer<String> input;
  private static Buffer<String> wordDigits;
  private static Buffer<String> reversed;
  public static void main(String[] args) {
    // Command Line Argument info
    if (args.length != 2) {
      System.out.println("Usage:  java Driver <input file> <buffer size>");
      System.exit(-1);
    }

    // Get the command line args
    String inputFile = args[0];
    int bufferSize = Integer.parseInt(args[1]);

    // Make sure the buffer size is positive. A little extra security rarely kills anyone.
    if(bufferSize <= 0){
      System.out.println("Oops! Non-Positive Buffer Size was entered!");
      System.exit(-1);
    }

    // Initialize the buffers
    input = new Buffer<String>(bufferSize);
    wordDigits = new Buffer<String>(bufferSize);
    reversed = new Buffer<String>(bufferSize);

    // Thread 1: Read the file and send to T2
    FileRead reader = new FileRead(inputFile, input);
    Thread readerThread = new Thread(reader);
    readerThread.start();

    // Thread 2: Take input from T1, replace digits with words, and send to T3
    DigitReplace replacer = new DigitReplace(input, wordDigits);
    Thread replacerThread = new Thread(replacer);
    replacerThread.start();

    // Thread 3: Take input from T2, reverse lines, and send to T4
    Reverse reverser = new Reverse(wordDigits, reversed);
    Thread reverserThread = new Thread(reverser);
    reverserThread.start();

    // Thread 4: Take input from T3 and print to STDOUT
    Print printer = new Print(reversed);
    Thread printerThread = new Thread(printer);
    printerThread.start();

    // This thread has done its job, and we no longer need it.
    // But that's okay. It's cool. It can stay.

  }
}
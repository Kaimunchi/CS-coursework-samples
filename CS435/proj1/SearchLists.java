import java.util.ArrayList;
/**
 * Searches a number of lists for a common variable.
 * @author Brandon Ingli
 * @version 1.0
 */
public class SearchLists implements Runnable{
  /** Shared Memory */
  private ListsData data;
  /** Number to find in common between the data.lists */
  private int toFind;

  /**
   * Constructor for a SearchLists object/"frame"
   * @param data reference to shared memory
   * @param toFind time to find in common
   */
  public SearchLists(ListsData data, int toFind){
    this.data = data;
    this.toFind = toFind;
  }

  public void run(){
    boolean stillValid = true;
    int i = 0;

    // Go through each list while there is the possibility of a common time
    while (stillValid && i < data.getLists().size()){
      ArrayList<Integer> l = data.getListAt(i);
      boolean inList = false;

      // Go through the list, stopping if we find the common time
      int j = 0;
      while (!inList && j < l.size()){
        inList = (l.get(j) == toFind);
        j++;
      }

      stillValid = inList; // As long as we keep finding values, it still
                           // holds that we have a valid common value.
      i++;
    }

    // If we found a common time, announce that fact.
    if(stillValid){
      data.setFound(true);
      System.out.println(toFind + " is a common meeting time.");
    }
  }
}
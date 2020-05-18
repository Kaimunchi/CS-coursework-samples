import java.util.ArrayList;

/**
 * Shared Data for the SearchLists Threads
 * @author Brandon Ingli
 * @version 1.0
 */
public class ListsData {
  /** True if a common time was found by one of the Threads */
  private boolean found = false;
  /** The lists other than the first one. */
  private ArrayList<ArrayList<Integer>> lists = new ArrayList<ArrayList<Integer>>();
  
  /** Set the value of the found variable */
  public void setFound(boolean val){
    found = val;
  }
  /**
   * Get the value of found
   * @return found
   */
  public boolean getFound(){
    return found;
  }

  /**
   * Add an ArrayList of Integers to the lists
   * @param list ArrayList<Integer> to add to lists
   */
  public void addToLists(ArrayList<Integer> list){
    lists.add(list);
  }
  /**
   * Gets the "Matrix" of lists
   * @return ArrayList<ArrayList<Integer>> of lists
   */
  public ArrayList<ArrayList<Integer>> getLists(){
    return lists;
  }
  /**
   * Gets one particular ArrayList from the "Matrix"
   * @param pos int position of the list to get
   * @return ArrayList<Integer> at position pos in lists
   */
  public ArrayList<Integer> getListAt(int pos){
    return lists.get(pos);
  }
  /**
   * Resets the "Matrix" back to a new object
   */
  public void resetLists(){
    lists = new ArrayList<ArrayList<Integer>>();
  }
}
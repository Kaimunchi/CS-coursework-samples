package fileSync;

import java.io.Serializable;

/**
 * Implements a Singly Linked List of FileMetadataNodes
 * Based on CustomLinkedList
 * @author Dr. Chetan Jaiswal
 * @author Brandon Ingli
 * @version 1.0
 */

public class FileMetadataLinkedList implements Serializable{
	
	private FileMetadataNode start;
    private int size ;
 
    /**
     * Constructs an empty FileMetadataLinkedList
     */
    public FileMetadataLinkedList() {
        start = null;
        size = 0;
    }
    
    /**
     * Constructs a copy of a FileMetadataLinkedList
     * @param other FileMetadataLinkedList to copy
     */
    public FileMetadataLinkedList(FileMetadataLinkedList other) {
    	FileMetadataNode aNode = other.getStart();
    	FileMetadataNode curr = this.getStart();
    	
    	while(aNode != null) {
    		if(curr == null) {
    			start = curr = new FileMetadataNode(aNode);
    			aNode = aNode.getNext();
    			continue;
    		}
    		else {
    			curr.setNext(new FileMetadataNode(aNode));
    		}
    		aNode = aNode.getNext();
    		curr = curr.getNext();
    	}
    }
    
    /**  
     * Function to check if list is empty
     * @return true: if the list is empty, false: otherwise
     */
    public boolean isEmpty() {
    	if(start == null)
    		return true;
    	else
    		return false;
    }
    
    /**  
     * Function to get size of list
     * @return size of the list (# of nodes)
     */
    public int getSize() {
        return size;
    } 
    
    /**  
     * Function to insert an element at beginning
     * @param val value to be inserted at the first node of the list
     */
    public void insertAtStart(FileMetadata val) {
        FileMetadataNode node = new FileMetadataNode(val, null);    
        size++ ;    
        if(start == null) {
            start = node;
        }
        else {
            node.setNext(start);
            start = node;
        }
    }
    
    /**
     * Inserts an element at the end of the list
     * @param val element to insert at the end of the list.
     */
    public void insertAtEnd(FileMetadata val) {
    	insertAtPos(getSize()+1, val);
    }
    
    /**  
     * Function to insert an element at position
     * @param val value to be inserted in the list
     * @param pos position of the value in the list
     * @precondition pos > 1
     * @precondition pos <= size+1
     */
    public void insertAtPos(int pos, FileMetadata val){
    	if( pos <= 0 || pos > size+1) {
    		throw new IndexOutOfBoundsException();
    	}
    	
    	FileMetadataNode node = new FileMetadataNode(val, null);                
        FileMetadataNode aFileMetadataNode = start;
        pos = pos - 1 ;
        for (int i = 1; i <= size; i++) {
            if (i == pos) {
            	if(i == size-1) {
            		aFileMetadataNode.setNext(node);
            		break;
            	}
                FileMetadataNode tmp = aFileMetadataNode.getNext() ;
                aFileMetadataNode.setNext(node);
                node.setNext(tmp);
                break;
            }
            aFileMetadataNode = aFileMetadataNode.getNext();
        }
        size++ ;
    }
    
    /**  
     * Function to delete an element at position
     * @param pos position from which the item needs to be deleted (node position)
     * @precondition pos >= 1
     * @precondition pos <= size
     */
    public void deleteAtPos(int pos){
    	if(pos < 1 || pos > size) {
    		throw new IndexOutOfBoundsException();
    	}
    	
        if (pos == 1) {
            start = start.getNext();
            size--; 
            return ;
        }
        if (pos == size) {
            FileMetadataNode s = start;
            FileMetadataNode t = start;
            while (s.getNext() != null) {
                t = s;
                s = s.getNext();
            }
            t.setNext(null);
            size --;
            return;
        }
        FileMetadataNode aFileMetadataNode = start;
        pos = pos - 1 ;
        for (int i = 1; i < size - 1; i++) {
            if (i == pos) {
                FileMetadataNode tmp = aFileMetadataNode.getNext();
                tmp = tmp.getNext();
                aFileMetadataNode.setNext(tmp);
                break;
            }
            aFileMetadataNode = aFileMetadataNode.getNext();
        }
        size-- ;
    }    
    
    /**
     * updates the value of a node at the position pos
     * @param pos position of the node
     * @param newVal new value that needs to be updated at the pos
     * @precondition 1 <= pos <= size
     */
    public void updateAtPos(int pos, FileMetadata newVal) {
    	if(pos < 1 || pos > size) {
    		throw new IndexOutOfBoundsException();
    	}
    	
    	FileMetadataNode aFileMetadataNode = start;
    	int currPos = 1;
    	
    	while(currPos != pos){
    		if (aFileMetadataNode == null) {
    			return;
    		}
    		aFileMetadataNode = aFileMetadataNode.getNext();
    		currPos++;
    	}
    	
    	aFileMetadataNode.setData(newVal);
    }
    
    /**
     * Gets the start reference of the linkedlist 
     * @return the start reference of the linkedlist
     */
    public FileMetadataNode getStart() {
    	return start;
    }
    
    /**
     * Gets the end reference of the linkedlist
     * @return the end reference of the linkedlist
     */
    public FileMetadataNode getEnd() {
    	FileMetadataNode aNode = getStart();
    	if(aNode == null) {
    		return aNode;
    	}
    	
    	while(aNode.getNext() != null) {
    		aNode = aNode.getNext();
    	}
    	
    	return aNode;
    }
    
    /**
     * Gets a String representation of this FileMetadataLinkedList.
     * @return String representation fo this FileMetadataLinkedList
     */
    @Override
    public String toString() {
    	String toReturn = "";
    	FileMetadataNode aNode = getStart();
    	
    	if(aNode == null) {
    		return "[Empty List]";
    	}
    	
    	while(aNode.getNext() != null) {
    		toReturn += "\t" + aNode.getData().toString() + " ->\n";
    		aNode = aNode.getNext();
    	}
    	
    	toReturn += "\t" + aNode.getData().toString();
    	
    	return toReturn;
    }
    
    /**
	 * Determines if given object is equal to this one
	 * @return true if this is equal to the given object
	 */
    @Override
    public boolean equals(Object other) {
    	if(this == other) {
    		return true;
    	}
    	
    	if(!(other instanceof FileMetadataLinkedList)) {
    		return false;
    	}
    	
    	FileMetadataLinkedList fmll = (FileMetadataLinkedList) other;
    	
    	FileMetadataNode thisNode = this.getStart();
    	FileMetadataNode otherNode = fmll.getStart();
    	
    	while(thisNode != null) {
    		if(!thisNode.equals(otherNode)) {
    			return false;
    		}
    		thisNode = thisNode.getNext();
    		otherNode = otherNode.getNext();
    	}
    	
    	return true;
    }

}
 
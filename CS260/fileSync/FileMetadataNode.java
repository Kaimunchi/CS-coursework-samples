package fileSync;

import java.io.Serializable;

/**
 * A Node for a FileMetadataLinkedList.
 * @author Brandon Ingli
 * @version 1.0
 *
 */
public class FileMetadataNode implements Serializable{
	
	private FileMetadata data;
	private FileMetadataNode next;
	
	/**
	 * Constructs a default Node with data and next as null.
	 */
	public FileMetadataNode() {
		this.data = null;
		this.next = null;
	}
	
	/**
	 * Constructs a node with the given data and a null next node
	 * @param data Data to store in this node
	 */
	public FileMetadataNode(FileMetadata data) {
		this.data = data;
		this.next = null;
	}
	
	/**
	 * Constructs a Node with given data pointing to given Node
	 * @param data FileMetadata reference to store in Node.
	 * @param next Node for this Node to point to.
	 */
	public FileMetadataNode(FileMetadata data, FileMetadataNode next) {
		this.data = data;
		this.next = next;
	}
	
	
	/**
	 * Copy Constructor for a FileMetadataNode
	 * @param other FileMetadataNode to copy
	 * @postcondition this.next is a copy of other.next
	 */
	public FileMetadataNode(FileMetadataNode other){
		if(other.getData() == null) {
			this.data = new FileMetadata();
		}
		else {
			this.data = new FileMetadata(other.getData());
		}
		
		if(other.getNext() == null) {
			this.next = null;
		}
		else {
			this.next = new FileMetadataNode(other.getNext());
		}
	}

	/**
	 * Gets FileMetadata reference stored in this Node.
	 * @return FileMetadata stored in this Node.
	 */
	public FileMetadata getData() {
		return data;
	}

	/**
	 * Sets FileMetadata reference stored in this Node.
	 * @param data Reference to store in this Node.
	 */
	public void setData(FileMetadata data) {
		this.data = data;
	}

	/**
	 * Gets reference to next Node.
	 * @return Reference to next Node in list.
	 */
	public FileMetadataNode getNext() {
		return next;
	}

	/**
	 * Sets reference to next Node.
	 * @param next Reference to next Node in list.
	 */
	public void setNext(FileMetadataNode next) {
		this.next = next;
	}
	
	/**
	 * Gets String representation of this Node
	 * @return String representation of this Node
	 */
	@Override
	public String toString() {
		String toReturn = "[Data: " + this.getData();
		toReturn += "\t Next: " + this.getNext();
		
		return toReturn;
	}
	
	/**
	 * Determines if given object is equal to this one
	 * @return true if this is equal to the given object
	 */
	@Override
	public boolean equals(Object other) {
		if(other == this) {
			return true;
		}
		if(!(other instanceof FileMetadataNode)) {
			return false;
		}
		
		FileMetadataNode fdn = (FileMetadataNode)other;
		
		if(!(this.getData().equals(fdn.getData()))) {
			return false;
		}
		
		if(this.getNext() == null && fdn.getNext() == null) {
			return true;
		}
		else if(this.getNext() == null && fdn.getNext() != null) {
			return false;
		}
		else if(this.getNext() != null && fdn.getNext() == null) {
			return false;
		}
		
		if(!(this.getNext().equals(fdn.getNext()))) {
			return false;
		}
		return true;
	}
	
	
}

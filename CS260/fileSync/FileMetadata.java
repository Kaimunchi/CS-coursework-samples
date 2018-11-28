package fileSync;

import java.io.Serializable;

/**
 * A class to hold metadata about a file.
 * @author Brandon Ingli
 * @version 1.0
 * @invariant Class is immutable
 */
public class FileMetadata implements Serializable{
	
	private final String fileName;
	private final String relativePath;
	private final long lastModified;
	private final long size;
	
	/**
	 * Constructs a new FileMetadata
	 * @param fileName String representation of file name
	 * @param relativePath String representation of relative path from a home directory
	 * @see <code>java.nio.file.Path.relativize(Path)</code>
	 * @param lastModified long of last modified time in milliseconds from Epoch
	 * @see <code>java.nio.file.attribute.FileTime.tomillis()</code>
	 * @param size size of file in Bytes
	 */
	public FileMetadata(String fileName, String relativePath, long lastModified, long size) {
		this.fileName = fileName;
		this.relativePath = relativePath.replace('\\', '/');
		this.lastModified = lastModified;
		this.size = size;
	}
	
	/**
	 * Constructs a copy of other
	 * @param other FileMetadata to copy
	 */
	public FileMetadata(FileMetadata other) {
		this.fileName = other.getFileName();
		this.relativePath = other.getRelativePath();
		this.lastModified = other.getLastModified();
		this.size = other.getSize();
	}
	
	/**
	 * Default constructor that sets all attributes to their zero-values
	 * @warn For use by Jackson during Deserialization. End users should use another constructor.
	 * @see <code>public FileMetadata(String, String, long, long)</code>
	 */
	public FileMetadata() {
		fileName = null;
		relativePath = null;
		lastModified = 0;
		size = 0;
	}
	
	/**
	 * Gets File Name
	 * @return Path object representing file name
	 */
	public String getFileName() {
		return fileName;
	}
	
	/**
	 * Gets Last Modified Time of File
	 * @return long representing last modified time in millis since Epoch
	 */
	public long getLastModified() {
		return lastModified;
	}
	
	/**
	 * Gets String representation of Relative Path
	 * @return String representation of Relative Path
	 */
	public String getRelativePath() {
		return relativePath;
	}
	
	
	/**
	 * Gets Size of file
	 * @return Size of File in Bytes
	 */
	public long getSize() {
		return size;
	}
	
	/**
	 * Gets String representation of this FileMetadata.
	 * @return String representation of this FileMetadata.
	 */
	@Override
	public String toString() {
		String toReturn = "[FileName: " + fileName + "\t";
		toReturn += "filePath: " + relativePath + "\t";
		toReturn += "LastModified: " + lastModified + "\t";
		toReturn += "Size: " + Long.toString(size) + "]";
		
		return toReturn;
	}
	
	/**
	 * Determines if given object is equal to this one
	 * @return true if this is equal to the given object
	 */
	@Override
	public boolean equals(Object o) {
		if(this == o) {
			return true;
		}
		if(! (o instanceof FileMetadata)) {
			return false;
		}
		
		FileMetadata other = (FileMetadata) o;
		
		if(!this.getFileName().equals(other.getFileName())) {
			return false;
		}
		
		if(!this.getRelativePath().equals(other.getRelativePath())) {
			return false;
		}
		
		if(this.getLastModified() != other.getLastModified()) {
			return false;
		}
		
		if(this.getSize() != other.getSize()) {
			return false;
		}
		
		return true;
	}
}

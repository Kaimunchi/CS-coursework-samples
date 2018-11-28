package fileSync;

import java.io.IOException;
import java.io.Serializable;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Catalogs files' metadata (recursively) in a home directory
 * @author Brandon Ingli
 * @version 1.0
 * @invariant home directory Path
 */
public class FileLog implements Serializable{
	
	transient private final Path home;
	private FileMetadataLinkedList list;
	
	/**
	 * Constructs a FileLog with the given home directory
	 * @param home home directory
	 * @precondition home must refer to a directory
	 */
	public FileLog(Path home){
		if(!Files.isDirectory(home)) {
			throw new IllegalArgumentException();
		}
		
		if(!home.isAbsolute()) {
			this.home = home.toAbsolutePath();
		}
		else {
			this.home = home;
		}
		list = null;
	}
	
	/**
	 * Default constructor that doesn't set home.
	 * @warn For use by Jackson during Deserialization. End users should use the other constructor.
	 * @see <code>public FileLog(Path home)</code>
	 */
	public FileLog() {
		home = null;
		list = null;
	}
	
	/**
	 * Logs the Home directory's files.
	 */
	public void logHomeDirectory() {
		list = new FileMetadataLinkedList();
		logDirectory(home);
	}
	
	/**
	 * Helper Method to recursively log a specified directory's files. 
	 * @param dir Directory to log
	 * @precondition dir must refer to a directory.
	 */
	private void logDirectory(Path dir) {
		DirectoryStream<Path> directoryStream;
		try {
			directoryStream = Files.newDirectoryStream(dir);
		} catch (IOException e) {
			return;
		}
		for (Path p : directoryStream) {
			if(Files.isDirectory(p)) {
				logDirectory(p);
			}
			else {
				try {
					FileMetadata temp = new FileMetadata(p.getFileName().toString(), home.relativize(p).toString(), Files.getLastModifiedTime(p).toMillis(), Files.size(p));
					if(list.getSize() == 0) {
						list.insertAtStart(temp);
					}
					else {
						list.insertAtEnd(temp);
					}
				} catch (IOException e) {
					continue;
				}
			}
		}
	}
	
	/**
	 * Gets the Path of the home directory 
	 * @return Path of the home directory
	 */
	public Path getHome() {
		return home;
	}
	
	/**
	 * Gets a copy of the file list
	 * @return a copy of the FileMetadataLinkedList holding the file listing
	 */
	public FileMetadataLinkedList getList() {
		return new FileMetadataLinkedList(list);
	}
	
	/**
	 * Gets String representation of FileLog
	 * @return String representation of FileLog
	 */
	@Override
	public String toString() {
		String toReturn = "Home: " + home.toString() + "\n";
		toReturn += "list:\n" + list.toString();
		
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
		
		if(!(o instanceof FileLog)) {
			return false;
		}
		
		FileLog other = (FileLog) o;
		
		if(!this.getHome().equals(other.getHome())) {
			return false;
		}
		
		if(!this.getList().equals(other.getList())) {
			return false;
		}
		
		return true;
	}
	
}

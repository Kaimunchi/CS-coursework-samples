package fileSync;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Utility class that Reads and Writes FIleLogs to and from Disk
 * @author Brandon Ingli
 * @version 1.0
 */
public class FileLogIO {
	
	/**
	 * Writes FileLog to a file with the specified path
	 * @param filePath Path to where the file is to be stored
	 * @param log FileLog to write
	 * @throws IOException If an error arises creating or writing to the file.
	 * @postcondition file at filePath exists
	 */
	public static void writeToFile(Path filePath, FileLog log) throws IOException {
		
		ObjectMapper mapper = new ObjectMapper();
		mapper.setVisibility(PropertyAccessor.ALL, Visibility.NONE);
		mapper.setVisibility(PropertyAccessor.FIELD, Visibility.ANY);
		
		mapper.writeValue(new File(filePath.toString()), log);
	}
	
	/**
	 * Loads a FileLog from the specified file
	 * @param file Path to FileLog save file (.json)
	 * @return FileLog loaded from file
	 * @throws IOException if an error arises opening or parsing the file
	 * @precondition file exists
	 */
	public static FileLog loadFromFile(Path file) throws IOException {
		ObjectMapper mapper = new ObjectMapper();
		mapper.setVisibility(PropertyAccessor.ALL, Visibility.NONE);
		mapper.setVisibility(PropertyAccessor.FIELD, Visibility.ANY);
		
		FileLog log = mapper.readValue(new File(file.toString()), FileLog.class);
		
		return log;
	}
}

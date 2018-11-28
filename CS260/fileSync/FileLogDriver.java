package fileSync;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Scanner;

/**
 * Driver Class to test the functionality of the FileLog
 * @author Brandon Ingli
 * @version 1.0
 */
public class FileLogDriver {
	public static void main(String[] args) {
		Scanner s = new Scanner(System.in);
		
		System.out.print("Enter your home directory path: ");
		
		Path home = Paths.get(s.next());
		
		s.close();
		
		FileLog log = new FileLog(home);
		
		System.out.println("Scanning The Directory: " + home);
		
		log.logHomeDirectory();
		
		System.out.println("Logging complete. Writing to file.");
		
		Path jsonPath = Paths.get(log.getHome().getParent().toString(), "testlog.json");
		try {
			FileLogIO.writeToFile(jsonPath, log);
		} catch (IOException e) {
			System.err.println("IO Exception Occurred. Terminating Driver.");
			e.printStackTrace();
			System.exit(1);
		}
		
		System.out.println("Here's a string representation of what we found: ");
		System.out.println(log);
		
		System.out.println("\nNow We'll try loading that back in.");
		
		FileLog log2 = null;
		try {
			log2 = FileLogIO.loadFromFile(jsonPath);
		} catch (IOException e) {
			System.err.println("IO Exception Occurred. Terminating Driver.");
			e.printStackTrace();
			System.exit(2);
		}
		
		System.out.println("Here's a String representation of that: ");
		System.out.println(log2);
		
	}
}

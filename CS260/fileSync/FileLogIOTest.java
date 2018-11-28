package fileSync;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.jupiter.api.Test;

class FileLogIOTest {

	@Test
	void test() {
		FileLog fl = new FileLog(Paths.get("D:/Documents/!TrumanDocs/2018-19/CS260/Project/home"));
		
		fl.logHomeDirectory();
		
		try {
			FileLogIO.writeToFile(Paths.get(fl.getHome().getParent().toString(),  "filelogio_test.json"), fl);
		} catch (IOException e) {
			e.printStackTrace();
			fail("IOException");
		}
		
		Path logFile = Paths.get("D:/Documents/!TrumanDocs/2018-19/CS260/Project/filelogio_test.json");
		
		assertTrue(Files.exists(logFile));
		
		FileLog loaded = null;
		try {
			loaded = FileLogIO.loadFromFile(logFile);
		} catch (IOException e) {
			e.printStackTrace();
			fail("IOException");
		}
		
		assertEquals(fl, loaded);
		
		assertThrows(IOException.class, ()->{FileLog failure = FileLogIO.loadFromFile(Paths.get("C:/nothere.txt"));});
		assertThrows(Exception.class, ()->{FileLog failure2 = FileLogIO.loadFromFile(null);});
	}

}

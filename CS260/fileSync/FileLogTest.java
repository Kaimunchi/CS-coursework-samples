package fileSync;

import static org.junit.jupiter.api.Assertions.*;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.jupiter.api.Test;

class FileLogTest {

	@Test
	void testFileLogConstructor() {
		Path home = Paths.get("D:\\Documents\\!TrumanDocs\\2018-19\\CS260\\Project\\home");
		
		FileLog fl = new FileLog(home);
		
		assertEquals(home, fl.getHome());
		assertThrows(Exception.class, ()->{FileLog fl2 = new FileLog(null);});
		assertThrows(Exception.class, ()->{FileLog fl2 = new FileLog(Paths.get("D:\\Documents\\!TrumanDocs\\2018-19\\CS260\\Project\\home\\ufoundme.txt"));});
	}

	@Test
	void testLogHomeDirectory() {
		Path home = Paths.get("D:\\Documents\\!TrumanDocs\\2018-19\\CS260\\Project\\home");
		FileLog fl = new FileLog(home);
		
		FileMetadataLinkedList ll = new FileMetadataLinkedList();
		ll.insertAtStart(new FileMetadata("super_secret_document.txt", "coolio\\super_secret_document.txt", 1538525475943L, 0));
		ll.insertAtEnd(new FileMetadata("jaiswal_is_the_best.txt", "jaiswal_is_the_best.txt", 1538529700077L, 0));
		ll.insertAtEnd(new FileMetadata("ufoundme.txt", "ufoundme.txt", 1538515613856L, 0));
		ll.insertAtEnd(new FileMetadata("apple.txt", "zebra/apple.txt", 1538765519584L, 0));
		ll.insertAtEnd(new FileMetadata("banana.txt", "zebra/banana.txt", 1538765529295L, 0));
		
		fl.logHomeDirectory();
		
		assertEquals(ll, fl.getList());
		
	}

	@Test
	void testGetHome() {
		Path home = Paths.get("D:\\Documents\\!TrumanDocs\\2018-19\\CS260\\Project\\home");
		FileLog fl = new FileLog(home);
		
		assertEquals(home, fl.getHome());
	}

	@Test
	void testGetList() {
		Path home = Paths.get("D:\\Documents\\!TrumanDocs\\2018-19\\CS260\\Project\\home");
		FileLog fl = new FileLog(home);
		
		FileMetadataLinkedList ll = new FileMetadataLinkedList();
		ll.insertAtStart(new FileMetadata("super_secret_document.txt", "coolio\\super_secret_document.txt", 1538525475943L, 0));
		ll.insertAtEnd(new FileMetadata("jaiswal_is_the_best.txt", "jaiswal_is_the_best.txt", 1538529700077L, 0));
		ll.insertAtEnd(new FileMetadata("ufoundme.txt", "ufoundme.txt", 1538515613856L, 0));
		ll.insertAtEnd(new FileMetadata("apple.txt", "zebra/apple.txt", 1538765519584L, 0));
		ll.insertAtEnd(new FileMetadata("banana.txt", "zebra/banana.txt", 1538765529295L, 0));
		
		fl.logHomeDirectory();
		
		assertEquals(ll, fl.getList());
		assertNotSame(ll, fl.getList());
	}

}

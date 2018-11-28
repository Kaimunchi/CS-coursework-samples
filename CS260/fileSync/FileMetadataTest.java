package fileSync;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class FileMetadataTest {

	@Test
	void testFileMetadataFileMetadata() {
		FileMetadata fm = new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20);
		FileMetadata copy = new FileMetadata(fm);
		
		assertEquals(fm, copy);
		assertNotSame(fm, copy);
		
		assertThrows(Exception.class, ()->{FileMetadata failure = new FileMetadata(null);});
	}

	@Test
	void testGetFileName() {
		FileMetadata fm = new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20);
		assertEquals("test.txt", fm.getFileName());
	}

	@Test
	void testGetLastModified() {
		FileMetadata fm = new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20);
		assertEquals(0, fm.getLastModified());
	}

	@Test
	void testGetRelativePath() {
		FileMetadata fm = new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20);
		assertEquals("this/is/a/test.txt", fm.getRelativePath());
	}

	@Test
	void testGetSize() {
		FileMetadata fm = new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20);
		assertEquals(20, fm.getSize());
	}

}

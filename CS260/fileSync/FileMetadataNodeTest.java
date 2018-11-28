package fileSync;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class FileMetadataNodeTest {

	@Test
	void testFileMetadataNodeCopyConstructor() {
		FileMetadataNode fmd = new FileMetadataNode(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		FileMetadataNode copy = new FileMetadataNode(fmd);
		
		assertEquals(fmd, copy);
		assertNotSame(fmd, copy);

	}

	@Test
	void testGetData() {
		FileMetadataNode fmd = new FileMetadataNode(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		FileMetadata data = new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20);
		
		assertEquals(fmd.getData(), data);
	}

	@Test
	void testSetData() {
		FileMetadataNode fmd = new FileMetadataNode(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		FileMetadata data = new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20);
		FileMetadata newdata = new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50);
		
		fmd.setData(newdata);
		
		assertEquals(fmd.getData(), newdata);
		assertNotEquals(fmd.getData(), data);
	}

	@Test
	void testGetNext() {
		FileMetadataNode fmd = new FileMetadataNode(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		FileMetadataNode first = new FileMetadataNode(new FileMetadata("test.txt", "this/is/a/test.txt", 30, 50), fmd);
		
		FileMetadataNode next = first.getNext();
		
		assertSame(next, fmd);
	}

	@Test
	void testSetNext() {
		FileMetadataNode fmd = new FileMetadataNode(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		FileMetadataNode first = new FileMetadataNode(new FileMetadata("test.txt", "this/is/a/test.txt", 30, 50));
		
		first.setNext(fmd);
		
		FileMetadataNode next = first.getNext();
		
		assertSame(next, fmd);
	}

}

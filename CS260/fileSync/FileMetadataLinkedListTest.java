package fileSync;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class FileMetadataLinkedListTest {

	@Test
	void testFileMetadataLinkedListFileMetadataLinkedList() {
		FileMetadataLinkedList fmll = new FileMetadataLinkedList();
		fmll.insertAtStart(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		fmll.insertAtEnd(new FileMetadata("test2.txt", "test2.txt.", 30, 20));
		
		FileMetadataLinkedList copy = new FileMetadataLinkedList(fmll);
		
		assertEquals(fmll, copy);
		assertNotSame(fmll, copy);
		
		assertThrows(Exception.class, ()->{FileMetadataLinkedList failure = new FileMetadataLinkedList(null);});
	}

	@Test
	void testIsEmpty() {
		FileMetadataLinkedList fmll = new FileMetadataLinkedList();
		assertTrue(fmll.isEmpty());
		
		fmll.insertAtStart(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		assertFalse(fmll.isEmpty());
	}

	@Test
	void testGetSize() {
		FileMetadataLinkedList fmll = new FileMetadataLinkedList();
		assertEquals(0, fmll.getSize());
		
		fmll.insertAtStart(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		assertEquals(1, fmll.getSize());
		
		fmll.insertAtStart(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		assertEquals(2, fmll.getSize());
	}

	@Test
	void testInsertAtStart() {
		FileMetadataLinkedList fmll = new FileMetadataLinkedList();
		fmll.insertAtStart(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		
		assertEquals(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20), fmll.getStart().getData());
		
		fmll.insertAtStart(new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50));
		
		assertEquals(new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50), fmll.getStart().getData());
	}

	@Test
	void testInsertAtEnd() {
		FileMetadataLinkedList fmll = new FileMetadataLinkedList();
		fmll.insertAtStart(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		
		assertEquals(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20), fmll.getStart().getData());
		
		fmll.insertAtEnd(new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50));
		
		assertEquals(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20), fmll.getStart().getData());
		
		FileMetadataNode node = fmll.getStart().getNext();
		assertEquals(new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50), node.getData());
	}

	@Test
	void testInsertAtPos() {
		FileMetadataLinkedList fmll = new FileMetadataLinkedList();
		fmll.insertAtStart(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		
		assertEquals(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20), fmll.getStart().getData());
		
		fmll.insertAtPos(2, new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50));
		
		assertEquals(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20), fmll.getStart().getData());
		
		FileMetadataNode node = fmll.getStart().getNext();
		assertEquals(new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50), node.getData());
		
		assertThrows(IndexOutOfBoundsException.class, ()->{fmll.insertAtPos(99, null);});
		assertThrows(IndexOutOfBoundsException.class, ()->{fmll.insertAtPos(0, null);});
	}

	@Test
	void testDeleteAtPos() {
		FileMetadataLinkedList fmll = new FileMetadataLinkedList();
		fmll.insertAtStart(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		fmll.insertAtStart(new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50));
		fmll.deleteAtPos(2);
		
		assertEquals(new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50), fmll.getStart().getData());
		assertThrows(IndexOutOfBoundsException.class, ()->{fmll.deleteAtPos(99);});
		assertThrows(IndexOutOfBoundsException.class, ()->{fmll.deleteAtPos(0);});
		
	}

	@Test
	void testUpdateAtPos() {
		FileMetadataLinkedList fmll = new FileMetadataLinkedList();
		fmll.insertAtStart(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		fmll.insertAtStart(new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50));
		fmll.updateAtPos(2, new FileMetadata("test3.txt", "this/is/a/test3.txt", 30, 50));
		
		assertEquals(new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50), fmll.getStart().getData());
		assertEquals(new FileMetadata("test3.txt", "this/is/a/test3.txt", 30, 50), fmll.getStart().getNext().getData());
		
		assertThrows(IndexOutOfBoundsException.class, ()->{fmll.updateAtPos(99, null);});
		assertThrows(IndexOutOfBoundsException.class, ()->{fmll.updateAtPos(0, null);});
	}

	@Test
	void testGetStart() {
		FileMetadataLinkedList fmll = new FileMetadataLinkedList();
		fmll.insertAtStart(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		fmll.insertAtStart(new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50));
		
		assertEquals(new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50), fmll.getStart().getData());
		
		fmll = new FileMetadataLinkedList();
		assertEquals(null, fmll.getStart());
		
	}
	
	@Test
	void testGetEnd() {
		FileMetadataLinkedList fmll = new FileMetadataLinkedList();
		fmll.insertAtStart(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20));
		fmll.insertAtStart(new FileMetadata("test2.txt", "this/is/a/test2.txt", 30, 50));
		
		assertEquals(new FileMetadata("test.txt", "this/is/a/test.txt", 0, 20), fmll.getEnd().getData());
		
		fmll = new FileMetadataLinkedList();
		assertEquals(null, fmll.getEnd());
	}

}

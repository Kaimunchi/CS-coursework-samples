package fileSync;

import static org.junit.jupiter.api.Assertions.*;

import java.nio.file.Paths;

import org.junit.jupiter.api.Test;

class FileSyncTest {


	@Test
	void fileSyncTest() {
		final String homeFolder = "D:/Documents/cs260project/localHome";
		final String serverRoot = "D:/Documents/cs260project/serverRoot";
		
		FileSyncServer server = new FileSyncServer(serverRoot);
		FileSyncClient client = new FileSyncClient(homeFolder);
		
		
		server.setVerbose(true);
		Thread serverThread = new Thread() {
			public void run() {
				server.startServer();
			}
		};
		
		serverThread.start();
		
		try {
			Thread.sleep(1000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
		client.setVerbose(true);
		Thread clientThread = new Thread() {
			public void run() {
				client.startClient();
			}
		};
		
		clientThread.start();
		try {
			clientThread.join();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		FileLog homeLog = new FileLog(Paths.get(homeFolder));
		FileLog serverLog = new FileLog(Paths.get(serverRoot, "Brandon-4KDell", "localHome"));
		
		homeLog.logHomeDirectory();
		serverLog.logHomeDirectory();
		
		FileMetadataLinkedList homeList = homeLog.getList();
		FileMetadataLinkedList serverList = serverLog.getList();
		
		FileMetadataNode homeNode = homeList.getStart();
		FileMetadataNode serverNode = serverList.getStart();
		
		while(homeNode != null && serverNode != null) {
			assertEquals(homeNode.getData().getFileName(), serverNode.getData().getFileName());
			assertEquals(homeNode.getData().getRelativePath(), serverNode.getData().getRelativePath());
			assertEquals(homeNode.getData().getSize(), serverNode.getData().getSize());
			
			homeNode = homeNode.getNext();
			serverNode = serverNode.getNext();
		}
	}

}

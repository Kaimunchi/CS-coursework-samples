package fileSync;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;

/**
 * The server component of fileSync to accept FileSyncClients for backup.
 * @author Brandon Ingli
 *
 */
public class FileSyncServer {
	
	private boolean verbose = false;
	
	private final int PORT_NUMBER = 9998;
	
	private final int MAX_FILE_BUFFER_SIZE = 1000;
	
	private Path serverRoot;
	private Path serverHome;
	private Path logFile;
	
	private ServerSocket serverSocket = null;
	private Socket socket = null;
	private InputStream inStream = null;
	private OutputStream outStream = null;
	
	/**
	 * Constructs a new FileSyncServer
	 * @param serverRoot String path to the server's root folder
	 */
	public FileSyncServer(String serverRoot) {
		this.serverRoot = Paths.get(serverRoot);
	}
	
	/**
	 * Waits for TCP connection on PORT_NUMBER and gets the Socket and I/O streams of said connection.
	 */
	public void getConnectionToClient() {
		try {
			socket = serverSocket.accept();
			
			log("Just connected to " + socket.getInetAddress());
			
			inStream = socket.getInputStream();
			outStream = socket.getOutputStream();
			
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
	
	/**
	 * Gets the Hostname of the Client to identify it
	 * @return Hostname of Client, with spaces, '/', '\', and ':' replaced with '_'
	 * @throws SocketException if Socket is disconnected
	 * @throws IOException if error reading from inputStream
	 * @precondition connection to client has been established
	 */
	public String getHostname() throws SocketException, IOException {
		
		if(!socket.isConnected()) {
			throw new SocketException();
		}
		
		synchronized(socket) {
			outStream.write("<<Hostname".getBytes("UTF-8"));
		}
		
		log("Requested Hostname from Client.");
		
		byte[] readBuffer = new byte[200];
		
		if(!socket.isConnected()) {
			throw new SocketException();
		}
		
		int size = inStream.read(readBuffer);
		
		if(size <= 0) {
			throw new IOException();
		}
		
		byte[] messageByteArray = new byte[size];
		System.arraycopy(readBuffer, 0, messageByteArray, 0, size);
		String hostname = new String(messageByteArray, "UTF-8");
		
		hostname.replace(' ', '_');
		hostname.replace('/', '_');
		hostname.replace('\\', '_');
		hostname.replace(':', '_');
		
		log("Got hostname from client: " + hostname);
		
		return hostname;
	}
	
	/**
	 * Gets the Home Folder name of the Client to what it's backing up
	 * @return Home folder name of Client, with spaces, '/', '\', and ':' replaced with '_'
	 * @throws SocketException if Socket is disconnected
	 * @throws IOException if error reading from inputStream
	 * @precondition connection to client has been established
	 */
	public String getHomeFolderName() throws SocketException, IOException {
		
		if(!socket.isConnected()) {
			throw new SocketException();
		}
		
		synchronized(socket) {
			outStream.write("<<HomeName".getBytes("UTF-8"));
		}
		
		log("Requested Home Folder Name from Client.");
		
		byte[] readBuffer = new byte[200];
		
		if(!socket.isConnected()) {
			throw new SocketException();
		}
		
		int size = inStream.read(readBuffer);
		
		if(size <= 0) {
			throw new IOException();
		}
		
		byte[] messageByteArray = new byte[size];
		System.arraycopy(readBuffer, 0, messageByteArray, 0, size);
		String homename = new String(messageByteArray, "UTF-8");
		
		homename.replace(' ', '_');
		homename.replace('/', '_');
		homename.replace('\\', '_');
		homename.replace(':', '_');
		
		log("Got Home Name from client: " + homename);
		
		return homename;
	}
	
	/**
	 * Gets the current FileLog from the Client
	 * @return current FileLog of the Client
	 * @throws SocketException if socket is disconnected
	 * @throws IOException if error reading from inputStream
	 * @precondition connection to client has been established
	 */
	public FileLog getCurrentFileLog() throws SocketException, IOException {
		if(!socket.isConnected()) {
			throw new SocketException();
		}
		
		synchronized(socket) {
			outStream.write("<<FileLog".getBytes("UTF-8"));
		}
		
		log("Requested FileLog from Client.");
		
		ObjectInputStream objInStream = new ObjectInputStream(inStream);
		
		FileLog log = null;
		try {
			log = (FileLog) objInStream.readObject();
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
			System.exit(10);
		}
		
		log("Received FileLog from Client.");
		
		return log;
	}
	
	/**
	 * Gets the file at relPath from the Client
	 * @param relPath relativePath (from home) of the file to retrieve
	 * @throws IOException on error reading file from stream or creating file on Server
	 * @postcondition Requisite parent directories are also created.
	 * @precondition connection to client has been established
	 */
	public void getFile(String relPath) throws IOException{
		if(!socket.isConnected()) {
			throw new SocketException();
		}
		
		synchronized(socket) {
			outStream.write(relPath.getBytes("UTF-8"));
		}
		
		log("Requested file from Client: " + relPath);
		
		DataInputStream dataInStream = new DataInputStream(inStream);
		File newFile = new File(Paths.get(serverHome.toAbsolutePath().toString(), relPath).toString());
		new File(Paths.get(newFile.getParent()).toString()).mkdirs(); //Creates requisite parent directories
		FileOutputStream fileOut = new FileOutputStream(newFile, false); //Overwrites instead of appending
		
		byte [] data = null;
    	try
    	{
    		//read the size of the file <- coming from Server
    		long fileSize = dataInStream.readLong();
    		int bufferSize=0;
    		
    		//decide the data reading bufferSize
    		if(fileSize > MAX_FILE_BUFFER_SIZE)
    			bufferSize = MAX_FILE_BUFFER_SIZE;
    		else
    			bufferSize = (int)fileSize;
    		
    		data = new byte[bufferSize];	
    		
    		//now read the file coming from Server & save it onto disk
  
    		long totalBytesRead = 0;
    		while(true)
    		{
    			//read bufferSize number of bytes from Server
    			int readBytes = dataInStream.read(data,0,bufferSize);

    			byte[] arrayBytes = new byte[readBytes];
    			System.arraycopy(data, 0, arrayBytes, 0, readBytes);
    			totalBytesRead = totalBytesRead + readBytes;
    			
    			if(readBytes>0)
    			{
    				//write the data to the file
    				fileOut.write(arrayBytes);
    	    		fileOut.flush();
    			}

    			//stop if fileSize number of bytes are read
    			if(totalBytesRead == fileSize)
    				break;
    			
    			//update fileSize for the last remaining block of data
    			if((fileSize-totalBytesRead) < MAX_FILE_BUFFER_SIZE)
    				bufferSize = (int) (fileSize-totalBytesRead);
    			
    			//reinitialize the data buffer
    			data = new byte[bufferSize];
    		}
    		
    		log(relPath + " size is: " + fileSize + "\tBytes Read: " + totalBytesRead);
    		
    		fileOut.close();
    	}
    	catch(Exception e)
    	{
    		e.printStackTrace();
    	}
		
		log("Received File from Client: " + relPath);
	}
	
	/**
	 * Determines which files to sync from client to server
	 * @return String Array of relative paths for files to sync
	 * @throws IOException on error getting current file log from client
	 * @precondition connection to client has been established
	 */
	public String[] getFilesToSync() throws IOException {
		
		log("Determining files to sync.");
		
		ArrayList<String> filesToSync = new ArrayList<String>();
		FileLog currentLog = getCurrentFileLog();
		FileMetadataLinkedList list = currentLog.getList();
		
		if(!Files.exists(logFile)) {	// All files are to be synced
			
			log("No old log file. Will sync all files.");
			
			FileMetadataNode node = list.getStart();
			while(node != null) {
				filesToSync.add(node.getData().getRelativePath());
				node = node.getNext();
			}
			
			FileLogIO.writeToFile(logFile, currentLog);
			
			log("Syncing " + Integer.toString(filesToSync.size()) + " Files.");
			
			return filesToSync.toArray(new String[filesToSync.size()]);
			
		}
		
		log("Old log file found.");
		
		FileLog oldLog = FileLogIO.loadFromFile(logFile);
		FileMetadataLinkedList oldList = oldLog.getList();
		
		FileMetadataNode currNode = list.getStart();
		
		while(currNode != null) {
			boolean syncMe = true;
			
			FileMetadataNode oldNode = oldList.getStart();
			while(oldNode != null && !oldNode.getData().getRelativePath().equals(currNode.getData().getRelativePath())) {
				oldNode = oldNode.getNext();
			}
			
			if(oldNode != null && oldNode.getData().getLastModified() == currNode.getData().getLastModified()) {
				syncMe = false;
			}
			
			if(syncMe) {
				filesToSync.add(currNode.getData().getRelativePath());
			}
			
			currNode = currNode.getNext();
		}
		
		FileLogIO.writeToFile(logFile, currentLog);
		
		log("Syncing " + Integer.toString(filesToSync.size()) + " Files.");
		
		return filesToSync.toArray(new String[filesToSync.size()]);
		
	}
	
	/**
	 * Cleans up the FileSyncServer in preparation for another client.
	 * @postcondition all streams with Client are closed
	 * @postcondition Client is told to shut down
	 * @postcondition all remnants of this Client (Path, socket, and stream references) are forgotten
	 */
	public void cleanup() {
		try {
			synchronized(socket) {
				outStream.write("<<Complete".getBytes("UTF-8"));
			}
			
			log("Told Client we're done. Cleaning up for next client.");
			
			inStream.close();
			outStream.close();
			socket.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		serverHome = null;
		logFile = null;
		socket = null;
		inStream = null;
		outStream = null;
	}
	
	/**
	 * Starts the FileSyncServer around the given server root directory
	 */
	public void startServer() {	
		try {
			serverSocket = new ServerSocket(PORT_NUMBER);
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(11);
		}
		
		while(true) {
			try {
				getConnectionToClient();
				
				String hostname = getHostname();
				
				String homename = getHomeFolderName();
				
				serverHome = Paths.get(serverRoot.toAbsolutePath().toString(), hostname, homename);
				new File(serverHome.toString()).mkdirs(); //Creates dirs if needed
				
				logFile = Paths.get(serverHome.getParent().toString(), homename+".json");
				
				String[] filesToSync = getFilesToSync();
				
				for(String file : filesToSync) {
					getFile(file);
				}
				
			} catch(RuntimeException e) {
				e.printStackTrace();
				System.exit(12);
			} catch(Exception e){
				e.printStackTrace();
			} finally {
				cleanup();
			}
		}
	}
	
	/**
	 * Stops the Server
	 * @precondition Best Practice if server is properly cleaned up
	 * @see cleanup()
	 */
	public void stopServer() {
		log("Shutting Down.");
		System.exit(0);
	}
	
	/**
	 * Sets the verbose level of this Server
	 * @param verbose state to set verbose to
	 */
	public void setVerbose(boolean verbose) {
		this.verbose = verbose;
	}
	
	/**
	 * Prints a message to the screen if this Server was set to verbose
	 * @param message message to print
	 */
	private void log(String message) {
		if(verbose) {
			System.out.println("[Server] " + message);
		}
	}
	
	public static void main(String[] args) {
		FileSyncServer server = null;
		
		if(args.length == 1 && (args[0].equals("-v") || args[0].equals("/v"))) {
			System.err.println("No file path argument given to FileSyncServer. Shutting Down.");
			System.exit(2);
		}
		else if(args.length == 2 && (args[0].equals("-v") || args[0].equals("/v"))) {
			server = new FileSyncServer(args[1]);
			server.setVerbose(true);
		}
		else if(args.length == 1) {
			server = new FileSyncServer(args[0]);
		}
		else if(args.length == 2 && !(args[0].equals("-v") || args[0].equals("/v"))) {
			System.err.println("Unrecognized argument " + args[0] + " to FileSyncServer. Shutting Down.");
			System.exit(1);
		}
		else if(args.length > 2) {
			System.err.println("Too Many Arguments to FileSyncServer. Shutting Down.");
			System.exit(2);
		}
		else {
			System.err.println("Too Few Arguments to FileSyncServer. Shutting Down.");
			System.exit(2);
		}
		server.startServer();
	}

}

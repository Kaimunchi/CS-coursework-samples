package fileSync;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * The client component of fileSync to connect to a FileSyncServer and backup.
 * @author Brandon Ingli
 *
 */
public class FileSyncClient {
	private boolean verbose = false;
	
	private final String SERVER_LOCATION = "brandoningli.com";
	private final int SERVER_PORT_NUMBER = 9998;
	
	private final int MAX_FILE_BUFFER_SIZE = 1000;
	
	private Path home;
	
	private Socket socket = null;
	private InputStream inStream = null;
	private OutputStream outStream = null;
	
	/**
	 * Constructs a new FileSyncClient
	 * @param home String path to home directory to sync
	 */
	public FileSyncClient(String home) {
		this.home = Paths.get(home);
	}
	
	/**
	 * Connects to server and fetches in/out streams
	 * @throws IOException if error creating Socket or I/O Streams
	 * @throws UnknownHostException if IP of Server cannot be determinted
	 * @precondition FileSyncServer is up at SERVER_LOCATION:SERVER_PORT_NUMBER
	 * @postcondition TCP streams established
	 */
	public void connectToServer() throws UnknownHostException, IOException {
		socket = new Socket(SERVER_LOCATION, SERVER_PORT_NUMBER);
		
		log("Connected to Server at " + socket.getInetAddress());
		
		inStream = socket.getInputStream();
		outStream = socket.getOutputStream();
	}
	
	/**
	 * Sends Hostname to Server. If Hostname is empty, a representation of the user's IP address is sent.
	 * @throws IOException if local host cannot be resolved, or if UTF-8 encoding fails.
	 * @precondition connection to Server established
	 */
	public void sendHostname() throws IOException{
		
		log("Server requested Hostname.");
		
		String hostname = InetAddress.getLocalHost().getHostName();
		
		if(hostname.length() <= 0) {
			hostname = InetAddress.getLocalHost().toString();
		}
		
		log("My hostname is " + hostname);
		
		if(socket.isConnected()) {
			synchronized(socket) {
				outStream.write(hostname.getBytes("UTF-8"));
			}
			log("Sent hostname to Server.");
		}
	}
	
	/**
	 * Sends Home Folder Name to Server.
	 * @throws IOException if local host cannot be resolved, or if UTF-8 encoding fails.
	 * @precondition connection to Server established
	 */
	public void sendHomeFolderName() throws IOException{
		
		log("Server requested Home Name.");
		
		String homename = new File(home.toString()).getName();
		
		log("My Home Folder name is: " + homename);
		
		if(socket.isConnected()) {
			synchronized(socket) {
				outStream.write(homename.getBytes("UTF-8"));
			}
			log("Sent Home Name to Server.");
		}
	}
	
	/**
	 * Logs the home directory and sends it to the Server
	 * @throws IOException on error creating or writing to outStream
	 * @precondition connection to Server established
	 */
	public void sendCurrentFileLog() throws IOException {
		
		log("Server requested current FileLog.");
		
		FileLog log = new FileLog(home);
		
		log.logHomeDirectory();
		
		log("Finished FileLog of home.");
		
		ObjectOutputStream objOutStream = new ObjectOutputStream(outStream);
		
		if(socket.isConnected()) {
			synchronized(socket) {
				objOutStream.writeObject(log);
			}
			log("Sent FileLog to Server.");
		}
	}
	
	/**
	 * Sends file at relPath to the Server
	 * @param relPath relative path (from home) of file to send
	 * @throws FileNotFoundException if file at relPath doesn't exist
	 * @throws IOException on error reading file or writing to outStream
	 * @precondition connection to Server established
	 */
	public void sendFile(String relPath) throws FileNotFoundException, IOException {
		
		log("Server requested file: " + relPath);
		
		byte[] data = null;
		int bufferSize = 0;
		
		File file = new File(home.toAbsolutePath().toString(), relPath);
		if(!file.exists()) {
			throw new FileNotFoundException();
		}
		FileInputStream fileInput = new FileInputStream(file);
		
		long fileSize = file.length();
		
		DataOutputStream dataOutStream = new DataOutputStream(outStream);
		
		dataOutStream.writeLong(fileSize);
		outStream.flush();
		
		if(fileSize > MAX_FILE_BUFFER_SIZE) {
			bufferSize = MAX_FILE_BUFFER_SIZE;
		} else {
			bufferSize = (int) fileSize;
		}
		
		data = new byte[bufferSize];
		
		long totalBytesRead = 0;
		while(true) {
			int readBytes = fileInput.read(data);
			
			outStream.write(data);
			outStream.flush();
			
			if(readBytes == -1) { //EOF
				break;
			}
			
			totalBytesRead += readBytes;
			
			if(totalBytesRead == fileSize) {
				break;
			}
			
			if((fileSize - totalBytesRead) < MAX_FILE_BUFFER_SIZE) {
				bufferSize = (int) (fileSize - totalBytesRead);
			}
			
			data = new byte[bufferSize];
			
		}
		
		fileInput.close();
		
		log("File sent to Server: " + relPath);
		
	}
	
	/**
	 * Cleans up the FileSyncClient in preparation for termination.
	 * @postcondition Connection to Server is properly ended
	 */
	public void cleanup() {
		try {
			log("Cleaning up for shutdown.");
			inStream.close();
			outStream.close();
			socket.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Performs action based on given command from Server
	 * @param command command from Server
	 * @precondition connection to Server established
	 */
	private void actOnCommand(String command){
		switch (command) {
			case "<<Hostname":
				try {
					sendHostname();
				} catch (IOException io) {
					io.printStackTrace();
					System.exit(10);
				}
				
				break;
				
			case "<<HomeName":
				try {
					sendHomeFolderName();
				} catch (IOException e1) {
					e1.printStackTrace();
					System.exit(11);
				}
				
				break;
				
			case "<<FileLog":
				try {
					sendCurrentFileLog();
				} catch (IOException e) {
					e.printStackTrace();
					System.exit(12);
				}
				
				break;
				
			case "<<Complete":
				cleanup();
				log("Goodbye!");
				System.exit(0);
				break;
				
			default:
				try {
					sendFile(command);
				} catch (IOException e) {
					e.printStackTrace();
					System.exit(13);
				}
		}
	}
	
	/**
	 * Starts the FileSyncClient for given home Directory
	 */
	public void startClient() {
		try {
			connectToServer();
		
		
			while(socket.isConnected()) {
				
				byte[] readBuffer = new byte[200];
				
				int size = inStream.read(readBuffer);
				
				if(size < 0) {
					Thread.sleep(500);
					continue;
				}
				
				byte[] messageByteArray = new byte[size];
				System.arraycopy(readBuffer, 0, messageByteArray, 0, size);
				String command = new String(messageByteArray, "UTF-8");
				
				actOnCommand(command);
			}
			
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(14);
		} catch (InterruptedException e) {
			e.printStackTrace();
			System.exit(15);
		}
		
	}
	
	/**
	 * Sets the verbose status of this Client
	 * @param verbose state to set verbose to
	 */
	public void setVerbose(boolean verbose) {
		this.verbose = verbose;
	}
	
	/**
	 * Prints a message if the client was set as verbose
	 * @param message message to print
	 */
	private void log(String message) {
		if(verbose) {
			System.out.println("[Client] " + message);
		}
	}
	
	public static void main(String[] args) {
		FileSyncClient client = null;
		
		if(args.length == 1 && (args[0].equals("-v") || args[0].equals("/v"))) {
			System.err.println("No file path argument given to FileSyncClient. Shutting Down.");
			System.exit(2);
		}
		else if(args.length == 2 && (args[0].equals("-v") || args[0].equals("/v"))) {
			client = new FileSyncClient(args[1]);
			client.setVerbose(true);
		}
		else if(args.length == 1) {
			client = new FileSyncClient(args[0]);
		}
		else if(args.length == 2 && !(args[0].equals("-v") || args[0].equals("/v"))) {
			System.err.println("Unrecognized argument " + args[0] + " to FileSyncClient. Shutting Down.");
			System.exit(1);
		}
		else if(args.length > 2) {
			System.err.println("Too Many Arguments to FileSyncClient. Shutting Down.");
			System.exit(2);
		}
		else {
			System.err.println("Too Few Arguments to FileSyncClient. Shutting Down.");
			System.exit(2);
		}
		client.startClient();
	}
	
}

# Documentation

## Table of Contents

- [Documentation](#documentation)
  - [Table of Contents](#table-of-contents)
- [Dependency](#dependency)
- [`public class FileLog implements Serializable`](#public-class-filelog-implements-serializable)
  - [`public FileLog(Path home)`](#public-filelogpath-home)
  - [`public FileLog()`](#public-filelog)
  - [`public void logHomeDirectory()`](#public-void-loghomedirectory)
  - [`private void logDirectory(Path dir)`](#private-void-logdirectorypath-dir)
  - [`public Path getHome()`](#public-path-gethome)
  - [`public FileMetadataLinkedList getList()`](#public-filemetadatalinkedlist-getlist)
  - [`@Override public String toString()`](#override-public-string-tostring)
  - [`@Override public boolean equals(Object o)`](#override-public-boolean-equalsobject-o)
- [`public class FileLogIO`](#public-class-filelogio)
  - [`public static void writeToFile(Path filePath, FileLog log) throws IOException`](#public-static-void-writetofilepath-filepath-filelog-log-throws-ioexception)
  - [`public static FileLog loadFromFile(Path file) throws IOException`](#public-static-filelog-loadfromfilepath-file-throws-ioexception)
- [`public class FileMetadata implements Serializable`](#public-class-filemetadata-implements-serializable)
  - [`public FileMetadata(String fileName, String relativePath, long lastModified, long size)`](#public-filemetadatastring-filename-string-relativepath-long-lastmodified-long-size)
  - [`public FileMetadata(FileMetadata other)`](#public-filemetadatafilemetadata-other)
  - [`public FileMetadata()`](#public-filemetadata)
  - [`public String getFileName()`](#public-string-getfilename)
  - [`public long getLastModified()`](#public-long-getlastmodified)
  - [`public String getRelativePath()`](#public-string-getrelativepath)
  - [`public long getSize()`](#public-long-getsize)
  - [`@Override public String toString()`](#override-public-string-tostring-1)
  - [`@Override public boolean equals(Object o)`](#override-public-boolean-equalsobject-o-1)
- [`public class FileMetadataLinkedList implements Serializable`](#public-class-filemetadatalinkedlist-implements-serializable)
  - [`public FileMetadataLinkedList()`](#public-filemetadatalinkedlist)
  - [`public FileMetadataLinkedList(FileMetadataLinkedList other)`](#public-filemetadatalinkedlistfilemetadatalinkedlist-other)
  - [`public boolean isEmpty()`](#public-boolean-isempty)
  - [`public int getSize()`](#public-int-getsize)
  - [`public void insertAtStart(FileMetadata val)`](#public-void-insertatstartfilemetadata-val)
  - [`public void insertAtEnd(FileMetadata val)`](#public-void-insertatendfilemetadata-val)
  - [`public void insertAtPos(int pos, FileMetadata val)`](#public-void-insertatposint-pos-filemetadata-val)
  - [`public void deleteAtPos(int pos)`](#public-void-deleteatposint-pos)
  - [`public void updateAtPos(int pos, FileMetadata newVal)`](#public-void-updateatposint-pos-filemetadata-newval)
  - [`public FileMetadataNode getStart()`](#public-filemetadatanode-getstart)
  - [`public FileMetadataNode getEnd()`](#public-filemetadatanode-getend)
  - [`@Override public String toString()`](#override-public-string-tostring-2)
  - [`@Override public boolean equals(Object other)`](#override-public-boolean-equalsobject-other)
- [`public class FileMetadataNode implements Serializable`](#public-class-filemetadatanode-implements-serializable)
  - [`public FileMetadataNode()`](#public-filemetadatanode)
  - [`public FileMetadataNode(FileMetadata data)`](#public-filemetadatanodefilemetadata-data)
  - [`public FileMetadataNode(FileMetadata data, FileMetadataNode next)`](#public-filemetadatanodefilemetadata-data-filemetadatanode-next)
  - [`public FileMetadataNode(FileMetadataNode other)`](#public-filemetadatanodefilemetadatanode-other)
  - [`public FileMetadata getData()`](#public-filemetadata-getdata)
  - [`public void setData(FileMetadata data)`](#public-void-setdatafilemetadata-data)
  - [`public FileMetadataNode getNext()`](#public-filemetadatanode-getnext)
  - [`public void setNext(FileMetadataNode next)`](#public-void-setnextfilemetadatanode-next)
  - [`@Override public String toString()`](#override-public-string-tostring-3)
  - [`@Override public boolean equals(Object other)`](#override-public-boolean-equalsobject-other-1)
- [`public class FileSyncClient`](#public-class-filesyncclient)
  - [`public FileSyncClient(String home)`](#public-filesyncclientstring-home)
  - [`public void connectToServer() throws UnknownHostException, IOException`](#public-void-connecttoserver-throws-unknownhostexception-ioexception)
  - [`public void sendHostname() throws IOException`](#public-void-sendhostname-throws-ioexception)
  - [`public void sendHomeFolderName() throws IOException`](#public-void-sendhomefoldername-throws-ioexception)
  - [`public void sendCurrentFileLog() throws IOException`](#public-void-sendcurrentfilelog-throws-ioexception)
  - [`public void sendFile(String relPath) throws FileNotFoundException, IOException`](#public-void-sendfilestring-relpath-throws-filenotfoundexception-ioexception)
  - [`public void cleanup()`](#public-void-cleanup)
  - [`private void actOnCommand(String command)`](#private-void-actoncommandstring-command)
  - [`public void startClient()`](#public-void-startclient)
  - [`public void setVerbose(boolean verbose)`](#public-void-setverboseboolean-verbose)
  - [`private void log(String message)`](#private-void-logstring-message)
- [`public class FileSyncServer`](#public-class-filesyncserver)
  - [`public FileSyncServer(String serverRoot)`](#public-filesyncserverstring-serverroot)
  - [`public void getConnectionToClient()`](#public-void-getconnectiontoclient)
  - [`public String getHostname() throws SocketException, IOException`](#public-string-gethostname-throws-socketexception-ioexception)
  - [`public String getHomeFolderName() throws SocketException, IOException`](#public-string-gethomefoldername-throws-socketexception-ioexception)
  - [`public FileLog getCurrentFileLog() throws SocketException, IOException`](#public-filelog-getcurrentfilelog-throws-socketexception-ioexception)
  - [`public void getFile(String relPath) throws IOException`](#public-void-getfilestring-relpath-throws-ioexception)
  - [`public String[] getFilesToSync() throws IOException`](#public-string-getfilestosync-throws-ioexception)
  - [`public void cleanup()`](#public-void-cleanup-1)
  - [`public void startServer()`](#public-void-startserver)
  - [`public void stopServer()`](#public-void-stopserver)
  - [`public void setVerbose(boolean verbose)`](#public-void-setverboseboolean-verbose-1)
  - [`private void log(String message)`](#private-void-logstring-message-1)

# Dependency
* Jackson JSON Library

# `public class FileLog implements Serializable`

Catalogs files' metadata (recursively) in a home directory

 * **Author:** Brandon Ingli
 * **Version:** 1.0
 * **Invariant:** home directory Path

## `public FileLog(Path home)`

Constructs a FileLog with the given home directory

 * **Parameters:** `home` — home directory
 * **Precondition:** home must refer to a directory

## `public FileLog()`

Default constructor that doesn't set home.

 * **See also:** <code>public FileLog(Path home)</code>

## `public void logHomeDirectory()`

Logs the Home directory's files.

## `private void logDirectory(Path dir)`

Helper Method to recursively log a specified directory's files.

 * **Parameters:** `dir` — Directory to log
 * **Precondition:** dir must refer to a directory.

## `public Path getHome()`

Gets the Path of the home directory

 * **Returns:** Path of the home directory

## `public FileMetadataLinkedList getList()`

Gets a copy of the file list

 * **Returns:** a copy of the FileMetadataLinkedList holding the file listing

## `@Override  public String toString()`

Gets String representation of FileLog

 * **Returns:** String representation of FileLog

## `@Override  public boolean equals(Object o)`

Determines if given object is equal to this one

 * **Returns:** true if this is equal to the given object


# `public class FileLogIO`

Utility class that Reads and Writes FIleLogs to and from Disk

 * **Author:** Brandon Ingli
 * **Version:** 1.0

## `public static void writeToFile(Path filePath, FileLog log) throws IOException`

Writes FileLog to a file with the specified path

 * **Parameters:**
   * `filePath` — Path to where the file is to be stored
   * `log` — FileLog to write
 * **Exceptions:** `IOException` — If an error arises creating or writing to the file.
 * **Postcondition:** file at filePath exists

## `public static FileLog loadFromFile(Path file) throws IOException`

Loads a FileLog from the specified file

 * **Parameters:** `file` — Path to FileLog save file (.json)
 * **Returns:** FileLog loaded from file
 * **Exceptions:** `IOException` — if an error arises opening or parsing the file
 * **Precondition:** file exists



# `public class FileMetadata implements Serializable`

A class to hold metadata about a file.

 * **Author:** Brandon Ingli
 * **Version:** 1.0
 * **Invariant:** Class is ummutable

## `public FileMetadata(String fileName, String relativePath, long lastModified, long size)`

Constructs a new FileMetadata

 * **Parameters:**
   * `fileName` — String representation of file name
   * `relativePath` — String representation of relative path from a home directory
   * `lastModified` — long of last modified time in milliseconds from Epoch
   * `size` — size of file in Bytes
 * **See also:**
   * <code>java.nio.file.Path.relativize(Path)</code>
   * <code>java.nio.file.attribute.FileTime.tomillis()</code>

## `public FileMetadata(FileMetadata other)`

Constructs a copy of other

 * **Parameters:** `other` — FileMetadata to copy

## `public FileMetadata()`

Default constructor that sets all attributes to their zero-values

 * **Warning:** For use by Jackson during Deserialization
 * **See also:** <code>public FileMetadata(String, String, long, long)</code>

## `public String getFileName()`

Gets File Name

 * **Returns:** Path object representing file name

## `public long getLastModified()`

Gets Last Modified Time of File

 * **Returns:** long representing last modified time in millis since Epoch

## `public String getRelativePath()`

Gets String representation of Relative Path

 * **Returns:** String representation of Relative Path

## `public long getSize()`

Gets Size of file

 * **Returns:** Size of File in Bytes

## `@Override  public String toString()`

Gets String representation of this FileMetadata.

 * **Returns:** String representation of this FileMetadata.

## `@Override  public boolean equals(Object o)`

Determines if given object is equal to this one

 * **Returns:** true if this is equal to the given object


# `public class FileMetadataLinkedList implements Serializable`

Implements a Singly Linked List of FileMetadataNodes Based on CustomLinkedList

 * **Author:**
   * Dr. Chetan Jaiswal
   * Brandon Ingli
 * **Version:** 1.0

## `public FileMetadataLinkedList()`

Constructs an empty FileMetadataLinkedList

## `public FileMetadataLinkedList(FileMetadataLinkedList other)`

Constructs a copy of a FileMetadataLinkedList

 * **Parameters:** `other` — FileMetadataLinkedList to copy

## `public boolean isEmpty()`

Function to check if list is empty

 * **Returns:** true: if the list is empty, false: otherwise

## `public int getSize()`

Function to get size of list

 * **Returns:** size of the list (# of nodes)

## `public void insertAtStart(FileMetadata val)`

Function to insert an element at beginning

 * **Parameters:** `val` — value to be inserted at the first node of the list

## `public void insertAtEnd(FileMetadata val)`

Inserts an element at the end of the list

 * **Parameters:** `val` — element to insert at the end of the list.

## `public void insertAtPos(int pos, FileMetadata val)`

Function to insert an element at position

 * **Parameters:**
   * `val` — value to be inserted in the list
   * `pos` — position of the value in the list
* **Preconditions:**
   * pos > 1
   * pos <= size+1

## `public void deleteAtPos(int pos)`

Function to delete an element at position

 * **Parameters:** `pos` — position from which the item needs to be deleted (node position)
 * **Preconditions:**
   * pos >= 1
   * pos <= size

## `public void updateAtPos(int pos, FileMetadata newVal)`

updates the value of a node at the position pos

 * **Parameters:**
   * `pos` — position of the node
   * `newVal` — new value that needs to be updated at the pos
  * **Precondition:** 1 <= pos <= size

## `public FileMetadataNode getStart()`

Gets the start reference of the linkedlist

 * **Returns:** the start reference of the linkedlist

## `public FileMetadataNode getEnd()`

Gets the end reference of the linkedlist

 * **Returns:** the end reference of the linkedlist

## `@Override public String toString()`

Gets a String representation of this FileMetadataLinkedList.

 * **Returns:** String representation fo this FileMetadataLinkedList

## `@Override public boolean equals(Object other)`

Determines if given object is equal to this one

 * **Returns:** true if this is equal to the given object


# `public class FileMetadataNode implements Serializable`

A Node for a FileMetadataLinkedList.

 * **Author:** Brandon Ingli
 * **Version:** 1.0

     <p>

## `public FileMetadataNode()`

Constructs a default Node with data and next as null.

## `public FileMetadataNode(FileMetadata data)`

Constructs a node with the given data and a null next node

 * **Parameters:** `data` — Data to store in this node

## `public FileMetadataNode(FileMetadata data, FileMetadataNode next)`

Constructs a Node with given data pointing to given Node

 * **Parameters:**
   * `data` — FileMetadata reference to store in Node.
   * `next` — Node for this Node to point to.

## `public FileMetadataNode(FileMetadataNode other)`

Copy Constructor for a FileMetadataNode

 * **Parameters:** `other` — FileMetadataNode to copy
 * **Postcondition:** this.next is a copy of other.next

## `public FileMetadata getData()`

Gets FileMetadata reference stored in this Node.

 * **Returns:** FileMetadata stored in this Node.

## `public void setData(FileMetadata data)`

Sets FileMetadata reference stored in this Node.

 * **Parameters:** `data` — Reference to store in this Node.

## `public FileMetadataNode getNext()`

Gets reference to next Node.

 * **Returns:** Reference to next Node in list.

## `public void setNext(FileMetadataNode next)`

Sets reference to next Node.

 * **Parameters:** `next` — Reference to next Node in list.

## `@Override  public String toString()`

Gets String representation of this Node

 * **Returns:** String representation of this Node

## `@Override  public boolean equals(Object other)`

Determines if given object is equal to this one

 * **Returns:** true if this is equal to the given object


# `public class FileSyncClient`

The client component of fileSync to connect to a FileSyncServer and backup.

 * **Author:** Brandon Ingli


## `public FileSyncClient(String home)`

Constructs a new FileSyncClient

 * **Parameters:** `home` — String path to home directory to sync

## `public void connectToServer() throws UnknownHostException, IOException`

Connects to server and fetches in/out streams

 * **Exceptions:**
   * `IOException` — if error creating Socket or I/O Streams
   * `UnknownHostException` — if IP of Server cannot be determinted
  * **Precondition:** FileSyncServer is up at SERVER_LOCATION:SERVER_PORT_NUMBER
  * **Postcondition:** TCP streams established

## `public void sendHostname() throws IOException`

Sends Hostname to Server. If Hostname is empty, a representation of the user's IP address is sent.

 * **Exceptions:** `IOException` — if local host cannot be resolved, or if UTF-8 encoding fails.
 * **Precondition:** connection to Server established

## `public void sendHomeFolderName() throws IOException`

Sends Home Folder Name to Server.

 * **Exceptions:** `IOException` — if local host cannot be resolved, or if UTF-8 encoding fails.
 * **Precondition:** connection to Server established

## `public void sendCurrentFileLog() throws IOException`

Logs the home directory and sends it to the Server

 * **Exceptions:** `IOException` — on error creating or writing to outStream
 * **Precondition:** connection to Server established

## `public void sendFile(String relPath) throws FileNotFoundException, IOException`

Sends file at relPath to the Server

 * **Parameters:** `relPath` — relative path (from home) of file to send
 * **Exceptions:**
   * `FileNotFoundException` — if file at relPath doesn't exist
   * `IOException` — on error reading file or writing to outStream
 * **Precondition:** connection to Server established

## `public void cleanup()`

Cleans up the FileSyncClient in preparation for termination.

* **Postcondition:** Connection to Server is properly ended

## `private void actOnCommand(String command)`

Performs action based on given command from Server

 * **Parameters:** `command` — command from Server
 * **Precondition:** connection to Server established

## `public void startClient()`

Starts the FileSyncClient for given home Directory

## `public void setVerbose(boolean verbose)`

Sets the verbose status of this Client

 * **Parameters:** `verbose` — state to set verbose to

## `private void log(String message)`

Prints a message if the client was set as verbose

 * **Parameters:** `message` — message to print


# `public class FileSyncServer`

The server component of fileSync to accept FileSyncClients for backup.

 * **Author:** Brandon Ingli


## `public FileSyncServer(String serverRoot)`

Constructs a new FileSyncServer

 * **Parameters:** `serverRoot` — String path to the server's root folder

## `public void getConnectionToClient()`

Waits for TCP connection on PORT_NUMBER and gets the Socket and I/O streams of said connection.

## `public String getHostname() throws SocketException, IOException`

Gets the Hostname of the Client to identify it

 * **Returns:** Hostname of Client, with spaces, '/', '\', and ':' replaced with '_'
 * **Exceptions:**
   * `SocketException` — if Socket is disconnected
   * `IOException` — if error reading from inputStream
 * **Precondition:** connection to client has been established

## `public String getHomeFolderName() throws SocketException, IOException`

Gets the Home Folder name of the Client to what it's backing up

 * **Returns:** Home folder name of Client, with spaces, '/', '\', and ':' replaced with '_'
 * **Exceptions:**
   * `SocketException` — if Socket is disconnected
   * `IOException` — if error reading from inputStream
 * **Precondition:** connection to client has been established

## `public FileLog getCurrentFileLog() throws SocketException, IOException`

Gets the current FileLog from the Client

 * **Returns:** current FileLog of the Client
 * **Exceptions:**
   * `SocketException` — if socket is disconnected
   * `IOException` — if error reading from inputStream
 * **Precondition:** connection to client has been established

## `public void getFile(String relPath) throws IOException`

Gets the file at relPath from the Client

 * **Parameters:** `relPath` — relativePath (from home) of the file to retrieve
 * **Exceptions:** `IOException` — on error reading file from stream or creating file on Server
 * **Precondition:** connection to client has been established
 * **Postcondition:** Requisite parent directories are also created

## `public String[] getFilesToSync() throws IOException`

Determines which files to sync from client to server

 * **Returns:** String Array of relative paths for files to sync
 * **Exceptions:** `IOException` — on error getting current file log from client
 * **Precondition:** connection to client has been established

## `public void cleanup()`

Cleans up the FileSyncServer in preparation for another client.

* **Postconditions:**
  * all streams with Client are closed
  * Client is told to shut down
  * all remnants of this Client (Path, socket, and stream references) are forgotten


## `public void startServer()`

Starts the FileSyncServer around the given server root directory

## `public void stopServer()`

Stops the Server

 * **See also:** cleanup()
 * **Precondition:** Best Practice if server is properly cleaned up

## `public void setVerbose(boolean verbose)`

Sets the verbose level of this Server

 * **Parameters:** `verbose` — state to set verbose to

## `private void log(String message)`

Prints a message to the screen if this Server was set to verbose

 * **Parameters:** `message` — message to print
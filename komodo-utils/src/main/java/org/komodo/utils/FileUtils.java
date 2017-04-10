/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */

package org.komodo.utils;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

/**
 * File Utilities
 */
public class FileUtils implements StringConstants {
//
//    public static final char SEPARATOR = '/';
//
    /**
     * The default buffer size for working with files. Defaults to {@value}.
     */
    public static int DEFAULT_BUFFER_SIZE = 8092;
//    public static String TEMP_DIRECTORY;
//
//    public final static String JAVA_IO_TEMP_DIR = "java.io.tmpdir";//$NON-NLS-1$
//
//    private static final String TEMP_FILE = "delete.me"; //$NON-NLS-1$
//    private static final String TEMP_FILE_RENAMED = "delete.me.old"; //$NON-NLS-1$
//
//    public final static char[] SUFFIX_class = ".class".toCharArray(); //$NON-NLS-1$
//    public final static char[] SUFFIX_CLASS = ".CLASS".toCharArray(); //$NON-NLS-1$
//    public final static char[] SUFFIX_java = ".java".toCharArray(); //$NON-NLS-1$
//    public final static char[] SUFFIX_JAVA = ".JAVA".toCharArray(); //$NON-NLS-1$
//    public final static char[] SUFFIX_jar = ".jar".toCharArray(); //$NON-NLS-1$
//    public final static char[] SUFFIX_JAR = ".JAR".toCharArray(); //$NON-NLS-1$
//    public final static char[] SUFFIX_zip = ".zip".toCharArray(); //$NON-NLS-1$
//    public final static char[] SUFFIX_ZIP = ".ZIP".toCharArray(); //$NON-NLS-1$
//
//    static {
//        final String tempDirPath = System.getProperty(JAVA_IO_TEMP_DIR);
//        TEMP_DIRECTORY = (tempDirPath.endsWith(File.separator) ? tempDirPath : tempDirPath + File.separator);
//    }
//
//    /**<p>
//     * Convert the specified file name to end with the specified extension if it doesn't already end with an extension.
//     * </p>
//     * @param name
//     *              The file name.
//     * @param extension
//     *              The extension to append to the file name.
//     * @return The file name with an extension.
//     * @since 4.0
//     */
//    public static String toFileNameWithExtension(final String name,
//                                                 final String extension) {
//        return toFileNameWithExtension(name, extension, false);
//    }
//
//    /**<p>
//     * Convert the specified file name to end with the specified extension if it doesn't already end with an extension.  If force
//     * is true, the specified extension will be appended to the name if the name doesn't end with that particular extension.
//     * </p>
//     * @param name
//     *              The file name.
//     * @param extension
//     *              The extension to append to the file name.
//     * @param force
//     *              Indicates whether to force the specified extension as the extension of the file name.
//     * @return The file name with an extension.
//     * @since 4.0
//     */
//    public static String toFileNameWithExtension(final String name,
//                                                 final String extension,
//                                                 final boolean force) {
//        if (name == null) {
//            throw new IllegalArgumentException(Messages.getString(Messages.FileUtils.The_name_of_the_file_may_not_be_null));
//        }
//        if (extension == null) {
//            throw new IllegalArgumentException(Messages.getString(Messages.FileUtils.The_file_extension_may_not_be_null));
//        }
//        if (name.endsWith(extension)) {
//            return name;
//        }
//        if (!force && name.indexOf(FILE_EXTENSION_SEPARATOR) >= 0) {
//            return name;
//        }
//        final int nameLen = name.length() - 1;
//        final int extLen = extension.length();
//        final boolean nameEndsWithExtChr = (nameLen >= 0 && name.charAt(nameLen) == FILE_EXTENSION_SEPARATOR.charAt(0));
//        final boolean extBeginsWithExtChr = (extLen > 0 && extension.charAt(0) == FILE_EXTENSION_SEPARATOR.charAt(0));
//        if (nameEndsWithExtChr && extBeginsWithExtChr) {
//            return name.substring(0, nameLen) + extension;
//        }
//        if (!nameEndsWithExtChr && !extBeginsWithExtChr) {
//            return name + FILE_EXTENSION_SEPARATOR + extension;
//        }
//        return name + extension;
//    }
//
//    /**
//     * Determine whether the specified name is valid for a file or folder on the current file system.
//     * @param newName the new name to be checked
//     * @return true if the name is null or contains no invalid characters for a folder or file, or false otherwise
//     */
//    public static boolean isFilenameValid(String newName) {
//        return true; //TODO: just catch an exception when the file is accessed or created
//    }
//
//    /**
//     * @param path
//     * @return
//     */
//    public static String getBaseFileNameWithoutExtension(String path) {
//        return StringUtils.getFirstToken(StringUtils.getLastToken(path, "/"), "."); //$NON-NLS-1$ //$NON-NLS-2$
//    }
//
//    /**
//     * Obtains the file extension of the specified <code>File</code>. The extension is considered to be all the
//     * characters after the last occurrence of {@link Constants#FILE_EXTENSION_SEPARATOR_CHAR} in the pathname
//     * of the input.
//     * @param theFile the file whose extension is being requested
//     * @return the extension or <code>null</code> if not found
//     * @since 4.2
//     */
//    public static String getExtension(File theFile) {
//        return getExtension(theFile.getPath());
//    }
//
//    /**
//     * Obtains the file extension of the specified file name. The extension is considered to be all the
//     * characters after the last occurrence of {@link Constants#FILE_EXTENSION_SEPARATOR_CHAR}.
//     * @param theFileName the file whose extension is being requested
//     * @return the extension or <code>null</code> if not found
//     * @since 4.2
//     */
//    public static String getExtension(String theFileName) {
//        String result = null;
//        final int index = theFileName.lastIndexOf(FILE_EXTENSION_SEPARATOR);
//
//        // make sure extension char is found and is not the last char in the path
//        if ((index != -1) && ((index + 1) != theFileName.length())) {
//            result = theFileName.substring(index + 1);
//        }
//
//        return result;
//    }
//
//    /**
//     * Returns true iff str.toLowerCase().endsWith(".jar") || str.toLowerCase().endsWith(".zip")
//     * implementation is not creating extra strings.
//     * @param name 
//     * @return 
//     */
//    public final static boolean isArchiveFileName(String name) {
//        int nameLength = name == null ? 0 : name.length();
//        int suffixLength = SUFFIX_JAR.length;
//        if (nameLength < suffixLength) return false;
//
//        // try to match as JAR file
//        for (int i = 0; i < suffixLength; i++) {
//            char c = name.charAt(nameLength - i - 1);
//            int suffixIndex = suffixLength - i - 1;
//            if (c != SUFFIX_jar[suffixIndex] && c != SUFFIX_JAR[suffixIndex]) {
//
//                // try to match as ZIP file
//                suffixLength = SUFFIX_ZIP.length;
//                if (nameLength < suffixLength) return false;
//                for (int j = 0; j < suffixLength; j++) {
//                    c = name.charAt(nameLength - j - 1);
//                    suffixIndex = suffixLength - j - 1;
//                    if (c != SUFFIX_zip[suffixIndex] && c != SUFFIX_ZIP[suffixIndex]) return false;
//                }
//                return true;
//            }
//        }
//        return true;
//    }

    /**
     * Returns fileArray {@code ArrayList} of {@code File} objects that match a pattern in the specified directory. 
     * @param pathToScan The path to look for the matching files
     * @param startWith The beginning portion of the file name
     * @param endsWith The ending portion of the file name (i.e. ".jar")
     * @return fileArray An ArrayList of 
     * @since 8.5
     */
    public final static ArrayList<File> getFilesForPattern(String pathToScan, String startWith, String endsWith) {
	    String target_file ;  // fileThatYouWantToFilter
	    File folderToScan = new File(pathToScan); 
	
		File[] listOfFiles = folderToScan.listFiles();
		ArrayList<File> list = new ArrayList<File>();
		
		for (File file:listOfFiles) {
	        if (file.isFile()) {
	            target_file = file.getName();
	            if (target_file.startsWith(startWith)
		                 && target_file.endsWith(endsWith)) {
	            	list.add(file);
		        }
		    }
		 }
		
		return list;    
    }
    
    /**
     * Read a file, extract its contents, ensuring the file reader is closed.
     *
     * @param file the file to read
     * 
     * @return contents of file as a {@link String}
     * 
     * @throws FileNotFoundException if file cannot be found
     */
    public static String readSafe(File file) throws FileNotFoundException {
        String result;
        FileReader reader = null;
        try {
            reader = new FileReader(file);
            result = read(reader);
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception e) {
                	 KLog.getLogger().error(e.getMessage());
                }
            }
        }
                
        return result;
    }
    
    /**
     * Read the given {@link Reader} and return its contents
     * as a {@link String}
     * 
     * @param reader the reader
     * 
     * @return contents as a {@link String}
     */
    public static String read(Reader reader) {
        StringWriter writer = new StringWriter();
        BufferedReader bufferedReader = null;
        try {
            bufferedReader = new BufferedReader(reader);
            while (bufferedReader.ready()) {
                String line = bufferedReader.readLine();
                writer.write(line);
                writer.write(StringConstants.LINE_SEPARATOR);
            }
        } catch (IOException e) {
            // TODO:
        } finally {
            if (bufferedReader != null) {
                try {
                    bufferedReader.close();
                } catch (Exception e) {
                	KLog.getLogger().error(e.getMessage());
                }
            }
        }
        return writer.toString();
    }

//
//    /**
//     * Copy a file. Overwrites the destination file if it exists.
//     * @param fromFile 
//     * @param destDirectory 
//     * @param overwrite 
//     * 
//     * @param fromFileName
//     * @param toFileName
//     * @return 
//     * @throws IOException 
//     * @since 4.3
//     */
//    public static File copy(final File fromFile, final File destDirectory, final boolean overwrite) throws IOException {
//        final File toFile = new File(destDirectory, fromFile.getName());
//
//        if (toFile.exists()) if (overwrite) toFile.delete();
//        else {
//            throw new IOException(Messages.getString(Messages.FileUtils.File_already_exists,toFile.getName()));
//        }
//
//        if (!fromFile.exists()) throw new FileNotFoundException(Messages.getString(Messages.FileUtils.File_does_not_exist_1,fromFile.getName()));
//
//        FileInputStream fis = null;
//        try {
//            fis = new FileInputStream(fromFile);
//            write(fis, toFile);
//        } finally {
//            if (fis != null) fis.close();
//        }
//        return toFile;
//    }
//
//    /**
//     * @param source
//     * @param destination
//     * @throws Exception
//     */
//    public static void copy(final InputStream source, final OutputStream destination) throws Exception {
//        final byte[] buf = new byte[DEFAULT_BUFFER_SIZE];
//        for (int len = source.read(buf); len >= 0; len = source.read(buf))
//            destination.write(buf, 0, len);
//    }
//
//    /**
//     * Copy a file. Overwrites the destination file if it exists.
//     * 
//     * @param fromFileName
//     * @param toFileName
//     * @throws IOException 
//     * @since 4.3
//     */
//    public static void copy(final String fromFileName, final String toFileName) throws IOException {
//        copy(fromFileName, toFileName, true);
//    }
//
//    /**
//     * Copy a file
//     * 
//     * @param fromFileName
//     * @param toFileName
//     * @param overwrite whether to overwrite the destination file if it exists.
//     * @throws IOException
//     * @since 4.3
//     */
//    public static void copy(final String fromFileName,
//                            final String toFileName,
//                            final boolean overwrite) throws IOException {
//        final File toFile = new File(toFileName);
//
//        if (toFile.exists()) if (overwrite) toFile.delete();
//        else {
//            throw new IOException(Messages.getString(Messages.FileUtils.File_already_exists,toFileName));
//        }
//
//        final File fromFile = new File(fromFileName);
//        if (!fromFile.exists()) throw new FileNotFoundException(Messages.getString(Messages.FileUtils.File_does_not_exist_1,fromFileName));
//
//        FileInputStream fis = null;
//        try {
//            fis = new FileInputStream(fromFile);
//            write(fis, toFileName);
//        } finally {
//            if (fis != null) fis.close();
//        }
//    }
//
//    /**
//     * Copy recursively the <code>sourceDirectory</code> and all its contents to the <code>targetDirectory</code>. If
//     * <code>targetDirectory</code> does not exist, it will be created.
//     * 
//     * @param sourceDirectory The source directory to copy
//     * @param targetDirectory The target directory to copy to
//     * @param filter files to ignore
//     * @throws Exception If the source directory does not exist.
//     * @since 4.3
//     */
//    public static void copyDirectoriesRecursively(final File sourceDirectory,
//                                                  final File targetDirectory,
//                                                  final FilenameFilter filter) throws Exception {
//        copyRecursively(sourceDirectory, targetDirectory, filter, true);
//    }
//
//    /**
//     * Copy file from originating directory to the destination directory.
//     * 
//     * @param orginDirectory
//     * @param destDirectory
//     * @param fileName
//     * @throws Exception
//     * @since 4.4
//     */
//    public static void copyFile(final String orginDirectory,
//                                final String destDirectory,
//                                final String fileName) throws Exception {
//
//        copyFile(orginDirectory, fileName, destDirectory, fileName);
//    }
//
//    /**
//     * Copy file from originating directory to the destination directory.
//     * 
//     * @param orginDirectory
//     * @param orginFileName
//     * @param destDirectory
//     * @param destFileName
//     * @throws Exception
//     * @since 4.4
//     */
//    public static void copyFile(final String orginDirectory,
//                                final String orginFileName,
//                                final String destDirectory,
//                                final String destFileName) throws Exception {
//
//        FileUtils.copy(orginDirectory + File.separator + orginFileName, destDirectory + File.separator + destFileName);
//    }
//
//    /**
//     * Copy recursively from the <code>sourceDirectory</code> all its contents to the <code>targetDirectory</code>. if
//     * <code>includeSourceRoot<code> == <code>true</code>, copy <code>sourceDirectory</code> itself, else only copy
//     * <code>sourceDirectory</code>'s contents. If <code>targetDirectory</code> does not exist, it will be created.
//     * 
//     * @param sourceDirectory
//     * @param targetDirectory
//     * @param filter - files which should not be copied
//     * @param includeSourceRoot
//     * @throws FileNotFoundException
//     * @throws Exception
//     * @since 4.3
//     */
//    public static void copyRecursively(final File sourceDirectory,
//                                       final File targetDirectory,
//                                       final FilenameFilter filter,
//                                       final boolean includeSourceRoot) throws FileNotFoundException, Exception {
//        if (!sourceDirectory.exists()) throw new FileNotFoundException(Messages.getString(Messages.FileUtils.File_does_not_exist_1, sourceDirectory));
//
//        if (!sourceDirectory.isDirectory()) throw new FileNotFoundException(Messages.getString(Messages.FileUtils.Not_a_directory,sourceDirectory));
//
//        File targetDir = new File(targetDirectory.getAbsolutePath() + File.separatorChar + sourceDirectory.getName());
//        if (includeSourceRoot) // copy source directory
//        targetDir.mkdir();
//        else // copy only source directory contents
//        targetDir = new File(targetDirectory.getAbsolutePath() + File.separatorChar);
//
//        File[] sourceFiles = null;
//        if (filter != null) sourceFiles = sourceDirectory.listFiles(filter);
//        else sourceFiles = sourceDirectory.listFiles();
//
//        for (final File srcFile : sourceFiles)
//            if (srcFile.isDirectory()) {
//                final File childTargetDir = new File(targetDir.getAbsolutePath());
//                copyRecursively(srcFile, childTargetDir, filter, true);
//            } else copy(srcFile.getAbsolutePath(), targetDir.getAbsolutePath() + File.separatorChar + srcFile.getName());
//    }
//
//    /**
//     * Compute checksum for the given file.
//     * 
//     * @param f The file for which checksum needs to be computed
//     * @return The checksum
//     * @throws Exception 
//     * @since 4.3
//     */
//    public static long getCheckSum(final File f) throws Exception {
//        ArgCheck.isNotNull(f);
//        FileInputStream is = null;
//        try {
//            is = new FileInputStream(f);
//            return ChecksumUtils.computeChecksum(is).getValue();
//        } finally {
//            if (is != null) try {
//                is.close();
//            } catch (final IOException err1) {
//            }
//        }
//    }
//
//    /**
//     * @param filename 
//     * @return
//     */
//    public static String getFilenameWithoutExtension(final String filename) {
//        if (filename == null || filename.length() == 0) return filename;
//        final int extensionIndex = filename.lastIndexOf('.');
//        if (extensionIndex == -1) return filename; // not found
//        if (extensionIndex == 0) return ""; //$NON-NLS-1$
//        return filename.substring(0, extensionIndex);
//    }
//
//    /**
//     * @param theFileName
//     * @return
//     */
//    public static String normalizeFileName(final String theFileName) {
//        if (theFileName == null) return null;
//        if (theFileName.length() == 0) return theFileName;
//
//        try {
//            return URLDecoder.decode(theFileName, "UTF-8"); //$NON-NLS-1$
//        } catch (final UnsupportedEncodingException e) {
//            return theFileName;
//        }
//    }

    /**
     * @param directory the directory whose children should be removed
     */
    public static void removeChildrenRecursively(final File directory) {
        final File[] files = directory.listFiles();
        if (files != null) for (final File file2 : files) {
            final File file = file2;
            if (file.isDirectory()) removeDirectoryAndChildren(file);
            else if (!file.delete()) file.deleteOnExit();
        }
    }

    /**
     * @param directory the directory to delete
     */
    public static void removeDirectoryAndChildren(final File directory) {
        removeChildrenRecursively(directory);
        if (!directory.delete()) directory.deleteOnExit();
    }
//
//    /**
//     * Test whether it's possible to read and write files in the specified directory.
//     * 
//     * @param dirPath Name of the directory to test
//     * @throws KomodoCoreRuntimeException
//     * @since 4.3
//     */
//    public static void testDirectoryPermissions(final String dirPath) throws KomodoCoreRuntimeException {
//
//        // try to create a file
//        final File tmpFile = new File(dirPath + File.separatorChar + TEMP_FILE);
//        boolean success = false;
//        try {
//            success = tmpFile.createNewFile();
//        } catch (final IOException e) {
//        }
//        if (!success) {
//            throw new KomodoCoreRuntimeException(Messages.getString(Messages.FileUtils.Unable_to_create_file_in, dirPath));
//        }
//
//        // test if file can be written to
//        if (!tmpFile.canWrite()) {
//            throw new KomodoCoreRuntimeException(Messages.getString(Messages.FileUtils.Unable_to_write_file_in, dirPath));
//        }
//
//        // test if file can be read
//        if (!tmpFile.canRead()) {
//            throw new KomodoCoreRuntimeException(Messages.getString(Messages.FileUtils.Unable_to_read_file_in, dirPath));
//        }
//
//        // test if file can be renamed
//        final File newFile = new File(dirPath + File.separatorChar + TEMP_FILE_RENAMED);
//        success = false;
//        try {
//            success = tmpFile.renameTo(newFile);
//        } catch (final Exception e) {
//        }
//        if (!success) {
//            throw new KomodoCoreRuntimeException(Messages.getString(Messages.FileUtils.Unable_to_rename_file_in, dirPath));
//        }
//
//        // test if file can be deleted
//        success = false;
//        try {
//            success = newFile.delete();
//        } catch (final Exception e) {
//        }
//        if (!success) {
//            throw new KomodoCoreRuntimeException(Messages.getString(Messages.FileUtils.Unable_to_delete_file_in, dirPath));
//        }
//    }

    /**
     * Write an InputStream to a byte array.
     * @param is
     * @throws IOException
     */
    public static byte[] write(final InputStream is) throws IOException {
        ByteArrayOutputStream bos = null;
        try {
            bos = new ByteArrayOutputStream();
            final byte[] buff = new byte[DEFAULT_BUFFER_SIZE];
            int bytesRead;

            // Simple read/write loop.
            while (-1 != (bytesRead = is.read(buff, 0, buff.length)))
                bos.write(buff, 0, bytesRead);

            bos.flush();

            return bos.toByteArray();
        } finally {
            if (bos != null)
                bos.close();
        }
    }

    /**
     * Write an InputStream to a file.
     * @param is 
     * @param f 
     * @throws IOException 
     */
    public static void write(final InputStream is, final File f) throws IOException {
        write(is, f, DEFAULT_BUFFER_SIZE);
    }

    /**
     * Write an InputStream to a file.
     * @param is 
     * @param f 
     * @param bufferSize 
     * @throws IOException 
     */
    public static void write(final InputStream is, final File f, final int bufferSize) throws IOException {
        f.delete();
        final File parentDir = f.getParentFile();
        if (parentDir != null) parentDir.mkdirs();

        FileOutputStream fio = null;
        BufferedOutputStream bos = null;
        try {
            fio = new FileOutputStream(f);
            bos = new BufferedOutputStream(fio);
            if (bufferSize > 0) {
                final byte[] buff = new byte[bufferSize];
                int bytesRead;

                // Simple read/write loop.
                while (-1 != (bytesRead = is.read(buff, 0, buff.length)))
                    bos.write(buff, 0, bytesRead);
            }
            bos.flush();
        } finally {
            if (bos != null) bos.close();
            if (fio != null) fio.close();
        }
    }

    /**
     * Write an InputStream to a file.
     * @param is 
     * @param fileName 
     * @throws IOException 
     */
    public static void write(final InputStream is, final String fileName) throws IOException {
        final File f = new File(fileName);
        write(is, f);
    }

    /**
     *  Write a byte array to a file.
     * @param data 
     * @param fileName 
     * @throws IOException 
     */
    public static void write(byte[] data, String fileName) throws IOException {
        ByteArrayInputStream bais = null;
        InputStream is = null;
        try {
            bais = new ByteArrayInputStream(data);
            is = new BufferedInputStream(bais);

            write(is, fileName);
        } finally {
            if (is != null) {
                is.close();
            }
            if (bais != null) {
                bais.close();
            }
        }
    }
    
    /**
     *  Write a byte array to a file.
     * @param data 
     * @param file 
     * @throws IOException 
     */
     public static void write(byte[] data, File file) throws IOException {
         ByteArrayInputStream bais = null;
         InputStream is = null;
         try {
             bais = new ByteArrayInputStream(data);
             is = new BufferedInputStream(bais);
    
             write(is, file);  
         } finally {
             if (is != null) {
                 is.close();
             }
             if (bais != null) {
                 bais.close();
             }
         }
     }

     /**
      * @param inStream
      * @return a string representation of the content of the given stream
      * @throws IOException
      */
     public static String streamToString(InputStream inStream) throws IOException {
         ArgCheck.isNotNull(inStream, "input stream");

         BufferedReader reader = new BufferedReader(new InputStreamReader(inStream));
         StringBuilder builder = new StringBuilder();
         String line;
         while ((line = reader.readLine()) != null) {
             builder.append(line);
             builder.append(NEW_LINE);
         }

         return builder.toString().trim();
     }

     /**
      * @param inStream stream to convert
      * @return <code>byte[]</code>
      * @throws IOException if error occurs
      */
    public static byte[] streamToByteArray(InputStream inStream) throws IOException {
        ArgCheck.isNotNull(inStream, "input stream");

        ByteArrayOutputStream buffer = new ByteArrayOutputStream();

        try {
            int nRead;
            byte[] data = new byte[1024];
            while ((nRead = inStream.read(data, 0, data.length)) != -1) {
                buffer.write(data, 0, nRead);
            }

            buffer.flush();
            return buffer.toByteArray();
        } finally {
            buffer.close();

            if (inStream != null)
                inStream.close();
        }
    }

     public static String tempDirectory() {
         // If deployed to a jboss server then try and use its tmp directory
         String property = System.getProperty(JBOSS_SERVER_TMP_DIR);
         if (property != null)
             return property;

         // Default to using java tmp dir
         return System.getProperty(JAVA_IO_TMPDIR);
     }

     /**
      * Parse the given xml string and returns a {@link Document}
      * @param xml
      * @return the document consistent with the given xml string
      * @throws KException
      */
     public static Document createDocument(String xml) throws KException {
         String xmlText = xml.replaceAll(NEW_LINE, SPACE);
         xmlText = xmlText.replaceAll(">[\\s]+<", CLOSE_ANGLE_BRACKET + OPEN_ANGLE_BRACKET); //$NON-NLS-1$
         xmlText = xmlText.replaceAll("[\\s]+", SPACE); //$NON-NLS-1$
         xmlText = xmlText.replaceAll("CDATA\\[[\\s]+", "CDATA["); //$NON-NLS-1$ //$NON-NLS-2$
         xmlText = xmlText.replaceAll("; \\]\\]", ";]]"); //$NON-NLS-1$ //$NON-NLS-2$

         final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
         dbf.setIgnoringElementContentWhitespace(true);
         dbf.setIgnoringComments(true);

         try {
             final DocumentBuilder db = dbf.newDocumentBuilder();
             final Document doc = db.parse(new InputSource(new StringReader(xmlText)));
             doc.setXmlStandalone(true);
             doc.normalizeDocument();

             return doc;
         } catch (final Exception e) {
             throw new KException(e);
         }
     }

    /**
     * Extract a zip file to the given destination directory.
     *
     * @param fileStream a file stream to a zip file. Cannot be <code>null</code>.
     *                  Will be closed on completion.
     * @param destDirectory the directory in which the zip file should be extracted. Cannot be <code>null</code>
     *
     * @throws Exception if an error occurs
     *
     * Note: This function uses a {@link ZipFile} with a temp file to extract
     *            the {@link InputStream}. This is necessary since {@link ZipInputStream}
     *            cannot be relied upon to return all entries hence fails some of the time to
     *            extract all files.
     */
    public static void zipExtract(InputStream fileStream, File destDirectory) throws Exception {
        ArgCheck.isNotNull(fileStream, "file stream");
        ArgCheck.isNotNull(destDirectory, "destination directory");
        ArgCheck.isTrue(destDirectory.isDirectory(), "destination is not a directory");

        File tmpFile = null;
        ZipFile zipFile = null;
        try {
            String prefix = destDirectory.getName();

            // make sure prefix is at least 3 chars
            while ( prefix.length() < 3 ) {
                prefix += '_';
            }

            tmpFile = File.createTempFile(prefix, ZIP_SUFFIX);
            write(fileStream, tmpFile);

            zipFile = new ZipFile(tmpFile);
            Enumeration<? extends ZipEntry> entries = zipFile.entries();
            if (!entries.hasMoreElements())
                return;

            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();
                InputStream zipStream = null;

                try {
                    String name = entry.getName();

                    final byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];

                    zipStream = zipFile.getInputStream(entry);
                    File newFile = new File( destDirectory, name );

                    //
                    // creates all non existent directories
                    //
                    if (entry.isDirectory()) {
                        Files.createDirectories(newFile.toPath());
                        continue;
                    } else {
                        Files.createDirectories(newFile.getParentFile().toPath());
                    }

                    FileOutputStream fos = new FileOutputStream(newFile);

                    int len;
                    while ((len = zipStream.read(buffer)) > 0) {
                        fos.write(buffer, 0, len);
                    }

                    fos.close();

                } finally {
                    if (zipStream != null)
                        zipStream.close();
                }
            }
        } finally {
            if (zipFile != null)
                zipFile.close();

            if (tmpFile != null)
                tmpFile.delete();
        }
    }

    private static void zipAddDirectory(String basePath, ZipOutputStream zos, File srcDirectory) throws Exception {
        File[] files = srcDirectory.listFiles();

        if (files.length == 0)
            return;

        for (File file : files) {
            if (file.isDirectory()) {
                String path = basePath + file.getName() + FORWARD_SLASH;
                zos.putNextEntry(new ZipEntry(path));
                zipAddDirectory(path, zos, file);
                zos.closeEntry();
                continue;
            }

            FileInputStream fin = null;
            try {
                byte[] buffer = new byte[4096];
                fin = new FileInputStream(file);
                zos.putNextEntry(new ZipEntry(basePath + file.getName()));

                int length;
                while ((length = fin.read(buffer)) > 0) {
                    zos.write(buffer, 0, length);
                }

                zos.closeEntry();
                fin.close();

            } finally {
                if (fin != null)
                    fin.close();
            }
        }
    }

    /**
     * Creates a zip file from the contents of the given directory
     *
     * @param srcDirectory the directory to zip-up
     * @param zipFile the destination zip file
     * @return the file handle of the new zip file
     *
     * @throws Exception if error occurs
     */
    public static File zipFromDirectory(File srcDirectory, File zipFile) throws Exception {
        ArgCheck.isNotNull(srcDirectory, "srcDirectory");
        ArgCheck.isTrue(srcDirectory.isDirectory(), "source is not a directory");
        ArgCheck.isNotNull(zipFile, "zipFile");

        FileOutputStream fos = null;
        ZipOutputStream zos = null;
        try {
            if (zipFile.exists())
                zipFile.delete();

            fos = new FileOutputStream(zipFile);
            zos = new ZipOutputStream(fos);

            zipAddDirectory(EMPTY_STRING, zos, srcDirectory);

            return zipFile;
        } finally {
            if (zos != null)
                zos.close();

            if (fos != null)
                fos.close();
        }
    }

    private FileUtils() {
    }
}

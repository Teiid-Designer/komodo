package org.komodo.shell.commands.core;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.List;
import java.util.Properties;

import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Exports the referenced node to the specified file name and location
 * @author blafond
 *
 */
public class ExportCommand extends BuiltInShellCommand implements StringConstants {
    private static final String EXPORT = "export"; //$NON-NLS-1$
	private static final String XML_EXT = "xml"; //$NON-NLS-1$
	
    /**
     * Constructor.
     * @param wsStatus the workspace status
     */
    public ExportCommand(WorkspaceStatus wsStatus) {
        super(EXPORT, wsStatus);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        String objPathArg = requiredArgument(0, Messages.getString("ExportCommand.InvalidArgMsg_ObjectName")); //$NON-NLS-1$
        String filePathArg = requiredArgument(1, Messages.getString("ExportCommand.InvalidArgMsg_OutputFileName")); //$NON-NLS-1$

        if(!validateObjectPath(objPathArg)) {
        	return false;
        }
        if(!validateFileName(filePathArg)) {
        	return false;
        }
        
        try {
        	export(objPathArg, filePathArg);
        	
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("ExportCommand.ObjectExported", objPathArg, filePathArg)); //$NON-NLS-1$
            if (getWorkspaceStatus().getRecordingStatus())
                recordCommand(getArguments());
        } catch (Exception e) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("ExportCommand.Failure", objPathArg)); //$NON-NLS-1$
            print(CompletionConstants.MESSAGE_INDENT, TAB + e.getMessage());
            return false;
        }
        return true;
    }
    
    /**
     * Validate the supplied context path
     * @param objPath the objPath
     * @return 'true' if valid, 'false' if not.
     */
    private boolean validateObjectPath(String objPath) {
        return true;
    }
    
    /**
     * Validate the supplied fileName
     * @param fileName the file name
     * @return 'true' if valid, 'false' if not.
     */
    private boolean validateFileName(String fileName) {
        return true;
    }
    
    private void export(String objPath, String fileNameAndLocation) throws Exception {
    	WorkspaceStatus wsStatus = getWorkspaceStatus();
        WorkspaceManager wkspManager = wsStatus.getCurrentContext().getWorkspaceManager();
        UnitOfWork transaction = wsStatus.getTransaction();

        // Get the context for export
        WorkspaceContext contextToExport = ContextUtils.getContextForPath(wsStatus, objPath);
        KomodoObject objToExport = contextToExport.getKomodoObj();
         
        if( objToExport == null ) {
        	throw new Exception(Messages.getString("ExportCommand.cannotExport_objectDoesNotExist", objPath)); //$NON-NLS-1$
        }
        
        // Check for file location and name
        File theFile = new File(fileNameAndLocation);
        if( theFile.exists()) {
        	throw new Exception(Messages.getString("ExportCommand.cannotExport_fileAlreadyExists", fileNameAndLocation)); //$NON-NLS-1$
        }
        
        // Check object type
        if( objToExport.getTypeIdentifier(transaction).equals(KomodoType.VDB)) {
        	Vdb vdb = wkspManager.resolve(transaction, objToExport, Vdb.class);
        	if( vdb == null ) {
        		throw new Exception(" Could not resolve the VDB"); //$NON-NLS-1$
        	}
			Properties props = new Properties();
			props.put( ExportConstants.USE_TABS_PROP_KEY, true);

			String ddlXmlString = vdb.export(transaction, props);
			if (ddlXmlString == null || ddlXmlString .isEmpty()) {
				throw new Exception(Messages.getString("ExportCommand.cannotExport_problemWithVdb")); //$NON-NLS-1$
			}
			
			handleExport(ddlXmlString, fileNameAndLocation);
        	
        } else if( objToExport.getTypeIdentifier(transaction).equals(KomodoType.MODEL)) {
        	 // IF Model, then export as xyz.ddl file
        	throw new Exception(" EXPORT MODEL not yet implemented"); //$NON-NLS-1$
        }
       

    }
    
    /**
     * Export the current string content of the sql display to a user-selected file
     */
    private File handleExport(String contents, String fileName) {

    	String fileExtension = XML_EXT;
    	String fileNameString = fileName;
    			
        // If there is no file extension, add .xml
        if (fileNameString.indexOf('.') == -1 && fileExtension != null) {
        	fileNameString = fileNameString + "." + fileExtension; //$NON-NLS-1$
        }

        FileWriter fileWriter = null;
        BufferedWriter outputBufferWriter = null;
        PrintWriter printWriter = null;
        try {
            fileWriter = new FileWriter(fileNameString);
            outputBufferWriter = new BufferedWriter(fileWriter);
            printWriter = new PrintWriter(outputBufferWriter);
            printWriter.write(contents);
        } catch (Exception e) {
            // TODO: log...
        }

        finally {
            // Clean up writers & buffers
            if (printWriter != null) {
                printWriter.close();
            }

            try {
                if (outputBufferWriter != null) {
                    outputBufferWriter.close();
                }
            } catch (java.io.IOException e) {
            	// do nothing
            }

            try {
                if (fileWriter != null) {
                    fileWriter.close();
                }
            } catch (java.io.IOException e) {
            	// do nothing
            }
        }
        return new File(fileNameString);
    }
    
    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {
    	if (getArguments().isEmpty()) {
    		// The arg is expected to be a path
    		updateTabCompleteCandidatesForPath(candidates, getWorkspaceStatus().getCurrentContext(), true, lastArgument);

    		// Do not put space after it - may want to append more to the path
    		return CompletionConstants.NO_APPEND_SEPARATOR;
    	} else if (getArguments().size()==1) {
    		// This arg is required filePath
    		if(lastArgument==null) {
    			candidates.add("<filePath>"); //$NON-NLS-1$
    		}
    		return 0;
    	}
    	return -1;
    }
}

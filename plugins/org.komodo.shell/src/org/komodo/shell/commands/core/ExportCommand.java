package org.komodo.shell.commands.core;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;

/**
 * Exports the referenced node to the specified file name and location
 * @author blafond
 *
 */
public class ExportCommand extends BuiltInShellCommand implements StringConstants {
	private static final String XML_EXT = "xml"; //$NON-NLS-1$
	
    /**
     * Constructor.
     * @param name the command name
     * @param wsStatus the workspace status
     */
    public ExportCommand(String name, WorkspaceStatus wsStatus) {
        super(name, wsStatus);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        String objNameArg = requiredArgument(0, Messages.getString("ExportCommand.InvalidArgMsg_ObjectName")); //$NON-NLS-1$
        String fileNameAndLocation = requiredArgument(1, Messages.getString("ExportCommand.InvalidArgMsg_OutputFileName")); //$NON-NLS-1$

        try {
        	export(objNameArg, fileNameAndLocation);
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("ExportCommand.ObjectExported", objNameArg, fileNameAndLocation)); //$NON-NLS-1$
            if (getWorkspaceStatus().getRecordingStatus())
                recordCommand(getArguments());
        } catch (Exception e) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("ExportCommand.Failure", objNameArg)); //$NON-NLS-1$
            print(CompletionConstants.MESSAGE_INDENT, TAB + e.getMessage());
            return false;
        }
        return true;
    }

    private void export(String objName, String fileNameAndLocation) throws Exception {
        WorkspaceStatus wsStatus = getWorkspaceStatus();

        WorkspaceContext currentContext = wsStatus.getCurrentContext();
        Repository repository = wsStatus.getCurrentContext().getRepository();
        WorkspaceManager wkspManager = WorkspaceManager.getInstance(repository);
        KomodoObject parent = currentContext.getKomodoObj();
        
        KomodoObject child = parent.getChild(null, objName);
        if( child == null ) {
        	throw new Exception(Messages.getString("ExportCommand.cannotExport_objectDoesNotExist", objName)); //$NON-NLS-1$
        }
        
        // Check for file location and name
        File theFile = new File(fileNameAndLocation);
        if( theFile.exists()) {
        	throw new Exception(Messages.getString("ExportCommand.cannotExport_fileAlreadyExists", fileNameAndLocation)); //$NON-NLS-1$
        }
        // Check object type
       
        
        if( child.getTypeIdentifier(null).equals(KomodoType.VDB)) {
        	Vdb vdb = wkspManager.resolve(null, child, Vdb.class);
        	if( vdb == null ) {
        		throw new Exception(" EXPORT VDB not yet implemented"); //$NON-NLS-1$
        	}
			Properties props = new Properties();
			props.put( ExportConstants.USE_TABS_PROP_KEY, true);

			String ddlXmlString = vdb.export(null, props);
			if (ddlXmlString == null || ddlXmlString .isEmpty()) {
				throw new Exception(Messages.getString("ExportCommand.cannotExport_problemWithVdb")); //$NON-NLS-1$
			}
			
			handleExport(ddlXmlString, fileNameAndLocation);
			
        	
        } else if( child.getTypeIdentifier(null).equals(KomodoType.MODEL)) {
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
			// List of potential completions
			List<String> potentialsList = new ArrayList<String>();
			List<WorkspaceContext> children = getWorkspaceStatus().getCurrentContext().getChildren();
			for(WorkspaceContext wsContext : children) {
				potentialsList.add(wsContext.getName());
			}
    		// --------------------------------------------------------------
    		// No arg - offer children relative to current context.
    		// --------------------------------------------------------------
    		if(lastArgument==null) {
    			candidates.addAll(potentialsList);
    		// --------------------------------------------------------------
    		// One arg - determine the completion options for it.
    		// --------------------------------------------------------------
    		} else {
    			for (String item : potentialsList) {
    				if (item.toUpperCase().startsWith(lastArgument.toUpperCase())) {
    					candidates.add(item);
    				}
    			}
    		}
            return 0;
        }
        return -1;
    }
}

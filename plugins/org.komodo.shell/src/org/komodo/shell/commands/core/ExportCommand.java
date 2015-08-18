package org.komodo.shell.commands.core;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Schema;
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
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Exports the referenced node to the specified file name and location
 * @author blafond
 *
 */
public class ExportCommand extends BuiltInShellCommand implements StringConstants {

    /**
     * The command name.
     */
    public static final String NAME = "export"; //$NON-NLS-1$

    private static final String SUBCMD_DDL = "ddl"; //$NON-NLS-1$
    private static final String SUBCMD_VDB = "vdb"; //$NON-NLS-1$
    private static final List<String> SUBCMDS = Arrays.asList(SUBCMD_VDB, SUBCMD_DDL);
    
    /**
     * @param wsStatus
     *        the workspace status
     */
    public ExportCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        String subcmdArg = requiredArgument(0, Messages.getString(Messages.ExportCommand.InvalidArgMsgSubCommand));

        String objPathArg = requiredArgument(1, Messages.getString(Messages.ExportCommand.InvalidArgMsgObjectName)); 
        String filePathArg = requiredArgument(2, Messages.getString(Messages.ExportCommand.InvalidArgMsgOutputFileName));

        // --------------------------------------------
        // Validate the supplied paths
        // --------------------------------------------
        if(!validateObjectPath(objPathArg)) {
        	return false;
        }
        if(!validateFileName(filePathArg)) {
        	return false;
        }
        
        // Get object being exported
        WorkspaceContext contextToExport = ContextUtils.getContextForPath(getWorkspaceStatus(), objPathArg);
        KomodoObject objToExport = contextToExport.getKomodoObj();
        if(objToExport==null ) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ExportCommand.CannotExportObjectDoesNotExist, objPathArg));
            return false;
        }
        
        if (SUBCMD_VDB.equalsIgnoreCase(subcmdArg)) {
            // --------------------------------------------
            // Validate the supplied object is a VDB
            // --------------------------------------------
            KomodoType typeIdentifier = objToExport.getTypeIdentifier(getWorkspaceStatus().getTransaction());
            if( !typeIdentifier.equals(KomodoType.VDB)) {
                print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ExportCommand.ObjectNotAVdb, objPathArg));
                return false;
            }

        } else if (SUBCMD_DDL.equalsIgnoreCase(subcmdArg)) {
            // --------------------------------------------
            // Make sure the object is exportable
            // --------------------------------------------
            if( !(objToExport instanceof Exportable) ) {
                print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ExportCommand.CannotExportObjectNotExportable, objPathArg));
                return false;
            }
        }
        
        try {
        	export(objPathArg, filePathArg);
        } catch (Exception e) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ExportCommand.Failure, objPathArg));
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
        if (contextToExport == null)
        	throw new Exception(Messages.getString(Messages.ExportCommand.CannotExportObjectDoesNotExist, objPath)); 

        KomodoObject objToExport = contextToExport.getKomodoObj();

        if( objToExport == null ) {
        	throw new Exception(Messages.getString(Messages.ExportCommand.CannotExportObjectDoesNotExist, objPath)); 
        }

        // Check for file location and name
        File theFile = new File(fileNameAndLocation);
        if( theFile.exists()) {
        	throw new Exception(Messages.getString(Messages.ExportCommand.CannotExportFileAlreadyExists, fileNameAndLocation)); 
        }

        // Check object type
        String output = null;
        String fileExtension = null;

        KomodoType typeIdentifier = objToExport.getTypeIdentifier(transaction);
        Properties properties = new Properties();
        properties.put( ExportConstants.USE_TABS_PROP_KEY, true);

        if( typeIdentifier.equals(KomodoType.VDB)) {
            Vdb vdb = wkspManager.resolve(transaction, objToExport, Vdb.class);
            if( vdb == null ) {
                throw new Exception(Messages.getString(Messages.ExportCommand.CannotExportProblemWithVdb));
            }

            output = vdb.export(transaction, properties);
            fileExtension = XML;
        } else if (typeIdentifier.equals(KomodoType.MODEL)) {
            Model model = wkspManager.resolve(transaction, objToExport, Model.class);
            if( model == null ) {
                throw new Exception(Messages.getString(Messages.ExportCommand.CannotExportProblemWithModel));
            }

            output = model.export(transaction, properties);
            fileExtension = DDL;
        } else if (typeIdentifier.equals(KomodoType.SCHEMA)) {
            Schema schema = wkspManager.resolve(transaction, objToExport, Schema.class);
            if( schema == null )
                throw new Exception(Messages.getString(Messages.ExportCommand.CannotExportProblemWithSchema));

            output = schema.export(transaction, properties);
            fileExtension = DDL;
        } else {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ExportCommand.ExportOfTypeNotSupported, typeIdentifier.getType()));
            return;
        }

        if ( output == null ||  output.isEmpty()) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ExportCommand.NoContentExported, objPath));
            return;
        }

        handleExport( output, fileNameAndLocation, fileExtension);
        print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ExportCommand.ObjectExported, objPath, fileNameAndLocation));
    }

    /**
     * Export the current string content of the sql display to a user-selected file
     */
    private void handleExport(String contents, String fileName, String fileExtension) throws Exception {

        fileExtension = fileExtension == null ? XML : fileExtension;
        String fileNameString = fileName;

        // If there is no file extension, add .xml
        if (fileNameString.indexOf(DOT) == -1 && fileExtension != null) {
            fileNameString = fileNameString + DOT + fileExtension;
        }

        try ( final FileWriter fileWriter = new FileWriter( fileNameString );
              final BufferedWriter outputBufferWriter = new BufferedWriter( fileWriter );
              final PrintWriter printWriter = new PrintWriter( outputBufferWriter ) ) {
            printWriter.write( contents );
        }
    }

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {
    	if (getArguments().isEmpty()) {
    		// SubCommand completion options
    		if(lastArgument==null) {
    			candidates.addAll(SUBCMDS);
    		} else {
    			for (String subCmd : SUBCMDS) {
    				if (subCmd.toUpperCase().startsWith(lastArgument.toUpperCase())) {
    					candidates.add(subCmd);
    				}
    			}
    		}
    		return 0;
    	} else if (getArguments().size()==1) {
    		// The arg is expected to be a path
    		updateTabCompleteCandidatesForPath(candidates, getWorkspaceStatus().getCurrentContext(), true, lastArgument);

    		// Do not put space after it - may want to append more to the path
    		return CompletionConstants.NO_APPEND_SEPARATOR;
    	} else if (getArguments().size()==2) {
    		// This arg is required filePath
    		if(lastArgument==null) {
    			candidates.add("<filePath>"); //$NON-NLS-1$
    		}
    		return 0;
    	}
    	return -1;
    }
    
}

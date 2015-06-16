/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.commands.core;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.importer.ImportType;
import org.komodo.importer.ddl.DdlImporter;
import org.komodo.importer.vdb.VdbImporter;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;

/**
 * Import Command
 */
public class ImportCommand extends BuiltInShellCommand {

    private static final String IMPORT = "import"; //$NON-NLS-1$

    private static final String SUBCMD_DDL = "ddl"; //$NON-NLS-1$
    private static final String SUBCMD_VDB = "vdb"; //$NON-NLS-1$
    //private static final List<String> SUBCMDS =
    //		Arrays.asList(SUBCMD_VDB, SUBCMD_DDL);
    private static final List<String> SUBCMDS = Arrays.asList(SUBCMD_VDB);

    private ImportMessages importMessages = null;

    /**
     * Constructor.
     * @param wsStatus the workspace status
     */
    public ImportCommand(WorkspaceStatus wsStatus) {
        super(IMPORT, wsStatus);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        boolean success = false;
        String subcmdArg = requiredArgument(0, Messages.getString("ImportCommand.InvalidArgMsg_SubCommand")); //$NON-NLS-1$

    	WorkspaceStatus wsStatus = getWorkspaceStatus();
        if (SUBCMD_DDL.equalsIgnoreCase(subcmdArg)) {
        	// TODO: Currently not supported.  Importer needs work
            throw new InvalidCommandArgumentException(0, Messages.getString("ImportCommand.InvalidSubCommand")); //$NON-NLS-1$

//            // Check required args
//            String fileNameArg = requiredArgument(1, Messages.getString("ImportCommand.InvalidArgMsg_FileName")); //$NON-NLS-1$
//            String modelPathArg = optionalArgument(2);
//
//            // Validate fileName
//            if(!validateFileName(fileNameArg)) {
//            	return false;
//            }
//            // The supplied
//            WorkspaceContext modelContext = ContextUtils.getContextForPath(getWorkspaceStatus(), modelPathArg);
//            if(modelContext==null || !modelContext.getType().equals(KomodoType.MODEL.getType())) {
//            	// Can only import DDL into a model
//            	return false;
//            }
//
//            KomodoObject modelObject = importDdl(fileNameArg, modelContext);
//            if(modelObject!=null && !importMessages.hasError()) {
//            	Model theModel = (Model)modelObject;
//            	String modelName = theModel.getName(wsStatus.getTransaction());
//            	modelObject.rename(wsStatus.getTransaction(), modelName);
//                // Commit transaction
//                getWorkspaceStatus().commit("ImportCommand"); //$NON-NLS-1$
//
//                print(CompletionConstants.MESSAGE_INDENT, Messages.getString("ImportCommand.ModelImportSuccessMsg", modelPathArg, fileNameArg)); //$NON-NLS-1$
//                if (getWorkspaceStatus().getRecordingStatus())
//                    recordCommand(getArguments());
//            } else {
//                print(CompletionConstants.MESSAGE_INDENT, Messages.getString("ImportCommand.ImportFailedMsg", fileNameArg)); //$NON-NLS-1$
//                print(CompletionConstants.MESSAGE_INDENT, importMessages.errorMessagesToString());
//            }
        } else if (SUBCMD_VDB.equalsIgnoreCase(subcmdArg)) {
            // Check required args
            String fileNameArg = requiredArgument(1, Messages.getString("ImportCommand.InvalidArgMsg_FileName")); //$NON-NLS-1$
            String parentContextPath = optionalArgument(2);

            // Validate fileName
            if(!validateFileName(fileNameArg)) {
            	return false;
            }
            // Validate VDB is valid for the specified parent
            if(!validateParentPath(parentContextPath,KomodoType.VDB)) {
            	return false;
            }

            KomodoObject vdbObject = importVdb(fileNameArg,parentContextPath);

            if(vdbObject!=null && !importMessages.hasError()) {
            	Vdb theVdb = (Vdb)vdbObject;
            	String vdbName = theVdb.getVdbName(wsStatus.getTransaction());

            	// Check for already existing vdb with same name
            	WorkspaceContext parentContext = ContextUtils.getContextForPath(wsStatus, parentContextPath);
            	boolean okToImport = validateNotDuplicate(vdbName, KomodoType.VDB, parentContext);
            	// OK to keep VDB - rename it to vdbName
            	if(okToImport) {
            		vdbObject.rename(wsStatus.getTransaction(), vdbName);
                    wsStatus.commit("ImportCommand"); //$NON-NLS-1$

                    print(CompletionConstants.MESSAGE_INDENT, Messages.getString("ImportCommand.VdbImportSuccessMsg", fileNameArg)); //$NON-NLS-1$

                    success = true;
            	} else {
                    wsStatus.rollback("ImportCommand"); //$NON-NLS-1$
            	}
            } else {
                print(CompletionConstants.MESSAGE_INDENT, Messages.getString("ImportCommand.ImportFailedMsg", fileNameArg)); //$NON-NLS-1$
                print(CompletionConstants.MESSAGE_INDENT, importMessages.errorMessagesToString());
            }
        } else {
            throw new InvalidCommandArgumentException(0, Messages.getString("ImportCommand.InvalidSubCommand")); //$NON-NLS-1$
        }

        return success;
    }

    /**
     * Validate the supplied fileName
     * @param fileName the file name
     * @return 'true' if valid, 'false' if not.
     */
    private boolean validateFileName(String fileName) {
    	//TODO: Ensure valid file path is entered
        return true;
    }

    /**
     * Validates whether the type of object can be created as a child of the parent path
     * @param parentPath the model parent path
     * @param objType the type of object
     * @return 'true' if valid, 'false' if not.
     */
    private boolean validateParentPath(String parentPath, KomodoType objType) throws Exception {
    	WorkspaceContext parentContext = ContextUtils.getContextForPath(getWorkspaceStatus(), parentPath);
    	List<String> allowableChildTypes = parentContext.getAllowableChildTypes();
    	if(!allowableChildTypes.contains(objType.getType().toLowerCase())) {
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString("ImportCommand.childTypeNotAllowed", objType, parentContext.getFullName())); //$NON-NLS-1$
    		return false;
    	}
    	return true;
    }

    /**
     * Validates whether another child of the same name and type already exists
     * @param objName the name of the object
     * @param kType the type of Komodo object
     * @param parentContext the parent context
     * @return 'true' if exists, 'false' if not.
     */
    private boolean validateNotDuplicate(String objName, KomodoType kType, WorkspaceContext parentContext) throws Exception {
    	// Attempt to get child
    	WorkspaceContext child = parentContext.getChild(objName,kType.getType());

    	// If child exists, print message and return false
    	if(child!=null) {
    		print(CompletionConstants.MESSAGE_INDENT,Messages.getString("ImportCommand.cannotImport_wouldCreateDuplicate", objName, kType.getType())); //$NON-NLS-1$
    		return false;
    	}
        return true;
    }

    /**
     * Import ddl from file and add objects to the current model
     * @param ddlFile the file containing the DDL
     * @param modelPath the path which specifies the model location
     * @return the created Komodo object, null if error
     */
    private KomodoObject importDdl(String ddlFilePath, WorkspaceContext modelContext) {
        // Reset ImportMessages
        importMessages = new ImportMessages();
        String modelName = null;

        WorkspaceContext currentContext = getWorkspaceStatus().getCurrentContext();
        Repository repository = null;
        try {
            repository = currentContext.getRepository();
            modelName = modelContext.getName();
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
            return null;
        }

        ImportOptions importOptions = new ImportOptions();
        importOptions.setOption(OptionKeys.NAME, modelName);

        File ddlFile = new File(ddlFilePath);

        DdlImporter importer = new DdlImporter(repository);
        importer.setImportType(ImportType.MODEL);
        return importer.importDdl(ddlFile, importOptions, importMessages);
    }

    /**
     * Import vdb from file and add it at the specified context
     * @param vdbFile the file containing the vdb xml
     * @param vdbParentPath the path specifying where to put the VDb
     * @return the created komodo object, null if error
     */
    private KomodoObject importVdb(String vdbFile, String vdbParentPath) {
        // Reset ImportMessages
        importMessages = new ImportMessages();

        WorkspaceContext parentContext = ContextUtils.getContextForPath(getWorkspaceStatus(), vdbParentPath);
        Repository repository = null;
        try {
            repository = parentContext.getRepository();
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
            return null;
        }

        //TODO: Need a way to tell the importer where to put the VDB??
        File xmlFile = new File(vdbFile);

        ImportOptions importOptions = new ImportOptions();
        importOptions.setOption(OptionKeys.NAME, xmlFile.getName());

        VdbImporter importer = new VdbImporter(repository);
        return importer.importVdb(xmlFile, importOptions, importMessages);
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
    		// This arg is required filePath
            if(lastArgument==null) {
                candidates.add("<filePath>"); //$NON-NLS-1$
            }
            return 0;
    	} else if (getArguments().size()==2) {
    		// The arg is expected to be a path
    		updateTabCompleteCandidatesForPath(candidates, getWorkspaceStatus().getCurrentContext(), true, lastArgument);

    		// Do not put space after it - may want to append more to the path
    		return CompletionConstants.NO_APPEND_SEPARATOR;
    	}
    	return -1;
    }

}

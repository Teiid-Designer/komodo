/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.importer.commands;

import static org.komodo.importer.commands.ImportCommandMessages.ImportCommand.ImportError;
import static org.komodo.importer.commands.ImportCommandMessages.ImportCommand.InvalidArgMsg_FileName;
import static org.komodo.importer.commands.ImportCommandMessages.ImportCommand.InvalidArgMsg_SubCommand;
import static org.komodo.importer.commands.ImportCommandMessages.ImportCommand.InvalidDDLParentType;
import static org.komodo.importer.commands.ImportCommandMessages.ImportCommand.InvalidTargetPath;
import java.io.File;
import java.util.Arrays;
import java.util.List;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.importer.vdb.VdbImporter;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Import Command
 */
public class ImportCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "import"; //$NON-NLS-1$

    private static final String SUBCMD_DDL = "ddl"; //$NON-NLS-1$
    private static final String SUBCMD_VDB = "vdb"; //$NON-NLS-1$
    private static final List<String> SUBCMDS = Arrays.asList(SUBCMD_VDB, SUBCMD_DDL);

//    private static final String TEMP_IMPORT_CONTEXT = "TempImportContext"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public ImportCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            // Check the supplied args
            String subcmdArg = requiredArgument( 0, ImportCommandMessages.getString( InvalidArgMsg_SubCommand ) );
            String fileNameArg = requiredArgument( 1, ImportCommandMessages.getString( InvalidArgMsg_FileName ) );
            String parentContextPath = optionalArgument( 2 );

            // Validates the supplied fileNameArg is a valid, readable file
            if ( !validateReadableFileArg( fileNameArg ) ) {
                return new CommandResultImpl( false,
                                              Messages.getString( Messages.SHELL.FileNotAccessible, fileNameArg ),
                                              null );
            }

            // Validate the parentContext.  If not supplied (null arg) this returns the current context.
            KomodoObject parentContext = ContextUtils.getContextForPath( getWorkspaceStatus(), parentContextPath );
            if ( parentContext == null ) {
                return new CommandResultImpl( false,
                                              ImportCommandMessages.getString( InvalidTargetPath, parentContextPath ),
                                              null );
            }

            WorkspaceStatus wsStatus = getWorkspaceStatus();
            UnitOfWork trans = wsStatus.getTransaction();
            if ( SUBCMD_DDL.equalsIgnoreCase( subcmdArg ) ) {
                // Can only import DDL into Model and Schema
                KomodoType kType = parentContext.getTypeIdentifier( trans );
                if ( kType != KomodoType.MODEL && kType != KomodoType.SCHEMA ) {
                    return new CommandResultImpl( false,
                                                  ImportCommandMessages.getString( InvalidDDLParentType, parentContextPath ),
                                                  null );
                }

                // Import the DDL into the target context
                File ddlFile = new File( fileNameArg );
                ImportOptions importOptions = new ImportOptions();
                ImportMessages importMessages = new ImportMessages();

                //            WorkspaceContext tempContext = createTempContext(kType,TEMP_IMPORT_CONTEXT);
                //            if(tempContext==null) {
                //        		print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ImportCommand.ErrorCreatingTempNode,TEMP_IMPORT_CONTEXT));
                //        		return false;
                //            }
                //
                //            // Setup the import
                //            importDdl(getWorkspaceStatus().getTransaction(), ddlFile, tempContext, importOptions, importMessages);
                //
                //            if(!importMessages.hasError()) {
                //
                //        		print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ImportCommand.DdlImportInProgressMsg, ddlFile));
                //
                //            	// The commit will initiate sequencing
                //            	wsStatus.commitImport(ImportCommand.class.getSimpleName(), importMessages);
                //
                //            	// No sequencing problems - success
                //            	if(!importMessages.hasError()) {
                //        			// Move the children underneath the supplied parent context
                //                    List<WorkspaceContext> children = tempContext.getChildren();
                //                    for(WorkspaceContext child : children) {
                //                    	RenameCommand renameCommand = new RenameCommand(getWorkspaceStatus());
                //                    	renameCommand.setArguments(new Arguments( child.getFullName() + StringConstants.SPACE + parentContext.getFullName() + StringConstants.FORWARD_SLASH + child.getName() ));
                //                    	renameCommand.setOutput(null);  // Suppress rename output
                //                    	renameCommand.execute();
                //                    }
                //                    // Clean up the temp area
                //        			deleteContext(tempContext);
                //
                //        			// Print success message
                //        			print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ImportCommand.DdlImportSuccessMsg, fileNameArg));
                //
                //        			success = true;
                //        		// Problem with the import.  Fail and delete all the parents children
                //            	} else {
                //                    print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ImportCommand.ImportFailedMsg, fileNameArg));
                //                    print(CompletionConstants.MESSAGE_INDENT, importMessages.errorMessagesToString());
                //
                //        			deleteContext(tempContext);
                //            	}
                //            } else {
                //                print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ImportCommand.ImportFailedMsg, fileNameArg));
                //                print(CompletionConstants.MESSAGE_INDENT, importMessages.errorMessagesToString());
                //
                //    			deleteContext(tempContext);
                //            }
            } else if ( SUBCMD_VDB.equalsIgnoreCase( subcmdArg ) ) {
                // Validate VDB is valid for the specified parent
                //            if(!validateParentPath(parentContextPath,KomodoType.VDB)) {
                //            	return false;
                //            }

                // The import will be performed using the VDB filename.  If successful, it will be renamed to the VDB name
                File vdbFile = new File( fileNameArg );
                ImportOptions importOptions = new ImportOptions();
                importOptions.setOption( OptionKeys.NAME, vdbFile.getName() );

                // Determine if a VDB with 'filename' already exists
                //    		boolean okToContinue = validateNotDuplicate(vdbFile.getName(), KomodoType.VDB, parentContext);
                //    		if(!okToContinue) {
                //    			return false;
                //    		}

                // Set up the import.
                ImportMessages importMessages = new ImportMessages();
                importVdb( getWorkspaceStatus().getTransaction(), vdbFile, parentContext, importOptions, importMessages );

                //            if(!importMessages.hasError()) {
                //
                //        		print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ImportCommand.VdbImportInProgressMsg, vdbFile));
                //
                //            	// The commit will initiate sequencing
                //            	wsStatus.commitImport(ImportCommand.class.getSimpleName(), importMessages);
                //
                //            	// No sequencing problems.
                //            	if(!importMessages.hasError()) {
                //            		// Get the created VDB
                //            		KomodoObject parentObj = parentContext.getKomodoObj();
                //                    KomodoObject theVdb = parentObj.getChild(getWorkspaceStatus().getTransaction(), vdbFile.getName(), VdbLexicon.Vdb.VIRTUAL_DATABASE);
                //                    if(theVdb==null) {
                //                        print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ImportCommand.ImportFailedMsg, fileNameArg));
                //                        return false;
                //                    }
                //
                //                    // Want to rename it to the actual VDB name...
                //            		String vdbName = ((Vdb)theVdb).getVdbName(wsStatus.getTransaction());
                //            		boolean okToRename = validateNotDuplicate(vdbName, KomodoType.VDB, parentContext);
                //            		if(!okToRename) {
                //            			// Delete the VDB with filename that was already created
                //                        WorkspaceContext vdbContext = parentContext.getChild(vdbFile.getName(), KomodoType.VDB.getType());
                //                        deleteContext(vdbContext);
                //                        if ( isAutoCommit() ) {
                //                            wsStatus.commit( ImportCommand.class.getSimpleName() );
                //                        }
                //            		} else {
                //            			theVdb.rename(wsStatus.getTransaction(), vdbName);
                //
                //            			if ( isAutoCommit() ) {
                //            				wsStatus.commit( ImportCommand.class.getSimpleName() );
                //            			}
                //
                //            			print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ImportCommand.VdbImportSuccessMsg, fileNameArg));
                //
                //            			success = true;
                //            		}
                //                // Error here means there was a sequencing problem.  The VDB was created, so need to delete it.
                //            	} else {
                //                    print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ImportCommand.ImportFailedMsg, fileNameArg));
                //                    print(CompletionConstants.MESSAGE_INDENT, importMessages.errorMessagesToString());
                //
                //                    WorkspaceContext vdbContext = parentContext.getChild(vdbFile.getName(), KomodoType.VDB.getType());
                //                    deleteContext(vdbContext);
                //            	}
                //
                //            } else {
                //                print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ImportCommand.ImportFailedMsg, fileNameArg));
                //                print(CompletionConstants.MESSAGE_INDENT, importMessages.errorMessagesToString());
                //                wsStatus.rollback( ImportCommand.class.getSimpleName() );
                //            }
            } else {
                return new CommandResultImpl( false, ImportCommandMessages.getString( ImportCommandMessages.ImportCommand.InvalidSubCommand ), null );
            }
        } catch ( final Exception e ) {
            return new CommandResultImpl( false, ImportCommandMessages.getString( ImportError ), e );
        }

        return CommandResult.SUCCESS;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 3;
    }

//    private WorkspaceContext createTempContext(KomodoType kType, String tempName) {
//    	WorkspaceContext tempContext = null;
//    	WorkspaceContext rootContext = getWorkspaceStatus().getWorkspaceContext();
//    	try {
//    		String rootLocation = rootContext.getFullName();
//    		CreateCommand createCommand = new CreateCommand(getWorkspaceStatus());
//    		createCommand.setOutput(null);
//
//    		createCommand.setArguments( new Arguments( KomodoType.SCHEMA.getType() + StringConstants.SPACE + tempName + StringConstants.SPACE + rootLocation ) );
//    		boolean success = createCommand.execute();
//
//    		if(success) {
//    			tempContext = ContextUtils.getContextForPath(getWorkspaceStatus(),rootLocation + StringConstants.FORWARD_SLASH + tempName);
//    		}
//    	} catch (Exception e) {
//    		// Result tempContext is null
//    	}
//    	return tempContext;
//    }
//
//    private void deleteContext(WorkspaceContext context) {
//    	DeleteCommand deleteCommand = new DeleteCommand(getWorkspaceStatus());
//    	String contextName = null;
//    	try {
//    		contextName = context.getFullName();
//    		deleteCommand.setArguments( new Arguments( contextName ) );
//    		deleteCommand.setOutput(null);
//    		deleteCommand.execute();
//    	} catch (Exception e) {
//            print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ImportCommand.DeleteTempContextFailedMsg, contextName ));
//    	}
//    }

    /**
     * Validates whether the type of object can be created as a child of the parent path
     * @param parentPath the model parent path
     * @param objType the type of object
     * @return 'true' if valid, 'false' if not.
     */
//    private boolean validateParentPath(String parentPath, KomodoType objType) throws Exception {
//    	WorkspaceContext parentContext = ContextUtils.getContextForPath(getWorkspaceStatus(), parentPath);
//    	List<String> allowableChildTypes = parentContext.getAllowableChildTypes();
//    	if(!allowableChildTypes.contains(objType.getType().toLowerCase())) {
//            print(CompletionConstants.MESSAGE_INDENT,Messages.getString(Messages.ImportCommand.childTypeNotAllowed, objType, parentContext.getFullName()));
//    		return false;
//    	}
//    	return true;
//    }

    /**
     * Validates whether another child of the same name and type already exists
     * @param objName the name of the object
     * @param kType the type of Komodo object
     * @param parentContext the parent context
     * @return 'true' if exists, 'false' if not.
     */
//    private boolean validateNotDuplicate(String objName, KomodoType kType, WorkspaceContext parentContext) throws Exception {
//    	// Attempt to get child
//    	WorkspaceContext child = parentContext.getChild(objName,kType.getType());
//
//    	// If child exists, print message and return false
//    	if(child!=null) {
//    		print(CompletionConstants.MESSAGE_INDENT,Messages.getString(Messages.ImportCommand.cannotImport_wouldCreateDuplicate, objName, kType.getType()));
//    		return false;
//    	}
//        return true;
//    }

    /**
     * Import DDL from a file - adds the DDL object under the specified targetContext
     * @param uow the transaction
     * @param ddlFile the file containing the DDL
     * @param parentContext the context under which to place the new objects
     * @param importOptions the ImportOptions
     * @param importMessages holds the importer messages
     */
//    private void importDdl(UnitOfWork uow, File ddlFile, KomodoObject parentObj, ImportOptions importOptions, ImportMessages importMessages) {
//        Repository repository = null;
//        try {
//            repository = parentObj.getRepository();
//        } catch (Exception ex) {
//            importMessages.addErrorMessage(ex.getLocalizedMessage());
//            return;
//        }
//        DdlImporter importer = new DdlImporter(repository);
//
//        // Import - (sequencing will not initiate until we commit the transaction)
//        importer.importDdl(uow, ddlFile, parentObj, importOptions, importMessages);
//    }

    protected String getMessage( final Enum< ? > key,
                                 final Object... parameters ) {
        return ImportCommandMessages.getString( ImportCommandMessages.RESOURCE_BUNDLE, key.toString(), parameters );
    }

    /**
     * Import VDB from a file - adds the VDB under the specified parentContext
     * @param uow the transaction
     * @param vdbFile the file containing the vdb xml
     * @param parentObj the VDB parent WorkspaceContext
     * @param importOptions options for the import
     * @param importMessages messages from the import
     */
    private void importVdb(UnitOfWork uow, File vdbFile, KomodoObject parentObj, ImportOptions importOptions, ImportMessages importMessages) {
        Repository repository = null;
        try {
            repository = parentObj.getRepository();
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }
        VdbImporter importer = new VdbImporter(repository);

        // Import - (sequencing will not initiate until we commit the transaction)
        importer.importVdb(uow, vdbFile, parentObj, importOptions, importMessages);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
     */
    @Override
    public void printHelp( final int indent ) {
        print( indent,
               ImportCommandMessages.getString( ImportCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help" ) ); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
     */
    @Override
    public void printUsage( final int indent ) {
        print( indent,
               ImportCommandMessages.getString( ImportCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return true;
    }

}

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
package org.komodo.relational.commands.workspace;

import java.io.File;
import java.util.concurrent.TimeUnit;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.vdb.Vdb;
import org.komodo.repository.SynchronousCallback;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.i18n.I18n;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * A shell command to import a VDB
 */
public final class ImportVdbCommand extends WorkspaceShellCommand {

    static final String NAME = "import-vdb"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ImportVdbCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            String fileName = requiredArgument( 0, I18n.bind( WorkspaceCommandsI18n.missingInputFileName ) );

            // If there is no file extension, add .ddl
            if ( fileName.indexOf( DOT ) == -1 ) {
                fileName = fileName + DOT + "ddl"; //$NON-NLS-1$
            }

            // Validates the supplied fileNameArg is a valid, readable file
            String validationResult = validateReadableFileArg(fileName);
            if(!CompletionConstants.OK.equals(validationResult)) {
                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.inputFileError, fileName, validationResult ), null );
            }

            // The import will be performed using the VDB filename.  If successful, it will be renamed to the VDB name
            File vdbFile = new File(fileName);
            ImportOptions importOptions = new ImportOptions();
            importOptions.setOption(OptionKeys.NAME, vdbFile.getName());

            // Determine if a VDB with 'filename' already exists
            String validationMessage = validateNotDuplicate(vdbFile.getName(), KomodoType.VDB, getContext());
            if(!CompletionConstants.OK.equals(validationMessage)) {
                return new CommandResultImpl( false,
                                              I18n.bind( WorkspaceCommandsI18n.inputFileError, fileName, validationResult ),
                                              null );
            }

            // Set up the import.
            ImportMessages importMessages = new ImportMessages();
            importVdb(getTransaction(), vdbFile, getContext(), importOptions, importMessages);

            if(!importMessages.hasError()) {

                print(CompletionConstants.MESSAGE_INDENT, I18n.bind(WorkspaceCommandsI18n.vdbImportInProgressMsg, vdbFile));

                KomodoObject theVdb = null;
                // The commit will initiate sequencing
                commitImport(ImportVdbCommand.class.getSimpleName(), importMessages);

                // No sequencing problems.
                if(!importMessages.hasError()) {
                    // Get the created VDB
                    KomodoObject parentObj = getContext();
                    theVdb = parentObj.getChild(getTransaction(), vdbFile.getName(), VdbLexicon.Vdb.VIRTUAL_DATABASE);
                    if(theVdb==null) {
                        return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.importFailedMsg, fileName ), null );
                    }

                    // Want to rename it to the actual VDB name...
                    String vdbName = ((Vdb)theVdb).getVdbName(getTransaction());
                    validationResult = validateNotDuplicate(vdbName, KomodoType.VDB, parentObj);
                    if(!CompletionConstants.OK.equals(validationResult)) {
                        // Delete the VDB with filename that was already created
                        CommandResult deleteResult = deleteVdb(theVdb.getName(getTransaction()));
                        if(deleteResult!=null) {
                            return deleteResult;
                        }
                    } else {
                        theVdb.rename(getTransaction(), vdbName);
                        print(CompletionConstants.MESSAGE_INDENT, I18n.bind(WorkspaceCommandsI18n.vdbImportSuccessMsg, fileName));
                    }
                // Error here means there was a sequencing problem.  The VDB was created, so need to delete it.
                } else {
                    print(CompletionConstants.MESSAGE_INDENT, I18n.bind(WorkspaceCommandsI18n.importFailedMsg, fileName));
                    print(CompletionConstants.MESSAGE_INDENT, importMessages.errorMessagesToString());

                    CommandResult deleteResult = deleteVdb(vdbFile.getName());
                    if(deleteResult!=null) {
                        return deleteResult;
                    }
                }

            } else {
                print(CompletionConstants.MESSAGE_INDENT, I18n.bind(WorkspaceCommandsI18n.importFailedMsg, fileName));
                print(CompletionConstants.MESSAGE_INDENT, importMessages.errorMessagesToString());
            }

            return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.inputFileError, fileName ), null );
        } catch ( final Exception e ) {
            return new CommandResultImpl( false, I18n.bind( ShellI18n.commandFailure, NAME ), e );
        }
    }

    /**
     * Import VDB from a file - adds the VDB under the specified parentContext
     * @param uow the transaction
     * @param vdbFile the file containing the vdb xml
     * @param parentObj the VDB parent
     * @param importOptions options for the import
     * @param importMessages messages from the import
     */
    private void importVdb(UnitOfWork uow, File vdbFile, KomodoObject parentObj, ImportOptions importOptions, ImportMessages importMessages) {
        Repository repository = null;
        try {
            repository = getRepository();
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }
        VdbImporter importer = new VdbImporter(repository);

        // Import - (sequencing will not initiate until we commit the transaction)
        importer.importVdb(uow, vdbFile, parentObj, importOptions, importMessages);
    }

    private void commitImport( final String source, ImportMessages importMessages ) throws Exception {
        UnitOfWork trans = getTransaction();
        final String txName = trans.getName();
        trans.commit();

        Repository.UnitOfWorkListener uowListener = trans.getCallback();
        SynchronousCallback callback = null;
        if(uowListener!=null && uowListener instanceof SynchronousCallback) {
            callback = (SynchronousCallback)uowListener;
        }
        final boolean success = callback.await( 3, TimeUnit.MINUTES );
        if ( success ) {
            // For imports, if has callback error - add to import errors and return.
            if(callback.hasError()) {
                importMessages.addErrorMessage(callback.error());
                return;
            }
            final KException error = trans.getError();
            final Repository.UnitOfWork.State txState = trans.getState();

            if ( ( error != null ) || !State.COMMITTED.equals( txState ) ) {
                throw new KException( I18n.bind( ShellI18n.transactionCommitError, txName ), error );
            }
        } else {
            throw new KException( I18n.bind( ShellI18n.transactionTimeout, txName ) );
        }
    }

    /*
     * Delete the VDB with the provided name
     */
    private CommandResult deleteVdb(String vdbName) {
        CommandResult result = null;
        DeleteVdbCommand deleteCommand = new DeleteVdbCommand(getWorkspaceStatus());
        try {
            deleteCommand.setArguments( new Arguments( vdbName ) );
            deleteCommand.execute();
        } catch (Exception e) {
            result = new CommandResultImpl( false, I18n.bind(WorkspaceCommandsI18n.deleteTempVdbFailedMsg, vdbName ), null );
        }
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.importVdbHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.importVdbExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.importVdbUsage ) );
    }

    /**
     * Validates whether another child of the same name and type already exists
     * @param objName the name of the object
     * @param kType the type of Komodo object
     * @param parentObj the parent object
     * @return "OK" if not duplicate, other message if duplicate;.
     */
    private String validateNotDuplicate(String objName, KomodoType kType, KomodoObject parentObj) throws Exception {
        // Get all children of desired type
        KomodoObject[] children = parentObj.getChildrenOfType(getTransaction(), kType.getType());

        // look for child with matching name
        KomodoObject child = null;
        for(KomodoObject kObj : children) {
            if(kObj.getName(getTransaction()).equals(objName)) {
                child=kObj;
                break;
            }
        }

        // If child exists, print message and return false
        if(child!=null) {
            return I18n.bind(WorkspaceCommandsI18n.cannotImportWouldCreateDuplicate, objName, kType.getType());
        }
        return CompletionConstants.OK;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 1;
    }

}

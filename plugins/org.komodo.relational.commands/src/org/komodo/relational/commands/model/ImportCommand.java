/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.INPUT_FILE_ERROR;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.MISSING_INPUT_FILE_NAME;
import static org.komodo.relational.commands.model.ModelCommandMessages.ImportCommand.DdlImportInProgressMsg;
import static org.komodo.relational.commands.model.ModelCommandMessages.ImportCommand.DdlImportSuccessMsg;
import static org.komodo.relational.commands.model.ModelCommandMessages.ImportCommand.DeleteTempContextFailedMsg;
import static org.komodo.relational.commands.model.ModelCommandMessages.ImportCommand.ErrorCreatingTempNode;
import static org.komodo.relational.commands.model.ModelCommandMessages.ImportCommand.ImportFailedMsg;
import java.io.File;
import java.util.concurrent.TimeUnit;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.relational.Messages;
import org.komodo.relational.commands.workspace.CreateSchemaCommand;
import org.komodo.relational.commands.workspace.DeleteSchemaCommand;
import org.komodo.relational.importer.ddl.DdlImporter;
import org.komodo.repository.SynchronousCallback;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.CdCommand;
import org.komodo.shell.commands.RenameCommand;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * A shell command to import DDL.
 */
public final class ImportCommand extends ModelShellCommand {

    static final String NAME = "import-ddl"; //$NON-NLS-1$

    private static final String TEMP_IMPORT_CONTEXT = "TempImportContext"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ImportCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            String fileName = requiredArgument( 0, getWorkspaceMessage( MISSING_INPUT_FILE_NAME ) );

            // If there is no file extension, add .ddl
            if ( fileName.indexOf( DOT ) == -1 ) {
                fileName = fileName + DOT + "ddl"; //$NON-NLS-1$
            }

            // Validates the supplied fileNameArg is a valid, readable file
            String validationResult = validateReadableFileArg(fileName);
            if(!CompletionConstants.OK.equals(validationResult)) {
                return new CommandResultImpl( false, getWorkspaceMessage( INPUT_FILE_ERROR, fileName, validationResult ), null );
            }

            // Import the DDL into the target context
            File ddlFile = new File(fileName);
            ImportOptions importOptions = new ImportOptions();
            ImportMessages importMessages = new ImportMessages();

            // Create a temp schema to put the imported model
            KomodoObject tempSchema = createSchema(TEMP_IMPORT_CONTEXT);
            if(tempSchema==null) {
                return new CommandResultImpl( false, getMessage( ErrorCreatingTempNode, TEMP_IMPORT_CONTEXT ), null );
            }

            WorkspaceStatus wsStatus = getWorkspaceStatus();
            // Setup the import
            importDdl(getTransaction(), ddlFile, tempSchema, importOptions, importMessages);

            if(!importMessages.hasError()) {

                print(CompletionConstants.MESSAGE_INDENT, getMessage(DdlImportInProgressMsg, ddlFile));

                // The commit will initiate sequencing
                commitImport(ImportCommand.class.getSimpleName(), importMessages);

                // No sequencing problems - success
                if(!importMessages.hasError()) {
                    // Move the children underneath the supplied parent context
                    KomodoObject[] children = tempSchema.getChildren(getTransaction());
                    for(KomodoObject child : children) {
                        RenameCommand renameCommand = new RenameCommand(getWorkspaceStatus());
                        String oldFullName = wsStatus.getDisplayPath(child);
                        String contextName = wsStatus.getDisplayPath(getContext());
                        renameCommand.setArguments(new Arguments( oldFullName + StringConstants.SPACE + contextName + StringConstants.FORWARD_SLASH + child.getName(getTransaction()) ));
                        renameCommand.execute();
                    }
                    // Clean up the temp schema
                    deleteSchema(wsStatus.getDisplayPath(tempSchema));

                    return new CommandResultImpl( true, getMessage( DdlImportSuccessMsg, fileName ), null );
                // Problem with the import.  Fail and delete all the parents children
                } else {
                    print(CompletionConstants.MESSAGE_INDENT, getMessage(ImportFailedMsg, fileName));
                    print(CompletionConstants.MESSAGE_INDENT, importMessages.errorMessagesToString());

                    deleteSchema(wsStatus.getDisplayPath(tempSchema));
                }
            } else {
                print(CompletionConstants.MESSAGE_INDENT, getMessage(ImportFailedMsg, fileName));
                print(CompletionConstants.MESSAGE_INDENT, importMessages.errorMessagesToString());

                deleteSchema(wsStatus.getDisplayPath(tempSchema));
            }

            return new CommandResultImpl( false, getWorkspaceMessage( INPUT_FILE_ERROR, fileName ), null );
        } catch ( final Exception e ) {
            return new CommandResultImpl( false, getMessage( SHELL.CommandFailure, NAME ), e );
        }
    }

    /**
     * Import DDL from a file - adds the DDL object under the specified targetContext
     * @param uow the transaction
     * @param ddlFile the file containing the DDL
     * @param parentObj the KomodoObject under which to place the new objects
     * @param importOptions the ImportOptions
     * @param importMessages holds the importer messages
     */
    private void importDdl(UnitOfWork uow, File ddlFile, KomodoObject parentObj, ImportOptions importOptions, ImportMessages importMessages) {
        Repository repository = null;
        try {
            repository = parentObj.getRepository();
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
            return;
        }
        DdlImporter importer = new DdlImporter(repository);

        // Import - (sequencing will not initiate until we commit the transaction)
        importer.importDdl(uow, ddlFile, parentObj, importOptions, importMessages);
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
                throw new KException( Messages.getString( SHELL.TRANSACTION_COMMIT_ERROR, txName ), error );
            }
        } else {
            throw new KException( Messages.getString( SHELL.TRANSACTION_TIMEOUT, txName ) );
        }
    }

    /*
     * Create a Schema to place imported model temporarily
     */
    private KomodoObject createSchema(String schemaName) {
        KomodoObject resultSchema = null;
        try {
            // Save original context
            KomodoObject origContext = getContext();

            // Cd into workspace to create temp Schema
            CdCommand cdCommand = new CdCommand(getWorkspaceStatus());
            cdCommand.setArguments( new Arguments( "/" ) );  //$NON-NLS-1$
            CommandResult result = cdCommand.execute();

            if(!result.isOk()) {
                return resultSchema;
            }

            // Create the temp schema
            CreateSchemaCommand createCommand = new CreateSchemaCommand(getWorkspaceStatus());

            createCommand.setArguments( new Arguments( schemaName ) );
            result = createCommand.execute();

            if(result.isOk()) {
                final KomodoObject[] schemas = getWorkspaceManager().findSchemas(getTransaction());
                for(KomodoObject schema : schemas) {
                    if(schema.getName(getTransaction()).equals(schemaName)) {
                        resultSchema = schema;
                        break;
                    }
                }
            }

            // Cd back into the original Context
            String path = getWorkspaceStatus().getDisplayPath(origContext);
            cdCommand.setArguments(new Arguments(path));
            cdCommand.execute();
        } catch (Exception e) {
            // Result tempContext is null
        }
        return resultSchema;
    }

    /*
     * Delete Schema used as temp location
     */
    private CommandResult deleteSchema(String schemaName) {
        DeleteSchemaCommand deleteCommand = new DeleteSchemaCommand(getWorkspaceStatus());
        try {
            deleteCommand.setArguments( new Arguments( schemaName ) );
            deleteCommand.execute();
        } catch (Exception e) {
            print(CompletionConstants.MESSAGE_INDENT, getMessage(DeleteTempContextFailedMsg, schemaName ));
        }
        CommandResult result = null;
        return result;
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

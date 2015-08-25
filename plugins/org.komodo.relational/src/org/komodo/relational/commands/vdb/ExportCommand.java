/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.ERROR_WRITING_FILE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_OUTPUT_FILE_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.OUTPUT_FILE_ERROR;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.ExportCommand.VDB_EXPORTED;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.io.File;
import java.io.FileWriter;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to export a VDB.
 */
public final class ExportCommand extends VdbShellCommand {

    static final String NAME = "vdb-export"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ExportCommand( final WorkspaceStatus status ) {
        super( NAME, false, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String fileName = requiredArgument( 0, getWorkspaceMessage(MISSING_OUTPUT_FILE_NAME) );
        final boolean override = Boolean.getBoolean( optionalArgument( 1, "false" ) ); //$NON-NLS-1$
        final File file = new File( fileName );

        if ( file.createNewFile() || ( file.exists() && override ) ) {
            final UnitOfWork uow = getTransaction();
            final Vdb vdb = getVdb();
            final String manifest = vdb.export( uow, null );

            // write file
            try ( final FileWriter recordingFileWriter = new FileWriter( fileName, false ) ) {
                recordingFileWriter.write( manifest );
                recordingFileWriter.flush();
                print( MESSAGE_INDENT, getMessage(VDB_EXPORTED, vdb.getName( uow ), fileName, override ) );
                return true;
            } catch ( final Exception e ) {
                print( MESSAGE_INDENT, getWorkspaceMessage(ERROR_WRITING_FILE, fileName ) );
                return false;
            }
        }

        print( MESSAGE_INDENT, getWorkspaceMessage(OUTPUT_FILE_ERROR, fileName ) );
        return false;
    }

}

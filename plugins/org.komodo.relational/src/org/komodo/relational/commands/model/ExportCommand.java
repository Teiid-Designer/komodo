/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.ERROR_DDL_EMPTY;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.ERROR_WRITING_FILE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_OUTPUT_FILE_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.OUTPUT_FILE_ERROR;
import static org.komodo.relational.commands.model.ModelCommandMessages.ExportCommand.DDL_EXPORTED;
import java.io.File;
import java.io.FileWriter;
import java.util.Properties;
import org.komodo.relational.model.Model;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * A shell command to export the Model DDL.
 */
public final class ExportCommand extends ModelShellCommand {

    static final String NAME = "export-ddl"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ExportCommand( final WorkspaceStatus status ) {
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
            String fileName = requiredArgument( 0, getWorkspaceMessage( MISSING_OUTPUT_FILE_NAME ) );

            // If there is no file extension, add .ddl
            if ( fileName.indexOf( DOT ) == -1 ) {
                fileName = fileName + DOT + "ddl"; //$NON-NLS-1$
            }

            final boolean override = Boolean.getBoolean( optionalArgument( 1, "false" ) ); //$NON-NLS-1$
            final File file = new File( fileName );

            if ( file.createNewFile() || ( file.exists() && override ) ) {
                final UnitOfWork uow = getTransaction();
                final Model model = getModel();
                Properties properties = new Properties();
                properties.put( ExportConstants.USE_TABS_PROP_KEY, true );
                final String ddl = model.export( uow, properties );
                
                // No DDL context to export
                if(StringUtils.isEmpty(ddl)) {
                    return new CommandResultImpl( false, getWorkspaceMessage( ERROR_DDL_EMPTY ), null );
                }

                // write file
                try ( final FileWriter recordingFileWriter = new FileWriter( fileName, false ) ) {
                    recordingFileWriter.write( ddl );
                    recordingFileWriter.flush();
                    return new CommandResultImpl( getMessage( DDL_EXPORTED, model.getName( uow ), fileName, override ) );
                } catch ( final Exception e ) {
                    return new CommandResultImpl( false, getWorkspaceMessage( ERROR_WRITING_FILE, fileName ), e );
                }
            }

            return new CommandResultImpl( false, getWorkspaceMessage( OUTPUT_FILE_ERROR, fileName ), null );
        } catch ( final Exception e ) {
            return new CommandResultImpl( false, Messages.getString( SHELL.CommandFailure, NAME ), e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 2;
    }

}

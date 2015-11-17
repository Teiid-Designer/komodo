/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.schema;

import java.io.File;
import java.io.FileWriter;
import java.util.Properties;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.model.Schema;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to export the Schema DDL.
 */
public final class ExportCommand extends SchemaShellCommand {

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
            String fileName = requiredArgument( 0, I18n.bind( WorkspaceCommandsI18n.missingOutputFileName ) );

            // If there is no file extension, add .ddl
            if ( fileName.indexOf( DOT ) == -1 ) {
                fileName = fileName + DOT + "ddl"; //$NON-NLS-1$
            }

            final boolean override = Boolean.getBoolean( optionalArgument( 1, "false" ) ); //$NON-NLS-1$
            final File file = new File( fileName );

            if ( file.createNewFile() || ( file.exists() && override ) ) {
                final UnitOfWork uow = getTransaction();
                final Schema schema = getSchema();
                Properties properties = new Properties();
                properties.put( ExportConstants.USE_TABS_PROP_KEY, true );
                final String ddl = schema.export( uow, properties );

                // No DDL context to export
                if(StringUtils.isEmpty(ddl)) {
                    return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.errorDdlEmpty ), null );
                }

                // write file
                try ( final FileWriter recordingFileWriter = new FileWriter( fileName, false ) ) {
                    recordingFileWriter.write( ddl );
                    recordingFileWriter.flush();
                    return new CommandResultImpl( I18n.bind( SchemaCommandsI18n.ddlExported,
                                                             schema.getName( uow ),
                                                             fileName,
                                                             override ) );
                } catch ( final Exception e ) {
                    return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.errorWritingFile, fileName ), e );
                }
            }

            return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.outputFileError, fileName ), null );
        } catch ( final Exception e ) {
            return new CommandResultImpl( e );
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( SchemaCommandsI18n.exportHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( SchemaCommandsI18n.exportExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( SchemaCommandsI18n.exportUsage ) );
    }

}

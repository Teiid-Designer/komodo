/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to export a VDB.
 */
public final class ExportCommand extends VdbShellCommand {

    static final String NAME = "export-vdb"; //$NON-NLS-1$

    private static final List< String > VALID_OVERWRITE_ARGS = Arrays.asList( new String[] { "-o", "--overwrite" } ); //$NON-NLS-1$ //$NON-NLS-2$;

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

            // If there is no file extension, add .xml
            if ( fileName.indexOf( DOT ) == -1 ) {
                fileName = fileName + DOT + "xml"; //$NON-NLS-1$
            }

            final String overwriteArg = optionalArgument( 1, null );
            final boolean overwrite = !StringUtils.isBlank( overwriteArg );

            // make sure overwrite arg is valid
            if ( overwrite && !VALID_OVERWRITE_ARGS.contains( overwriteArg ) ) {
                return new CommandResultImpl( false, I18n.bind( VdbCommandsI18n.overwriteArgInvalid, overwriteArg ), null );
            }

            final File file = new File( fileName );

            // If file exists, must have overwrite option
            if(file.exists() && !overwrite) {
                return new CommandResultImpl( false, I18n.bind( VdbCommandsI18n.fileExistsOverwriteDisabled, fileName ), null );
            }

            if ( file.createNewFile() || ( file.exists() && overwrite ) ) {
                final UnitOfWork uow = getTransaction();
                final Vdb vdb = getVdb();
                Properties properties = new Properties();
                properties.put( ExportConstants.USE_TABS_PROP_KEY, true );
                final String manifest = vdb.export( uow, properties );

                // write file
                try{
                    Files.write(Paths.get(file.getPath()), manifest.getBytes());
                    return new CommandResultImpl( I18n.bind( VdbCommandsI18n.vdbExported, vdb.getName( uow ), fileName, overwrite ) );
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
        print( indent, I18n.bind( VdbCommandsI18n.exportHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( VdbCommandsI18n.exportExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( VdbCommandsI18n.exportUsage ) );
    }

}

/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.workspace;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A shell command to export a Vdb from Workspace context.
 */
public final class ExportVdbCommand extends WorkspaceShellCommand {

    static final String NAME = "export-vdb"; //$NON-NLS-1$

    private static final List< String > VALID_OVERWRITE_ARGS = Arrays.asList( new String[] { "-o", "--overwrite" } ); //$NON-NLS-1$ //$NON-NLS-2$;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ExportVdbCommand( final WorkspaceStatus status ) {
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
            final String vdbName = requiredArgument( 0, I18n.bind( WorkspaceCommandsI18n.missingVdbName ) );
            String fileName = requiredArgument( 1, I18n.bind( WorkspaceCommandsI18n.missingOutputFileName ) );

            // If there is no file extension, add .xml
            if ( fileName.indexOf( DOT ) == -1 ) {
                fileName = fileName + DOT + "xml"; //$NON-NLS-1$
            }

            final String overwriteArg = optionalArgument( 2, null );
            final boolean overwrite = !StringUtils.isBlank( overwriteArg );

            // make sure overwrite arg is valid
            if ( overwrite && !VALID_OVERWRITE_ARGS.contains( overwriteArg ) ) {
                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.overwriteArgInvalid, overwriteArg ), null );
            }

            // Determine if the VDB exists
            if(!getWorkspaceManager().hasChild(getTransaction(), vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE)) {
                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.vdbNotFound, vdbName ), null );
            }

            // Get the VDB to Export
            final KomodoObject vdbObj = getWorkspaceManager().getChild(getTransaction(), vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
            final Vdb vdbToExport = Vdb.RESOLVER.resolve(getTransaction(), vdbObj);

            final File file = new File( fileName );

            // If file exists, must have overwrite option
            if(file.exists() && !overwrite) {
                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.fileExistsOverwriteDisabled, fileName ), null );
            }

            if ( file.createNewFile() || ( file.exists() && overwrite ) ) {
                final UnitOfWork uow = getTransaction();
                Properties properties = new Properties();
                properties.put( ExportConstants.USE_TABS_PROP_KEY, true );
                final String manifest = vdbToExport.export( uow, properties );

                // Write the file
                try{
                    Files.write(Paths.get(file.getPath()), manifest.getBytes());
                    return new CommandResultImpl( I18n.bind( WorkspaceCommandsI18n.vdbExported, vdbToExport.getName( uow ), fileName, overwrite ) );
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
        return 3;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.exportVdbHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.exportVdbExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.exportVdbUsage ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        final UnitOfWork uow = getTransaction();
        final WorkspaceManager mgr = getWorkspaceManager();
        final KomodoObject[] vdbs = mgr.findVdbs(uow);
        List<String> existingVdbNames = new ArrayList<String>(vdbs.length);
        for(KomodoObject vdb : vdbs) {
            existingVdbNames.add(vdb.getName(uow));
        }

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingVdbNames );
            } else {
                for ( final String item : existingVdbNames ) {
                    if ( item.startsWith( lastArgument ) ) {
                        candidates.add( item );
                    }
                }
            }
        }
        return TabCompletionModifier.AUTO;
    }

}

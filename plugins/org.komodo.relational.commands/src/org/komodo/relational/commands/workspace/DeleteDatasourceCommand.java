/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.workspace;

import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to delete a Datasource object.
 */
public final class DeleteDatasourceCommand extends WorkspaceShellCommand {

    static final String NAME = "delete-datasource"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteDatasourceCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        CommandResult result = null;

        try {
            final String sourceName = requiredArgument( 0, I18n.bind( WorkspaceCommandsI18n.missingDatasourceName ) );

            final WorkspaceManager mgr = getWorkspaceManager();
            final KomodoObject[] datasources = mgr.findDatasources(getTransaction());
            KomodoObject[] objToDelete = new KomodoObject[1];
            for(KomodoObject datasource : datasources) {
                if(datasource.getName(getTransaction()).equals(sourceName)) {
                    objToDelete[0] = datasource;
                    break;
                }
            }

            if(objToDelete[0]==null) {
                result = new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.datasourceNotFound, sourceName ), null );
            } else {
                mgr.delete(getTransaction(), objToDelete);
                result = new CommandResultImpl( I18n.bind( WorkspaceCommandsI18n.datasourceDeleted, sourceName ) );
            }
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.deleteDatasourceError ), e );
        }

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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.deleteDatasourceHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.deleteDatasourceExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.deleteDatasourceUsage ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        final UnitOfWork uow = getTransaction();
        final WorkspaceManager mgr = getWorkspaceManager();
        final KomodoObject[] datasources = mgr.findDatasources(getTransaction());
        List<String> existingDsNames = new ArrayList<String>(datasources.length);
        for(KomodoObject datasource : datasources) {
            existingDsNames.add(datasource.getName(uow));
        }

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingDsNames );
            } else {
                for ( final String item : existingDsNames ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }

            return 0;
        }

        // no tab completion
        return -1;
    }

}

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
import org.komodo.relational.commands.server.ServerCommandProvider;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to delete a Teiid object.
 */
public final class DeleteTeiidCommand extends WorkspaceShellCommand {

    static final String NAME = "delete-teiid"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteTeiidCommand( final WorkspaceStatus status ) {
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
            final String teiidName = requiredArgument( 0, I18n.bind( WorkspaceCommandsI18n.missingTeiidName ) );

            final WorkspaceManager mgr = getWorkspaceManager();
            final KomodoObject[] teiids = mgr.findTeiids(getTransaction());
            KomodoObject[] objToDelete = new KomodoObject[1];
            for(KomodoObject teiid : teiids) {
                if(teiid.getName(getTransaction()).equals(teiidName)) {
                    objToDelete[0] = teiid;
                    break;
                }
            }

            if(objToDelete[0]==null) {
                result = new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.teiidNotFound, teiidName ), null );
            } else {
                // If server being deleted is the current default server, remove the default
                KomodoObject defaultTeiid = getWorkspaceStatus().getStateObjects().get( ServerCommandProvider.SERVER_DEFAULT_KEY );
                if(defaultTeiid!=null && defaultTeiid.getName(getTransaction()).equals(teiidName)) {
                    getWorkspaceStatus().setStateObject( ServerCommandProvider.SERVER_DEFAULT_KEY, null );
                }

                mgr.delete(getTransaction(), objToDelete);
                result = new CommandResultImpl( I18n.bind( WorkspaceCommandsI18n.teiidDeleted, teiidName ) );
            }
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.deleteTeiidError ), e );
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
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.deleteTeiidHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.deleteTeiidExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.deleteTeiidUsage ) );
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
        final KomodoObject[] teiids = mgr.findTeiids(getTransaction());
        List<String> existingTeiidNames = new ArrayList<String>(teiids.length);
        for(KomodoObject teiid : teiids) {
            existingTeiidNames.add(teiid.getName(uow));
        }

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingTeiidNames );
            } else {
                for ( final String item : existingTeiidNames ) {
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

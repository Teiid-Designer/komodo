/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import static org.komodo.relational.commands.WorkspaceCommandMessages.DeleteTeiidCommand.DELETE_TEIID_ERROR;
import static org.komodo.relational.commands.WorkspaceCommandMessages.DeleteTeiidCommand.TEIID_DELETED;
import static org.komodo.relational.commands.WorkspaceCommandMessages.DeleteTeiidCommand.TEIID_NOT_FOUND;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_TEIID_NAME;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;

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
            final String teiidName = requiredArgument( 0, getMessage( MISSING_TEIID_NAME ) );

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
                result = new CommandResultImpl( false, getMessage( TEIID_NOT_FOUND, teiidName ), null );
            } else {
                mgr.delete(getTransaction(), objToDelete);
                result = new CommandResultImpl( getMessage( TEIID_DELETED, teiidName ) );
            }
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( DELETE_TEIID_ERROR ), e );
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

/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import static org.komodo.relational.commands.WorkspaceCommandMessages.DeleteSchemaCommand.DELETE_SCHEMA_ERROR;
import static org.komodo.relational.commands.WorkspaceCommandMessages.DeleteSchemaCommand.SCHEMA_DELETED;
import static org.komodo.relational.commands.WorkspaceCommandMessages.DeleteSchemaCommand.SCHEMA_NOT_FOUND;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_SCHEMA_NAME;
import java.util.ArrayList;
import java.util.List;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete a Schema.
 */
public final class DeleteSchemaCommand extends WorkspaceShellCommand {

    static final String NAME = "delete-schema"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteSchemaCommand( final WorkspaceStatus status ) {
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
            final String schemaName = requiredArgument( 0, getMessage( MISSING_SCHEMA_NAME ) );

            final WorkspaceManager mgr = getWorkspaceManager();
            final KomodoObject[] schemas = mgr.getChildrenOfType(getTransaction(), KomodoLexicon.Schema.NODE_TYPE);
            KomodoObject[] objToDelete = new KomodoObject[1];
            for(KomodoObject schema : schemas) {
                if(schema.getName(getTransaction()).equals(schemaName)) {
                    objToDelete[0] = schema;
                    break;
                }
            }
            
            if(objToDelete[0]==null) {
                result = new CommandResultImpl( false, getMessage( SCHEMA_NOT_FOUND ), null );
            } else {
                mgr.delete(getTransaction(), objToDelete);
                result = new CommandResultImpl( getMessage( SCHEMA_DELETED, schemaName ) );
            }
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( DELETE_SCHEMA_ERROR ), e );
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
        final KomodoObject[] schemas = mgr.getChildrenOfType(uow, KomodoLexicon.Schema.NODE_TYPE);
        List<String> existingSchemaNames = new ArrayList<String>(schemas.length);
        for(KomodoObject schema : schemas) {
            existingSchemaNames.add(schema.getName(uow));
        }

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingSchemaNames );
            } else {
                for ( final String item : existingSchemaNames ) {
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

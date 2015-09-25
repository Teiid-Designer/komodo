/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.vdb.VdbCommandMessages.DeleteImportCommand.DELETE_IMPORT_ERROR;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.DeleteImportCommand.IMPORT_DELETED;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.MISSING_IMPORT_NAME;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete a VDB import from a VDB.
 */
public final class DeleteImportCommand extends VdbShellCommand {

    static final String NAME = "delete-import"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteImportCommand( final WorkspaceStatus status ) {
        super( NAME, status );
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
            final String importName = requiredArgument( 0, getMessage( MISSING_IMPORT_NAME ) );

            final Vdb vdb = getVdb();
            vdb.removeImport( getTransaction(), importName );

            result = new CommandResultImpl( getMessage( IMPORT_DELETED, importName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( DELETE_IMPORT_ERROR ), e );
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
        final Vdb vdb = getVdb();
        final VdbImport[] imports = vdb.getImports( uow );
        List<String> existingImportNames = new ArrayList<String>(imports.length);
        for(VdbImport theImport : imports) {
            existingImportNames.add(theImport.getName(uow));
        }

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingImportNames );
            } else {
                for ( final String item : existingImportNames ) {
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

/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.storedprocedure;

import java.util.List;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.RenameCommand;
import org.komodo.utils.i18n.I18n;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

/**
 * The {@link ProcedureResultSet result} child cannot be renamed.
 */
public final class StoredProcedureRenameCommand extends RenameCommand {

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public StoredProcedureRenameCommand( final WorkspaceStatus status ) {
        super( status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final String name = requiredArgument( 0, I18n.bind( ShellI18n.missingRenameFirstArg ) );
            final String newChildName = optionalArgument( 1 ); // for renaming a child

            // new name or new child name cannot be the name of the result set
            if ( CreateProcedure.RESULT_SET.equals( name ) || CreateProcedure.RESULT_SET.equals( newChildName ) ) {
                return new CommandResultImpl( false,
                                              I18n.bind( StoredProcedureCommandsI18n.resultSetRenameError,
                                                         getContext().getName( getTransaction() ) ),
                                              null );
            }

            return super.doExecute();
        } catch ( Exception e ) {
            return new CommandResultImpl( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        try {
            return StoredProcedure.RESOLVER.resolvable( getTransaction(), getContext() );
        } catch ( final Exception ex ) {
            return false;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.commands.RenameCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( final String lastArgument,
                                                final List< CharSequence > candidates ) throws Exception {
        final TabCompletionModifier result = super.tabCompletion( lastArgument, candidates );
        candidates.remove( CreateProcedure.RESULT_SET ); // result set can't be renamed
        return result;
    }

}

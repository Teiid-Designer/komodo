/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.pushdownfunction;

import static org.komodo.relational.commands.pushdownfunction.PushdownFunctionCommandMessages.SetResultSetCommand.RESULT_SET_TYPE_SET;
import static org.komodo.relational.commands.pushdownfunction.PushdownFunctionCommandMessages.General.MISSING_RESULT_SET_TYPE;
import static org.komodo.relational.commands.storedprocedure.StoredProcedureCommandMessages.General.INVALID_RESULT_SET_TYPE;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to set the result set for a PushdownFunction.
 */
public final class SetResultSetCommand extends PushdownFunctionShellCommand {

    static final String NAME = "set-result-set"; //$NON-NLS-1$

    protected static final List< String > ALL_TYPES = Arrays.asList( new String[] { DataTypeResultSet.class.getSimpleName(),
                                                                                    TabularResultSet.class.getSimpleName()} );
    
    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetResultSetCommand( final WorkspaceStatus status ) {
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
            final String rsType = requiredArgument( 0, getMessage( MISSING_RESULT_SET_TYPE ) );

            final PushdownFunction func = getPushdownFunction();
            if(rsType.equals(DataTypeResultSet.class.getSimpleName())) {
                func.setResultSet( getTransaction(), DataTypeResultSet.class );
            } else if(rsType.equals(TabularResultSet.class.getSimpleName())) {
                func.setResultSet( getTransaction(), TabularResultSet.class );
            } else {
                new CommandResultImpl( false, getMessage( INVALID_RESULT_SET_TYPE, rsType ), null );
            }

            result = new CommandResultImpl( getMessage( RESULT_SET_TYPE_SET, rsType ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
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

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( ALL_TYPES );
            } else {
                for ( final String item : ALL_TYPES ) {
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

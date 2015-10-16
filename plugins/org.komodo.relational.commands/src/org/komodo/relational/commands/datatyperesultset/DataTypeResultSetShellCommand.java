/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datatyperesultset;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link DataTypeResultSet DataTypeResultSet}-related shell commands.
 */
abstract class DataTypeResultSetShellCommand extends RelationalShellCommand {

    protected static final String ARRAY = "array"; //$NON-NLS-1$
    protected static final String LENGTH = "length"; //$NON-NLS-1$
    protected static final String TYPE = "type"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { ARRAY, LENGTH, TYPE } );

    protected DataTypeResultSetShellCommand( final String name,
                                           final WorkspaceStatus status ) {
        super( status, name );
    }

    protected DataTypeResultSet getDataTypeResultSet() throws Exception {
        assert getContext() instanceof DataTypeResultSet;
        return DataTypeResultSet.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return DataTypeResultSet.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            // exception returns false
        }
        return false;
    }

    @Override
    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(DataTypeResultSetCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }

    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, Messages.getString( DataTypeResultSetCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help", getName() ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, Messages.getString( DataTypeResultSetCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".examples" ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, Messages.getString( DataTypeResultSetCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}

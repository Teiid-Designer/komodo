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
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link DataTypeResultSet}-related shell commands.
 */
abstract class DataTypeResultSetShellCommand extends RelationalShellCommand {

    protected static final String DATATYPE_NAME = "datatypeName"; //$NON-NLS-1$
    protected static final String DESCRIPTION = "ANNOTATION"; //$NON-NLS-1$
    protected static final String LENGTH = "datatypeLength"; //$NON-NLS-1$
    protected static final String NAME_IN_SOURCE = "NAMEINSOURCE"; //$NON-NLS-1$
    protected static final String NULLABLE = "nullable"; //$NON-NLS-1$
    protected static final String PRECISION = "datatypePrecision"; //$NON-NLS-1$
    protected static final String SCALE = "datatypeScale"; //$NON-NLS-1$
    protected static final String UUID = "UUID"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { DATATYPE_NAME,
                                                                                    DESCRIPTION,
                                                                                    LENGTH,
                                                                                    NAME_IN_SOURCE,
                                                                                    NULLABLE,
                                                                                    PRECISION,
                                                                                    SCALE,
                                                                                    UUID } );

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

}

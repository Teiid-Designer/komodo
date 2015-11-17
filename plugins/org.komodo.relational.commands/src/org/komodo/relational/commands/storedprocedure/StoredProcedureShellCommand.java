/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.storedprocedure;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link StoredProcedure StoredProcedure}-related shell commands.
 */
abstract class StoredProcedureShellCommand extends RelationalShellCommand {

    protected static final String DESCRIPTION = "ANNOTATION"; //$NON-NLS-1$
    protected static final String NAME_IN_SOURCE = "NAMEINSOURCE"; //$NON-NLS-1$
    protected static final String NATIVE_QUERY = "NATIVE_QUERY"; //$NON-NLS-1$
    protected static final String NON_PREPARED = "NON_PREPARED"; //$NON-NLS-1$
    protected static final String SCHEMA_ELEMENT_TYPE = "schemaElementType"; //$NON-NLS-1$
    protected static final String UPDATE_COUNT = "UPDATECOUNT"; //$NON-NLS-1$
    protected static final String UUID = "UUID"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { DESCRIPTION, NAME_IN_SOURCE, NATIVE_QUERY,
                                                                                    NON_PREPARED, SCHEMA_ELEMENT_TYPE,
                                                                                    UPDATE_COUNT, UUID } );

    protected StoredProcedureShellCommand( final String name,
                                           final WorkspaceStatus status ) {
        super( status, name );
    }

    protected StoredProcedure getStoredProcedure() throws Exception {
        assert getContext() instanceof StoredProcedure;
        return StoredProcedure.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return StoredProcedure.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            // exception returns false
        }
        return false;
    }

}

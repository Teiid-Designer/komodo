/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datasource;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.model.Schema;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link Datasource Datasource}-related shell commands.
 */
abstract class DatasourceShellCommand extends RelationalShellCommand {

    protected static final String JNDI_NAME = "jndiName"; //$NON-NLS-1$
    protected static final String DRIVER_NAME = "driverName"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { JNDI_NAME, DRIVER_NAME } );

    protected DatasourceShellCommand( final String name,
                                  final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Datasource getDatasource() throws Exception {
        assert getContext() instanceof Schema;
        return Datasource.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Datasource.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}

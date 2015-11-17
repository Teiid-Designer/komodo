/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.teiid;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link Teiid Teiid}-related shell commands.
 */
abstract class TeiidShellCommand extends RelationalShellCommand {

    protected static final String ADMIN_PORT = "adminPort"; //$NON-NLS-1$
    protected static final String ADMIN_PASSWORD = "adminPswd"; //$NON-NLS-1$
    protected static final String ADMIN_SECURE = "adminSecure"; //$NON-NLS-1$
    protected static final String ADMIN_USER = "adminUser"; //$NON-NLS-1$
    protected static final String JDBC_PORT = "jdbcPort"; //$NON-NLS-1$
    protected static final String JDBC_PASSWORD = "jdbcPswd"; //$NON-NLS-1$
    protected static final String JDBC_SECURE = "jdbcSecure"; //$NON-NLS-1$
    protected static final String JDBC_USER = "jdbcUser"; //$NON-NLS-1$
    protected static final String HOST = "host"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { ADMIN_PORT, ADMIN_PASSWORD, ADMIN_SECURE,
                                                                                    ADMIN_USER, JDBC_PORT, JDBC_PASSWORD,
                                                                                    JDBC_SECURE, JDBC_USER, HOST } );

    protected TeiidShellCommand( final String name,
                                 final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Teiid getTeiid() throws Exception {
        assert getContext() instanceof Teiid;
        return Teiid.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Teiid.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            // exception returns false
        }
        return false;
    }

}

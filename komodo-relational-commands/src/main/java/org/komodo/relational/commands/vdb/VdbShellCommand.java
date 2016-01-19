/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link Vdb VDB}-related shell commands.
 */
abstract class VdbShellCommand extends RelationalShellCommand {

    protected static final String ALLOWED_LANGUAGES = "allowed-languages"; //$NON-NLS-1$
    protected static final String AUTHENTICATION_TYPE = "authentication-type"; //$NON-NLS-1$
    protected static final String CONNECTION_TYPE = "connectionType"; //$NON-NLS-1$
    protected static final String DESCRIPTION = "description"; //$NON-NLS-1$
    protected static final String GSS_PATTERN = "gss-pattern"; //$NON-NLS-1$
    protected static final String ORIGINAL_FILE_PATH = "originalFile"; //$NON-NLS-1$
    protected static final String PASSWORD_PATTERN = "password-pattern"; //$NON-NLS-1$
    protected static final String PREVIEW = "preview"; //$NON-NLS-1$
    protected static final String QUERY_TIMEOUT = "query-timeout"; //$NON-NLS-1$
    protected static final String SECURITY_DOMAIN = "security-domain"; //$NON-NLS-1$
    protected static final String VERSION = "version"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { ALLOWED_LANGUAGES, AUTHENTICATION_TYPE,
                                                                                    CONNECTION_TYPE, DESCRIPTION, GSS_PATTERN,
                                                                                    ORIGINAL_FILE_PATH, PASSWORD_PATTERN, PREVIEW,
                                                                                    QUERY_TIMEOUT, SECURITY_DOMAIN, VERSION } );

    protected VdbShellCommand( final String name,
                               final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Vdb getVdb() throws Exception {
        assert getContext() instanceof Vdb;
        return Vdb.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Vdb.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}

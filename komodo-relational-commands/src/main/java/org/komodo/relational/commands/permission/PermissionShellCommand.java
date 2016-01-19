/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.permission;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.Permission;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link Permission permission}-related shell commands.
 */
abstract class PermissionShellCommand extends RelationalShellCommand {

    protected static final String ALLOW_ALTER = "allowAlter"; //$NON-NLS-1$
    protected static final String ALLOW_CREATE = "allowCreate"; //$NON-NLS-1$
    protected static final String ALLOW_DELETE = "allowDelete"; //$NON-NLS-1$
    protected static final String ALLOW_EXECUTE = "allowExecute"; //$NON-NLS-1$
    protected static final String ALLOW_LANGUAGE = "allowLanguage"; //$NON-NLS-1$
    protected static final String ALLOW_READ = "allowRead"; //$NON-NLS-1$
    protected static final String ALLOW_UPDATE = "allowUpdate"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { ALLOW_ALTER, ALLOW_CREATE, ALLOW_DELETE,
                                                                                    ALLOW_EXECUTE, ALLOW_LANGUAGE, ALLOW_READ,
                                                                                    ALLOW_UPDATE } );

    protected PermissionShellCommand( final String name,
                                      final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Permission getPermission() throws Exception {
        assert getContext() instanceof Permission;
        return Permission.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Permission.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}

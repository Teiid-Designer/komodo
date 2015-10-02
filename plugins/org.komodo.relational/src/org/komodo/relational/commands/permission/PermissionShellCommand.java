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
import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.internal.PermissionImpl;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link Permission Permission}-related shell commands.
 */
abstract class PermissionShellCommand extends RelationalShellCommand {

    protected static final String ALLOW_ALTER = "allow-alter"; //$NON-NLS-1$
    protected static final String ALLOW_CREATE = "allow-create"; //$NON-NLS-1$
    protected static final String ALLOW_DELETE = "allow-delete"; //$NON-NLS-1$
    protected static final String ALLOW_EXECUTE = "allow-execute"; //$NON-NLS-1$
    protected static final String ALLOW_LANGUAGE = "allow-language"; //$NON-NLS-1$
    protected static final String ALLOW_READ = "allow-read"; //$NON-NLS-1$
    protected static final String ALLOW_UPDATE = "allow-update"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { ALLOW_ALTER, ALLOW_CREATE, ALLOW_DELETE,
                                                                                    ALLOW_EXECUTE, ALLOW_LANGUAGE, ALLOW_READ,
                                                                                    ALLOW_UPDATE } );

    protected PermissionShellCommand( final String name,
                                      final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Permission getPermission() throws Exception {
        return new PermissionImpl( getTransaction(), getRepository(), getPath() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return PermissionImpl.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            // exception returns false
        }
        return false;
    }

    @Override
    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(PermissionCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
     */
    @Override
    public void printHelp( final int indent ) {
        print( indent, Messages.getString( PermissionCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help" ) ); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
     */
    @Override
    public void printUsage( final int indent ) {
        print( indent, Messages.getString( PermissionCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}

/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datarole;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.internal.DataRoleImpl;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for {@link DataRole data role}-related shell commands.
 */
abstract class DataRoleShellCommand extends RelationalShellCommand {

    protected static final String ALLOWED_CREATE_TEMPORARY_TABLES = "allowCreateTemporaryTables"; //$NON-NLS-1$
    protected static final String ANY_AUTHENTICATED = "anyAuthenticated"; //$NON-NLS-1$
    protected static final String DESCRIPTION = "description"; //$NON-NLS-1$
    protected static final String GRANT_ALL = "grantAll"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { ALLOWED_CREATE_TEMPORARY_TABLES,
                                                                                    ANY_AUTHENTICATED, DESCRIPTION, GRANT_ALL } );

    protected DataRoleShellCommand( final String name,
                                    final WorkspaceStatus status ) {
        super( status, name );
    }

    protected DataRole getDataRole() throws Exception {
        return new DataRoleImpl( getTransaction(), getRepository(), getPath() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return DataRoleImpl.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            // exception returns false
        }
        return false;
    }

    @Override
    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(DataRoleCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
     */
    @Override
    public void printHelp( final int indent ) {
        print( indent, Messages.getString( DataRoleCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help" ) ); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
     */
    @Override
    public void printUsage( final int indent ) {
        print( indent, Messages.getString( DataRoleCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}

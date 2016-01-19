/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdbimport;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for {@link VdbImport import VDB}-related shell commands.
 */
abstract class VdbImportShellCommand extends RelationalShellCommand {

    protected static final String IMPORT_DATA_POLICIES = "importDataPolicies"; //$NON-NLS-1$
    protected static final String VERSION = "version"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { IMPORT_DATA_POLICIES, VERSION } );

    protected VdbImportShellCommand( final String name,
                                     final WorkspaceStatus status ) {
        super( status, name );
    }

    protected VdbImport getVdbImport() throws Exception {
        assert getContext() instanceof VdbImport;
        return VdbImport.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return VdbImport.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}

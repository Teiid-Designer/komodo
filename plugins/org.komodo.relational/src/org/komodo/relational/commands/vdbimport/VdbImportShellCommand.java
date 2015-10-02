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
import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.vdb.internal.VdbImportImpl;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for {@link DataRole data role}-related shell commands.
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
        return new VdbImportImpl( getTransaction(), getRepository(), getPath() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return VdbImportImpl.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            // exception returns false
        }
        return false;
    }

    @Override
    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(VdbImportCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }

    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, Messages.getString( VdbImportCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help", getName() ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, Messages.getString( VdbImportCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".examples" ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, Messages.getString( VdbImportCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}

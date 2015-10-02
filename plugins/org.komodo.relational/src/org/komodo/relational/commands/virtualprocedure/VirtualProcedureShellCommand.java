/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.virtualprocedure;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.relational.model.internal.VirtualProcedureImpl;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link VirtualProcedure VirtualProcedure}-related shell commands.
 */
abstract class VirtualProcedureShellCommand extends RelationalShellCommand {

    protected static final String AS_CLAUSE_STATEMENT = "as-clause-statement"; //$NON-NLS-1$
    protected static final String DESCRIPTION = "description"; //$NON-NLS-1$
    protected static final String NAME_IN_SOURCE = "name-in-source"; //$NON-NLS-1$
    protected static final String SCHEMA_ELEMENT_TYPE = "schema-element-type"; //$NON-NLS-1$
    protected static final String UPDATE_COUNT = "update-count"; //$NON-NLS-1$
    protected static final String UUID = "uuid"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { AS_CLAUSE_STATEMENT, DESCRIPTION,
                                                                                    NAME_IN_SOURCE, SCHEMA_ELEMENT_TYPE,
                                                                                    UPDATE_COUNT, UUID } );

    protected VirtualProcedureShellCommand( final String name,
                                            final WorkspaceStatus status ) {
        super( status, name );
    }

    protected VirtualProcedure getVirtualProcedure() throws Exception {
        return new VirtualProcedureImpl( getTransaction(), getRepository(), getPath() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return VirtualProcedureImpl.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            // exception returns false
        }
        return false;
    }

    @Override
    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(VirtualProcedureCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }

    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, Messages.getString( VirtualProcedureCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help", getName() ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, Messages.getString( VirtualProcedureCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".examples" ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, Messages.getString( VirtualProcedureCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}

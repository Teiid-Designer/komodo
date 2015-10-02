/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.pushdownfunction;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.internal.PushdownFunctionImpl;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link PushdownFunction PushdownFunction}-related shell commands.
 */
abstract class PushdownFunctionShellCommand extends RelationalShellCommand {

    protected static final String AGGREGATE = "aggregate"; //$NON-NLS-1$
    protected static final String ALLOWS_DISTINCT = "allows-distinct"; //$NON-NLS-1$
    protected static final String ALLOWS_ORDERBY = "allows-orderby"; //$NON-NLS-1$
    protected static final String ANALYTIC = "analytic"; //$NON-NLS-1$
    protected static final String DECOMPOSABLE = "decomposable"; //$NON-NLS-1$
    protected static final String DESCRIPTION = "description"; //$NON-NLS-1$
    protected static final String DETERMINISM = "determinism"; //$NON-NLS-1$
    protected static final String NAME_IN_SOURCE = "name-in-source"; //$NON-NLS-1$
    protected static final String NULL_ON_NULL = "null-on-null"; //$NON-NLS-1$
    protected static final String SCHEMA_ELEMENT_TYPE = "schema-element-type"; //$NON-NLS-1$
    protected static final String UPDATE_COUNT = "update-count"; //$NON-NLS-1$
    protected static final String USES_DISTINCT_ROWS = "uses-distinct-rows"; //$NON-NLS-1$
    protected static final String UUID = "uuid"; //$NON-NLS-1$
    protected static final String VAR_ARGS = "var-args"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { AGGREGATE, ALLOWS_DISTINCT, ALLOWS_ORDERBY,
                                                                                  ANALYTIC, DECOMPOSABLE, DESCRIPTION, DETERMINISM,
                                                                                  NAME_IN_SOURCE, NULL_ON_NULL, SCHEMA_ELEMENT_TYPE,
                                                                                  UPDATE_COUNT, USES_DISTINCT_ROWS, UUID, VAR_ARGS} );

    protected PushdownFunctionShellCommand( final String name,
                                            final WorkspaceStatus status ) {
        super( status, name );
    }

    protected PushdownFunction getPushdownFunction() throws Exception {
        return new PushdownFunctionImpl( getTransaction(), getRepository(), getPath() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return PushdownFunctionImpl.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            // exception returns false
        }
        return false;
    }

    @Override
    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(PushdownFunctionCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
     */
    @Override
    public void printHelp( final int indent ) {
        print( indent, Messages.getString( PushdownFunctionCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help" ) ); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
     */
    @Override
    public void printUsage( final int indent ) {
        print( indent, Messages.getString( PushdownFunctionCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}

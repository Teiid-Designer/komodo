/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.userdefinedfunction;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.relational.model.internal.UserDefinedFunctionImpl;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link UserDefinedFunction UserDefinedFunction}-related shell commands.
 */
abstract class UserDefinedFunctionShellCommand extends RelationalShellCommand {

    protected static final String AGGREGATE = "aggregate"; //$NON-NLS-1$
    protected static final String ALLOWS_DISTINCT = "allows-distinct"; //$NON-NLS-1$
    protected static final String ALLOWS_ORDERBY = "allows-orderby"; //$NON-NLS-1$
    protected static final String ANALYTIC = "analytic"; //$NON-NLS-1$
    protected static final String CATEGORY = "category"; //$NON-NLS-1$
    protected static final String DECOMPOSABLE = "decomposable"; //$NON-NLS-1$
    protected static final String DESCRIPTION = "description"; //$NON-NLS-1$
    protected static final String DETERMINISM = "determinism"; //$NON-NLS-1$
    protected static final String JAVA_CLASS = "java-class"; //$NON-NLS-1$
    protected static final String JAVA_METHOD = "java-method"; //$NON-NLS-1$
    protected static final String NAME_IN_SOURCE = "name-in-source"; //$NON-NLS-1$
    protected static final String NULL_ON_NULL = "null-on-null"; //$NON-NLS-1$
    protected static final String SCHEMA_ELEMENT_TYPE = "schema-element-type"; //$NON-NLS-1$
    protected static final String UPDATE_COUNT = "update-count"; //$NON-NLS-1$
    protected static final String USES_DISTINCT_ROWS = "uses-distinct-rows"; //$NON-NLS-1$
    protected static final String UUID = "uuid"; //$NON-NLS-1$
    protected static final String VAR_ARGS = "var-args"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { AGGREGATE, ALLOWS_DISTINCT, ALLOWS_ORDERBY,
                                                                                    ANALYTIC, CATEGORY, DECOMPOSABLE, DESCRIPTION,
                                                                                    DETERMINISM, JAVA_CLASS, JAVA_METHOD,
                                                                                    NAME_IN_SOURCE, NULL_ON_NULL,
                                                                                    SCHEMA_ELEMENT_TYPE, UPDATE_COUNT,
                                                                                    USES_DISTINCT_ROWS, UUID, VAR_ARGS } );

    protected UserDefinedFunctionShellCommand( final String name,
                                               final WorkspaceStatus status ) {
        super( status, name );
    }

    protected UserDefinedFunction getUserDefinedFunction() throws Exception {
        return new UserDefinedFunctionImpl( getTransaction(), getRepository(), getPath() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return UserDefinedFunctionImpl.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            // exception returns false
        }
        return false;
    }

    @Override
    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(UserDefinedFunctionCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
     */
    @Override
    public void printHelp( final int indent ) {
        print( indent, Messages.getString( UserDefinedFunctionCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help" ) ); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
     */
    @Override
    public void printUsage( final int indent ) {
        print( indent, Messages.getString( UserDefinedFunctionCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}

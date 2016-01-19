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
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link UserDefinedFunction UserDefinedFunction}-related shell commands.
 */
abstract class UserDefinedFunctionShellCommand extends RelationalShellCommand {

    protected static final String AGGREGATE = "AGGREGATE"; //$NON-NLS-1$
    protected static final String ALLOWS_DISTINCT = "ALLOWS_DISTINCT"; //$NON-NLS-1$
    protected static final String ALLOWS_ORDERBY = "ALLOWS_ORDERBY"; //$NON-NLS-1$
    protected static final String ANALYTIC = "ANALYTIC"; //$NON-NLS-1$
    protected static final String CATEGORY = "CATEGORY"; //$NON-NLS-1$
    protected static final String DECOMPOSABLE = "DECOMPOSABLE"; //$NON-NLS-1$
    protected static final String DESCRIPTION = "ANNOTATION"; //$NON-NLS-1$
    protected static final String DETERMINISM = "DETERMINISM"; //$NON-NLS-1$
    protected static final String JAVA_CLASS = "JAVA_CLASS"; //$NON-NLS-1$
    protected static final String JAVA_METHOD = "JAVA_METHOD"; //$NON-NLS-1$
    protected static final String NAME_IN_SOURCE = "NAMEINSOURCE"; //$NON-NLS-1$
    protected static final String NULL_ON_NULL = "NULL_ON_NULL"; //$NON-NLS-1$
    protected static final String SCHEMA_ELEMENT_TYPE = "schemaElementType"; //$NON-NLS-1$
    protected static final String UPDATE_COUNT = "UPDATECOUNT"; //$NON-NLS-1$
    protected static final String USES_DISTINCT_ROWS = "USES_DISTINCT_ROWS"; //$NON-NLS-1$
    protected static final String UUID = "UUID"; //$NON-NLS-1$
    protected static final String VAR_ARGS = "VARARGS"; //$NON-NLS-1$

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
        assert getContext() instanceof UserDefinedFunction;
        return UserDefinedFunction.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return UserDefinedFunction.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}

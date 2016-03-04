/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.commands.pushdownfunction;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link PushdownFunction pushdown function}-related shell commands.
 */
abstract class PushdownFunctionShellCommand extends RelationalShellCommand {

    protected static final String AGGREGATE = "AGGREGATE"; //$NON-NLS-1$
    protected static final String ALLOWS_DISTINCT = "ALLOWS_DISTINCT"; //$NON-NLS-1$
    protected static final String ALLOWS_ORDERBY = "ALLOWS_ORDERBY"; //$NON-NLS-1$
    protected static final String ANALYTIC = "ANALYTIC"; //$NON-NLS-1$
    protected static final String DECOMPOSABLE = "DECOMPOSABLE"; //$NON-NLS-1$
    protected static final String DESCRIPTION = "ANNOTATION"; //$NON-NLS-1$
    protected static final String DETERMINISM = "DETERMINISM"; //$NON-NLS-1$
    protected static final String NAME_IN_SOURCE = "NAMEINSOURCE"; //$NON-NLS-1$
    protected static final String NULL_ON_NULL = "NULL_ON_NULL"; //$NON-NLS-1$
    protected static final String SCHEMA_ELEMENT_TYPE = "schemaElementType"; //$NON-NLS-1$
    protected static final String UPDATE_COUNT = "UPDATECOUNT"; //$NON-NLS-1$
    protected static final String USES_DISTINCT_ROWS = "USES_DISTINCT_ROWS"; //$NON-NLS-1$
    protected static final String UUID = "UUID"; //$NON-NLS-1$
    protected static final String VAR_ARGS = "VARARGS"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { AGGREGATE, ALLOWS_DISTINCT, ALLOWS_ORDERBY,
                                                                                  ANALYTIC, DECOMPOSABLE, DESCRIPTION, DETERMINISM,
                                                                                  NAME_IN_SOURCE, NULL_ON_NULL, SCHEMA_ELEMENT_TYPE,
                                                                                  UPDATE_COUNT, USES_DISTINCT_ROWS, UUID, VAR_ARGS} );

    protected PushdownFunctionShellCommand( final String name,
                                            final WorkspaceStatus status ) {
        super( status, name );
    }

    protected PushdownFunction getPushdownFunction() throws Exception {
        assert getContext() instanceof PushdownFunction;
        return PushdownFunction.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return PushdownFunction.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}

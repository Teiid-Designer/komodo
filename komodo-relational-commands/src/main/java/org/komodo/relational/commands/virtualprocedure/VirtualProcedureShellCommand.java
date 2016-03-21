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
package org.komodo.relational.commands.virtualprocedure;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.VirtualProcedure;
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
        assert getContext() instanceof VirtualProcedure;
        return VirtualProcedure.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return VirtualProcedure.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}

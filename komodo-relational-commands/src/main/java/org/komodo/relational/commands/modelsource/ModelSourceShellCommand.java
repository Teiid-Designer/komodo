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
package org.komodo.relational.commands.modelsource;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link ModelSource model source}-related shell commands.
 */
abstract class ModelSourceShellCommand extends RelationalShellCommand {

    protected static final String JNDI_NAME = "sourceJndiName"; //$NON-NLS-1$
    protected static final String TRANSLATOR_NAME = "sourceTranslator"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { JNDI_NAME, TRANSLATOR_NAME } );

    protected ModelSourceShellCommand( final String name,
                                       final WorkspaceStatus status ) {
        super( status, name );
    }

    protected ModelSource getModelSource() throws Exception {
        assert getContext() instanceof ModelSource;
        return ModelSource.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return ModelSource.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}

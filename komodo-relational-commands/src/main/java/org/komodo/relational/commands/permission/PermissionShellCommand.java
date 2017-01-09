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
package org.komodo.relational.commands.permission;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.Permission;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link Permission permission}-related shell commands.
 */
abstract class PermissionShellCommand extends RelationalShellCommand {

    protected static final String ALLOW_ALTER = "allowAlter"; //$NON-NLS-1$
    protected static final String ALLOW_CREATE = "allowCreate"; //$NON-NLS-1$
    protected static final String ALLOW_DELETE = "allowDelete"; //$NON-NLS-1$
    protected static final String ALLOW_EXECUTE = "allowExecute"; //$NON-NLS-1$
    protected static final String ALLOW_LANGUAGE = "allowLanguage"; //$NON-NLS-1$
    protected static final String ALLOW_READ = "allowRead"; //$NON-NLS-1$
    protected static final String ALLOW_UPDATE = "allowUpdate"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { ALLOW_ALTER, ALLOW_CREATE, ALLOW_DELETE,
                                                                                    ALLOW_EXECUTE, ALLOW_LANGUAGE, ALLOW_READ,
                                                                                    ALLOW_UPDATE } );

    protected PermissionShellCommand( final String name,
                                      final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Permission getPermission() throws Exception {
        assert getContext() instanceof Permission;
        return Permission.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Permission.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}

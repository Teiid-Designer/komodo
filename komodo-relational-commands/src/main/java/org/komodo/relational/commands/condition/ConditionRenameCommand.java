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
package org.komodo.relational.commands.condition;

import java.util.List;
import org.komodo.relational.vdb.Condition;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.RenameCommand;

/**
 * Since {@link Condition condition} has no children, this command overrides the default {@link RenameCommand} to not allow
 * children to be renamed.
 */
public final class ConditionRenameCommand extends RenameCommand {

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ConditionRenameCommand( final WorkspaceStatus status ) {
        super( status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.commands.RenameCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 1;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        try {
            return Condition.RESOLVER.resolvable( getTransaction(), getContext() );
        } catch ( final Exception ex ) {
            return false;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.commands.RenameCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( String lastArgument,
                                                List< CharSequence > candidates ) throws Exception {
        return TabCompletionModifier.NO_AUTOCOMPLETION;
    }

}

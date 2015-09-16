/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.shell.commands.core;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Properties;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.PrintUtils;

/**
 * ShowGlobalCommand - shows global workspace properties.
 *
 */
public class ShowGlobalCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "show-global"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public ShowGlobalCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

    @Override
    public boolean isValidForCurrentContext() {
        return true;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final Properties globalProperties = getWorkspaceStatus().getProperties();

        // Print properties header
        final String globalPropsHeader = Messages.getString( Messages.ShowGlobalCommand.GlobalPropertiesHeader ); 
        print( MESSAGE_INDENT, globalPropsHeader );

        // Print the properties
        String nameTitle = Messages.getString( SHELL.PROPERTY_NAME_HEADER );
        String valueTitle = Messages.getString( SHELL.PROPERTY_VALUE_HEADER );
        PrintUtils.printProperties(getWorkspaceStatus(), globalProperties, nameTitle, valueTitle);
        
        return true;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#shouldCommit()
     */
    @Override
    protected boolean shouldCommit() {
        return false;
    }

}

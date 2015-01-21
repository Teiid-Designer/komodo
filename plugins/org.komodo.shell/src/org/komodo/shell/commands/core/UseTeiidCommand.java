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
package org.komodo.shell.commands.core;

import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.runtime.TeiidInstance;

/**
 *
 */
public class UseTeiidCommand extends BuiltInShellCommand implements StringConstants {

    private static final String USE_TEIID = "useTeiid"; //$NON-NLS-1$

    /**
     * @param wsStatus
     */
    public UseTeiidCommand(WorkspaceStatus wsStatus) {
        super(USE_TEIID, wsStatus);
    }

    @Override
    public boolean execute() throws Exception {

        Arguments args = getArguments();
        if (args == null || args.isEmpty()) {
            printHelp(0);
            printUsage(0);
            return false;
        }
        
        String nameOrId = requiredArgument(0, Messages.getString("UseTeiidCommand.invalidName")); //$NON-NLS-1$
        WorkspaceStatus wStatus = getWorkspaceStatus();

        Repository repository = wStatus.getCurrentContext().getRepository();
        KomodoObject workspace = repository.komodoWorkspace(null);
        WorkspaceManager wkspManager = WorkspaceManager.getInstance(repository);
        
        List<Teiid> teiids = wkspManager.findTeiids(null, workspace);

        if (teiids == null || teiids.size() == 0) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("UseTeiidCommand.noInstancesDefined")); //$NON-NLS-1$
            return false;
        }

        for (Teiid teiid : teiids) {
            String teiidName = teiid.getName(null);
            if (nameOrId.equals(teiid.getId(null)) || nameOrId.equals(teiidName)) {
                wStatus.setTeiid(teiid);
                TeiidInstance teiidInstance = teiid.getTeiidInstance();

                teiidInstance.connect();
                boolean connected = teiidInstance.isConnected();
                print(CompletionConstants.MESSAGE_INDENT, Messages.getString("UseTeiidCommand.teiidSetOk", teiidName, connected)); //$NON-NLS-1$
                return connected;
            }
        }

        print(CompletionConstants.MESSAGE_INDENT, Messages.getString("UseTeiidCommand.noTeiidWithName", nameOrId)); //$NON-NLS-1$
        return false;
    }
}

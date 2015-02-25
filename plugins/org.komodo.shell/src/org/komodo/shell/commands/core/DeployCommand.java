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

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.runtime.TeiidInstance;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 *
 */
public class DeployCommand extends BuiltInShellCommand implements StringConstants {

    private static final String DEPLOY = "deploy"; //$NON-NLS-1$

    private static final String VDB_TYPE = VdbLexicon.Vdb.VIRTUAL_DATABASE;

    /**
     * @param wsStatus a workspace status (cannot be <code>null</code>)
     */
    public DeployCommand(WorkspaceStatus wsStatus) {
        super(DEPLOY, wsStatus);
    }
    
    @Override
    public boolean execute() throws Exception {
        WorkspaceStatus wsStatus = getWorkspaceStatus();
        WorkspaceContext context = wsStatus.getCurrentContext();
        Arguments args = getArguments();
        KomodoObject vdbToDeploy = null;

        if (args == null || args.isEmpty()) {
            //
            // No arguments so expect us to be inside a vdb
            //
            KomodoObject kObj = context.getKomodoObj();
            Descriptor type = kObj.getPrimaryType(null);
            if (VDB_TYPE.equals(type.getName())) {
                vdbToDeploy = kObj;
            }
        } else {
            //
            // Expect 1 argument that defines the name of the vdb that is a
            // child of where we are currently located
            //
            String childVdbName = requiredArgument(0, Messages.getString("DeployCommand.invalidName")); //$NON-NLS-1$
            WorkspaceContext childContext = context.getChild(childVdbName, VDB_TYPE);
            if (childContext != null) {
                vdbToDeploy = childContext.getKomodoObj();
            }
        }

        Repository repository = wsStatus.getCurrentContext().getRepository();
        WorkspaceManager wkspManager = WorkspaceManager.getInstance(repository);

        Vdb vdb = wkspManager.resolve(null, vdbToDeploy, Vdb.class);
        if (vdb == null)
            throw new InvalidCommandArgumentException(0, Messages.getString("DeployCommand.InvalidCommand")); //$NON-NLS-1$

        Teiid teiid = wsStatus.getTeiid();
        if (teiid == null) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("DeployCommand.noTeiidDefined")); //$NON-NLS-1$
            return false;
        }

        TeiidInstance teiidInstance = teiid.getTeiidInstance();
        if (! teiidInstance.isConnected())
            teiidInstance.connect();

        if (! teiidInstance.isConnected()) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("DeployCommand.noTeiidConnection")); //$NON-NLS-1$
            return false;
        }

        String vdbXml = vdb.export(null);
        if (vdbXml == null || vdbXml.isEmpty()) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("DeployCommand.exportFailure")); //$NON-NLS-1$
            return false;
        }

        InputStream stream = new ByteArrayInputStream(vdbXml.getBytes());
        teiidInstance.deployDynamicVdb(vdb.getName(null), stream);

        return true;
    }
}

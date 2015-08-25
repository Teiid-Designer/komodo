/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for workspace-level commands.
 */
public class WorkspaceCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for workspace shell commands.
     */
    public WorkspaceCommandProvider() {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#provideCommands()
     */
    @Override
    public Map< String, Class< ? extends ShellCommand >> provideCommands() {
        final Map< String, Class< ? extends ShellCommand >> result = new HashMap<>();
        result.put( CreateVdbCommand.NAME, CreateVdbCommand.class );
        result.put( SetCustomPropertyCommand.NAME, SetCustomPropertyCommand.class );

        return result;
    }
    
    @Override
    public WorkspaceManager resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(kObj.getRepository());
        if(wkspMgr.getAbsolutePath().equals(kObj.getAbsolutePath())) {
            return wkspMgr;
        }
        return null;
    }

}

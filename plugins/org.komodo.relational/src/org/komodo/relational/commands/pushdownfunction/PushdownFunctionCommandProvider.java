/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.pushdownfunction;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.internal.PushdownFunctionImpl;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for Pushdown Functions.
 */
public class PushdownFunctionCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for Pushdown Function shell commands.
     */
    public PushdownFunctionCommandProvider() {
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

        result.put( AddParameterCommand.NAME, AddParameterCommand.class );

        result.put( DeleteParameterCommand.NAME, DeleteParameterCommand.class );

        result.put( SetPushdownFunctionPropertyCommand.NAME, SetPushdownFunctionPropertyCommand.class );

        return result;
    }
    
    @Override
    public PushdownFunction resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(PushdownFunctionImpl.RESOLVER.resolvable(uow, kObj)) {
            return PushdownFunctionImpl.RESOLVER.resolve(uow, kObj);
        }
        return null;
    }
    
    @Override
    public boolean isRoot ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(PushdownFunctionImpl.RESOLVER.resolvable(uow, kObj)) {
            return false;
        }
        return false;
    }
    
    @Override
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(PushdownFunctionImpl.RESOLVER.resolvable(uow, kObj)) {
            PushdownFunction func = PushdownFunctionImpl.RESOLVER.resolve(uow, kObj);
            return func.getTypeDisplayName();
        }
        return null;
    }

    /**
     * @throws KException the exception 
     */
    @Override
    public String getStatusMessage ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        return null;
    }
    
    /**
     * @throws KException the exception 
     */
    /* (non-Javadoc)
     * @see org.komodo.shell.api.ShellCommandProvider#initWorkspaceState(org.komodo.shell.api.WorkspaceStatus)
     */
    @Override
    public void initWorkspaceState(WorkspaceStatus wsStatus) throws KException {
        // Init any workspace state
    }

}

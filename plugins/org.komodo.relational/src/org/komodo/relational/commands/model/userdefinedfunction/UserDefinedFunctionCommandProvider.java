/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.model.userdefinedfunction;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.relational.model.internal.UserDefinedFunctionImpl;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for UserDefined Functions.
 */
public class UserDefinedFunctionCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for UserDefined Function shell commands.
     */
    public UserDefinedFunctionCommandProvider() {
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

//        result.put( SetTablePropertyCommand.NAME, SetTablePropertyCommand.class );

        return result;
    }
    
    @Override
    public UserDefinedFunction resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(UserDefinedFunctionImpl.RESOLVER.resolvable(uow, kObj)) {
            return UserDefinedFunctionImpl.RESOLVER.resolve(uow, kObj);
        }
        return null;
    }
    
    @Override
    public boolean isRoot ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(UserDefinedFunctionImpl.RESOLVER.resolvable(uow, kObj)) {
            return false;
        }
        return false;
    }
    
    @Override
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(UserDefinedFunctionImpl.RESOLVER.resolvable(uow, kObj)) {
            UserDefinedFunction func = UserDefinedFunctionImpl.RESOLVER.resolve(uow, kObj);
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

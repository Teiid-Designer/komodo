/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.model;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for Models.
 */
public class ModelCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for Model shell commands.
     */
    public ModelCommandProvider() {
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

        result.put( AddPushdownFunctionCommand.NAME, AddPushdownFunctionCommand.class );
        result.put( AddSourceCommand.NAME, AddSourceCommand.class );
        result.put( AddStoredProcedureCommand.NAME, AddStoredProcedureCommand.class );
        result.put( AddTableCommand.NAME, AddTableCommand.class );
        result.put( AddUserDefinedFunctionCommand.NAME, AddUserDefinedFunctionCommand.class );
        result.put( AddViewCommand.NAME, AddViewCommand.class );
        result.put( AddVirtualProcedureCommand.NAME, AddVirtualProcedureCommand.class );

        result.put( DeletePushdownFunctionCommand.NAME, DeletePushdownFunctionCommand.class );
        result.put( DeleteSourceCommand.NAME, DeleteSourceCommand.class );
        result.put( DeleteStoredProcedureCommand.NAME, DeleteStoredProcedureCommand.class );
        result.put( DeleteTableCommand.NAME, DeleteTableCommand.class );
        result.put( DeleteUserDefinedFunctionCommand.NAME, DeleteUserDefinedFunctionCommand.class );
        result.put( DeleteViewCommand.NAME, DeleteViewCommand.class );
        result.put( DeleteVirtualProcedureCommand.NAME, DeleteVirtualProcedureCommand.class );

        result.put( SetModelPropertyCommand.NAME, SetModelPropertyCommand.class );

        return result;
    }
    
    @Override
    public Model resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(ModelImpl.RESOLVER.resolvable(uow, kObj)) {
            return ModelImpl.RESOLVER.resolve(uow, kObj);
        }
        return null;
    }
    
    @Override
    public boolean isRoot ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(ModelImpl.RESOLVER.resolvable(uow, kObj)) {
            return false;
        }
        return false;
    }
    
    @Override
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(ModelImpl.RESOLVER.resolvable(uow, kObj)) {
            Model model = ModelImpl.RESOLVER.resolve(uow, kObj);
            return model.getTypeDisplayName();
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

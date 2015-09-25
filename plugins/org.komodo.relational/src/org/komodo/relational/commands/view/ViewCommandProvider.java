/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.view;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.model.View;
import org.komodo.relational.model.internal.ViewImpl;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for Tables.
 */
public class ViewCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for View shell commands.
     */
    public ViewCommandProvider() {
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

        result.put( AddColumnCommand.NAME, AddColumnCommand.class );

        result.put( DeleteColumnCommand.NAME, DeleteColumnCommand.class );

        result.put( SetViewPropertyCommand.NAME, SetViewPropertyCommand.class );

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @SuppressWarnings( "unchecked" )
    @Override
    public View resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(ViewImpl.RESOLVER.resolvable(uow, kObj)) {
            return ViewImpl.RESOLVER.resolve(uow, kObj);
        }
        return null;
    }

    @Override
    public boolean isRoot ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(ViewImpl.RESOLVER.resolvable(uow, kObj)) {
            return false;
        }
        return false;
    }

    @Override
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(ViewImpl.RESOLVER.resolvable(uow, kObj)) {
            View view = ViewImpl.RESOLVER.resolve(uow, kObj);
            return view.getTypeDisplayName();
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

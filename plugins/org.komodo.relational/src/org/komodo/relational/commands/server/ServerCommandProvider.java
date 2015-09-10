/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.server;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.teiid.internal.TeiidImpl;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for VDBs.
 */
public class ServerCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for VDB shell commands.
     */
    public ServerCommandProvider() {
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

        result.put( ServerConnectCommand.NAME, ServerConnectCommand.class );
        result.put( ServerDisconnectCommand.NAME, ServerDisconnectCommand.class );
        result.put( ServerSetCommand.NAME, ServerSetCommand.class );
        result.put( ServerShowVdbsCommand.NAME, ServerShowVdbsCommand.class );
        result.put( ServerShowTranslatorsCommand.NAME, ServerShowTranslatorsCommand.class );
        result.put( ServerShowDatasourcesCommand.NAME, ServerShowDatasourcesCommand.class );
        result.put( ServerShowDatasourceTypesCommand.NAME, ServerShowDatasourceTypesCommand.class );

        return result;
    }
    
    @Override
    public Teiid resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(TeiidImpl.RESOLVER.resolvable(uow, kObj)) {
            return TeiidImpl.RESOLVER.resolve(uow, kObj);
        }
        return null;
    }
    
    @Override
    public boolean isRoot ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(TeiidImpl.RESOLVER.resolvable(uow, kObj)) {
            return false;
        }
        return false;
    }
    
    /**
     * @throws KException the exception 
     */
    @Override
    public boolean isServer ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(TeiidImpl.RESOLVER.resolvable(uow, kObj)) {
            return true;
        }
        return false;
    }
    
    @Override
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(TeiidImpl.RESOLVER.resolvable(uow, kObj)) {
            Teiid teiid = TeiidImpl.RESOLVER.resolve(uow, kObj);
            return teiid.getTypeDisplayName();
        }
        return null;
    }

}

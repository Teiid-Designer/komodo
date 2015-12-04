/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.server;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;

/**
 * Common methods used by server commands
 */
public class ServerUtils {
    
    /**
     * Get the teiid object with the supplied name from the workspace
     * @param wsMgr the workspace manager
     * @param wsStatus the workspace status
     * @param serverName the name of the teiid object
     * @return the teiid object
     * @throws KException the exception
     */
    public static Teiid getWorkspaceTeiidObject(WorkspaceManager wsMgr, WorkspaceStatus wsStatus, String serverName) throws KException {
        Teiid resultTeiid = null;
        Teiid[] teiids = wsMgr.findTeiids(wsStatus.getTransaction());

        if (teiids == null || teiids.length == 0) {
            return resultTeiid;
        }

        for (Teiid theTeiid : teiids) {
            String teiidName = theTeiid.getName(wsStatus.getTransaction());
            if (serverName.equals(theTeiid.getId(wsStatus.getTransaction())) || serverName.equals(teiidName)) {
                resultTeiid = theTeiid;
                break;
            }
        }
        return resultTeiid;
    }
    
    /**
     * Return the deployed datasource names from the TeiidInstance
     * @param teiid the Teiid instance
     * @param transaction the transaction
     * @return the collection of data source names
     * @throws Exception the exception
     */
    public static List<String> getDatasourceNames(Teiid teiid, UnitOfWork transaction) throws Exception {
        Collection< TeiidDataSource > sources = teiid.getTeiidInstance(transaction).getDataSources();
        if(sources.isEmpty()) return Collections.EMPTY_LIST;
        
        List< String > existingSourceNames = new ArrayList< String >();
        for ( TeiidDataSource source : sources ) {
            existingSourceNames.add( source.getName() );
        }
        return existingSourceNames;
    }

    /**
     * Return the deployed datasource display names from the TeiidInstance
     * @param teiid the Teiid instance
     * @param transaction the transaction
     * @return the collection of data source display names
     * @throws Exception the exception
     */
    public static List<String> getDatasourceDisplayNames(Teiid teiid, UnitOfWork transaction) throws Exception {
        Collection< TeiidDataSource > sources = teiid.getTeiidInstance(transaction).getDataSources();
        if(sources.isEmpty()) return Collections.EMPTY_LIST;
        
        List< String > existingSourceNames = new ArrayList< String >();
        for ( TeiidDataSource source : sources ) {
            existingSourceNames.add( source.getDisplayName() );
        }
        return existingSourceNames;
    }

    /**
     * Return the deployed VDB names from the TeiidInstance
     * @param teiid the Teiid instance
     * @param transaction the transaction
     * @return the collection of vdb names
     * @throws Exception the exception
     */
    public static List<String> getVdbNames(Teiid teiid, UnitOfWork transaction) throws Exception {
        Collection< TeiidVdb > vdbs = teiid.getTeiidInstance(transaction).getVdbs();
        if(vdbs.isEmpty()) return Collections.EMPTY_LIST;
        
        List< String > existingVdbNames = new ArrayList< String >();
        for ( TeiidVdb vdb : vdbs ) {
            existingVdbNames.add( vdb.getName() );
        }
        return existingVdbNames;
    }

    /**
     * Return the Translator names from the TeiidInstance
     * @param teiid the Teiid instance
     * @param transaction the transaction
     * @return the collection of translator names
     * @throws Exception the exception
     */
    public static List<String> getTranslatorNames(Teiid teiid, UnitOfWork transaction) throws Exception {
        Collection< TeiidTranslator > translators = teiid.getTeiidInstance(transaction).getTranslators();
        if(translators.isEmpty()) return Collections.EMPTY_LIST;
        
        List< String > existingTranslatorNames = new ArrayList< String >();
        for ( TeiidTranslator translator : translators ) {
            existingTranslatorNames.add( translator.getName() );
        }
        return existingTranslatorNames;
    }

}

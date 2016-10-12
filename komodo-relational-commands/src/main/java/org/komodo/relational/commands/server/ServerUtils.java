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
package org.komodo.relational.commands.server;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.utils.StringUtils;

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
     * Determine if the teiid instance has a source type
     * @param teiidInstance the Teiid instance
     * @param sourceType the source type
     * @return 'true' if the type exists on the server, 'false' if not.
     * @throws Exception the exception
     */
    public static boolean hasDatasourceType(TeiidInstance teiidInstance, String sourceType) throws Exception {
        assert( teiidInstance != null );

        // Look for matching name
        Set<String> serverTypes = teiidInstance.getDataSourceTypeNames();
        for(String serverType : serverTypes) {
            if(serverType.equals(sourceType)) {
                return true;
            }
        }

        return false;
    }
    
    /**
     * Return the deployed datasource names from the TeiidInstance
     * @param teiidInstance the Teiid instance
     * @return the collection of data source names
     * @throws Exception the exception
     */
    public static List<String> getDatasourceNames(TeiidInstance teiidInstance) throws Exception {
        assert( teiidInstance != null );

        Collection< TeiidDataSource > sources = teiidInstance.getDataSources();
        if(sources.isEmpty()) return Collections.emptyList();

        List< String > existingSourceNames = new ArrayList< String >();
        for ( TeiidDataSource source : sources ) {
            existingSourceNames.add( source.getName() );
        }
        return existingSourceNames;
    }

    /**
     * Return the deployed datasource display names from the TeiidInstance
     * @param teiidInstance the Teiid instance
     * @return the collection of data source display names
     * @throws Exception the exception
     */
    public static List<String> getDatasourceDisplayNames(TeiidInstance teiidInstance) throws Exception {
        assert( teiidInstance != null );

        Collection< TeiidDataSource > sources = teiidInstance.getDataSources();
        if(sources.isEmpty()) return Collections.emptyList();
        
        List< String > existingSourceNames = new ArrayList< String >();
        for ( TeiidDataSource source : sources ) {
            existingSourceNames.add( source.getDisplayName() );
        }
        return existingSourceNames;
    }

    /**
     * Return the deployed datasource jndi names from the TeiidInstance
     * @param teiidInstance the Teiid instance
     * @return the collection of data source jndi names
     * @throws Exception the exception
     */
    public static List<String> getDatasourceJndiNames(TeiidInstance teiidInstance) throws Exception {
        assert( teiidInstance != null );

        Collection< TeiidDataSource > sources = teiidInstance.getDataSources();
        if(sources.isEmpty()) return Collections.emptyList();
        
        List< String > existingJndiNames = new ArrayList< String >();
        for ( TeiidDataSource source : sources ) {
            String jndiName = source.getPropertyValue(TeiidInstance.DATASOURCE_JNDINAME);
            if(!StringUtils.isEmpty(jndiName)) {
                existingJndiNames.add( jndiName );
            }
        }
        return existingJndiNames;
    }

    /**
     * Return the deployed VDB names from the TeiidInstance
     * @param teiidInstance the Teiid instance
     * @return the collection of vdb names
     * @throws Exception the exception
     */
    public static List<String> getVdbNames(TeiidInstance teiidInstance) throws Exception {
        assert( teiidInstance != null );

        Collection< String > vdbNames = teiidInstance.getVdbNames();
        if(vdbNames.isEmpty()) return Collections.emptyList();
        
        return new ArrayList<>(vdbNames);
    }

    /**
     * Return the Translator names from the TeiidInstance
     * @param teiidInstance the Teiid instance
     * @return the collection of translator names
     * @throws Exception the exception
     */
    public static List<String> getTranslatorNames(TeiidInstance teiidInstance) throws Exception {
        assert( teiidInstance != null );

        Collection< TeiidTranslator > translators = teiidInstance.getTranslators();
        if(translators.isEmpty()) return Collections.emptyList();
        
        List< String > existingTranslatorNames = new ArrayList< String >();
        for ( TeiidTranslator translator : translators ) {
            existingTranslatorNames.add( translator.getName() );
        }
        return existingTranslatorNames;
    }

}

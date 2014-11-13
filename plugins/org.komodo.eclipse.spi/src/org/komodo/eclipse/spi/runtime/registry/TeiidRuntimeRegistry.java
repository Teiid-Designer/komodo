/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.komodo.eclipse.spi.runtime.registry;

import java.sql.Driver;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.osgi.util.NLS;
import org.komodo.eclipse.spi.KEclipseSPIPlugin;
import org.komodo.eclipse.spi.Messages;
import org.komodo.eclipse.spi.registry.AbstractExtensionRegistry;
import org.komodo.spi.query.QueryService;
import org.komodo.spi.runtime.ExecutionAdmin;
import org.komodo.spi.runtime.ExecutionAdminFactory;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.type.DataTypeManager;

/**
 *
 */
public class TeiidRuntimeRegistry extends AbstractExtensionRegistry<TeiidVersion, ExecutionAdminFactory> {
    
    private static final String EXT_POINT_ID = "org.teiid.designer.spi.teiidRuntimeClient"; //$NON-NLS-1$

    private static final String FACTORY_ID = "runtimeFactory"; //$NON-NLS-1$

    private static final String VERSION_ELEMENT_ID = "version"; //$NON-NLS-1$
    
    private static final String MAJOR_ATTRIBUTE_ID = "major"; //$NON-NLS-1$
    
    private static final String MINOR_ATTRIBUTE_ID = "minor"; //$NON-NLS-1$
    
    private static final String MICRO_ATTRIBUTE_ID = "micro"; //$NON-NLS-1$
    
    private static TeiidRuntimeRegistry registry;

    /**
     * Get the singleton instance of this registry
     * 
     * @return singleton {@link TeiidRuntimeRegistry}
     * 
     * @throws Exception
     */
    public static TeiidRuntimeRegistry getInstance() throws Exception {
        if (registry == null) {
            registry = new TeiidRuntimeRegistry();
        }

        return registry;
    }

    private TeiidRuntimeRegistry() throws Exception {
        super(EXT_POINT_ID, FACTORY_ID);
    }

    @Override
    protected void register(IConfigurationElement configurationElement, ExecutionAdminFactory adminFactory) {
        IConfigurationElement[] versions = configurationElement.getChildren(VERSION_ELEMENT_ID);
        for (IConfigurationElement version : versions) {
            String major = version.getAttribute(MAJOR_ATTRIBUTE_ID);
            String minor = version.getAttribute(MINOR_ATTRIBUTE_ID);
            String micro = version.getAttribute(MICRO_ATTRIBUTE_ID);

            TeiidVersion teiidVersion = new DefaultTeiidVersion(major, minor, micro);
            register(teiidVersion, adminFactory);
        }
    }

    /**
     * Get an {@link ExecutionAdminFactory} applicable for the given teiid instance version
     *
     * @param teiidVersion
     *
     * @return instance of {@link ExecutionAdminFactory}
     * @throws Exception
     */
    public ExecutionAdminFactory getExecutionAdminFactory(TeiidVersion teiidVersion) throws Exception {
        ExecutionAdminFactory factory = search(teiidVersion);
        if (factory == null)
            throw new Exception(NLS.bind(Messages.NoExecutionAdminFactory, teiidVersion));

        return factory;
    }

    /**
     * Get an {@link ExecutionAdmin} applicable for the given teiid instance
     * 
     * @param teiidInstance
     * 
     * @return instance of {@link ExecutionAdmin}
     * @throws Exception 
     */
    public ExecutionAdmin getExecutionAdmin(TeiidInstance teiidInstance) throws Exception {
        ExecutionAdminFactory factory = getExecutionAdminFactory(teiidInstance.getVersion());
        return factory.createExecutionAdmin(teiidInstance);
    }

    /**
     * Get the teiid data type manager service
     * 
     * @param teiidVersion
     * 
     * @return instance of {@link DataTypeManager}
     * @throws Exception 
     */
    public DataTypeManager getDataTypeManagerService(TeiidVersion teiidVersion) throws Exception {
        ExecutionAdminFactory factory = search(teiidVersion);
        if (factory == null)
            throw new Exception(NLS.bind(Messages.NoExecutionAdminFactory, teiidVersion));
        
        return factory.getDataTypeManagerService(teiidVersion);
    }

    /**
     * Get the Teiid Driver for the given teiid instance version
     *
     * @param teiidVersion
     *
     * @return the Teiid Driver
     * @throws Exception
     */
    public Driver getTeiidDriver(TeiidVersion teiidVersion) throws Exception {
        ExecutionAdminFactory factory = search(teiidVersion);
        if (factory == null)
            throw new Exception(NLS.bind(Messages.NoExecutionAdminFactory, teiidVersion));

        return factory.getTeiidDriver(teiidVersion);
    }

    /**
     * Get the teiid sql syntax service
     * 
     * @param teiidVersion
     * 
     * @return instance of {@link QueryService}
     * @throws Exception 
     */
    public QueryService getQueryService(TeiidVersion teiidVersion) throws Exception {
        ExecutionAdminFactory factory = search(teiidVersion);
        if (factory == null)
            throw new Exception(NLS.bind(Messages.NoExecutionAdminFactory, teiidVersion));
        
        return factory.getQueryService(teiidVersion);
    }
    
    /**
     * @param teiidVersion
     * @return
     */
    private ExecutionAdminFactory search(TeiidVersion teiidVersion) {
        
        ExecutionAdminFactory factory = getRegistered(teiidVersion);
        if (factory != null)
            return factory;
        
        for (Map.Entry<TeiidVersion, ExecutionAdminFactory> entry : getRegisteredEntries()) {
            TeiidVersion entryVersion = entry.getKey();
            
            if (teiidVersion.compareTo(entryVersion))
                return entry.getValue();
        }
        
        return null;
    }
    
    /**
     * Retrieve all registered teiid versions
     * 
     * @return unmodifiable collection
     */
    public Collection<TeiidVersion> getRegisteredTeiidVersions() {
        return getRegisteredKeys();
    }

    /**
     * Get the ultimate default teiid instance version. This is the provided
     * as the default teiid instance version IF the user has not configured
     * a teiid instance connection nor set the default teiid instance preference.
     *
     * This attempts to derive the latest version of teiid instance from
     * the installed client runtimes but if none, returns the
     * hardcoded default value.
     *
     * @return {@link TeiidVersion} default version
     */
    public static TeiidVersion deriveUltimateDefaultTeiidVersion() {
        TeiidVersion lastTestedDefault = Version.DEFAULT_TEIID_VERSION.get();

        Collection<TeiidVersion> teiidVersions = null;
        try {
            teiidVersions = getInstance().getRegisteredTeiidVersions();
        } catch (Exception ex) {
            KEclipseSPIPlugin.log(ex);
            return lastTestedDefault;
        }

        if (teiidVersions == null || teiidVersions.isEmpty())
            return lastTestedDefault;

        if (teiidVersions.size() == 1)
            return teiidVersions.iterator().next();

        // Find the latest version by sorting the registered client runtime versions
        List<String> items = new ArrayList<String>(teiidVersions.size());
        for (TeiidVersion teiidVersion : teiidVersions) {
            /*
             * Do not offer unreleased and untested versions by default.
             * Does not stop the user choosing such versions but avoids
             * displaying them up-front.
             */
            if (teiidVersion.isGreaterThan(lastTestedDefault))
                continue;

            items.add(teiidVersion.toString());
        }
        Collections.sort(items, Collections.reverseOrder());

        return new DefaultTeiidVersion(items.get(0));
    }
}

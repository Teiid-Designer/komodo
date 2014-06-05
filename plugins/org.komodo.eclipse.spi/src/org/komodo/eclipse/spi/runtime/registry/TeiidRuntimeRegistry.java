/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
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
import org.komodo.spi.query.IQueryService;
import org.komodo.spi.runtime.IExecutionAdmin;
import org.komodo.spi.runtime.IExecutionAdminFactory;
import org.komodo.spi.runtime.ITeiidServer;
import org.komodo.spi.runtime.version.ITeiidServerVersion;
import org.komodo.spi.runtime.version.TeiidServerVersion;
import org.komodo.spi.type.IDataTypeManagerService;

/**
 * @since 1.0
 */
public class TeiidRuntimeRegistry extends AbstractExtensionRegistry<ITeiidServerVersion, IExecutionAdminFactory> {
    
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
    protected void register(IConfigurationElement configurationElement, IExecutionAdminFactory adminFactory) {
        IConfigurationElement[] versions = configurationElement.getChildren(VERSION_ELEMENT_ID);
        for (IConfigurationElement version : versions) {
            String major = version.getAttribute(MAJOR_ATTRIBUTE_ID);
            String minor = version.getAttribute(MINOR_ATTRIBUTE_ID);
            String micro = version.getAttribute(MICRO_ATTRIBUTE_ID);

            ITeiidServerVersion serverVersion = new TeiidServerVersion(major, minor, micro);
            register(serverVersion, adminFactory);
        }
    }

    /**
     * Get an {@link IExecutionAdminFactory} applicable for the given server version
     *
     * @param teiidServerVersion
     *
     * @return instance of {@link IExecutionAdminFactory}
     * @throws Exception
     */
    public IExecutionAdminFactory getExecutionAdminFactory(ITeiidServerVersion teiidServerVersion) throws Exception {
        IExecutionAdminFactory factory = search(teiidServerVersion);
        if (factory == null)
            throw new Exception(NLS.bind(Messages.NoExecutionAdminFactory, teiidServerVersion));

        return factory;
    }

    /**
     * Get an {@link IExecutionAdmin} applicable for the given server
     * 
     * @param teiidServer
     * 
     * @return instance of {@link IExecutionAdmin}
     * @throws Exception 
     */
    public IExecutionAdmin getExecutionAdmin(ITeiidServer teiidServer) throws Exception {
        IExecutionAdminFactory factory = getExecutionAdminFactory(teiidServer.getServerVersion());
        return factory.createExecutionAdmin(teiidServer);
    }

    /**
     * Get the teiid data type manager service
     * 
     * @param teiidServerVersion
     * 
     * @return instance of {@link IDataTypeManagerService}
     * @throws Exception 
     */
    public IDataTypeManagerService getDataTypeManagerService(ITeiidServerVersion teiidServerVersion) throws Exception {
        IExecutionAdminFactory factory = search(teiidServerVersion);
        if (factory == null)
            throw new Exception(NLS.bind(Messages.NoExecutionAdminFactory, teiidServerVersion));
        
        return factory.getDataTypeManagerService(teiidServerVersion);
    }

    /**
     * Get the Teiid Driver for the given server version
     *
     * @param teiidServerVersion
     *
     * @return the Teiid Driver
     * @throws Exception
     */
    public Driver getTeiidDriver(ITeiidServerVersion teiidServerVersion) throws Exception {
        IExecutionAdminFactory factory = search(teiidServerVersion);
        if (factory == null)
            throw new Exception(NLS.bind(Messages.NoExecutionAdminFactory, teiidServerVersion));

        return factory.getTeiidDriver(teiidServerVersion);
    }

    /**
     * Get the teiid sql syntax service
     * 
     * @param teiidServerVersion
     * 
     * @return instance of {@link IQueryService}
     * @throws Exception 
     */
    public IQueryService getQueryService(ITeiidServerVersion teiidServerVersion) throws Exception {
        IExecutionAdminFactory factory = search(teiidServerVersion);
        if (factory == null)
            throw new Exception(NLS.bind(Messages.NoExecutionAdminFactory, teiidServerVersion));
        
        return factory.getQueryService(teiidServerVersion);
    }
    
    /**
     * @param serverVersion
     * @return
     */
    private IExecutionAdminFactory search(ITeiidServerVersion serverVersion) {
        
        IExecutionAdminFactory factory = getRegistered(serverVersion);
        if (factory != null)
            return factory;
        
        for (Map.Entry<ITeiidServerVersion, IExecutionAdminFactory> entry : getRegisteredEntries()) {
            ITeiidServerVersion entryVersion = entry.getKey();
            
            if (serverVersion.compareTo(entryVersion))
                return entry.getValue();
        }
        
        return null;
    }
    
    /**
     * Retrieve all registered server versions
     * 
     * @return unmodifiable collection
     */
    public Collection<ITeiidServerVersion> getRegisteredServerVersions() {
        return getRegisteredKeys();
    }

    /**
     * Get the ultimate default teiid instance version. This is the provided
     * as the default teiid instance version IF the user has not configured
     * a server connection nor set the default teiid instance preference.
     *
     * This attempts to derive the latest version of server from
     * the installed client runtimes but if none, returns the
     * hardcoded default value.
     *
     * @return {@link ITeiidServerVersion} default version
     */
    public static ITeiidServerVersion deriveUltimateDefaultServerVersion() {
        ITeiidServerVersion lastTestedDefault = TeiidServerVersion.DEFAULT_TEIID_SERVER;

        Collection<ITeiidServerVersion> serverVersions = null;
        try {
            serverVersions = getInstance().getRegisteredServerVersions();
        } catch (Exception ex) {
            KEclipseSPIPlugin.log(ex);
            return lastTestedDefault;
        }

        if (serverVersions == null || serverVersions.isEmpty())
            return lastTestedDefault;

        if (serverVersions.size() == 1)
            return serverVersions.iterator().next();

        // Find the latest server version by sorting the registered client runtime versions
        List<String> items = new ArrayList<String>(serverVersions.size());
        for (ITeiidServerVersion serverVersion : serverVersions) {
            /*
             * Do not offer unreleased and untested versions by default.
             * Does not stop the user choosing such versions but avoids
             * displaying them up-front.
             */
            if (serverVersion.isGreaterThan(lastTestedDefault))
                continue;

            items.add(serverVersion.toString());
        }
        Collections.sort(items, Collections.reverseOrder());

        return new TeiidServerVersion(items.get(0));
    }
}

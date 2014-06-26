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
import org.komodo.spi.runtime.ITeiidInstance;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.komodo.spi.type.IDataTypeManagerService;

/**
 *
 */
public class TeiidRuntimeRegistry extends AbstractExtensionRegistry<ITeiidVersion, IExecutionAdminFactory> {
    
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

            ITeiidVersion teiidVersion = new TeiidVersion(major, minor, micro);
            register(teiidVersion, adminFactory);
        }
    }

    /**
     * Get an {@link IExecutionAdminFactory} applicable for the given teiid instance version
     *
     * @param teiidVersion
     *
     * @return instance of {@link IExecutionAdminFactory}
     * @throws Exception
     */
    public IExecutionAdminFactory getExecutionAdminFactory(ITeiidVersion teiidVersion) throws Exception {
        IExecutionAdminFactory factory = search(teiidVersion);
        if (factory == null)
            throw new Exception(NLS.bind(Messages.NoExecutionAdminFactory, teiidVersion));

        return factory;
    }

    /**
     * Get an {@link IExecutionAdmin} applicable for the given teiid instance
     * 
     * @param teiidInstance
     * 
     * @return instance of {@link IExecutionAdmin}
     * @throws Exception 
     */
    public IExecutionAdmin getExecutionAdmin(ITeiidInstance teiidInstance) throws Exception {
        IExecutionAdminFactory factory = getExecutionAdminFactory(teiidInstance.getVersion());
        return factory.createExecutionAdmin(teiidInstance);
    }

    /**
     * Get the teiid data type manager service
     * 
     * @param teiidVersion
     * 
     * @return instance of {@link IDataTypeManagerService}
     * @throws Exception 
     */
    public IDataTypeManagerService getDataTypeManagerService(ITeiidVersion teiidVersion) throws Exception {
        IExecutionAdminFactory factory = search(teiidVersion);
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
    public Driver getTeiidDriver(ITeiidVersion teiidVersion) throws Exception {
        IExecutionAdminFactory factory = search(teiidVersion);
        if (factory == null)
            throw new Exception(NLS.bind(Messages.NoExecutionAdminFactory, teiidVersion));

        return factory.getTeiidDriver(teiidVersion);
    }

    /**
     * Get the teiid sql syntax service
     * 
     * @param teiidVersion
     * 
     * @return instance of {@link IQueryService}
     * @throws Exception 
     */
    public IQueryService getQueryService(ITeiidVersion teiidVersion) throws Exception {
        IExecutionAdminFactory factory = search(teiidVersion);
        if (factory == null)
            throw new Exception(NLS.bind(Messages.NoExecutionAdminFactory, teiidVersion));
        
        return factory.getQueryService(teiidVersion);
    }
    
    /**
     * @param teiidVersion
     * @return
     */
    private IExecutionAdminFactory search(ITeiidVersion teiidVersion) {
        
        IExecutionAdminFactory factory = getRegistered(teiidVersion);
        if (factory != null)
            return factory;
        
        for (Map.Entry<ITeiidVersion, IExecutionAdminFactory> entry : getRegisteredEntries()) {
            ITeiidVersion entryVersion = entry.getKey();
            
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
    public Collection<ITeiidVersion> getRegisteredTeiidVersions() {
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
     * @return {@link ITeiidVersion} default version
     */
    public static ITeiidVersion deriveUltimateDefaultTeiidVersion() {
        ITeiidVersion lastTestedDefault = Version.DEFAULT_TEIID_VERSION.get();

        Collection<ITeiidVersion> teiidVersions = null;
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
        for (ITeiidVersion teiidVersion : teiidVersions) {
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

        return new TeiidVersion(items.get(0));
    }
}

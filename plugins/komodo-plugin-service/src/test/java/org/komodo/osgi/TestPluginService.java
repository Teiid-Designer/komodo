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
package org.komodo.osgi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.net.URL;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import org.junit.Test;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.storage.StorageConnector;
import org.komodo.spi.storage.StorageConnector.Descriptor;
import org.komodo.spi.storage.StorageService;
import org.osgi.framework.Bundle;

public class TestPluginService extends AbstractTestPluginService implements StringConstants {

    private static final String TEST_BUNDLES_DIR = "testBundles";

    private static final String HELLO_PATH = TEST_BUNDLES_DIR + File.separator + "hello-1.0.jar";

    protected void installTestBundle() throws Exception {
        URL helloBundleUrl = getClass().getClassLoader().getResource(HELLO_PATH);
        service.installBundle(helloBundleUrl);
    }

    @Test
    public void testPluginServiceBasics() throws Exception {
        assertEquals(Bundle.ACTIVE, service.getState());

        assertEquals(bundleCount() + 1, service.installedBundles().size());

        service.shutdown();

        // Should be resolved but not active
        assertEquals(Bundle.RESOLVED, service.getState());
    }

    @Test
    public void testBundleInstallation() throws Exception {
        assertEquals(Bundle.ACTIVE, service.getState());

        installTestBundle();

        List<String> bundles = service.installedBundles();
        assertEquals(bundleCount() + 2, bundles.size()); // contains framework bundle, hello and teiid bundles

        boolean hasHello = false;
        for (String bundle : bundles) {
            if (bundle.equals("Hello")) {
                hasHello = true;
                break;
            }
        }

        assertTrue(hasHello);
    }

    @Test
    public void testBundleStartingStopping() throws Exception {
        assertEquals(Bundle.ACTIVE, service.getState());

        installTestBundle();

        String bundleName = "Hello";
        service.startBundle(bundleName);
        assertEquals(Bundle.ACTIVE, service.bundleState(bundleName));

        service.stopBundle(bundleName);
        assertEquals(Bundle.RESOLVED, service.bundleState(bundleName));
    }

    @Test
    public void testGetGitStorageConnector()  throws Exception {
        assertEquals(Bundle.ACTIVE, service.getState());

        String storageType = "git";
        Set<String> storageTypes = service.getSupportedStorageTypes();
        assertTrue(storageTypes.contains(storageType));

        StorageService storageService = service.getStorageService(storageType);
        assertNotNull(storageService);

        Properties parameters = new Properties();
        parameters.setProperty("repo-path-property", "http://github.com/test/blob.git");
        StorageConnector connector = storageService.getConnector(parameters);
        assertNotNull(connector);

        Set<Descriptor> descriptors = connector.getDescriptors();
        assertNotNull(descriptors);
        assertTrue(descriptors.size() > 0);
    }
}

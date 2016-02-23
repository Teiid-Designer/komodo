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

import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.Set;
import javax.jcr.Node;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;
import org.komodo.osgi.teiid.UnsupportedTeiidException;
import org.komodo.spi.annotation.AnnotationUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.DataTypeManager;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KEnvironment;
import org.modeshape.jcr.api.Session;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.launch.Framework;
import org.osgi.framework.launch.FrameworkFactory;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

public class PluginService implements StringConstants {

    private static final String VERSION_PREFIX = ";version=";

    private static final String INDEX_FILENAME = "index.xml";

    public static final String INDEX = "bundles" + FORWARD_SLASH + INDEX_FILENAME;

    public static final String PLUGIN_DIRECTORY_PROPERTY = "plugin.service.directory";

    static {
        KEnvironment.checkDataDirProperty();
    }

    private static PluginService instance;

    public static PluginService getInstance() throws Exception {
        if (instance == null)
            instance = new PluginService();

        return instance;
    }

    private String pluginServiceDir = System.getProperty(SystemConstants.ENGINE_DATA_DIR) + File.separator + "plugin-service"; 
    private String configDir = pluginServiceDir + File.separator + "configuration";
    private String cacheDir = pluginServiceDir + File.separator + "cache";

    private final Framework framework;

    private PluginServiceTracker tracker;

    private TeiidService teiidService;

    private Map<TeiidVersion, String> bundleIndex = new HashMap<>();

    private PluginService() throws Exception {
        createDirectory(pluginServiceDir);
        createDirectory(configDir);
        createDirectory(cacheDir);

        FrameworkFactory frameworkFactory = ServiceLoader.load(FrameworkFactory.class).iterator().next();
        Map<String, String> config = new HashMap<String, String>();

        config.put("osgi.console", "");
        config.put("osgi.clean", "true");
        config.put("osgi.bundles.defaultStartLevel", "4");
        config.put("osgi.configuration.area", configDir);
        config.put("org.osgi.framework.storage", cacheDir);
        config.put("org.osgi.framework.storage.clean", "onFirstInit");

        // Add the packages to export to the osgi plugins
        StringBuffer exportedPkgs = exportPackages();
        config.put("org.osgi.framework.system.packages.extra", exportedPkgs.toString());

        framework = frameworkFactory.newFramework(config);
        start();
    }

    private StringBuffer exportPackages() {
        StringBuffer pkgs = new StringBuffer();

        pkgs.append(StringConstants.class.getPackage().getName()).append(COMMA);
        pkgs.append(TeiidService.class.getPackage().getName()).append(COMMA);
        pkgs.append(TeiidInstance.class.getPackage().getName()).append(COMMA);
        pkgs.append(TeiidVersion.class.getPackage().getName()).append(COMMA);
        pkgs.append(DataTypeManager.class.getPackage().getName()).append(COMMA);
        pkgs.append(AnnotationUtils.class.getPackage().getName()).append(COMMA);
        pkgs.append(TeiidSqlLexicon.class.getPackage().getName()).append(COMMA);

        //
        // 3rd party dependencies in the bundles get wired up with a version requirement
        // so need to provide the version pragma with these packages to match
        //
        VersionProvider provider = VersionProvider.getInstance();
        pkgs.append(Session.class.getPackage().getName())
                .append(VERSION_PREFIX)
                .append(SPEECH_MARK)
                .append(provider.getModeshapeVersion())
                .append(SPEECH_MARK)
                .append(COMMA);

        // Need to add in javax.jcr.Node support but also need to specify its version
        // for apache felix to wire it up correctly.
        pkgs.append(Node.class.getPackage().getName())
                .append(VERSION_PREFIX)
                .append(SPEECH_MARK)
                .append(provider.getJcrVersion())
                .append(SPEECH_MARK);

        return pkgs;
    }

    private void createDirectory(String path) {
        File dir = new File(path);
        if (dir.exists())
            return;

        dir.mkdirs();
    }

    private Bundle findBundleBySymbolicName(String symbolicName) throws Exception {
        Bundle[] bundles = framework.getBundleContext().getBundles();
        if (bundles == null || bundles.length == 0)
            return null;

        Bundle theBundle = null;
        for (Bundle bundle : bundles) {
            if (bundle.getSymbolicName().equals(symbolicName)) {
                theBundle = bundle;
                break;
            }
        }
        return theBundle;
    }

    private String searchBundleIndex(TeiidVersion teiidVersion) {
        String bundleName = bundleIndex.get(teiidVersion);
        if (bundleName != null)
            return bundleName;

        teiidVersion = new DefaultTeiidVersion(teiidVersion.getMajor(),
                                                                               teiidVersion.getMinor(),
                                                                               TeiidVersion.WILDCARD);
        return bundleIndex.get(teiidVersion);
    }

    /**
     * @return the set of supported Teiid versions
     */
    public Set<TeiidVersion> getSupportedTeiidVersions() {
        return Collections.unmodifiableSet(bundleIndex.keySet());
    }

    BundleContext getContext() {
         return framework.getBundleContext(); 
    }

    /**
     * @return the state of this service
     */
    public int getState() {
        if (framework == null)
            return Bundle.UNINSTALLED;

        return framework.getState();
    }

    /**
     * Start the plugin service
     *
     * @throws Exception
     */
    public void start() throws Exception {
        ArgCheck.isNotNull(framework);

        if (getState() == Bundle.ACTIVE)
            return;

        framework.start();

        this.tracker = new PluginServiceTracker(this);
        this.tracker.open();

        installBundles();
    }

    //
    // Find the index file and read its contents to get the available bundles
    //
    private void installBundles() throws Exception {
        URL idxUrl = getClass().getClassLoader().getResource(INDEX);

        DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder();
        InputStream idxUrlStream = null;
        try {
            idxUrlStream = idxUrl.openStream();

            Document doc = docBuilder.parse(idxUrlStream);

            // Use XPath to locate the teiid jar filenames
            XPathFactory xpathFactory = XPathFactory.newInstance();
            XPath xpath = xpathFactory.newXPath();

            // XPath expression to find all the teiid jar filenames
            XPathExpression expr = xpath.compile("/teiid/filename/text()");

            //evaluate expression result on XML document
            NodeList nodes = (NodeList)expr.evaluate(doc, XPathConstants.NODESET);
            for (int i = 0; i < nodes.getLength(); i++) {
                String bundleName = nodes.item(i).getNodeValue();

                // Generate the bundle location url and attempt to the install
                URL bundleUrl = new URL(idxUrl.toString().replaceAll(INDEX_FILENAME, bundleName));
                installBundle(bundleUrl);
            }
        } finally {
            if (idxUrlStream != null)
                idxUrlStream.close();
        }
    }

    /**
     * Shutdown the plugin service
     *
     * @throws Exception
     */
    public void shutdown() throws Exception {
        if (framework == null)
            return;

        if (getState() <= Bundle.INSTALLED)
            return;

        this.tracker.close();

        framework.stop();

        // Block until framework stops or timeout
        framework.waitForStop(30000);
    }

    /**
     * Install bundle located at the given url
     *
     * @param bundleUrl
     * @throws Exception
     */
    public void installBundle(URL bundleUrl) throws Exception {
        if (bundleUrl == null)
            return; // nothing to do

        BundleContext context = framework.getBundleContext();

        InputStream bundleStream = bundleUrl.openStream();
        Bundle bundle = context.installBundle(bundleUrl.toString(), bundleStream);

        Dictionary<String,String> headers = bundle.getHeaders();
        Enumeration<String> keys = headers.keys();
        while(keys.hasMoreElements()) {
            String key = keys.nextElement();
            if (!TeiidService.VERSION_PROPERTY.equals(key))
                continue;

            // This bundle is a teiid service bundle
            String value = headers.get(key);
            TeiidVersion teiidVersion = new DefaultTeiidVersion(value);
            bundleIndex.put(teiidVersion, bundle.getSymbolicName());

            // Add a fallback
            teiidVersion = new DefaultTeiidVersion(teiidVersion.getMajor(),
                                                                                   teiidVersion.getMinor(),
                                                                                   TeiidVersion.WILDCARD);
            bundleIndex.put(teiidVersion, bundle.getSymbolicName());
        }
    }

    /**
     * @return a list of all bundles currently installed
     */
    public List<String> installedBundles() {
        Bundle[] bundles = framework.getBundleContext().getBundles();
        if (bundles == null || bundles.length == 0)
            return Collections.emptyList();

        List<String> names = new ArrayList<>(bundles.length);
        for (Bundle bundle : bundles) {
            names.add(bundle.getSymbolicName());
        }

        return names;
    }

    /**
     * @param symbolicName
     * @return the state of the bundle with the given symbolic name
     * @throws Exception
     */
    public int bundleState(String symbolicName) throws Exception {
        Bundle bundle = findBundleBySymbolicName(symbolicName);
        if (bundle == null)
            throw new Exception(Messages.getString(Messages.PluginService.BundleNotFound, symbolicName));

        return bundle.getState();
    }

    /**
     * Start the bundle with the symbolic name
     *
     * @param symbolicName
     * @throws Exception
     */
    public void startBundle(String symbolicName) throws Exception {
        Bundle bundle = findBundleBySymbolicName(symbolicName);
        if (bundle == null)
            throw new Exception(Messages.getString(Messages.PluginService.BundleNotFound, symbolicName));

        if(bundle.getState() == Bundle.ACTIVE)
            return;

        if (bundle.getHeaders().get(Constants.FRAGMENT_HOST) != null)
            throw new Exception(Messages.getString(Messages.PluginService.BundleFragmentStartError, symbolicName));

        bundle.start();
    }

    /**
     * Stop the bundle with the symbolic name
     *
     * @param symbolicName
     * @throws Exception
     */
    public void stopBundle(String symbolicName) throws Exception {
        Bundle bundle = findBundleBySymbolicName(symbolicName);
        if (bundle == null)
            throw new Exception(Messages.getString(Messages.PluginService.BundleNotFound, symbolicName));

        if(bundle.getState() < Bundle.ACTIVE)
            return;

        bundle.stop();
    }

    public TeiidService getTeiidService() {
        return teiidService;
    }

    public TeiidService getTeiidService(TeiidVersion version) throws Exception {
        if (teiidService != null) {
            if (teiidService.getVersion().equals(version))
                return teiidService;

            //
            // teiid service is not the appropriate version so stop its parent bundle
            //
            String parentBundleName = teiidService.getParentBundle();

            // Once the bundle has stopped, the PluginServiceTracker
            // should null the teiidService field
            stopBundle(parentBundleName);

            if (getTeiidService() != null)
                throw new Exception(Messages.getString(Messages.PluginService.TeiidServiceBundleFailedToStop, parentBundleName));
        }

        String symbolicName = searchBundleIndex(version);
        if (symbolicName == null)
            throw new UnsupportedTeiidException(version);

        Bundle bundle = findBundleBySymbolicName(symbolicName);
        if (bundle == null)
            throw new UnsupportedTeiidException(version);

        // Once the bundle has become active, the PluginServiceTracker
        // should assign the TeiidService back to this service
        startBundle(symbolicName);

        return this.teiidService;
    }

    void setTeiidService(TeiidService teiidService) {
        this.teiidService = teiidService;
    }
}

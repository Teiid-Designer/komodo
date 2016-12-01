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
import java.util.concurrent.TimeUnit;
import javax.activation.MimeType;
import javax.crypto.Cipher;
import javax.crypto.spec.PSource;
import javax.jcr.Node;
import javax.management.JMException;
import javax.management.remote.JMXConnector;
import javax.naming.Binding;
import javax.naming.directory.SearchControls;
import javax.naming.ldap.LdapName;
import javax.net.SocketFactory;
import javax.net.ssl.SSLContext;
import javax.script.ScriptEngineManager;
import javax.security.auth.SubjectDomainCombiner;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.login.LoginContext;
import javax.security.auth.spi.LoginModule;
import javax.security.auth.x500.X500Principal;
import javax.security.sasl.Sasl;
import javax.sql.RowSet;
import javax.sql.rowset.serial.SerialArray;
import javax.transaction.Transaction;
import javax.transaction.xa.Xid;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.stream.events.XMLEvent;
import javax.xml.stream.util.XMLEventConsumer;
import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMLocator;
import javax.xml.transform.sax.SAXTransformerFactory;
import javax.xml.transform.stax.StAXSource;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.SchemaFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;
import org.komodo.osgi.storage.StorageServiceProvider;
import org.komodo.plugin.framework.AbstractBundleService;
import org.komodo.spi.KException;
import org.komodo.spi.annotation.AnnotationUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.storage.StorageConnector;
import org.komodo.spi.storage.StorageService;
import org.komodo.spi.type.DataTypeManager;
import org.komodo.spi.uuid.WorkspaceUUIDService;
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
import org.xml.sax.XMLReader;
import org.xml.sax.ext.DefaultHandler2;
import org.xml.sax.helpers.XMLReaderFactory;

public class PluginService implements StringConstants {

    public static final String INDEX_BUNDLE_PATH = "/bundles/bundle/filename/text()";

    private static final String VERSION_PREFIX = ";version=";

    private static final String INDEX_FILENAME = "index.xml";

    public static final String INDEX = "bundles" + FORWARD_SLASH + INDEX_FILENAME;

    public static final String PLUGIN_DIRECTORY_PROPERTY = "plugin.service.directory";

    static {
        KEnvironment.checkDataDirProperty();
    }

    private static final Object lock = new Object();

    private static PluginService instance;

    //
    // Double check synchronization to protect felix bundle cache
    // from attempts to start it multiple times from concurrent threads
    //
    public static PluginService getInstance() throws Exception {
        PluginService service = instance;
        if (service == null) {
            synchronized (lock) {
                // While we were waiting for the lock, another
                // thread may have instantiated the object.
                service = instance;
                if (service == null) {
                    service = new PluginService();
                    instance = service;
                }
            }
        }

        return instance;
    }

    private String pluginServiceDir = System.getProperty(SystemConstants.ENGINE_DATA_DIR) + File.separator + "plugin-service"; 
    private String configDir = pluginServiceDir + File.separator + "configuration";
    private String cacheDir = pluginServiceDir + File.separator + "cache";

    private final Framework framework;

    private StorageServiceProvider storageServiceProvider;

    private int cacheExpirationValue = 10;

    private TimeUnit cacheExpirationUnits = TimeUnit.MINUTES;

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

    private String pkg(Class<?> klazz) {
        return klazz.getPackage().getName();
    }

    private StringBuffer exportPackages() {
        VersionProvider provider = VersionProvider.getInstance();
        StringBuffer pkgs = new StringBuffer();

        Class<?>[] classes = new Class<?>[] {
            // org.komodo.spi
            KException.class,
            // org.komodo.spi.constants
            StringConstants.class,
            // org.komodo.spi.query
            TeiidService.class,
            // org.komodo.spi.runtime
            TeiidInstance.class,
            // org.komodo.spi.runtime.version
            TeiidVersion.class,
            // org.komodo.spi.type
            DataTypeManager.class,
            // org.komodo.spi.annotation
            AnnotationUtils.class,
            // org.komodo.spi.lexicon
            TeiidSqlLexicon.class,
            // org.komodo.spi.outcome
            Outcome.class,
            // org.komodo.spi.uuid
            WorkspaceUUIDService.class,
            // org.komodo.spi.storage
            StorageConnector.class,
            // org.komodo.spi.repository
            Exportable.class,

            /**
             * plugin framework
             */
            // org.komodo.plugin.framework
            AbstractBundleService.class,

            /**
             * utils
             */
            // org.komodo.utils
            ArgCheck.class,

            /**
             * The javax libraries are not exported by default in osgi
             * so need to specify the packages needed by teiid
             */
            // javax.activation
            MimeType.class,
            // javax.crypto
            Cipher.class,
            // javax.crypto.spec
            PSource.class,
            // javax.management
            JMException.class,
            // javax.management.remote
            JMXConnector.class,
            // javax.naming
            Binding.class,
            // javax.naming.directory
            SearchControls.class,
            // javax.naming.ldap
            LdapName.class,
            // javax.net
            SocketFactory.class,
            // javax.net.ssl
            SSLContext.class,
            // javax.script
            ScriptEngineManager.class,
            // javax.security.auth
            SubjectDomainCombiner.class,
            // javax.security.auth.callback
            CallbackHandler.class,
            // javax.security.auth.login
            LoginContext.class,
            // javax.security.auth.spi
            LoginModule.class,
            // javax.security.auth.x500
            X500Principal.class,
            // javax.security.sasl
            Sasl.class,
            // javax.sql
            RowSet.class,
            // javax.sql.rowset.serial
            SerialArray.class,
            // javax.xml.datatype
            DatatypeFactory.class,
            // javax.xml.namespace
            QName.class,
            // javax.xml.parsers
            DocumentBuilder.class,
            // javax.xml.transform
            Transformer.class,
            // javax.xml.transform.dom
            DOMLocator.class,
            // javax.xml.transform.sax
            SAXTransformerFactory.class,
            // javax.xml.transform.stax
            StAXSource.class,
            // javax.xml.transform.stream
            StreamSource.class,
            // javax.xml.validation
            SchemaFactory.class,
            // javax.xml.xpath
            XPath.class,

            /**
             * The org.xml libraries are not exported by default in osgi
             * so need to specify the packages needed by teiid
             */
            // org.xml.sax
            XMLReader.class,
            // org.xml.sax.ext
            DefaultHandler2.class,
            // org.xml.sax.helpers
            XMLReaderFactory.class
        };

        for (Class<?> klazz : classes) {
            pkgs.append(pkg(klazz)).append(COMMA);
        }

        //
        // javax.xml.stream classes need a requirement version
        // even though they are being provided by the jre
        //
        Class<?>[] streamClasses = new Class<?>[] {
            // javax.xml.stream
            XMLStreamWriter.class,
            // javax.xml.stream.events
            XMLEvent.class,
            // javax.xml.stream.util
            XMLEventConsumer.class
        };

        pkgs.append(appendVersionedPackages(provider.getJavaxXmlStreamVersion(), streamClasses));

        //
        // javax.xml.stream classes need a requirement version
        // even though they are being provided by the jre
        //
        Class<?>[] txClasses = new Class<?>[] {
            // javax.transaction
            Transaction.class,
            // javax.transaction.xa
            Xid.class
        };

        pkgs.append(appendVersionedPackages(provider.getJavaxTransactionVersion(), txClasses));

        //
        // 3rd party dependencies in the bundles get wired up with a version requirement
        // so need to provide the version pragma with these packages to match
        //
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

    private String appendVersionedPackages(String version, Class<?>[] pkgClasses) {
        StringBuffer pkgs = new StringBuffer();
        for (Class<?> klazz : pkgClasses) {
            pkgs.append(pkg(klazz))
                    .append(VERSION_PREFIX)
                    .append(SPEECH_MARK)
                    .append(version)
                    .append(SPEECH_MARK)
                    .append(COMMA);
        }

        return pkgs.toString();
    }

    private void createDirectory(String path) {
        File dir = new File(path);
        if (dir.exists())
            return;

        dir.mkdirs();
    }

    private boolean isActive() {
        return getState() == Bundle.ACTIVE;
    }

    /**
     * @param symbolicName
     * @return the bundle with the given symbolic name or <code>null</code>
     * @throws Exception
     */
    public Bundle findBundleBySymbolicName(String symbolicName) throws Exception {
        if (!isActive())
            throw new Exception(Messages.getString(Messages.PluginService.ServiceNotStarted));
 
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

    /**
     * @return the bundle context of the framework
     */
    public BundleContext getContext() {
         return framework.getBundleContext(); 
    }

    /**
     * @return the state of this service
     */
    public int getState() {
        return framework.getState();
    }

    /**
     * Start the plugin service
     *
     * @throws Exception
     */
    public void start() throws Exception {
        if (isActive())
            return;

        if (getState() == Bundle.ACTIVE)
            return;

        framework.start();

        this.storageServiceProvider = new StorageServiceProvider(this);
        this.storageServiceProvider.open();

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
            XPathExpression expr = xpath.compile(INDEX_BUNDLE_PATH);

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
        if (!isActive())
            return;

        if (getState() <= Bundle.INSTALLED)
            return;

        if (storageServiceProvider != null)
            storageServiceProvider.dispose();

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
        if (!isActive())
            throw new Exception(Messages.getString(Messages.PluginService.ServiceNotStarted));

        if (bundleUrl == null)
            return; // nothing to do

        BundleContext context = framework.getBundleContext();

        InputStream bundleStream = bundleUrl.openStream();
        Bundle bundle = context.installBundle(bundleUrl.toString(), bundleStream);

        Dictionary<String,String> headers = bundle.getHeaders();
        Enumeration<String> keys = headers.keys();
        while(keys.hasMoreElements()) {
            String key = keys.nextElement();
            if (StorageService.STORAGE_ID_PROPERTY.equals(key)) {
                storageServiceProvider.register(headers.get(key), bundle.getSymbolicName());
                break;
            }
        }
    }

    /**
     * @return a list of all bundles currently installed
     */
    public List<String> installedBundles() throws Exception {
        if (!isActive())
            throw new Exception(Messages.getString(Messages.PluginService.ServiceNotStarted));

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

    /**
     * @return the set of supported storage services
     */
    public Set<String> getSupportedStorageTypes() throws Exception {
        if (!isActive())
            throw new Exception(Messages.getString(Messages.PluginService.ServiceNotStarted));

        return storageServiceProvider.getSupportedStorageTypes();
    }

    /**
     * @param storageType
     * @return the storage service for the given storage type
     * @throws Exception
     */
    public synchronized StorageService getStorageService(String storageType) throws Exception {
        if (!isActive())
            throw new Exception(Messages.getString(Messages.PluginService.ServiceNotStarted));

        return storageServiceProvider.getStorageService(storageType);
    }

    public int getCacheExpirationValue() {
        return cacheExpirationValue;
    }

    public void setCacheExpirationValue(int value) throws Exception {
        if (isActive())
            throw new Exception(Messages.getString(Messages.PluginService.CannotModifyCache));

        cacheExpirationValue = value;
    }

    public TimeUnit getCacheExpirationUnits() {
        return cacheExpirationUnits;
    }

    public void setCacheExpirationUnits(TimeUnit units) throws Exception {
        if (isActive())
            throw new Exception(Messages.getString(Messages.PluginService.CannotModifyCache));

        cacheExpirationUnits = units;
    }
}

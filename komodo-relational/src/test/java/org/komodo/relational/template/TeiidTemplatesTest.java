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
package org.komodo.relational.template;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.InputStream;
import java.util.Collection;
import java.util.List;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.ServerManager;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;

@RunWith( Arquillian.class )
public class TeiidTemplatesTest extends RelationalModelTest {

    private static String MYSQL_DRIVER = "mysql-connector";

    private TeiidInstance teiidInstance;

    private TeiidVersion getVersion() {
        return DefaultTeiidVersion.Version.DEFAULT_TEIID_VERSION.get();
    }

    public TeiidInstance getTeiidInstance() {
        return teiidInstance;
    }

    private synchronized Teiid getDefaultTeiid() throws Exception {
        ServerManager serverManager = ServerManager.getInstance(_repo);
        UnitOfWork uow = null;

        uow = sysTx(); //$NON-NLS-1$
        Teiid teiid = serverManager.getDefaultServer(uow);

        teiid.setHost(uow, HostProvider.DEFAULT_HOST);
        teiid.setAdminUser(uow, TeiidAdminInfo.DEFAULT_ADMIN_USERNAME);
        teiid.setAdminPassword(uow, TeiidAdminInfo.DEFAULT_ADMIN_PASSWORD);
        teiid.setAdminPort(uow, TeiidAdminInfo.Util.defaultPort(getVersion()));

        teiid.setJdbcUsername(uow, TeiidJdbcInfo.DEFAULT_JDBC_USERNAME);
        teiid.setJdbcPassword(uow, TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD);
        teiid.setJdbcPort(uow, TeiidJdbcInfo.DEFAULT_PORT);
        teiid.setJdbcSecure(uow, true);

        sysCommit();

        return teiid;
    }

    @Before
    public void setup() throws Exception {
        teiidInstance = getDefaultTeiid().getTeiidInstance(getTransaction());
    }

    @After
    public void teardown() throws Exception {
        if (teiidInstance != null) {
            teiidInstance.disconnect();
            teiidInstance = null;
        }
    }

    private void wait(int seconds) {
        try {
            Thread.sleep(seconds * 1000);
        } catch (Exception ex) {
            // Nothing required
        }
    }

    private void setTeiidCacheExpireThreshold(long expireThreshold) throws KException, Exception {
        KomodoObject teiidCache = _repo.komodoTeiidCache(sysTx());
        assertNotNull(teiidCache);

        teiidCache.setProperty(sysTx(), KomodoLexicon.TeiidCache.EXPIRATION_THRESHOLD, expireThreshold);
        sysCommit();
    }

    private void deployMySQLDriver() throws Exception {
        InputStream driverStream = TestUtilities.mySqlDriver();
        assertNotNull(driverStream);

        File driverFile = TestUtilities.createTempFile(MYSQL_DRIVER, ".jar");
        driverFile.deleteOnExit();
        FileUtils.write(driverStream, driverFile);
        getTeiidInstance().deployDriver(MYSQL_DRIVER, driverFile);

        wait(2);
    }

    @Test
    public void testGetTemplates() throws Exception {
        Teiid teiid = getDefaultTeiid();

        getTeiidInstance().connect();
        assertTrue(getTeiidInstance().isConnected());

        //
        // Deploy the mysql driver
        //
        deployMySQLDriver();

        //
        // Check teiid instance actually has some data source templates
        //
        Collection<String> templateNames = getTeiidInstance().getDataSourceTemplateNames();
        assertTrue(templateNames.size() > 0);

        //
        // Check the mysql driver template is returned
        //
        String mysqlDriver = null;
        for (String templateName  : templateNames) {
            if (templateName == null)
                continue; // annoying null template

            if (templateName.startsWith(MYSQL_DRIVER) && ! templateName.contains("Fabric"))
                mysqlDriver = templateName;
        }
        assertNotNull(mysqlDriver);

        //
        // Zero the cache expire threshold to ensure caches are overwritten
        //
        setTeiidCacheExpireThreshold(0);

        //
        // Cache the contents of the teiid server
        //
        CachedTeiid teiidCache = teiid.importContent(sysTx());
        sysCommit();

        //
        // Check all the templates have been fetched
        //
        Template[] templates = teiidCache.getTemplates(sysTx());
        assertTrue(templates.length > 0);

        for (Template template : templates) {
            String name = template.getName(sysTx());
            templateNames.remove(name);
        }

        // Remove the null template which is just annoying!!
        templateNames.remove(null);
        assertTrue(templateNames.isEmpty());

        //
        // Sanity check the templates and template entries
        //
        for (Template template : templates) {
            List<TemplateEntry> entries = template.getEntries(getTransaction());
            assertTrue(entries.size() > 0);

            boolean hasConnFactoryClassKey = false;
            for (TemplateEntry entry : entries) {
                if (entry.isConstrainedToAllowedValues(getTransaction())) {
                    Collection<Object> allowedValues = entry.getAllowedValues(getTransaction());
                    assertTrue(allowedValues.size() > 0);

                    Object defaultValue = entry.getDefaultValue(getTransaction());
                    if (defaultValue != null) {
                        assertTrue(allowedValues.contains(entry.getDefaultValue(getTransaction())));

                        String className = entry.getTypeClassName(getTransaction());
                        assertEquals(className, defaultValue.getClass().getCanonicalName());
                    }
                }

                if (entry.getName(getTransaction()).equals(Template.CONN_FACTORY_CLASS_KEY)) {
                    hasConnFactoryClassKey = true;
                }
            }

            assertEquals(! hasConnFactoryClassKey, template.isJdbc(getTransaction()));
        }
    }
}

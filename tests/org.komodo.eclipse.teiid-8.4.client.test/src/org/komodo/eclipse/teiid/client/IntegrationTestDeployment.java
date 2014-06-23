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
package org.komodo.eclipse.teiid.client;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileInputStream;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import org.jboss.arquillian.junit.Arquillian;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.eclipse.teiid.client.util.AdminUtil;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.test.utils.SmartTestKomodoSuite;
import org.komodo.test.utils.TeiidInstanceBuilder;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.AdminException;
import org.teiid.adminapi.DataPolicy;
import org.teiid.adminapi.Model;
import org.teiid.adminapi.PropertyDefinition;
import org.teiid.adminapi.Session;
import org.teiid.adminapi.VDB;
import org.teiid.adminapi.VDB.ConnectionType;
import org.teiid.adminapi.VDB.Status;
import org.teiid.jdbc.TeiidDriver;
import org.teiid.runtime.client.admin.AdminSpec;

@RunWith(Arquillian.class)
@SuppressWarnings("nls")
public class IntegrationTestDeployment {

	private ITeiidVersion teiidVersion = new TeiidVersion(ITeiidVersion.VersionID.TEIID_8_4.toString());

	private AdminSpec adminSpec = AdminSpec.getInstance(teiidVersion);

	private Admin admin;

	@Before
	public void setup() throws Exception {

		TeiidInstanceBuilder teiidBuilder = new TeiidInstanceBuilder(teiidVersion);
		teiidBuilder.setHost("localhost");
		teiidBuilder.setPort(9999);
		teiidBuilder.setUserName("admin");
		teiidBuilder.setPassword("admin");

		admin = adminSpec.createAdmin(teiidBuilder.getTeiidInstance());
	}

	@After
	public void teardown() throws AdminException {
		AdminUtil.cleanUp(admin);
		admin.close();
	}

	private VDB deployVdb(String testVdbFileName, String testVdbName, int version) throws Exception {
		File vdbFile = SmartTestKomodoSuite.getTestDataFile(this.getClass(), testVdbFileName);
		admin.deploy(testVdbFileName, new FileInputStream(vdbFile));
		AdminUtil.waitForVDBLoad(admin, testVdbName, version, 3);

		assertFalse(admin.getVDBs().isEmpty());

		VDB vdb = admin.getVDB(testVdbName, version);
		assertNotNull(vdb);
		return vdb;
	}

	private VDB deployBQTVdb() throws Exception {
		VDB vdb = deployVdb("bqt.vdb", "bqt", 1);
		assertNotNull(vdb);
		assertFalse(vdb.getModels().isEmpty());

		Model model = vdb.getModels().get(0);
		admin.assignToModel("bqt", 1, model.getName(), "Source", "h2", "java:jboss/datasources/ExampleDS");

		return vdb;
	}

	@Test
	public void testChainedDelegates() throws Exception {
		Properties props = new Properties();
		props.setProperty("connection-url", "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1");
		props.setProperty("user-name", "sa");
		props.setProperty("password", "sa");

		AdminUtil.createDataSource(admin, "Oracle11_PushDS", "h2", props);
		File fakeJarFile = SmartTestKomodoSuite.getTestDataFile(this.getClass(), "fake.jar");
		admin.deploy("fake.jar", new FileInputStream(fakeJarFile));
		try {
			deployVdb("chained-vdb.xml", "Chorus", 1);
		} finally {
			admin.undeploy("fake.jar");
		}
	}

	@Test
	public void testVDBDeployment() throws Exception {
		Collection<?> vdbs = admin.getVDBs();
		assertTrue(vdbs.isEmpty());

		Collection<String> dsNames = admin.getDataSourceNames();
		if (dsNames.contains("Oracle11_PushDS")) {
			admin.deleteDataSource("Oracle11_PushDS");
		}

		VDB vdb = deployBQTVdb();
		assertFalse(vdb.isValid());
		assertTrue(AdminUtil.waitForVDBLoad(admin, "bqt", 1, 3));
		assertFalse(vdb.isValid());

		Properties props = new Properties();
		props.setProperty("connection-url", "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1");
		props.setProperty("user-name", "sa");
		props.setProperty("password", "sa");
		props.setProperty("connection-properties", "foo=bar,blah=blah");

		admin.createDataSource("Oracle11_PushDS", "h2", props);

		vdb = admin.getVDB("bqt", 1);
		assertTrue(vdb.isValid());
		assertTrue(vdb.getStatus().equals(Status.ACTIVE));
	}

	public void testGetDatasourceProperties() throws Exception {
		Properties props = new Properties();
		props.setProperty("connection-url", "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1");
		props.setProperty("user-name", "sa");
		props.setProperty("password", "sa");
		props.setProperty("connection-properties", "foo=bar,blah=blah");
		props.setProperty("max-pool-size", "4");

		admin.createDataSource("Oracle11_PushDS", "h2", props);

		Properties p = admin.getDataSource("Oracle11_PushDS");
		assertEquals("4", p.getProperty("max-pool-size"));
		assertEquals("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", p.getProperty("connection-url"));

		admin.deleteDataSource("Oracle11_PushDS");

		p = new Properties();
		p.setProperty("class-name", "org.teiid.resource.adapter.ws.WSManagedConnectionFactory");
		p.setProperty("EndPoint", "{endpoint}");
		props.setProperty("max-pool-size", "4");
		admin.createDataSource("nowhere", "teiid-connector-ws.rar", p);

		assertEquals("org.teiid.resource.adapter.ws.WSManagedConnectionFactory", p.getProperty("class-name"));
		assertEquals("4", p.getProperty("max-pool-size"));
		assertEquals("{endpoint}", p.getProperty("EndPoint"));

		admin.deleteDataSource("nowhere");
	}

	@Test
	public void testVDBConnectionType() throws Exception {
		VDB vdb = deployBQTVdb();
		assertEquals(ConnectionType.BY_VERSION, vdb.getConnectionType());

		try {
			Connection conn = TeiidDriver.getInstance().connect("jdbc:teiid:bqt@mm://localhost:31000;user=user;password=user", null);
			conn.close();
		} catch (Exception e) {
			fail("must have succeeded in connection");
		}

		admin.changeVDBConnectionType("bqt", 1, ConnectionType.NONE);

		try {
			TeiidDriver.getInstance().connect("jdbc:teiid:bqt@mm://localhost:31000;user=user;password=user", null);
			fail("should have failed to connect as no new connections allowed");
		} catch (Exception e) {
			// pass
		}

		VDB vdb2 = deployVdb("bqt2.vdb", "bqt", 2);
		Model model = vdb.getModels().get(0);
		admin.assignToModel("bqt", 2, model.getName(), "Source", "h2", "java:jboss/datasources/ExampleDS");

		try {
			Connection conn = TeiidDriver.getInstance().connect("jdbc:teiid:bqt@mm://localhost:31000;user=user;password=user", null);
			conn.close();
		} catch (Exception e) {
			fail("should not have failed to connect");
		}

		admin.changeVDBConnectionType("bqt", 2, ConnectionType.ANY);
		try {
			Connection conn = TeiidDriver.getInstance().connect("jdbc:teiid:bqt@mm://localhost:31000;user=user;password=user", null);
			conn.close();
		} catch (Exception e) {
			fail("should have connected to the second vdb");
		}

		vdb = admin.getVDB("bqt", 2);
		model = vdb.getModels().get(0);
		assertEquals(model.getSourceConnectionJndiName("Source"), "java:jboss/datasources/ExampleDS");
		assertEquals(model.getSourceTranslatorName("Source"), "h2");
		assertEquals(ConnectionType.ANY, vdb.getConnectionType());
	}

	@Test
	public void testCacheTypes() throws Exception {
		String[] array = { Admin.Cache.PREPARED_PLAN_CACHE.toString(),
				Admin.Cache.QUERY_SERVICE_RESULT_SET_CACHE.toString() };
		Collection<String> types = admin.getCacheTypes();
		assertArrayEquals(array, types.toArray());
	}

	@Test
	public void testSessions() throws Exception {
		deployBQTVdb();

		Collection<? extends Session> sessions = admin.getSessions();
		assertEquals(0, sessions.size());

		Connection conn = TeiidDriver.getInstance().connect("jdbc:teiid:bqt@mm://localhost:31000;user=user;password=user;ApplicationName=test", null);
		sessions = admin.getSessions();
		assertEquals(1, sessions.size());
		Session s = sessions.iterator().next();

		assertEquals("user@teiid-security", s.getUserName());
		assertEquals("test", s.getApplicationName());
		assertEquals("bqt", s.getVDBName());
		assertEquals(1, s.getVDBVersion());
		assertNotNull(s.getSessionId());

		conn.close();

		conn = TeiidDriver.getInstance().connect("jdbc:teiid:bqt@mm://localhost:31000;user=user;password=user;ApplicationName=test", null);
		sessions = admin.getSessions();
		assertEquals(1, sessions.size());
		s = sessions.iterator().next();

		admin.terminateSession(s.getSessionId());
		sessions = admin.getSessions();
		assertEquals(0, sessions.size());
		conn.close();
	}

	@Test
	public void getDatasourceTemplateNames() throws Exception {
		Set<String> vals  = new HashSet<String>(Arrays.asList(new String[]{"infinispan", "file", "teiid-local", "teiid", 
				"salesforce", "ldap", "webservice", "h2", "google", "mongodb"}));
		deployBQTVdb();
		Set<String> templates = admin.getDataSourceTemplateNames();
		assertTrue(templates.containsAll(vals));
	}

	@Test
	public void getTemplatePropertyDefinitions() throws Exception {
		HashSet<String> props = new HashSet<String>();

		deployBQTVdb();

		Collection<? extends PropertyDefinition> pds = admin.getTemplatePropertyDefinitions("h2");
		for (PropertyDefinition pd : pds) {
			props.add(pd.getName());
		}
		assertTrue(props.contains("connection-url"));
		assertTrue(props.contains("user-name"));
		assertTrue(props.contains("password"));
		assertTrue(props.contains("check-valid-connection-sql"));
		assertTrue(props.contains("max-pool-size"));
		assertTrue(props.contains("connection-properties"));
		assertTrue(props.contains("max-pool-size"));

		HashSet<String> rar_props = new HashSet<String>();
		pds = admin.getTemplatePropertyDefinitions("file");
		for (PropertyDefinition pd : pds) {
			rar_props.add(pd.getName());
		}

		assertTrue(rar_props.contains("ParentDirectory"));
		assertTrue(rar_props.contains("FileMapping"));
		assertTrue(rar_props.contains("AllowParentPaths"));
		assertTrue(rar_props.contains("resourceadapter-class"));
		assertTrue(rar_props.contains("managedconnectionfactory-class"));
		assertTrue(rar_props.contains("max-pool-size"));
	}

	@Test
	public void getWorkerPoolStats() throws Exception {
		deployBQTVdb();
		assertNotNull(admin.getWorkerPoolStats());
	}

	@Test
	public void testDataRoleMapping() throws Exception {
		VDB vdb = deployVdb("bqt2.vdb", "bqt", 2);

		Model model = vdb.getModels().get(0);
		admin.assignToModel("bqt", 2, model.getName(), "Source", "h2", "java:jboss/datasources/ExampleDS");
		

		vdb = admin.getVDB("bqt", 2);
		assertTrue(vdb.isValid());
		List<DataPolicy> policies = vdb.getDataPolicies();
		assertEquals(1, policies.size());

		DataPolicy dp = policies.get(0);
		assertEquals("roleOne", dp.getName());
		assertEquals(2, dp.getPermissions().size());
		assertTrue(dp.isAllowCreateTemporaryTables());
		assertTrue(dp.isAnyAuthenticated());

		List<String> roleNames = dp.getMappedRoleNames();
		assertArrayEquals(new String[]{"ROLE1", "ROLE2"}, roleNames.toArray());
		
		admin.removeDataRoleMapping("bqt", 2, "roleOne", "ROLE1");

		vdb = admin.getVDB("bqt", 2);
		policies = vdb.getDataPolicies();
		dp = policies.get(0);

		roleNames = dp.getMappedRoleNames();
		assertArrayEquals(new String[] { "ROLE2" }, roleNames.toArray());

		admin.addDataRoleMapping("bqt", 2, "roleOne", "ROLE3");

		vdb = admin.getVDB("bqt", 2);
		policies = vdb.getDataPolicies();
		dp = policies.get(0);

		roleNames = dp.getMappedRoleNames();
		assertArrayEquals(new String[]{"ROLE2", "ROLE3"}, roleNames.toArray());
		
		admin.setAnyAuthenticatedForDataRole("bqt", 2, "roleOne", false);

		vdb = admin.getVDB("bqt", 2);
		policies = vdb.getDataPolicies();
		dp = policies.get(0);

		assertFalse(dp.isAnyAuthenticated());
	}

	@Test
	public void testCreateConnectionFactory() throws Exception {
		String deployedName = "wsOne";

		assertFalse(admin.getDataSourceNames().contains(deployedName));

		Properties p = new Properties();
		p.setProperty("class-name", "org.teiid.resource.adapter.ws.WSManagedConnectionFactory");
		p.setProperty("EndPoint", "{endpoint}");
		admin.createDataSource(deployedName, "webservice", p);

		assertTrue(admin.getDataSourceNames().contains(deployedName));

		admin.deleteDataSource(deployedName);

		assertFalse(admin.getDataSourceNames().contains(deployedName));

		admin.createDataSource(deployedName, "webservice", p);

		assertTrue(admin.getDataSourceNames().contains(deployedName));

		admin.deleteDataSource(deployedName);
	}

	private int assertMetadataLoadCount(boolean check, int expected) throws SQLException {				
		Connection conn = TeiidDriver.getInstance().connect("jdbc:teiid:test.1@mm://localhost:31000;user=user;password=user", null);		
		Statement stmt = conn.createStatement();
		stmt.execute("SELECT execCount FROM Matadata");
		ResultSet rs = stmt.getResultSet();
		rs.next();
		int execCount = rs.getInt(1);
		if (check) {
			assertEquals(expected, execCount);
		}
		rs.close();
		stmt.close();
		conn.close();
		return execCount;
	}	

	@Test
	public void testErrorDeployment() throws Exception {
		Collection<?> vdbs = admin.getVDBs();
		assertTrue(vdbs.isEmpty());

		VDB vdb = deployVdb("error-vdb.xml", "error", 1);
		assertEquals(Status.FAILED, vdb.getStatus());
	}
}

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
package org.komodo.core.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.PathNotFoundException;
import javax.jcr.Property;
import javax.jcr.PropertyIterator;
import javax.jcr.PropertyType;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.query.Query;
import javax.jcr.query.QueryManager;
import javax.jcr.query.QueryResult;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.test.utils.MultiUseAbstractTest;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;
import org.komodo.utils.KLog;
import org.modeshape.common.collection.Problem;
import org.modeshape.common.collection.Problems;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.jcr.JcrRepository;
import org.modeshape.jcr.ModeShapeEngine;
import org.modeshape.jcr.RepositoryConfiguration;
import org.modeshape.jcr.api.JcrConstants;

/**
 * Mapping of MODE-2463 issue where both a remove and re-add of the same node
 * cannot be conducted in the same session
 */
@SuppressWarnings( {"nls", "javadoc"} )
public class TestObjectOperations implements StringConstants {

    private static Path _dataDirectory;

    /**
     * Default location of the configuration of the test repository
     */
    private static final String DEFAULT_TEST_REPOSITORY_CONFIG = "test-local-repository-in-memory-config.json"; //$NON-NLS-1$

    @BeforeClass
    public static void oneTimeSetup() throws Exception {
        _dataDirectory = Files.createTempDirectory( "KomodoEngineDataDir" );
        System.setProperty( SystemConstants.ENGINE_DATA_DIR, _dataDirectory.toString() );
    }

    @AfterClass
    public static void oneTimeTeardown() throws InterruptedException, ExecutionException {
        System.out.println("Tearing down engine");
        if (engine == null)
            return;

        System.out.println("State reachable so shutting down");
        Future<Boolean> shutdown = engine.shutdown();
        // Await the shutdown
        shutdown.get();

        repository = null;
        engine = null;

        FileUtils.removeDirectoryAndChildren( _dataDirectory.toFile() );
    }

    private static ModeShapeEngine engine;

    private static JcrRepository repository;

    private InputStream getResource(String resource) {
        return MultiUseAbstractTest.class.getResourceAsStream(resource);
    }

    private InputStream getTestConfiguration() {
        return getResource(DEFAULT_TEST_REPOSITORY_CONFIG);
    }

    private void startEngine() throws Exception {
        if (repository != null)
            return;

        engine = new ModeShapeEngine();
        engine.start();

        InputStream inputStream = getTestConfiguration();
        assertNotNull("Configuration file cannot be found so input stream is null", inputStream); //$NON-NLS-1$
        RepositoryConfiguration config = RepositoryConfiguration.read(inputStream, DEFAULT_TEST_REPOSITORY_CONFIG);
        Problems problems = config.validate();
        if (problems.hasErrors()) {
            throw new RuntimeException("Problems with the configuration."); //$NON-NLS-1$
        }

        repository = engine.deploy(config);

        problems = repository.getStartupProblems();
        if (problems.hasErrors() || problems.hasWarnings()) {

            Iterator<Problem> iterator = problems.iterator();
            while(iterator.hasNext()) {
                Problem problem = iterator.next();
                switch (problem.getStatus()) {
                    case ERROR:
                        throw new RuntimeException("Error deploying repository: " + problem.getMessageString()); //$NON-NLS-1$
                    default:
                        KLog.getLogger().warn(problem.getMessageString(), problem.getThrowable());
                }
            }
        }
    }

    protected Session newSession() throws RepositoryException {
        return repository.login();
    }

    private void clearRepository() throws Exception {
        Session session = newSession();
        if (session != null && session.isLive()) {
            Node rootNode = session.getRootNode();
            NodeIterator children = rootNode.getNodes();
            while(children.hasNext()) {
                Node child = children.nextNode();
                try {
                    // Cannot legally remove system nodes and they are not created
                    // by the tests anyway so leave them alone
                    if (!child.isNodeType("mode:system")) //$NON-NLS-1$
                        child.remove();
                } catch (Exception ex) {
                    fail(ex.getLocalizedMessage());
                }
            }
            session.save();
            session.logout();
        }
    }

    @Before
    public void beforeEach() throws Exception {
        startEngine();
        clearRepository();
    }

    @After
    public void afterEach() throws Exception {
        clearRepository();
    }

    private void traverse(String tabs, Node node, StringBuffer buffer) throws Exception {
        buffer.append(tabs + node.getName() + NEW_LINE);

        PropertyIterator propertyIterator = node.getProperties();
        while (propertyIterator.hasNext()) {
            javax.jcr.Property property = propertyIterator.nextProperty();
            buffer.append(tabs + TAB + "@" + property.toString() + NEW_LINE);
        }

        javax.jcr.NodeIterator children = node.getNodes();
        while (children.hasNext()) {
            traverse(tabs + TAB, children.nextNode(), buffer);
        }
    }

    protected void traverse(Node node) throws Exception {
        StringBuffer buffer = new StringBuffer(NEW_LINE);
        traverse(TAB, node, buffer);
        KLog.getLogger().info(buffer.toString());
    }

    @Test
    @Ignore("Demonstrates the failure of querying on a session not yet committed.")
    public void testAddChild() throws Exception {
        String name = "testNode";
        String childName = "description";

        Session session1 = newSession();
        Node rootNode = session1.getRootNode();
        Node testNode = rootNode.addNode(name);
        assertNotNull(testNode);

        Node descNode = testNode.addNode(childName);
        assertNotNull(descNode);

        descNode = testNode.getNode(childName);
        assertNotNull(descNode);

        testNode = session1.getNode(testNode.getPath());
        assertNotNull(testNode.getNode(childName));

        traverse(testNode);

//        session1.save();
//        Session session2 = newSession();

        String queryStmt = "SELECT [jcr:path] FROM [" + JcrConstants.NT_UNSTRUCTURED + "]";

        QueryManager queryMgr = session1.getWorkspace().getQueryManager();
//        QueryManager queryMgr = session2.getWorkspace().getQueryManager();
        Query query = queryMgr.createQuery(queryStmt, Query.JCR_SQL2);
        QueryResult result = query.execute();
        NodeIterator itr = result.getNodes();

        int i = 0;
        while (itr.hasNext()) {
            i++;
        }

        assertTrue(i > 0);
    }

    @Test
    public void testRemoveThenAdd() throws Exception {
        String name = "testNode";

        Session session1 = newSession();
        Node rootNode = session1.getRootNode();
        Node testNode = rootNode.addNode(name);
        assertNotNull(testNode);
        String testNodePath = testNode.getPath();
        session1.save();
        session1.logout();

        Session session2 = newSession();
        rootNode = session2.getRootNode();
        assertNotNull(rootNode);
        testNode = session2.getNode(testNodePath);
        assertNotNull(testNode);

        testNode.remove();
        assertFalse(rootNode.hasNode(name));
        try {
            session2.getNode(testNodePath);
            fail("This should throw a PathNotFoundException since testNode has been removed"); //$NON-NLS-1$
        } catch (PathNotFoundException ex) {
            // Exception thrown good to continue
        }

        Node newTestNode = rootNode.addNode(name);

        /*
         * ISSUE #1
         *
         * In Modeshape 4.2, this will fail with:
         * testNodePath = /testNode
         * testNode.getPath() = /testNode[2]
         *
         * FIXED in Modeshape 4.3 and backported 8.3.x - MODE-2463
         */
        assertEquals(testNodePath, newTestNode.getPath());

        /*
         * ISSUE #2
         *
         * In Modeshape 4.2, the path of newTestNode is alledgedly
         * /testNode[2] so should be able to find it from session2,
         * except this throws a PathNotFoundException
         *
         * NO LONGER AN ISSUE in Modeshape 4.3 and backported 8.3.x - MODE-2463
         */
//        try {
//            session2.getNode(testNodePath + "[2]"); //$NON-NLS-1$
//        } catch (PathNotFoundException ex) {
//            // The path of newTestNode is reported as /testNode[2] but in fact there is no node there!
//            fail(ex.getMessage());
//        }

        /*
         * ISSUE #3
         *
         * In Modeshape 4.2, despite newTestNode claiming its path is /testNode[2], session2
         * cannot find it so where is newTestNode?
         *
         * Turns out that its been added to /testNode which is the correct
         * path but not what is being reported by newTestNode.getPath()
         *
         * Conclusion: bug in node.getPath(), returning incorrect absolute path
         *
         * FIXED IN Modeshape 4.3 and backported 8.3.x - MODE-2463
         */
        try {
            Node node = session2.getNode(testNodePath);
            assertNotNull(node);
            assertEquals("Node path should equal " + testNodePath, testNodePath, node.getPath()); //$NON-NLS-1$
        } catch (PathNotFoundException ex) {
            // No node found at testNodePath which should be correct
        }
    }

    private String toString(InputStream inputStream) throws Exception {
        BufferedInputStream bis = new BufferedInputStream(inputStream);
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        int result = bis.read();
        while (result != -1) {
            byte b = (byte)result;
            buf.write(b);
            result = bis.read();
        }

        return buf.toString();
    }

    /**
     * Demonstrates that the when a string property value is saved, an algorithm is run
     * which converts it to a binary value if the number of bytes > minimumBinarySizeInBytes.
     * The default in modeshape of this option is 4096KB.
     *
     * @throws Exception
     */
    @Test
    public void testDemonstratingPropertyValueConversion()  throws Exception {
        InputStream bookStream = TestUtilities.getResourceAsStream(TestUtilities.class,
        																												TestUtilities.RESOURCES_DIRECTORY,
        																												"books.xml");
        assertNotNull(bookStream);

        String bigContent = toString(bookStream);
        assertNotNull(bigContent);
        assertTrue(bigContent.length() > 0);

        //
        // Number of bytes is greater than minimumBinarySizeInBytes option
        // threshold in modeshape implying that this string value will be converted
        // into a binary stream upon saving of the session
        //
        assertTrue(bigContent.getBytes().length > 4096);

        InputStream tweetStream = TestUtilities.tweetExample();
        assertNotNull(tweetStream);

        String littleContent = toString(tweetStream);
        assertNotNull(littleContent);
        assertTrue(littleContent.length() > 0);

        //
        // Number of bytes is smaller than minimumBinarySizeInBytes option
        // threshold in modeshape implying that this string value will NOT be
        // converted into a binary stream upon saving of the session
        //
        assertTrue(littleContent.getBytes().length < 4096);


        String bigDataName = "BigData";
        String littleDataName = "LittleData";

        Session session1 = newSession();
        Node rootNode = session1.getRootNode();

        Node bigDataNode = rootNode.addNode(bigDataName);
        Node contentNode = bigDataNode.addNode(JcrLexicon.CONTENT.getString(), null);
        contentNode.setProperty(JcrLexicon.DATA.getString(), bigContent);
        Property bigDataProperty = contentNode.getProperty(JcrLexicon.DATA.getString());

        Node littleDataNode = rootNode.addNode(littleDataName);
        contentNode = littleDataNode.addNode(JcrLexicon.CONTENT.getString(), null);
        contentNode.setProperty(JcrLexicon.DATA.getString(), littleContent);
        Property littleDataProperty = contentNode.getProperty(JcrLexicon.DATA.getString());

        //
        // So up until this point both littleDataNode's and bigDataNode's data property
        // contains a property value of type STRING
        //
        assertEquals(PropertyType.STRING, bigDataProperty.getType());
        assertEquals(PropertyType.STRING, littleDataProperty.getType());

        session1.save();
        session1.logout();

        //
        // Demonstrates that once the session is saved and the byte size
        // of the string was larger than 4096KB the property value is changed
        // to a binary value
        //
        Session session2 = newSession();
        bigDataProperty = session2.getProperty(bigDataProperty.getPath());
        littleDataProperty = session2.getProperty(littleDataProperty.getPath());
        assertNotNull(bigDataProperty);
        assertNotNull(littleDataProperty);

        assertEquals(PropertyType.BINARY, bigDataProperty.getType());
        assertEquals(PropertyType.STRING, littleDataProperty.getType());

        traverse(session2.getNode(bigDataNode.getPath()));
        traverse(session2.getNode(littleDataNode.getPath()));
    }
}

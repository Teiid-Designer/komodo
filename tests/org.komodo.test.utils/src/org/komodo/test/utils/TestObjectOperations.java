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
package org.komodo.test.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;
import java.io.InputStream;
import java.util.Iterator;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.PathNotFoundException;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.utils.KLog;
import org.modeshape.common.collection.Problem;
import org.modeshape.common.collection.Problems;
import org.modeshape.jcr.JcrRepository;
import org.modeshape.jcr.ModeShapeEngine;
import org.modeshape.jcr.RepositoryConfiguration;

/**
 * Mapping of MODE-2463 issue where both a remove and re-add of the same node
 * cannot be conducted in the same session
 */
@SuppressWarnings( {"nls", "javadoc"} )
public class TestObjectOperations {

    /**
     * Default location of the configuration of the test repository
     */
    private static final String DEFAULT_TEST_REPOSITORY_CONFIG = "test-local-repository-in-memory-config.json"; //$NON-NLS-1$

    private ModeShapeEngine engine;

    private JcrRepository repository;

    private InputStream getTestConfiguration() {
        return getClass().getResourceAsStream(DEFAULT_TEST_REPOSITORY_CONFIG);
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

    @Test
    @Ignore("Mapping issue MODE-2463 - a remove then a re-add cannot be conducted in the same transaction")
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

        @SuppressWarnings( "unused" )
        Node newTestNode = rootNode.addNode(name);

        /*
         * ISSUE #1
         *
         * This will fail with:
         * testNodePath = /testNode
         * testNode.getPath() = /testNode[2]
         */
//        assertEquals(testNodePath, newTestNode.getPath());        // Uncomment to view failure

        /*
         * ISSUE #2
         *
         * The path of newTestNode is alledgedly /testNode[2] so should
         * be able to find it from session2, except this throws a PathNotFoundException
         */
        try {
            session2.getNode(testNodePath + "[2]"); //$NON-NLS-1$
        } catch (PathNotFoundException ex) {
            // The path of newTestNode is reported as /testNode[2] but in fact there is no node there!
//            fail(ex.getMessage());                                                    // Uncomment to view failure
        }

        /*
         * ISSUE #3
         *
         * Despite newTestNode claiming its path is /testNode[2], session2
         * cannot find it so where is newTestNode?
         *
         * Turns out that its been added to /testNode which is the correct
         * path but not what is being reported by newTestNode.getPath()
         *
         * Conclusion: bug in node.getPath(), returning incorrect absolute path
         */
        try {
            Node node = session2.getNode(testNodePath);
            assertEquals("Node path should equal " + testNodePath, testNodePath, node.getPath()); //$NON-NLS-1$
            fail("This should still throw a PathNotFoundException since /testNode[2] is the path of newTestNode"); //$NON-NLS-1$
        } catch (PathNotFoundException ex) {
            // No node found at testNodePath which should be correct
        }
    }
}

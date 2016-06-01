/*
 * Originally copied from ModeShape (http://www.modeshape.org).
 *
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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.concurrent.Future;
import javax.jcr.ImportUUIDBehavior;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Workspace;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.komodo.repository.KSequencerController;
import org.komodo.repository.KSequencersFactory;
import org.komodo.utils.KLog;
import org.modeshape.common.collection.Problem;
import org.modeshape.common.collection.Problems;
import org.modeshape.jcr.JcrRepository;
import org.modeshape.jcr.JcrSession;
import org.modeshape.jcr.ModeShapeEngine;
import org.modeshape.jcr.RepositoryConfiguration;
import org.modeshape.jcr.api.nodetype.NodeTypeManager;

/**
 * A base class for tests that require a new JcrSession but NOT a JcrRepository for each test method.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public abstract class MultiUseAbstractTest extends AbstractLoggingTest {

    private static JcrSession session;

    private static ModeShapeEngine engine;

    private static KSequencerController sequencers;

    private static JcrRepository repository;

    private static void stopRepository() throws Exception {
        try {
            clearRepository();

            if (sequencers != null)
                sequencers.dispose();

            Future<Boolean> shutdown = engine.shutdown();
            shutdown.get();
        } finally {
            repository = null;
            session = null;
            engine = null;
        }
    }

    private static void clearRepository() throws Exception {
        if (session != null && session.isLive()) {
            Node rootNode = session.getRootNode();
            NodeIterator children = rootNode.getNodes();
            while(children.hasNext()) {
                Node child = children.nextNode();
                try {
                    // Cannot legally remove system nodes and they are not created
                    // by the tests anyway so leave them alone
                    if (!child.isNodeType("mode:system"))
                        child.remove();
                } catch (Exception ex) {
                    fail(ex.getLocalizedMessage());
                }
            }
            session.save();
            session.logout();
        }
    }

    @AfterClass
    public static void afterAll() throws Exception {
        stopRepository();
    }

    /**
     * Provides the location of modeshape repository configuration file.
     * By default, this returns null and the DEFAULT_TEST_REPOSITORY_CONFIG
     * value is used instead.
     *
     * @see getTestConfiguration
     *
     * @return the location of modeshape's test configuration file
     */
    protected String getTestConfigurationPath() {
        return null;
    }

    private InputStream getTestConfiguration() {
        String configuration = getTestConfigurationPath();
        if (configuration == null) {
            throw new RuntimeException("Modeshape test configuration location path not defined");
        }

        return getClass().getClassLoader().getResourceAsStream(configuration);
    }

    private void startEngine() throws Exception {
        if (repository != null)
            return;

        engine = new ModeShapeEngine();
        engine.start();

        InputStream inputStream = getTestConfiguration();
        assertNotNull("Configuration file cannot be found so input stream is null", inputStream);
        RepositoryConfiguration config = RepositoryConfiguration.read(inputStream, "Test Configuration");
        Problems problems = config.validate();
        if (problems.hasErrors()) {
            throw new RuntimeException("Problems with the configuration.");
        }

        repository = engine.deploy(config);

        problems = repository.getStartupProblems();
        if (problems.hasErrors() || problems.hasWarnings()) {

            Iterator<Problem> iterator = problems.iterator();
            while(iterator.hasNext()) {
                Problem problem = iterator.next();
                switch (problem.getStatus()) {
                    case ERROR:
                        throw new RuntimeException("Error deploying repository: " + problem.getMessageString());
                    default:
                        KLog.getLogger().warn(problem.getMessageString(), problem.getThrowable());
                }
            }
        }

        // Add the sequencing listener
        sequencers = KSequencersFactory.getInstance().createSequencerController(config.getDefaultWorkspaceName(), repository);
    }

    @Before
    public void beforeEach() throws Exception {
        startEngine();
        clearRepository();

        // create a new session for each test ...
        session = newSession();
        assertNotNull(session);
        assertNotNull(session.getRootNode());
    }

    @After
    public void afterEach() throws Exception {
     // log out of the session after each test ...
        try {
            clearRepository();
            if ((session != null) && session.isLive())
                session.logout();
        } finally {
            session = null;
        }
    }

    protected KSequencerController sequencers() {
        return sequencers;
    }

    protected SynchronousSequencerListener addSequencingListenerLatch(String listenerId, Session session) throws Exception {
        return new SynchronousSequencerListener(listenerId, session, sequencers());
    }

    protected JcrSession session() {
        return session;
    }

    protected Workspace workspace() {
        return session.getWorkspace();
    }

    protected JcrSession newSession() throws RepositoryException {
        return repository.login();
    }

    /**
     * Utility method to get the resource on the classpath given by the supplied name
     *
     * @param name the name (or path) of the classpath resource
     * @return the input stream to the content; may be null if the resource does not exist
     */
    protected InputStream resourceStream(String name) {
        return getClass().getClassLoader().getResourceAsStream(name);
    }

    /**
     * Register the node types in the CND file at the given location on the classpath.
     *
     * @param resourceName the name of the CND file on the classpath
     * @throws RepositoryException if there is a problem registering the node types
     * @throws IOException if the CND file could not be read
     */
    protected void registerNodeTypes(String resourceName) throws RepositoryException, IOException {
        InputStream stream = resourceStream(resourceName);
        assertNotNull(stream);
        Workspace workspace = session().getWorkspace();
        NodeTypeManager ntMgr = (NodeTypeManager)workspace.getNodeTypeManager();
        ntMgr.registerNodeTypes(stream, true);
    }

    /**
     * Import under the supplied parent node the repository content in the XML file at the given location on the classpath.
     *
     * @param parent the node under which the content should be imported; may not be null
     * @param resourceName the name of the XML file on the classpath
     * @param uuidBehavior the UUID behavior; see {@link ImportUUIDBehavior} for values
     * @throws RepositoryException if there is a problem importing the content
     * @throws IOException if the XML file could not be read
     */
    protected void importContent(Node parent, String resourceName, int uuidBehavior) throws RepositoryException, IOException {
        InputStream stream = resourceStream(resourceName);
        assertNotNull(stream);
        parent.getSession().getWorkspace().importXML(parent.getPath(), stream, uuidBehavior);
    }

    /**
     * Import under the supplied parent node the repository content in the XML file at the given location on the classpath.
     *
     * @param parentPath the path to the node under which the content should be imported; may not be null
     * @param resourceName the name of the XML file on the classpath
     * @param uuidBehavior the UUID behavior; see {@link ImportUUIDBehavior} for values
     * @throws RepositoryException if there is a problem importing the content
     * @throws IOException if the XML file could not be read
     */
    protected void importContent(String parentPath, String resourceName, int uuidBehavior) throws RepositoryException, IOException {
        InputStream stream = resourceStream(resourceName);
        assertNotNull(stream);
        ((Session) session()).getWorkspace().importXML(parentPath, stream, uuidBehavior);
    }

    protected InputStream resource(String path) {
        InputStream stream = getClass().getClassLoader().getResourceAsStream(path);
        assertNotNull(stream);
        return stream;
    }
}

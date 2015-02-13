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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.modeshape.jcr.api.JcrConstants.JCR_MIXIN_TYPES;
import static org.modeshape.jcr.api.JcrConstants.JCR_PRIMARY_TYPE;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.Property;
import javax.jcr.PropertyIterator;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.Workspace;
import javax.jcr.observation.EventIterator;
import javax.jcr.observation.EventListener;
import javax.jcr.observation.EventListenerIterator;
import javax.jcr.observation.ObservationManager;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.utils.KLog;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.jcr.JcrSession;
import org.modeshape.jcr.api.JcrConstants;
import org.modeshape.jcr.api.observation.Event;
import org.modeshape.jcr.api.observation.Event.Sequencing;
/**
 * Class which serves as base for various sequencer unit tests. In addition to this, it uses the sequencing events fired by
 * ModeShape's {@link javax.jcr.observation.ObservationManager} to perform various assertions and therefore, acts as a test for
 * those as well.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public abstract class AbstractSequencerTest extends MultiUseAbstractTest implements Sequencing, StringConstants {

    private static final int DEFAULT_WAIT_TIME_SECONDS = 15;

    public static enum SequenceType {
        VDB("VDB Sequencer", "xml"),

        DDL("DDL Sequencer"),

        TSQL("Teiid SQL Sequencer");

        private String sequencerName;

        private String extension;

        private SequenceType(String sequencerName) {
            this.sequencerName = sequencerName;
            this.extension = this.name().toLowerCase();
        }

        private SequenceType(String sequencerName, String extension) {
            this.sequencerName = sequencerName;
            this.extension = extension;
        }

        /**
         * @return file extension for type
         */
        public String getExtension() {
            return extension;
        }

        /**
         * @return name of associated sequencer
         */
        public String getSequencerName() {
            return sequencerName;
        }
    }

    protected Node rootNode;

    private ObservationManager observationManager;

    /**
     * A [node path, node instance] map which is populated by the listener, once each sequencing event is received
     */
    private final Map<String, Node> sequencedNodes = new HashMap<String, Node>();

    /**
     * A [node path, latch] map which is used to block tests waiting for sequenced output, until either the node has been
     * sequenced or a timeout occurs
     */
    private final ConcurrentHashMap<String, CountDownLatch> nodeSequencedLatches = new ConcurrentHashMap<String, CountDownLatch>();

    /**
     * A [node path, latch] map which is used to block tests waiting for a sequencing failure, until either the failure has
     * occurred or a timeout occurs
     */
    private final ConcurrentHashMap<String, CountDownLatch> sequencingFailureLatches = new ConcurrentHashMap<String, CountDownLatch>();

    /**
     * A [sequenced node path, event] map which will hold all the received sequencing events, both in failure and non-failure
     * cases, using the path of the sequenced node as key.
     */
    private final ConcurrentHashMap<String, Event> sequencingEvents = new ConcurrentHashMap<String, Event>();

    private final KLog logger = KLog.getLogger();

    public ObservationManager getObservationManager() throws Exception {
        if (observationManager == null)
            observationManager = ((Workspace)session().getWorkspace()).getObservationManager();

        return observationManager;
    }

    protected TeiidVersion getTeiidVersion() {
        return TeiidVersionProvider.getInstance().getTeiidVersion();
    }

    @Override
    @Before
    public void beforeEach() throws Exception {
        super.beforeEach();
        rootNode = session().getRootNode();
        addSequencingListeners(session());
    }

    protected void addSequencingListeners( JcrSession session ) throws RepositoryException {
        observationManager = ((Workspace)session.getWorkspace()).getObservationManager();
        observationManager.addEventListener(new SequencingListener(), NODE_SEQUENCED, null, true, null, null, false);
        observationManager.addEventListener(new SequencingFailureListener(),
                                            NODE_SEQUENCING_FAILURE,
                                            null,
                                            true,
                                            null,
                                            null,
                                            false);
    }

    /**
     * @param countdown equivalent to number of sql query expressions to be sequenced
     * @param pathsToBeSequenced wilcarded pattern against which to compare the sequenced nodes
     * @return the latch for awaiting the sequencing
     * @throws Exception
     */
    protected CountDownLatch addPathLatchListener(int countdown, final List<String> pathsToBeSequenced) throws Exception {
        ObservationManager manager = getObservationManager();
        assertNotNull(manager);

        final CountDownLatch updateLatch = new CountDownLatch(countdown);
        manager.addEventListener(new SequencerListener(pathsToBeSequenced, updateLatch), NODE_SEQUENCED, null, true, null, null, false);

        return updateLatch;
    }

    /**
     * @param countdown equivalent to number of sql query expressions to be sequenced
     * @param pathsToBeSequenced wilcarded pattern against which to compare the sequenced nodes
     * @return the latch for awaiting the sequencing
     * @throws Exception
     */
    protected CountDownLatch addPathLatchListener(int countdown, String pathToBeSequenced) throws Exception {
        List<String> pathsToBeSequenced = new ArrayList<String>();
        pathsToBeSequenced.add(pathToBeSequenced);
        return addPathLatchListener(countdown, pathsToBeSequenced);
    }

    @Override
    @After
    public void afterEach() throws Exception {
        if (observationManager != null) {
            for (EventListenerIterator it = observationManager.getRegisteredEventListeners(); it.hasNext();) {
                observationManager.removeEventListener(it.nextEventListener());
            }
            observationManager = null;
        }
        super.afterEach();
        cleanupData();
    }

    private void cleanupData() {
        sequencedNodes.clear();
        sequencingEvents.clear();
        nodeSequencedLatches.clear();
        sequencingFailureLatches.clear();
    }

    private Node createNodeWithContentFromStream(String nodeRelativePath, InputStream inputStream) throws Exception {
        Node parent = rootNode;
        for (String pathSegment : nodeRelativePath.split(FORWARD_SLASH)) {
            parent = parent.addNode(pathSegment);
        }

        Node content = parent.addNode(JcrConstants.JCR_CONTENT);
        content.setProperty(JcrConstants.JCR_DATA,
                            session().getValueFactory().createBinary(inputStream));
        session().save();
        return parent;
    }

    protected Node prepareSequence(String fileName, InputStream inputStream) throws Exception {
        createNodeWithContentFromStream(fileName, inputStream);

        Node fileNode = session().getNode(FORWARD_SLASH + fileName);
        assertNotNull(fileNode);

        return fileNode;
    }

    protected Node prepareSequence(File textFile) throws Exception {
        return prepareSequence(textFile.getName(), new FileInputStream(textFile));
    }

    private File wrapText(String text, String extension) throws Exception {
        File tmpFile = File.createTempFile(extension, DOT + extension);
        tmpFile.deleteOnExit();
        FileWriter fw = new FileWriter(tmpFile);
        fw.write(text);
        fw.close();
        return tmpFile;
    }

    protected Node prepareSequence(String text, SequenceType sequenceType) throws Exception {
        File textFile = wrapText(text, sequenceType.getExtension());
        Node fileNode = prepareSequence(textFile);
        assertNotNull(fileNode);
        return fileNode;
    }

    /**
     * Retrieves a sequenced node using 5 seconds as maximum wait time.
     * 
     * @param parentNode an existing {@link Node}
     * @param relativePath the path under the parent node at which the sequenced node is expected to appear (note that this must
     *        be the path to the "new" node, always.
     * @return either the sequenced node or null, if something has failed.
     * @throws Exception if anything unexpected happens
     * @see AbstractSequencerTest#getOutputNode(javax.jcr.Node, String, int)
     */
    protected Node getOutputNode( Node parentNode,
                                  String relativePath ) throws Exception {
        return this.getOutputNode(parentNode, relativePath, DEFAULT_WAIT_TIME_SECONDS);
    }

    /**
     * Attempts to retrieve a node (which is expected to have been sequenced) under an existing parent node at a relative path.
     * The sequenced node "appears" when the {@link SequencingListener} is notified of the sequencing process. The thread which
     * calls this method either returns immediately if the node has already been sequenced, or waits a number of seconds for it to
     * become available.
     * 
     * @param parentNode an existing {@link Node}
     * @param relativePath the path under the parent node at which the sequenced node is expected to appear (note that this must
     *        be the path to the "new" node, always.
     * @param waitTimeSeconds the max number of seconds to wait.
     * @return either the sequenced node or null, if something has failed.
     * @throws Exception if anything unexpected happens
     */
    protected Node getOutputNode( Node parentNode,
                                  String relativePath,
                                  int waitTimeSeconds ) throws Exception {
        String parentNodePath = parentNode.getPath();
        String expectedPath = parentNodePath.endsWith(FORWARD_SLASH) ? parentNodePath + relativePath : parentNodePath + FORWARD_SLASH + relativePath;

        return getOutputNode(expectedPath, waitTimeSeconds);
    }

    protected Node getOutputNode( String expectedPath ) throws InterruptedException {
        return getOutputNode(expectedPath, DEFAULT_WAIT_TIME_SECONDS);
    }

    /**
     * Retrieves a new node under the given path, as a result of sequecing, or returns null if the given timeout occurs.
     * 
     * @param expectedPath
     * @param waitTimeSeconds
     * @return the output node
     * @throws InterruptedException
     */
    protected Node getOutputNode( String expectedPath,
                                  int waitTimeSeconds ) throws InterruptedException {
        if (!sequencedNodes.containsKey(expectedPath)) {
            createWaitingLatchIfNecessary(expectedPath, nodeSequencedLatches);
            logger.debug("Waiting for sequenced node at: " + expectedPath);
            CountDownLatch countDownLatch = nodeSequencedLatches.get(expectedPath);
            countDownLatch.await(waitTimeSeconds, TimeUnit.SECONDS);
        }
        nodeSequencedLatches.remove(expectedPath);
        return sequencedNodes.remove(expectedPath);
    }

    protected void verifyProperty( Node node, String propertyName, String expectedValue ) throws RepositoryException {
        Property property = node.getProperty(propertyName);
        Value value = property.isMultiple() ? property.getValues()[0] : property.getValue();
        assertEquals(expectedValue, value.getString());
    }

    protected void verifyProperty( Node node, String propertyName, long expectedValue ) throws RepositoryException {
        Property property = node.getProperty(propertyName);
        Value value = property.isMultiple() ? property.getValues()[0] : property.getValue();
        assertEquals(expectedValue, value.getLong());
    }

    protected void verifyProperty( Node node, String propertyName, boolean expectedValue ) throws RepositoryException {
        Property property = node.getProperty(propertyName);
        Value value = property.isMultiple() ? property.getValues()[0] : property.getValue();
        assertEquals(expectedValue, value.getBoolean());
    }

    protected void verifyProperty( Node node, String propertyName, java.sql.Date expectedValue ) throws RepositoryException {
        Property property = node.getProperty(propertyName);
        Value value = property.isMultiple() ? property.getValues()[0] : property.getValue();
        assertEquals(expectedValue, java.sql.Date.valueOf(value.getString()));
    }

    protected void verifyProperty( Node node, String propertyName, java.sql.Time expectedValue ) throws RepositoryException {
        Property property = node.getProperty(propertyName);
        Value value = property.isMultiple() ? property.getValues()[0] : property.getValue();
        assertEquals(expectedValue, java.sql.Time.valueOf(value.getString()));
    }

    protected boolean verifyHasProperty( Node node, String propNameStr ) throws RepositoryException {
        return node.hasProperty(propNameStr);
    }

    protected void verifyPrimaryType( Node node, String expectedValue ) throws RepositoryException {
        verifyProperty(node, JCR_PRIMARY_TYPE, expectedValue);
    }

    protected void verifyMixinType( Node node, String expectedValue ) throws RepositoryException {
        verifyProperty(node, JCR_MIXIN_TYPES, expectedValue);
    }
    
    protected void verifyMixinTypes( Node node, String... expectedValues ) throws RepositoryException {
        Value[] values = node.getProperty(JCR_MIXIN_TYPES).getValues();
        Set<String> valuesSet = new TreeSet<String>();
        for (Value value : values) {
            valuesSet.add(value.getString());
        }
        List<String> expectedValuesList = new ArrayList<String>(Arrays.asList(expectedValues));
        for (Iterator<String> expectedValuesIterator = expectedValuesList.iterator(); expectedValuesIterator.hasNext();) {
            assertTrue(valuesSet.contains(expectedValuesIterator.next()));
            expectedValuesIterator.remove();
        }
        assertTrue(expectedValuesList.isEmpty());
    }

    protected void verifyBaseProperties( Node node, String primaryType, String mixinType) throws RepositoryException {
        verifyPrimaryType(node, primaryType);
        if (mixinType == null)
            return;

        // Only if mixinType is not null do we check it
        verifyMixinType(node, mixinType);
    }

    protected Node findNode( Node parent, String nodePath, String... mixinTypes ) throws Exception {
        Node child = parent.getNode(nodePath);
        assertNotNull(child);
        verifyMixinTypes(child, mixinTypes);
        return child;
    }

    private void traverse(String tabs, Node node, StringBuffer buffer) throws Exception {
        buffer.append(tabs + node.getName() + NEW_LINE);

        PropertyIterator propertyIterator = node.getProperties();
        while(propertyIterator.hasNext()) {
            Property property = propertyIterator.nextProperty();
            buffer.append(tabs + TAB + "@" + property.toString() + NEW_LINE);
        }

        NodeIterator children = node.getNodes();
        while(children.hasNext()) {
            traverse(tabs + TAB, children.nextNode(), buffer);
        }
    }

    protected void traverse(Node node) throws Exception {
        StringBuffer buffer = new StringBuffer(NEW_LINE);
        traverse(TAB, node, buffer);
        KLog.getLogger().info(buffer.toString());
    }

    protected String enc(String input) {
        return session().encode(input);
    }

    protected Node verify(Node parentNode, String relativePath, int index, String mixinType) throws Exception {
        String indexExp = EMPTY_STRING;
        if (index > -1)
            indexExp = OPEN_SQUARE_BRACKET + index + CLOSE_SQUARE_BRACKET;

        Node childNode = null;
        if (parentNode.hasNode(relativePath)) {
            childNode = parentNode.getNode(relativePath + indexExp);
        } else
            childNode = parentNode.getNode(enc(relativePath ) + indexExp);
        assertNotNull(childNode);

        verifyBaseProperties(childNode, JcrConstants.NT_UNSTRUCTURED, mixinType);
        return childNode;
    }

    protected Node verify(Node parentNode, String relativePath, String mixinType) throws Exception {
        return verify(parentNode, relativePath, -1, mixinType);
    }

    protected Node verify(Node parentNode, String relativePath) throws Exception {
        return verify(parentNode, relativePath, -1, null);
    }

    protected void expectSequencingFailure( Node sequencedNode ) throws Exception {
        expectSequencingFailure(sequencedNode, 5);
    }

    protected void expectSequencingFailure( Node sequencedNode,
                                            int waitTimeSeconds ) throws Exception {
        String nodePath = sequencedNode.getPath();
        createWaitingLatchIfNecessary(nodePath, sequencingFailureLatches);
        CountDownLatch countDownLatch = sequencingFailureLatches.get(nodePath);
        assertTrue("Sequencing failure event not received", countDownLatch.await(waitTimeSeconds, TimeUnit.SECONDS));
        sequencingFailureLatches.remove(nodePath);
    }

    protected void createWaitingLatchIfNecessary( String expectedPath,
                                                  ConcurrentHashMap<String, CountDownLatch> latchesMap ) {
        latchesMap.putIfAbsent(expectedPath, new CountDownLatch(1));
    }

    protected void smokeCheckSequencingEvent( Event event,
                                              int expectedEventType,
                                              String... expectedEventInfoKeys ) throws RepositoryException {
        assertEquals(event.getType(), expectedEventType);
        Map<?, ?> info = event.getInfo();
        assertNotNull(info);
        for (String extraInfoKey : expectedEventInfoKeys) {
            assertNotNull(info.get(extraInfoKey));
        }
    }

    protected void assertCreatedBySessionUser( Node node,
                                               Session session ) throws RepositoryException {
        assertEquals(session.getUserID(), node.getProperty(JcrLexicon.CREATED_BY.getString()).getString());
    }

    private Map<?, ?> getSequencingEventInfo( Node sequencedNode ) throws RepositoryException {
        Event receivedEvent = sequencingEvents.get(sequencedNode.getPath());
        assertNotNull(receivedEvent);
        return receivedEvent.getInfo();
    }

    protected Map<?, ?> assertSequencingEventInfo( Node sequencedNode,
                                                   String expectedUserId,
                                                   String expectedSequencerName,
                                                   String expectedSelectedPath,
                                                   String expectedOutputPath ) throws RepositoryException {
        Map<?, ?> sequencingEventInfo = getSequencingEventInfo(sequencedNode);
        Assert.assertEquals(expectedUserId, sequencingEventInfo.get(Event.Sequencing.USER_ID));
        Assert.assertEquals(expectedSequencerName, sequencingEventInfo.get(Event.Sequencing.SEQUENCER_NAME));
        Assert.assertEquals(sequencedNode.getIdentifier(), sequencingEventInfo.get(Event.Sequencing.SEQUENCED_NODE_ID));

        Assert.assertEquals(sequencedNode.getPath(), sequencingEventInfo.get(Event.Sequencing.SEQUENCED_NODE_PATH));
        Assert.assertEquals(expectedSelectedPath, sequencingEventInfo.get(Event.Sequencing.SELECTED_PATH));
        Assert.assertEquals(expectedOutputPath, sequencingEventInfo.get(Event.Sequencing.OUTPUT_PATH));
        return sequencingEventInfo;
    }

    protected final class SequencingListener implements EventListener {

        @SuppressWarnings( "synthetic-access" )
        @Override
        public void onEvent( EventIterator events ) {
            while (events.hasNext()) {
                try {
                    Event event = (Event)events.nextEvent();
                    smokeCheckSequencingEvent(event,
                                              NODE_SEQUENCED,
                                              SEQUENCED_NODE_ID,
                                              SEQUENCED_NODE_PATH,
                                              OUTPUT_PATH,
                                              SELECTED_PATH,
                                              SEQUENCER_NAME,
                                              USER_ID);
                    sequencingEvents.putIfAbsent((String)event.getInfo().get(SEQUENCED_NODE_PATH), event);

                    String nodePath = event.getPath();
                    logger.debug("New sequenced node at: " + nodePath);
                    sequencedNodes.put(nodePath, session().getNode(nodePath));

                    // signal the node is available
                    createWaitingLatchIfNecessary(nodePath, nodeSequencedLatches);
                    nodeSequencedLatches.get(nodePath).countDown();
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
    }

    protected final class SequencingFailureListener implements EventListener {
        @SuppressWarnings( "synthetic-access" )
        @Override
        public void onEvent( EventIterator events ) {
            while (events.hasNext()) {
                try {
                    Event event = (Event)events.nextEvent();
                    smokeCheckSequencingEvent(event,
                                              NODE_SEQUENCING_FAILURE,
                                              SEQUENCED_NODE_ID,
                                              SEQUENCED_NODE_PATH,
                                              Event.Sequencing.SEQUENCING_FAILURE_CAUSE,
                                              OUTPUT_PATH,
                                              SELECTED_PATH,
                                              SEQUENCER_NAME,
                                              USER_ID);
                    String nodePath = event.getPath();

                    sequencingEvents.putIfAbsent(nodePath, event);
                    createWaitingLatchIfNecessary(nodePath, sequencingFailureLatches);
                    sequencingFailureLatches.get(nodePath).countDown();
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
    }
}

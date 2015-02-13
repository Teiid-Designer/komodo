/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.importer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.modeshape.jcr.api.JcrConstants.JCR_MIXIN_TYPES;
import static org.modeshape.jcr.api.JcrConstants.JCR_PRIMARY_TYPE;
import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.jcr.Node;
import javax.jcr.PropertyIterator;
import javax.jcr.Session;
import javax.jcr.observation.ObservationManager;
import org.komodo.repository.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.test.utils.AbstractLocalRepositoryTest;
import org.komodo.test.utils.NodePathListener;
import org.komodo.utils.KLog;
import org.modeshape.jcr.JcrSession;
import org.modeshape.jcr.api.JcrConstants;
import org.modeshape.jcr.api.observation.Event.Sequencing;

/**
 * AbstractImporterTest
 */
@SuppressWarnings( {"nls"} )
public abstract class AbstractImporterTest extends AbstractLocalRepositoryTest implements Sequencing {

    protected static final String DATA_DIRECTORY = File.separator + "data"; //$NON-NLS-1$
    private NodePathListener nodePathListener;

    protected InputStream setup(String fileName) {
        InputStream stream = getClass().getResourceAsStream(DATA_DIRECTORY + File.separator + fileName);
        assertNotNull(stream);
        return stream;
    }

    /**
     * @param countdown equivalent to number of sql query expressions to be sequenced
     * @param pathsToBeSequenced wilcarded patterns against which to compare the sequenced nodes
     * @return the latch for awaiting the sequencing
     * @throws Exception
     */
    protected CountDownLatch addSequencePathListener(UnitOfWork uow, final String... pathsToBeSequenced) throws Exception {
        Session session = session(uow);
        ObservationManager manager = session.getWorkspace().getObservationManager();
        assertNotNull(manager);

        final CountDownLatch updateLatch = new CountDownLatch(pathsToBeSequenced.length);
        List<String> seqPaths = Arrays.asList(pathsToBeSequenced);
        nodePathListener = new NodePathListener(seqPaths, updateLatch);
        manager.addEventListener(nodePathListener, NODE_SEQUENCED, null, true, null, null, false);
        return updateLatch;
    }

    protected abstract KomodoObject runImporter(Repository repository,
                                                UnitOfWork uow,
                                                InputStream inputStream,
                                                ImportOptions importOptions,
                                                ImportMessages importMessages);

    protected abstract KomodoObject runImporter(Repository repository, 
                                                UnitOfWork uow,
                                                File file,
                                                ImportOptions importOptions,
                                                ImportMessages importMessages);

    protected KomodoObject runImporter(Repository repository, UnitOfWork uow, Object content,
                                                                 ImportOptions importOptions, ImportMessages importMessages) {
        if (content instanceof File)
            return runImporter(repository, uow, (File) content, importOptions, importMessages);
        else if (content instanceof InputStream)
            return runImporter(repository, uow, (InputStream) content, importOptions, importMessages);

        fail("Content should be a file or input stream");
        return null;
    }
    
    protected KomodoObject executeImporter(Object content, ImportOptions importOptions,
                                                                        ImportMessages importMessages, String... sequencePaths)
                                                                        throws Exception {
        assertNotNull(_repo);
        assertNotNull(content);
        assertNotNull(importOptions);
        assertNotNull(importMessages);

//        setLoggingLevel(Level.ALL);

        UnitOfWork uow = _repo.createTransaction("test-importer", false, null);
        Session session = session(uow);

        CountDownLatch updateLatch = addSequencePathListener(uow, sequencePaths);

        KomodoObject kObject = runImporter(_repo, uow, content, importOptions, importMessages);
        if (importMessages.hasError()) {
            return null;
        }

        //
        // Save the session to ensure that the sequencers are executed.
        //
        // However, we cannot logout:
        // * Sequence listeners added through the observation manager are only applicable for the lifetime
        //    of the session.
        // * Once session.logout() is called, then all listeners are removed from the RepositoryChangeBus so
        //    sequencing will fire a completion event but the sequence listener will never receive it.
        //
        session.save();

        // Wait for the sequencing of the repository or timeout of 3 minutes
        assertTrue(updateLatch.await(3, TimeUnit.MINUTES));

        //
        // Commit and logout of the session after waiting for the sequence listener
        // to observe the completion of the sequencing
        //
        uow.commit();

        traverse(kObject);

        return kObject;
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

    protected void traverse(UnitOfWork uow) throws Exception {
        Session session = session(uow);
        StringBuffer buffer = new StringBuffer(NEW_LINE);
        traverse(TAB, session.getRootNode(), buffer);
        KLog.getLogger().info(buffer.toString());
    }

    private Session session(UnitOfWork uow) throws Exception {
        if (!(uow instanceof UnitOfWorkImpl))
            throw new Exception("Attempt to extract session from unit of work which is not a UnitOfWorkImpl");

        Session session = ((UnitOfWorkImpl)uow).getSession();
        return session;
    }

    /**
     * @param property
     * @return String representation of property and its values
     * @throws Exception
     */
    private String toString(Property property) throws Exception {
        StringBuilder sb = new StringBuilder();
        try {
            sb.append(property.getName(null)).append('=');
            if (property.isMultiple(null)) {
                sb.append('[');
                Object[] values = property.getValues(null);
                for (int i = 0; i < values.length; ++i) {
                    Object value = values[i];
                    sb.append(value);
                    if ((i + 1) < values.length)
                        sb.append(',');
                }
                sb.append(']');
            } else {
                Object value = property.getValue(null);
                sb.append(value);
            }
        } catch (Exception e) {
            sb.append(" on deleted node ").append(property.getAbsolutePath());
        }

        return sb.toString();
    }

    private void traverse(String tabs, KomodoObject kObject, StringBuffer buffer) throws Exception {
        buffer.append(tabs + kObject.getName(null) + NEW_LINE);

        String[] propertyNames = kObject.getPropertyNames(null);

        for (String propertyName : propertyNames) {
            Property property = kObject.getProperty(null, propertyName);
            buffer.append(tabs + TAB + "@" + toString(property) + NEW_LINE);
        }

        KomodoObject[] children = kObject.getChildren(null);
        for (int i = 0; i < children.length; ++i)
            traverse(tabs + TAB, children[i], buffer);
    }

    protected void traverse(KomodoObject kObject) throws Exception {
        StringBuffer buffer = new StringBuffer(NEW_LINE);
        traverse(TAB, kObject, buffer);
        KLog.getLogger().info(buffer.toString());
    }

    protected String enc(String input) throws Exception {
        UnitOfWork uow = _repo.createTransaction("test-encode-transaction", true, null);
        try {
            return ((JcrSession)session(uow)).encode(input);
        } finally {
            uow.commit();
        }
    }

    protected void verifyProperty(KomodoObject node, String propertyName, String... expectedValues) throws Exception {
        Property property = node.getProperty(null, propertyName);
        assertNotNull(property);

        List<String> values;
        if (property.isMultiple(null))
            values = Arrays.asList(property.getStringValues(null));
        else {
            values = new ArrayList<String>();
            values.add(property.getStringValue(null));
        }

        assertEquals(expectedValues.length, values.size());
        for (String expectedValue : expectedValues) {
            assertTrue(values.contains(expectedValue));
        }
    }

    protected void verifyPrimaryType(KomodoObject node, String expectedValue) throws Exception {
        verifyProperty(node, JCR_PRIMARY_TYPE, expectedValue);
    }

    protected void verifyMixinType(KomodoObject node, String... expectedValues) throws Exception {
        Property property = node.getProperty(null, JCR_MIXIN_TYPES);
        assertNotNull(property);

        List<String> values;
        if (property.isMultiple(null))
            values = Arrays.asList(property.getStringValues(null));
        else {
            values = new ArrayList<String>();
            values.add(property.getStringValue(null));
        }

        assertEquals(expectedValues.length, values.size());
        for (String expectedValue : expectedValues) {
            assertTrue(values.contains(expectedValue));
        }
    }

    protected void verifyBaseProperties(KomodoObject node, String primaryType, String mixinType) throws Exception {
        verifyPrimaryType(node, primaryType);
        if (mixinType == null)
            return;

        // Only if mixinType is not null do we check it
        verifyMixinType(node, mixinType);
    }

    protected KomodoObject verify(KomodoObject parentNode, String relativePath, String primaryType, int index, String mixinType) throws Exception {
        String indexExp = EMPTY_STRING;
        if (index > -1)
            indexExp = OPEN_SQUARE_BRACKET + index + CLOSE_SQUARE_BRACKET;

        KomodoObject childNode = null;
        if (parentNode.hasChild(null, relativePath)) {
            childNode = parentNode.getChild(null, relativePath + indexExp);
        } else childNode = parentNode.getChild(null, enc(relativePath) + indexExp);
        assertNotNull(childNode);

        verifyBaseProperties(childNode, primaryType, mixinType);
        return childNode;
    }

    protected KomodoObject verify(KomodoObject parentNode, String relativePath, String primaryType, String mixinType) throws Exception {
        return verify(parentNode, relativePath, primaryType, -1, mixinType);
    }

    protected KomodoObject verify(KomodoObject parentNode, String relativePath, String primaryType) throws Exception {
        return verify(parentNode, relativePath, primaryType, -1, null);
    }

    protected KomodoObject verify(KomodoObject parentNode, String relativePath) throws Exception {
        return verify(parentNode, relativePath, JcrConstants.NT_UNSTRUCTURED, -1, null);
    }
}

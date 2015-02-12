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
package org.komodo.importer.ddl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.modeshape.jcr.api.JcrConstants.JCR_MIXIN_TYPES;
import static org.modeshape.jcr.api.JcrConstants.JCR_PRIMARY_TYPE;
import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import javax.jcr.Node;
import javax.jcr.PropertyIterator;
import javax.jcr.Session;
import javax.jcr.observation.ObservationManager;
import org.komodo.repository.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.test.utils.AbstractLocalRepositoryTest;
import org.komodo.test.utils.SequencerListener;
import org.komodo.utils.KLog;
import org.modeshape.jcr.JcrSession;
import org.modeshape.jcr.api.JcrConstants;
import org.modeshape.jcr.api.observation.Event.Sequencing;

/**
 * AbstractCommandTest
 */
@SuppressWarnings("nls")
public abstract class AbstractDdlImporterTest extends AbstractLocalRepositoryTest
    implements Sequencing {

    protected static final String DATA_DIRECTORY = File.separator + "data"; //$NON-NLS-1$

	protected InputStream setup(String ddlFileName) {
	    InputStream stream = getClass().getResourceAsStream(DATA_DIRECTORY + File.separator + ddlFileName);
	    assertNotNull(stream);
		return stream;
	}

	/**
     * @param countdown equivalent to number of sql query expressions to be sequenced
     * @param pathsToBeSequenced wilcarded patterns against which to compare the sequenced nodes
     * @return the latch for awaiting the sequencing
     * @throws Exception
     */
	protected CountDownLatch addSequencePathListener(UnitOfWork uow, int countdown, final List<String> pathsToBeSequenced) throws Exception {
	    Session session = session(uow);
	    ObservationManager manager = session.getWorkspace().getObservationManager();
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
    protected CountDownLatch addSequencePathListener(UnitOfWork uow, int countdown, String pathToBeSequenced) throws Exception {
        List<String> pathsToBeSequenced = new ArrayList<String>();
        pathsToBeSequenced.add(pathToBeSequenced);
        return addSequencePathListener(uow, countdown, pathsToBeSequenced);
    }

    private void traverse(String tabs, Node node, StringBuffer buffer) throws Exception {
        buffer.append(tabs + node.getName() + NEW_LINE);

        PropertyIterator propertyIterator = node.getProperties();
        while(propertyIterator.hasNext()) {
            javax.jcr.Property property = propertyIterator.nextProperty();
            buffer.append(tabs + TAB + "@" + property.toString() + NEW_LINE);
        }

        javax.jcr.NodeIterator children = node.getNodes();
        while(children.hasNext()) {
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
        if (! (uow instanceof UnitOfWorkImpl))
            throw new Exception("Attempt to extract session from unit of work which is not a UnitOfWorkImpl");

        Session session = ((UnitOfWorkImpl) uow).getSession();
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

    protected String enc(UnitOfWork uow, String input) throws Exception {
        return ((JcrSession) session(uow)).encode(input);
    }

    protected void verifyProperty( KomodoObject node, String propertyName, String expectedValue ) throws Exception {
        Property property = node.getProperty(null, propertyName);
        assertNotNull(property);

        String value;
        if (property.isMultiple(null))
            value = property.getStringValues(null)[0];
        else
            value = property.getStringValue(null);

        assertEquals(expectedValue, value);
    }

    protected void verifyPrimaryType( KomodoObject node, String expectedValue ) throws Exception {
        verifyProperty(node, JCR_PRIMARY_TYPE, expectedValue);
    }

    protected void verifyMixinType( KomodoObject node, String expectedValue ) throws Exception {
        verifyProperty(node, JCR_MIXIN_TYPES, expectedValue);
    }

    protected void verifyBaseProperties( KomodoObject node, String primaryType, String mixinType) throws Exception {
        verifyPrimaryType(node, primaryType);
        if (mixinType == null)
            return;

        // Only if mixinType is not null do we check it
        verifyMixinType(node, mixinType);
    }

    protected KomodoObject verify(UnitOfWork uow, KomodoObject parentNode, String relativePath, int index, String mixinType) throws Exception {
        String indexExp = EMPTY_STRING;
        if (index > -1)
            indexExp = OPEN_SQUARE_BRACKET + index + CLOSE_SQUARE_BRACKET;

        KomodoObject childNode = null;
        if (parentNode.hasChild(null, relativePath)) {
            childNode = parentNode.getChild(null, relativePath + indexExp);
        } else
            childNode = parentNode.getChild(null, enc(uow, relativePath) + indexExp);
        assertNotNull(childNode);

        verifyBaseProperties(childNode, JcrConstants.NT_UNSTRUCTURED, mixinType);
        return childNode;
    }

    protected KomodoObject verify(UnitOfWork uow, KomodoObject parentNode, String relativePath, String mixinType) throws Exception {
        return verify(uow, parentNode, relativePath, -1, mixinType);
    }

    protected KomodoObject verify(UnitOfWork uow, KomodoObject parentNode, String relativePath) throws Exception {
        return verify(uow, parentNode, relativePath, -1, null);
    }
}

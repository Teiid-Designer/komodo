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
package org.komodo.modeshape.vdb.test.export;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.util.concurrent.TimeUnit;
import javax.jcr.Node;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;
import org.junit.Test;
import org.komodo.modeshape.visitor.VdbNodeVisitor;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.test.utils.AbstractSequencerTest;
import org.komodo.test.utils.SynchronousSequencerListener;
import org.komodo.test.utils.TestUtilities;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;
import org.w3c.dom.Document;

/**
 *
 */
@SuppressWarnings({"nls", "javadoc"})
public class TestVdbExport extends AbstractSequencerTest {

	@Override
    protected String getTestConfigurationPath() {
        return "test-repository-config.json";
    }

    private void saveSession() throws Exception {
        String requestId = getClass().getName() + session().hashCode();
        SynchronousSequencerListener listener = addSequencingListenerLatch(requestId, session());

        session().save();

        assertTrue(listener.await(TIME_TO_WAIT, TimeUnit.MINUTES));
        assertFalse(listener.exceptionOccurred());
    }

    private VdbNodeVisitor createNodeVisitor(Writer writer) throws Exception {
        XMLOutputFactory xof = XMLOutputFactory.newInstance();
        XMLStreamWriter xtw = null;
        xtw = xof.createXMLStreamWriter(writer);

        return new VdbNodeVisitor(getTeiidVersion(), xtw);
    }

    @Test(timeout=3000000)
    public void testBasicVdbExport() throws Exception {
        Node twitterExampleNode = TestUtilities.createTweetExampleNode(rootNode);
        saveSession();

        traverse(twitterExampleNode);

        //
        // Sequencing completed, now verify
        //
        Node tweet = verify(twitterExampleNode,"twitterview/Tweet", TeiidDdlLexicon.CreateTable.VIEW_STATEMENT);
        verify(tweet, TeiidSqlLexicon.Query.ID, TeiidSqlLexicon.Query.ID);

        //
        // Create node visitor and visit the jcr nodes
        //
        StringWriter testWriter = new StringWriter();
        VdbNodeVisitor visitor = createNodeVisitor(testWriter);
        visitor.visit(twitterExampleNode);

        //
        // Create an XML Document from the filled writer
        //
        String testXML = testWriter.toString();
        System.out.println(testXML);
        Document testDoc = TestUtilities.createDocument(testXML);

        //
        // Create comparison XML Document from the example xml files
        //
        InputStream compareStream = TestUtilities.tweetExample();
        Document compareDoc = TestUtilities.createDocument(compareStream);

        // Compare the XML documents. Unlike Document.isEqualNode(document)
        // the document nodes can be in a different order and the documents are
        // still equal.
        TestUtilities.compareDocuments(compareDoc, testDoc);
    }

    @Test(timeout=3000000)
    public void testBasicVdbExportUndefinedAttribute() throws Exception {
        Node twitterExampleNode = TestUtilities.createTweetExampleNoTransDescripNode(rootNode);
        saveSession();

        traverse(twitterExampleNode);

        //
        // Sequencing completed, now verify
        //
        Node tweet = verify(twitterExampleNode,"twitterview/Tweet", TeiidDdlLexicon.CreateTable.VIEW_STATEMENT);
        verify(tweet, TeiidSqlLexicon.Query.ID, TeiidSqlLexicon.Query.ID);

        //
        // Create node visitor and visit the jcr nodes
        //
        StringWriter testWriter = new StringWriter();
        VdbNodeVisitor visitor = createNodeVisitor(testWriter);
        visitor.visit(twitterExampleNode);

        //
        // Create an XML Document from the filled writer
        //
        String testXML = testWriter.toString();
        System.out.println(testXML);
        Document testDoc = TestUtilities.createDocument(testXML);

        //
        // Create comparison XML Document from the example xml files
        //
        InputStream compareStream = TestUtilities.undefinedAttrExample();
        Document compareDoc = TestUtilities.createDocument(compareStream);

        // Compare the XML documents. Unlike Document.isEqualNode(document)
        // the document nodes can be in a different order and the documents are
        // still equal.
        TestUtilities.compareDocuments(compareDoc, testDoc);
    }

    @Test(timeout=3000000)
    public void testAllElementsVdbExport() throws Exception {
        Node allElementsNode = TestUtilities.createAllElementsExampleNode(rootNode);
        saveSession();

        traverse(allElementsNode);

        //
        // Sequencing completed, now verify
        //
        Node testNode = verify(allElementsNode, "model-two/Test", TeiidDdlLexicon.CreateTable.VIEW_STATEMENT);
        verify(testNode, TeiidSqlLexicon.Query.ID, TeiidSqlLexicon.Query.ID);

        //
        // Create node visitor and visit the jcr nodes
        //
        StringWriter testWriter = new StringWriter();
        VdbNodeVisitor visitor = createNodeVisitor(testWriter);
        visitor.visit(allElementsNode);

        //
        // Create an XML Document from the filled writer
        //
        String testXML = testWriter.toString();
        Document testDoc = TestUtilities.createDocument(testXML);

        //
        // Create comparison XML Document from the example xml files
        //
        InputStream compareStream = TestUtilities.allElementsExample();
        Document compareDoc = TestUtilities.createDocument(compareStream);

        // Compare the XML documents. Unlike Document.isEqualNode(document)
        // the document nodes can be in a different order and the documents are
        // still equal.
        TestUtilities.compareDocuments(compareDoc, testDoc);
    }
}
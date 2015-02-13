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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.jcr.Node;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;
import org.junit.Test;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.visitor.VdbNodeVisitor;
import org.komodo.test.utils.AbstractSequencerTest;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.InputSource;

/**
 *
 */
@SuppressWarnings({"nls", "javadoc"})
public class TestVdbExport extends AbstractSequencerTest {

    /**
     * Tweet example file name
     */
    private static final String TWEET_EXAMPLE_NAME = "tweet-example-vdb";

    /**
     * Tweet example file suffix
     */
    private static final String TWEET_EXAMPLE_SUFFIX = DOT + XML;

    /**
     * All elements example file name
     */
    private static final String ALL_ELEMENTS_EXAMPLE_NAME = "teiid-vdb-all-elements";

    /**
     * All elements example file suffix
     */
    private static final String ALL_ELEMENTS_EXAMPLE_SUFFIX = DOT + XML;
    
    private InputStream getResourceAsStream(String parentDirectory, String fileName, String suffix) throws Exception {
        String filePath = parentDirectory + File.separator + fileName + suffix;
        InputStream fileStream = getClass().getResourceAsStream(filePath);
        assertNotNull("File " + filePath + " does not exist", fileStream);

        return fileStream;
    }

    private String streamToString(InputStream inStream) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(inStream));
        StringBuilder builder = new StringBuilder();
        String line;
        while ((line = reader.readLine()) != null) {
            builder.append(line);
            builder.append(NEW_LINE);
        }

        return builder.toString();
    }

    private Node createTweetExampleNode() throws Exception {
        String REST_TRANSLATOR = "rest";
        String TWITTER_MODEL = "twitter";
        String TWITTER_VIEW_MODEL = "twitterview";

        /*
         * tweet-example-vdb.xml
         *      @jcr:primaryType=vdb:virtualDatabase
         *      @jcr:mixinTypes=[mode:derived,mix:referenceable]
         *      @jcr:uuid={uuid-to-be-created}
         *      @mode:sha1={sha1-to-be-created}
         *      @vdb:preview=false
         *      @vdb:version=1
         *      @vdb:originalFile=/vdbs/declarativeModels-vdb.xml
         *      @vdb:name=twitter
         *      @vdb:description=Shows how to call Web Services
         *      @UseConnectorMetadata=cached
         */
        Node tweetExample = rootNode.addNode(TWEET_EXAMPLE_NAME + TWEET_EXAMPLE_SUFFIX);
        tweetExample.setPrimaryType(VdbLexicon.Vdb.VIRTUAL_DATABASE);
        tweetExample.addMixin("mode:derived");
        tweetExample.addMixin("mix:referenceable");
        tweetExample.setProperty(VdbLexicon.Vdb.NAME, "twitter");
        tweetExample.setProperty(VdbLexicon.Vdb.DESCRIPTION, "Shows how to call Web Services");

        // Miscellaneous property
        tweetExample.setProperty("UseConnectorMetadata", "cached");

        tweetExample.setProperty(VdbLexicon.Vdb.ORIGINAL_FILE, "/vdbs/" + TWEET_EXAMPLE_NAME + TWEET_EXAMPLE_SUFFIX);
        tweetExample.setProperty(VdbLexicon.Vdb.PREVIEW, false);
        tweetExample.setProperty(VdbLexicon.Vdb.VERSION, 1);

        /*
         *      vdb:translators
         *          @jcr:primaryType=vdb:translators
         */
        Node translators = tweetExample.addNode(VdbLexicon.Vdb.TRANSLATORS);
        translators.setPrimaryType(VdbLexicon.Vdb.TRANSLATORS);

        /*
         *          rest
         *              @jcr:primaryType=vdb:translator
         *              @DefaultServiceMode=MESSAGE
         *              @DefaultBinding=HTTP
         *              @vdb:type=ws
         *              @vdb:description=Rest Web Service translator
         */
        Node rest = translators.addNode(REST_TRANSLATOR);
        rest.setPrimaryType(VdbLexicon.Translator.TRANSLATOR);
        rest.setProperty(VdbLexicon.Translator.DESCRIPTION, "Rest Web Service translator");
        rest.setProperty("DefaultServiceMode", "MESSAGE");
        rest.setProperty("DefaultBinding", "HTTP");
        rest.setProperty(VdbLexicon.Translator.TYPE, "ws");

        /*
         *      twitter
         *          @jcr:primaryType=vdb:declarativeModel
         *          @jcr:uuid={uuid-to-be-created}
         *          @mmcore:modelType=PHYSICAL
         *          @vdb:sourceTranslator=rest
         *          @vdb:sourceName=twitter
         *          @vdb:metadataType=DDL
         *          @vdb:visible=true
         *          @vdb:sourceJndiName=java:/twitterDS
         */
        Node twitter = tweetExample.addNode(TWITTER_MODEL);
        twitter.setPrimaryType(VdbLexicon.Vdb.DECLARATIVE_MODEL);
        twitter.setProperty(CoreLexicon.JcrId.MODEL_TYPE, CoreLexicon.ModelType.PHYSICAL);
        twitter.setProperty(VdbLexicon.Model.VISIBLE, true);
        twitter.setProperty(VdbLexicon.Model.METADATA_TYPE, "DDL");

        /*
         *          vdb:sources
         *              @jcr:primaryType=vdb:sources
         */
        Node twitterSources = twitter.addNode(VdbLexicon.Vdb.SOURCES, VdbLexicon.Vdb.SOURCES);

        /*
         *              twitter
         *                  @jcr:primaryType=vdb:source
         *                  @vdb:sourceTranslator=rest
         *                  @vdb:sourceJndiName=java:/twitterDS
         */
        Node twitterSource = twitterSources.addNode(TWITTER_MODEL, VdbLexicon.Source.SOURCE);
        twitterSource.setProperty(VdbLexicon.Source.TRANSLATOR, REST_TRANSLATOR);
        twitterSource.setProperty(VdbLexicon.Source.JNDI_NAME, "java:/twitterDS");

        /*      
         *      twitterview
         *          @jcr:primaryType=vdb:declarativeModel
         *          @jcr:uuid={uuid-to-be-created}
         *          @mmcore:modelType=VIRTUAL
         *          @vdb:visible=true
         *          @vdb:metadataType=DDL
         *          @vdb:modelDefinition=CREATE VIRTUAL PROCEDURE getTweets(query varchar) RETURNS (created_on varchar(25), from_user varchar(25), to_user varchar(25), profile_image_url varchar(25), source varchar(25), text varchar(140)) AS select tweet.* from (call twitter.invokeHTTP(action => 'GET', endpoint =>querystring('',query as "q"))) w, XMLTABLE('results' passing JSONTOXML('myxml', w.result) columns created_on string PATH 'created_at', from_user string PATH 'from_user', to_user string PATH 'to_user', profile_image_url string PATH 'profile_image_url', source string PATH 'source', text string PATH 'text') tweet; CREATE VIEW Tweet AS select * FROM twitterview.getTweets;
         */
        Node twitterView = tweetExample.addNode(TWITTER_VIEW_MODEL);
        twitterView.setPrimaryType(VdbLexicon.Vdb.DECLARATIVE_MODEL);
        twitterView.setProperty(CoreLexicon.JcrId.MODEL_TYPE, CoreLexicon.ModelType.VIRTUAL);
        twitterView.setProperty(VdbLexicon.Model.METADATA_TYPE, "DDL");
        twitterView.setProperty(VdbLexicon.Model.VISIBLE, true);

        StringBuffer modelDefinition = new StringBuffer();
        modelDefinition.append("CREATE VIRTUAL PROCEDURE getTweets(IN query varchar) ")
                                .append("RETURNS TABLE (created_on varchar(25), from_user varchar(25), ")
                                .append("to_user varchar(25), profile_image_url varchar(25), source ")
                                .append("varchar(25), text varchar(140)) AS select tweet.* from ")
                                .append("(EXEC twitter.invokeHTTP(")
                                .append("action => 'GET', endpoint => querystring(\'', query as q))) AS w, ")
                                .append("XMLTABLE('results' passing JSONTOXML('myxml', w.result) columns ")
                                .append("created_on string PATH 'created_at', from_user string PATH 'from_user', ")
                                .append("to_user string PATH 'to_user', profile_image_url string PATH 'profile_image_url', ")
                                .append("source string PATH 'source', text string PATH 'text') AS tweet; ")
                                .append("CREATE VIEW Tweet AS select * FROM twitterview.getTweets;");
        twitterView.setProperty(VdbLexicon.Model.MODEL_DEFINITION, modelDefinition.toString());
        session().save();

        return tweetExample;
    }

    private Node createAllElementsExampleNode() throws Exception {
        /*
         * teiid-vdb-all-elements.xml
         *      @jcr:primaryType=vdb:virtualDatabase
         *      @jcr:mixinTypes=[mode:derived,mix:referenceable]
         *      @jcr:uuid={uuid-to-be-created}
         *      @mode:sha1={sha1-to-be-created}
         *      @vdb:preview=false
         *      @vdb:version=1
         *      @vdb:originalFile=/vdbs/teiid-vdb-all-elements.xml
         *      @vdb:name=myVDB
         *      @vdb:description=vdb description
         *      @vdb:connectionType=NONE
         *      @vdb-property2=vdb-value2
         *      @vdb-property=vdb-value
         */
        Node myVdbExample = rootNode.addNode(ALL_ELEMENTS_EXAMPLE_NAME + ALL_ELEMENTS_EXAMPLE_SUFFIX);
        myVdbExample.setPrimaryType(VdbLexicon.Vdb.VIRTUAL_DATABASE);
        myVdbExample.addMixin("mode:derived");
        myVdbExample.addMixin("mix:referenceable");
        myVdbExample.setProperty(VdbLexicon.Vdb.NAME, "myVDB");
        myVdbExample.setProperty(VdbLexicon.Vdb.DESCRIPTION, "vdb description");
        myVdbExample.setProperty(VdbLexicon.Vdb.CONNECTION_TYPE, "NONE");
        myVdbExample.setProperty(VdbLexicon.Vdb.ORIGINAL_FILE, "/vdbs/" + ALL_ELEMENTS_EXAMPLE_NAME + ALL_ELEMENTS_EXAMPLE_SUFFIX);
        myVdbExample.setProperty(VdbLexicon.Vdb.PREVIEW, false);
        myVdbExample.setProperty(VdbLexicon.Vdb.VERSION, 1);
        myVdbExample.setProperty("vdb-property2", "vdb-value2");
        myVdbExample.setProperty("vdb-property", "vdb-value");

        /*
         *      vdb:importVdbs
         *          @jcr:primaryType=vdb:importVdb
         */
        Node importVdbs = myVdbExample.addNode(VdbLexicon.Vdb.IMPORT_VDBS, VdbLexicon.Vdb.IMPORT_VDBS);

        /*
         *          x
         *              @jcr:primaryType=vdb:importVdb
         *              @vdb:version=2
         *              @vdb:import-data-policies=false
         */
        Node importVdb = importVdbs.addNode("x", VdbLexicon.ImportVdb.IMPORT_VDB);
        importVdb.setProperty(VdbLexicon.ImportVdb.VERSION, 2);
        importVdb.setProperty(VdbLexicon.ImportVdb.IMPORT_DATA_POLICIES, false);

        /*
         *      model-one
         *          @jcr:primaryType=vdb:declarativeModel
         *          @jcr:uuid={uuid-to-be-created}
         *          @mmcore:modelType=PHYSICAL
         *          @description=model description
         *          @vdb:visible=false
         *          @model-prop=model-value-override
         */
        Node modelOne = myVdbExample.addNode("model-one");
        modelOne.setPrimaryType(VdbLexicon.Vdb.DECLARATIVE_MODEL);
        modelOne.setProperty(CoreLexicon.JcrId.MODEL_TYPE, CoreLexicon.ModelType.PHYSICAL);
        modelOne.setProperty(VdbLexicon.Vdb.DESCRIPTION, "model description");
        modelOne.setProperty(VdbLexicon.Model.VISIBLE, false);
        modelOne.setProperty("model-prop", "model-value-override");

        /*
         *          vdb:sources
         *              @jcr:primaryType=vdb:sources
         */
        Node model1Sources = modelOne.addNode(VdbLexicon.Vdb.SOURCES, VdbLexicon.Vdb.SOURCES);

        /*
         *              s1
         *                  @jcr:primaryType=vdb:source
         *                  @vdb:sourceTranslator=translator
         *                  @vdb:sourceJndiName=java:mybinding
         */
        Node model1Src1 = model1Sources.addNode("s1", VdbLexicon.Source.SOURCE);
        model1Src1.setProperty(VdbLexicon.Source.TRANSLATOR, "translator");
        model1Src1.setProperty(VdbLexicon.Source.JNDI_NAME, "java:mybinding");

        /*
         *      model-two
         *          @jcr:primaryType=vdb:declarativeModel
         *          @jcr:uuid={uuid-to-be-created}
         *          @mmcore:modelType=VIRTUAL
         *          @vdb:visible=true
         *          @model-prop=model-value
         */
        Node modelTwo = myVdbExample.addNode("model-two");
        modelTwo.setPrimaryType(VdbLexicon.Vdb.DECLARATIVE_MODEL);
        modelTwo.setProperty(CoreLexicon.JcrId.MODEL_TYPE, CoreLexicon.ModelType.VIRTUAL);
        modelTwo.setProperty(VdbLexicon.Model.VISIBLE, true);
        modelTwo.setProperty("model-prop", "model-value");
        modelTwo.setProperty(VdbLexicon.Model.METADATA_TYPE, "DDL");

        String modelDefinition = "CREATE VIEW Test AS select * FROM Test.getTest;";
        modelTwo.setProperty(VdbLexicon.Model.MODEL_DEFINITION, modelDefinition);
        
        /*
         *          vdb:sources
         *              @jcr:primaryType=vdb:sources
         */
        Node model2Sources = modelTwo.addNode(VdbLexicon.Vdb.SOURCES, VdbLexicon.Vdb.SOURCES);

        /*
         *              s1
         *                  @jcr:primaryType=vdb:source
         *                  @vdb:sourceTranslator=translator
         *                  @vdb:sourceJndiName=java:binding-one
         */
        Node model2Src1 = model2Sources.addNode("s1", VdbLexicon.Source.SOURCE);
        model2Src1.setProperty(VdbLexicon.Source.TRANSLATOR, "translator");
        model2Src1.setProperty(VdbLexicon.Source.JNDI_NAME, "java:binding-one");

        /*
         *              s2
         *                  @jcr:primaryType=vdb:source
         *                  @vdb:sourceTranslator=translator
         *                  @vdb:sourceJndiName=java:binding-two
         */
        Node model2Src2 = model2Sources.addNode("s2", VdbLexicon.Source.SOURCE);
        model2Src2.setProperty(VdbLexicon.Source.TRANSLATOR, "translator");
        model2Src2.setProperty(VdbLexicon.Source.JNDI_NAME, "java:binding-two");
        
        /*
         *      vdb:translators
         *          @jcr:primaryType=vdb:translators
         */
        Node translators = myVdbExample.addNode(VdbLexicon.Vdb.TRANSLATORS);
        translators.setPrimaryType(VdbLexicon.Vdb.TRANSLATORS);

        /*
         *          oracleOverride
         *              @jcr:primaryType=vdb:translator
         *              @vdb:description=hello world
         *              @vdb:type=oracle
         *              my-property=my-value
         */
        Node oraTranslator = translators.addNode("oracleOverride");
        oraTranslator.setPrimaryType(VdbLexicon.Translator.TRANSLATOR);
        oraTranslator.setProperty(VdbLexicon.Translator.DESCRIPTION, "hello world");
        oraTranslator.setProperty(VdbLexicon.Translator.TYPE, "oracle");
        oraTranslator.setProperty("my-property", "my-value");

        /*
         *      vdb:dataRoles
         *          @jcr:primaryType=vdb:dataRoles
         */
        Node dataRoles = myVdbExample.addNode(VdbLexicon.Vdb.DATA_ROLES, VdbLexicon.Vdb.DATA_ROLES);

        /*
         *          roleOne
         *              @jcr:primaryType=vdb:dataRole
         *              @vdb:anyAuthenticated=false
         *              @vdb:grantAll=true
         *              @vdb:allowCreateTemporaryTables=true
         *              @vdb:description=roleOne described
         *              @vdb:mappedRoleNames=ROLE1, ROLE2
         */
        Node dataRole1 = dataRoles.addNode("roleOne", VdbLexicon.DataRole.DATA_ROLE);
        dataRole1.setProperty(VdbLexicon.Translator.DESCRIPTION, "roleOne described");
        dataRole1.setProperty(VdbLexicon.DataRole.ANY_AUTHENTICATED, false);
        dataRole1.setProperty(VdbLexicon.DataRole.GRANT_ALL, true);
        dataRole1.setProperty(VdbLexicon.DataRole.ALLOW_CREATE_TEMP_TABLES, true);
        dataRole1.setProperty(VdbLexicon.DataRole.MAPPED_ROLE_NAMES, new String[]{"ROLE1", "ROLE2"});

        /*
         *              vdb:permissions
         *                  @jcr:primaryType=vdb:permissions
         */
        Node permissions = dataRole1.addNode(VdbLexicon.DataRole.PERMISSIONS, VdbLexicon.DataRole.PERMISSIONS);

        /*
         *                  myTable.T1
         *                      @jcr.primaryType=vdb:permission
         *                      @allowRead=true
         */
        Node permission1 = permissions.addNode("myTable.T1", VdbLexicon.DataRole.Permission.PERMISSION);
        permission1.setProperty(VdbLexicon.DataRole.Permission.ALLOW_READ, true);

        /*
         *                  myTable.T2
         *                      @jcr.primaryType=vdb:permission
         *                      @allowCreate=true
         *                      @allowRead=false
         *                      @allowUpdate=true
         *                      @allowDelete=true
         *                      @allowExecute=true
         *                      @allowAlter=true
         */
        Node permission2 = permissions.addNode("myTable.T2", VdbLexicon.DataRole.Permission.PERMISSION);
        permission2.setProperty(VdbLexicon.DataRole.Permission.ALLOW_CREATE, true);
        permission2.setProperty(VdbLexicon.DataRole.Permission.ALLOW_READ, false);
        permission2.setProperty(VdbLexicon.DataRole.Permission.ALLOW_UPDATE, true);
        permission2.setProperty(VdbLexicon.DataRole.Permission.ALLOW_DELETE, true);
        permission2.setProperty(VdbLexicon.DataRole.Permission.ALLOW_EXECUTE, true);
        permission2.setProperty(VdbLexicon.DataRole.Permission.ALLOW_ALTER, true);

        /*
         *                      vdb:conditions
         *                          @jcr:primaryType=vdb:conditions
         */
        Node conditions = permission2.addNode(VdbLexicon.DataRole.Permission.CONDITIONS, VdbLexicon.DataRole.Permission.CONDITIONS);

        /*
         *                          col1 = user()
         *                              @jcr:primaryType=vdb:condition
         *                              @vdb:constraint=false
         */
        Node condition = conditions.addNode("col1 = user()", VdbLexicon.DataRole.Permission.Condition.CONDITION);
        condition.setProperty(VdbLexicon.DataRole.Permission.Condition.CONSTRAINT, false);

        /*
         *                  myTable.T2.col1
         *                      @jcr.primaryType=vdb:permission
         */
        Node permission3 = permissions.addNode("myTable.T2.col1", VdbLexicon.DataRole.Permission.PERMISSION);

        /*
         *                      vdb:masks
         *                          @jcr:primaryType=vdb:masks
         */
        Node masks = permission3.addNode(VdbLexicon.DataRole.Permission.MASKS, VdbLexicon.DataRole.Permission.MASKS);

        /*
         *                          col2
         *                              @jcr:primaryType=vdb:mask
         *                              @vdb:order=1
         */
        Node mask = masks.addNode("col2", VdbLexicon.DataRole.Permission.Mask.MASK);
        mask.setProperty(VdbLexicon.DataRole.Permission.Mask.ORDER, 1);

        /*
         *                  javascript
         *                      @jcr.primaryType=vdb:permission
         *                      @allowLanguage=true
         */
        Node permission4 = permissions.addNode("javascript", VdbLexicon.DataRole.Permission.PERMISSION);
        permission4.setProperty(VdbLexicon.DataRole.Permission.ALLOW_LANGUAGE, true);

        session().save();
        return myVdbExample;
    }

    private VdbNodeVisitor createNodeVisitor(Writer writer) throws Exception {
        XMLOutputFactory xof = XMLOutputFactory.newInstance();
        XMLStreamWriter xtw = null;
        xtw = xof.createXMLStreamWriter(writer);

        return new VdbNodeVisitor(getTeiidVersion(), xtw);
    }

    private Document createDocument(String xml) throws Exception {

        xml = xml.replaceAll(NEW_LINE, SPACE);
        xml = xml.replaceAll(">[\\s]+<", CLOSE_ANGLE_BRACKET + OPEN_ANGLE_BRACKET);
        xml = xml.replaceAll("[\\s]+", SPACE);
        xml = xml.replaceAll("CDATA\\[[\\s]+", "CDATA[");
        xml = xml.replaceAll("; \\]\\]", ";]]");

        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setIgnoringElementContentWhitespace(true);
        dbf.setIgnoringComments(true);
        DocumentBuilder db = dbf.newDocumentBuilder();

        Document doc = db.parse(new InputSource(new StringReader(xml)));
        doc .setXmlStandalone(true);
        doc.normalizeDocument();
        return doc;
    }

    private boolean compareAttributes(Element expectedElement, Element actualElement, StringBuilder errorMessages) {
        NamedNodeMap expectedAttrs = expectedElement.getAttributes();
        NamedNodeMap actualAttrs = actualElement.getAttributes();
        if (expectedAttrs.getLength() != actualAttrs.getLength()) {
            errorMessages.append(OPEN_ANGLE_BRACKET + expectedElement.getNodeName() + CLOSE_ANGLE_BRACKET + 
                                 " has different number of attributes (" + expectedAttrs.getLength() + ") to " + 
                                 OPEN_ANGLE_BRACKET + actualElement.getNodeName() + CLOSE_ANGLE_BRACKET +
                                 " (" + actualAttrs.getLength() + ")" + NEW_LINE);
            errorMessages.append(TAB + "Expected Attributes:" + NEW_LINE);
            for (int i = 0; i < expectedAttrs.getLength(); ++i) {
                errorMessages.append(TAB + expectedAttrs.item(i).getNodeName() + NEW_LINE);
            }
            errorMessages.append(NEW_LINE + TAB + "Actual Attributes:" + NEW_LINE);
            for (int i = 0; i < actualAttrs.getLength(); ++i) {
                errorMessages.append(TAB + actualAttrs.item(i).getNodeName() + NEW_LINE);
            }
            return false;
        }

        for (int i = 0; i < expectedAttrs.getLength(); i++) {
            Attr expectedAttr = (Attr)expectedAttrs.item(i);

            Attr actualAttr = null;
            if (expectedAttr.getNamespaceURI() == null)
                actualAttr = (Attr)actualAttrs.getNamedItem(expectedAttr.getName());
            else
                actualAttr = (Attr)actualAttrs.getNamedItemNS(expectedAttr.getNamespaceURI(), expectedAttr.getLocalName());

            if (actualAttr == null) {
                errorMessages.append("Attribute " + expectedAttr.getName() + " is missing from " + actualElement.getNodeName() + NEW_LINE);
                return false;
            }

            if (! expectedAttr.getValue().equals(actualAttr.getValue())) {
                errorMessages.append("Attribute '" + expectedAttr.getName() +
                                     "' expected value of '" + expectedAttr.getValue() + "' but was '" + actualAttr.getValue() +
                                     "' in " + OPEN_ANGLE_BRACKET + expectedElement.getNodeName() +
                                     CLOSE_ANGLE_BRACKET + NEW_LINE);
                return false;
            }
        }

        return true;
    }

    private boolean compareElements(org.w3c.dom.Node expected, org.w3c.dom.Node actual, StringBuilder errorMessages) {
        Element expectedElement = (Element) expected;
        Element actualElement = (Element) actual;
        String errorMsg = "Failed to match <" + expected.getNodeName() + "> with <" + actual.getNodeName() + ">: ";

        // compare element names
        if (expectedElement.getLocalName() != null) {
            if (! expectedElement.getLocalName().equals(actualElement.getLocalName()))
                errorMessages.append(errorMsg + " Different name\n");
                return false;
        }

        // compare element ns
        if (expectedElement.getNamespaceURI() != null) {
            if (! expectedElement.getNamespaceURI().equals(actualElement.getNamespaceURI()))
                errorMessages.append(errorMsg + " Different namespace URI\n");
                return false;
        }

        // compare attributes
        if (! compareAttributes(expectedElement, actualElement, errorMessages)) {
            return false;
        }

        // compare children
        NodeList expectedChildren = expectedElement.getChildNodes();
        NodeList actualChildren = actualElement.getChildNodes();
        if (expectedChildren.getLength() != actualChildren.getLength()) {
            errorMessages.append(errorMsg + " Different number of children: Expected = " + expectedChildren.getLength() + "\t Actual = " + actualChildren.getLength() + NEW_LINE);
            errorMessages.append(TAB + "Expected:" + NEW_LINE);
            for (int i = 0; i < expectedChildren.getLength(); ++i) {
                errorMessages.append(TAB + expectedChildren.item(i).getNodeName() + NEW_LINE);
            }
            errorMessages.append(NEW_LINE + TAB + "Actual:" + NEW_LINE);
            for (int i = 0; i < actualChildren.getLength(); ++i) {
                errorMessages.append(TAB + actualChildren.item(i).getNodeName() + NEW_LINE);
            }
            return false;
        }

        // Same number but could be in a different order
        for (int i = 0; i < expectedChildren.getLength(); ++i) {
            org.w3c.dom.Node expectedChild = expectedChildren.item(i);

            boolean matchMade = false;
            for (int j = 0; j < actualChildren.getLength(); ++j) {
                org.w3c.dom.Node actualChild = actualChildren.item(j);
                if (! expectedChild.getNodeName().equals(actualChild.getNodeName()))
                    continue;

                if (compareNodes(expectedChild, actualChild, errorMessages)) {
                    matchMade = true;
                    break;
                }
            }

            if (! matchMade) {
                errorMessages.append(errorMsg + " Failed to match child " + expectedChild.getNodeName() + " to any node\n");
                return false;
            }
        }

        return true;
    }

    private boolean compareTextNode(org.w3c.dom.Text expected, org.w3c.dom.Text actual, StringBuilder errorMessages) {
        String expectedData = expected.getData().trim();
        String actualData = actual.getData().trim();
        if (expectedData.equalsIgnoreCase(actualData))
            return true;

        errorMessages.append(expected.getData() + " does not match " + actual.getData() + NEW_LINE);
        return false;
    }

    private boolean compareNodes(org.w3c.dom.Node expected, org.w3c.dom.Node actual, StringBuilder errorMessages) {
        if (expected.getNodeType() != actual.getNodeType()) {
            return false;
        }

        if (expected instanceof Element)
            return compareElements(expected, actual, errorMessages);
        else if (expected instanceof Text)
            return compareTextNode((Text) expected, (Text) actual, errorMessages);

        return false;
    }

    private void compareDocuments(Document expected, Document actual) {
        assertNotNull(expected);
        assertNotNull(actual);
        assertEquals(expected.getNodeType(), actual.getNodeType());

        StringBuilder errorMessages = new StringBuilder();
        if (! compareNodes(expected.getDocumentElement(), actual.getDocumentElement(), errorMessages))
            fail(errorMessages.toString());
    }

    @Test(timeout=3000000)
    public void testBasicVdbExport() throws Exception {
        Node twitterExampleNode = createTweetExampleNode();

        List<String> pathsToBeSequenced = new ArrayList<String>();
        pathsToBeSequenced.add(".*\\/" + StandardDdlLexicon.STATEMENTS_CONTAINER + "\\/getTweets\\/" + 
                                                              TeiidSqlLexicon.Query.ID);
        pathsToBeSequenced.add(".*\\/" + StandardDdlLexicon.STATEMENTS_CONTAINER + "\\/Tweet\\/" +
                                                              TeiidSqlLexicon.Query.ID);

        CountDownLatch updateLatch = addPathLatchListener(1, pathsToBeSequenced);

        // Wait for the starting of the repository or timeout of 3 minutes
        updateLatch.await(3, TimeUnit.MINUTES);

        traverse(twitterExampleNode);

        //
        // Sequencing completed, now verify
        //
        Node tweet = verify(twitterExampleNode,"twitterview/" + StandardDdlLexicon.STATEMENTS_CONTAINER + "/Tweet", TeiidDdlLexicon.CreateTable.VIEW_STATEMENT);
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
        Document testDoc = createDocument(testXML);

        //
        // Create comparison XML Document from the example xml files
        //
        InputStream compareStream = getResourceAsStream(DATA_DIRECTORY, TWEET_EXAMPLE_NAME, TWEET_EXAMPLE_SUFFIX);
        String compareXML = streamToString(compareStream);
        Document compareDoc = createDocument(compareXML);

        // Compare the XML documents. Unlike Document.isEqualNode(document)
        // the document nodes can be in a different order and the documents are
        // still equal.
        compareDocuments(compareDoc, testDoc);
    }

    @Test(timeout=3000000)
    public void testAllElementsVdbExport() throws Exception {
        Node allElementsNode = createAllElementsExampleNode();

        CountDownLatch updateLatch = addPathLatchListener(1, ".*\\/" + StandardDdlLexicon.STATEMENTS_CONTAINER + "\\/Test\\/" + TeiidSqlLexicon.Query.ID);

        // Wait for the starting of the repository or timeout of 3 minutes
        updateLatch.await(3, TimeUnit.MINUTES);

        traverse(allElementsNode);

        //
        // Sequencing completed, now verify
        //
        Node testNode = verify(allElementsNode, "model-two/" + StandardDdlLexicon.STATEMENTS_CONTAINER + "/Test", TeiidDdlLexicon.CreateTable.VIEW_STATEMENT);
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
        Document testDoc = createDocument(testXML);

        //
        // Create comparison XML Document from the example xml files
        //
        InputStream compareStream = getResourceAsStream(DATA_DIRECTORY, ALL_ELEMENTS_EXAMPLE_NAME, ALL_ELEMENTS_EXAMPLE_SUFFIX);
        String compareXML = streamToString(compareStream);
        Document compareDoc = createDocument(compareXML);

        // Compare the XML documents. Unlike Document.isEqualNode(document)
        // the document nodes can be in a different order and the documents are
        // still equal.
        compareDocuments(compareDoc, testDoc);
    }
}
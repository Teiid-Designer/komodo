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
package org.komodo.importer.vdb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import org.junit.Test;
import org.komodo.importer.AbstractImporterTest;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.ImportType;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.jcr.api.JcrConstants;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * Test Class to test Teiid VDB import
 *
 */
@SuppressWarnings({"nls", "javadoc"})
public class TestTeiidVdbImporter extends AbstractImporterTest {

    private static final String TWEET_EXAMPLE = "tweet-example-vdb.xml";

    private static final String ALL_ELEMENTS_EXAMPLE_NAME = "teiid-vdb-all-elements.xml";

    @Override
    protected KomodoObject runImporter(Repository repository, UnitOfWork uow,
                                                                 File file, ImportOptions importOptions,
                                                                 ImportMessages importMessages) {
        VdbImporter importer = new VdbImporter(_repo, uow);
        return importer.importVdb(file, importOptions, importMessages);
    }

    @Override
    protected KomodoObject runImporter(Repository repository, UnitOfWork uow,
                                                                 InputStream inputStream, ImportOptions importOptions,
                                                                 ImportMessages importMessages) {
        VdbImporter importer = new VdbImporter(_repo, uow);
        return importer.importVdb(inputStream, importOptions, importMessages);
    }

    /**
     * Test Error condition - bad VDB file name supplied
     * Expected Outcome - Error message saying that the supplied file is not found
     */
    @Test
    public void testBadVdbFile() throws Exception {
        ImportOptions importOptions = new ImportOptions();
        importOptions.setImportType(ImportType.VDB);

        ImportMessages importMessages = new ImportMessages();

        KomodoObject vdbNode = executeImporter(new File("unknown.xml"), importOptions, importMessages);

        // No model created
        assertNull("Failed - expected null model ", vdbNode);

        // Should have 1 error message
        assertEquals(1, importMessages.getErrorMessages().size());

        String msg = importMessages.getErrorMessages().get(0);
        assertEquals("The specified File \"unknown.xml\" was not found",msg);
    }

    /**
     * Test Error condition - unreadable VDB file supplied.
     * Expected Outcome - Error Message saying that the supplied file is not readable 
     */
    @Test
    public void testUnreadableVdbFile() throws Exception {
        InputStream vdbStream = setup(TWEET_EXAMPLE);
        File tmpFile = File.createTempFile("unreadableFile", ".xml");
        Files.copy(vdbStream, tmpFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
        tmpFile.deleteOnExit();

        assertTrue(tmpFile.exists());
        assertTrue(tmpFile.length() > 0);

        // Make file unreadable
        tmpFile.setReadable(false);

        // Options for the import (default)
        ImportOptions importOptions = new ImportOptions();
        importOptions.setImportType(ImportType.VDB);

        // Saves Messages during import
        ImportMessages importMessages = new ImportMessages();

        KomodoObject vdbNode = executeImporter(tmpFile, importOptions, importMessages);

        // Set back to readable
        tmpFile.setReadable(true);

        // No model created
        assertNull("Failed - expected null model ", vdbNode);

        // Should have 1 error message
        assertEquals(1, importMessages.getErrorMessages().size());

        String msg = importMessages.getErrorMessages().get(0);
        assertEquals("The specified File \"" + tmpFile.getName() + "\" is not readable", msg);
    }

    /**
     * Test Error condition - empty VDB string supplied
     * Expected Outcome - Error Message saying that the supplied VDB string is empty
     */
    @Test
    public void testEmptyVdbString() throws Exception {
        File tmpFile = File.createTempFile("emptyFile", ".xml");
        tmpFile.deleteOnExit();

        assertTrue(tmpFile.exists());
        assertEquals(0, tmpFile.length());

        // Options for the import (default)
        ImportOptions importOptions = new ImportOptions();
        importOptions.setImportType(ImportType.VDB);

        // Saves Messages during import
        ImportMessages importMessages = new ImportMessages();

        KomodoObject vdbNode = executeImporter(tmpFile, importOptions, importMessages);

        // No model created
        assertNull("Failed - expected null model ", vdbNode);

        // Should have 1 error message
        assertEquals(1, importMessages.getErrorMessages().size());

        String msg = importMessages.getErrorMessages().get(0);
        assertEquals("The supplied content string is empty", msg);
    }

    private void verifyTweetExampleNode(KomodoObject tweetNode) throws Exception {
        String REST_TRANSLATOR = "rest";
        String TWITTER_MODEL = "twitter";
        String TWITTER_VIEW_MODEL = "twitterview";

        assertNotNull(tweetNode);

        /*
         * tweet-example-vdb.xml
         *      @jcr:primaryType=vdb:virtualDatabase
         *      @jcr:mixinTypes=[mix:referenceable]
         *      @jcr:uuid={uuid-to-be-created}
         *      @mode:sha1={sha1-to-be-created}
         *      @vdb:preview=false
         *      @vdb:version=1
         *      @vdb:originalFile=tweet-example-vdb.xml
         *      @vdb:name=twitter
         *      @vdb:description=Shows how to call Web Services
         *      @UseConnectorMetadata=cached
         */
        verify(tweetNode.getParent(null), TWEET_EXAMPLE, VdbLexicon.Vdb.VIRTUAL_DATABASE);
        verifyMixinType(tweetNode, "mix:referenceable");
        verifyProperty(tweetNode, VdbLexicon.Vdb.NAME, "twitter");
        verifyProperty(tweetNode, VdbLexicon.Vdb.DESCRIPTION, "Shows how to call Web Services");

        // Miscellaneous property
        verifyProperty(tweetNode, "UseConnectorMetadata", "cached");
        verifyProperty(tweetNode, VdbLexicon.Vdb.PREVIEW, Boolean.FALSE.toString());
        verifyProperty(tweetNode, VdbLexicon.Vdb.VERSION, Integer.toString(1));

        /*
         *      vdb:translators
         *          @jcr:primaryType=vdb:translators
         */
        KomodoObject translators = verify(tweetNode, VdbLexicon.Vdb.TRANSLATORS, VdbLexicon.Vdb.TRANSLATORS);

        /*
         *          rest
         *              @jcr:primaryType=vdb:translator
         *              @DefaultServiceMode=MESSAGE
         *              @DefaultBinding=HTTP
         *              @vdb:type=ws
         *              @vdb:description=Rest Web Service translator
         */
        KomodoObject rest = verify(translators, REST_TRANSLATOR, VdbLexicon.Translator.TRANSLATOR);
        verifyProperty(rest, VdbLexicon.Translator.DESCRIPTION, "Rest Web Service translator");
        verifyProperty(rest, "DefaultServiceMode", "MESSAGE");
        verifyProperty(rest, "DefaultBinding", "HTTP");
        verifyProperty(rest, VdbLexicon.Translator.TYPE, "ws");

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
        KomodoObject twitter = verify(tweetNode, TWITTER_MODEL, VdbLexicon.Vdb.DECLARATIVE_MODEL);
        verifyProperty(twitter, CoreLexicon.JcrId.MODEL_TYPE, CoreLexicon.ModelType.PHYSICAL);
        verifyProperty(twitter, VdbLexicon.Model.VISIBLE, Boolean.TRUE.toString());
        verifyProperty(twitter, VdbLexicon.Model.METADATA_TYPE, "DDL");

        /*
         *          vdb:sources
         *              @jcr:primaryType=vdb:sources
         */
        KomodoObject twitterSources = verify(twitter, VdbLexicon.Vdb.SOURCES, VdbLexicon.Vdb.SOURCES);

        /*
         *              twitter
         *                  @jcr:primaryType=vdb:source
         *                  @vdb:sourceTranslator=rest
         *                  @vdb:sourceJndiName=java:/twitterDS
         */
        KomodoObject twitterSource = verify(twitterSources, TWITTER_MODEL, VdbLexicon.Source.SOURCE);
        verifyProperty(twitterSource, VdbLexicon.Source.TRANSLATOR, REST_TRANSLATOR);
        verifyProperty(twitterSource, VdbLexicon.Source.JNDI_NAME, "java:/twitterDS");

        /*      
         *      twitterview
         *          @jcr:primaryType=vdb:declarativeModel
         *          @jcr:uuid={uuid-to-be-created}
         *          @mmcore:modelType=VIRTUAL
         *          @vdb:visible=true
         *          @vdb:metadataType=DDL
         *          @vdb:modelDefinition=CREATE VIRTUAL PROCEDURE getTweets(query varchar) RETURNS (created_on varchar(25), from_user varchar(25), to_user varchar(25), profile_image_url varchar(25), source varchar(25), text varchar(140)) AS select tweet.* from (call twitter.invokeHTTP(action => 'GET', endpoint =>querystring('',query as "q"))) w, XMLTABLE('results' passing JSONTOXML('myxml', w.result) columns created_on string PATH 'created_at', from_user string PATH 'from_user', to_user string PATH 'to_user', profile_image_url string PATH 'profile_image_url', source string PATH 'source', text string PATH 'text') tweet; CREATE VIEW Tweet AS select * FROM twitterview.getTweets;
         */
        KomodoObject twitterView = verify(tweetNode, TWITTER_VIEW_MODEL, VdbLexicon.Vdb.DECLARATIVE_MODEL);
        verifyProperty(twitterView, CoreLexicon.JcrId.MODEL_TYPE, CoreLexicon.ModelType.VIRTUAL);
        verifyProperty(twitterView, VdbLexicon.Model.METADATA_TYPE, "DDL");
        verifyProperty(twitterView, VdbLexicon.Model.VISIBLE, Boolean.TRUE.toString());

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
                                
        verifyProperty(twitterView, VdbLexicon.Model.MODEL_DEFINITION, modelDefinition.toString());

        KomodoObject ddlStatements = verify(twitterView, StandardDdlLexicon.STATEMENTS_CONTAINER);

        KomodoObject getTweets = verify(ddlStatements, "getTweets");

        KomodoObject getTweetsQuery = verify(getTweets, TeiidSqlLexicon.Query.ID);
        verify(getTweetsQuery, TeiidSqlLexicon.From.ID, JcrConstants.NT_UNSTRUCTURED, TeiidSqlLexicon.From.ID);
    }

    @Test
    public void testBasicVdbImport() throws Exception {
        InputStream vdbStream = setup(TWEET_EXAMPLE);

        // Options for the import (default)
        ImportOptions importOptions = new ImportOptions();
        importOptions.setImportType(ImportType.VDB);
        importOptions.setOption(OptionKeys.NAME, TWEET_EXAMPLE);

        // Saves Messages during import
        ImportMessages importMessages = new ImportMessages();

        KomodoObject vdbNode = executeImporter(vdbStream, importOptions, importMessages,
                                                                            ".*\\/ddl:statements\\/getTweets\\/tsql:query");

        // Test that a vdb was created
        assertNotNull("Failed - No Vdb Created ", vdbNode);

        // Test vdb name
        String vdbName = vdbNode.getName(null);
        assertEquals(importOptions.getOption(OptionKeys.NAME), vdbName);

        verifyTweetExampleNode(vdbNode);
    }

    private void verifyAllElementsExampleNode(KomodoObject allElementsNode) throws Exception {
        assertNotNull(allElementsNode);

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
         *      @vdb:connectionType=BY_VERSION
         *      @vdb-property2=vdb-value2
         *      @vdb-property=vdb-value
         */
        KomodoObject myVdbExample = verify(allElementsNode.getParent(null),
                                                                    ALL_ELEMENTS_EXAMPLE_NAME,
                                                                    VdbLexicon.Vdb.VIRTUAL_DATABASE,
                                                                    "mix:referenceable");
        assertEquals(allElementsNode, myVdbExample);
        verifyProperty(myVdbExample, VdbLexicon.Vdb.NAME, "myVDB");
        verifyProperty(myVdbExample, VdbLexicon.Vdb.DESCRIPTION, "vdb description");
        verifyProperty(myVdbExample, VdbLexicon.Vdb.CONNECTION_TYPE, "BY_VERSION");
        verifyProperty(myVdbExample, VdbLexicon.Vdb.PREVIEW, Boolean.FALSE.toString());
        verifyProperty(myVdbExample, VdbLexicon.Vdb.VERSION, Integer.toString(1));
        verifyProperty(myVdbExample, "vdb-property2", "vdb-value2");
        verifyProperty(myVdbExample, "vdb-property", "vdb-value");

        /*
         *      vdb:importVdbs
         *          @jcr:primaryType=vdb:importVdb
         */
        KomodoObject importVdbs = verify(myVdbExample, VdbLexicon.Vdb.IMPORT_VDBS, VdbLexicon.Vdb.IMPORT_VDBS);

        /*
         *          x
         *              @jcr:primaryType=vdb:importVdb
         *              @vdb:version=2
         *              @vdb:import-data-policies=false
         */
        KomodoObject importVdb = verify(importVdbs, "x", VdbLexicon.ImportVdb.IMPORT_VDB);
        verifyProperty(importVdb, VdbLexicon.ImportVdb.VERSION, Integer.toString(2));
        verifyProperty(importVdb, VdbLexicon.ImportVdb.IMPORT_DATA_POLICIES, Boolean.FALSE.toString());

        /*
         *      model-one
         *          @jcr:primaryType=vdb:declarativeModel
         *          @jcr:uuid={uuid-to-be-created}
         *          @mmcore:modelType=PHYSICAL
         *          @description=model description
         *          @vdb:visible=false
         */
        KomodoObject modelOne = verify(myVdbExample, "model-one", VdbLexicon.Vdb.DECLARATIVE_MODEL);
        verifyProperty(modelOne, CoreLexicon.JcrId.MODEL_TYPE, CoreLexicon.ModelType.PHYSICAL);
        verifyProperty(modelOne, VdbLexicon.Vdb.DESCRIPTION, "model description");
        verifyProperty(modelOne, VdbLexicon.Model.VISIBLE, Boolean.FALSE.toString());

        /*
         *          vdb:sources
         *              @jcr:primaryType=vdb:sources
         */
        KomodoObject model1Sources = verify(modelOne, VdbLexicon.Vdb.SOURCES, VdbLexicon.Vdb.SOURCES);

        /*
         *              s1
         *                  @jcr:primaryType=vdb:source
         *                  @vdb:sourceTranslator=translator
         *                  @vdb:sourceJndiName=java:mybinding
         */
        KomodoObject model1Src1 = verify(model1Sources, "s1", VdbLexicon.Source.SOURCE);
        verifyProperty(model1Src1, VdbLexicon.Source.TRANSLATOR, "translator");
        verifyProperty(model1Src1, VdbLexicon.Source.JNDI_NAME, "java:mybinding");

        /*
         *      model-two
         *          @jcr:primaryType=vdb:declarativeModel
         *          @jcr:uuid={uuid-to-be-created}
         *          @mmcore:modelType=VIRTUAL
         *          @vdb:visible=true
         */
        KomodoObject modelTwo = verify(myVdbExample, "model-two", VdbLexicon.Vdb.DECLARATIVE_MODEL);
        verifyProperty(modelTwo, CoreLexicon.JcrId.MODEL_TYPE, CoreLexicon.ModelType.VIRTUAL);
        verifyProperty(modelTwo, VdbLexicon.Model.VISIBLE, Boolean.TRUE.toString());
        verifyProperty(modelTwo, VdbLexicon.Model.METADATA_TYPE, "DDL");

        String modelDefinition = "CREATE VIEW Test AS SELECT * FROM Test.getTest;";
        verifyProperty(modelTwo, VdbLexicon.Model.MODEL_DEFINITION, modelDefinition);
        
        /*
         *          vdb:sources
         *              @jcr:primaryType=vdb:sources
         */
        KomodoObject model2Sources = verify(modelTwo, VdbLexicon.Vdb.SOURCES, VdbLexicon.Vdb.SOURCES);

        /*
         *              s1
         *                  @jcr:primaryType=vdb:source
         *                  @vdb:sourceTranslator=translator
         *                  @vdb:sourceJndiName=java:binding-one
         */
        KomodoObject model2Src1 = verify(model2Sources, "s1", VdbLexicon.Source.SOURCE);
        verifyProperty(model2Src1, VdbLexicon.Source.TRANSLATOR, "translator");
        verifyProperty(model2Src1, VdbLexicon.Source.JNDI_NAME, "java:binding-one");

        /*
         *              s2
         *                  @jcr:primaryType=vdb:source
         *                  @vdb:sourceTranslator=translator
         *                  @vdb:sourceJndiName=java:binding-two
         */
        KomodoObject model2Src2 = verify(model2Sources, "s2", VdbLexicon.Source.SOURCE);
        verifyProperty(model2Src2, VdbLexicon.Source.TRANSLATOR, "translator");
        verifyProperty(model2Src2, VdbLexicon.Source.JNDI_NAME, "java:binding-two");
        
        /*
         *      vdb:translators
         *          @jcr:primaryType=vdb:translators
         */
        KomodoObject translators = verify(myVdbExample, VdbLexicon.Vdb.TRANSLATORS, VdbLexicon.Vdb.TRANSLATORS);

        /*
         *          oracleOverride
         *              @jcr:primaryType=vdb:translator
         *              @vdb:description=hello world
         *              @vdb:type=oracle
         *              my-property=my-value
         */
        KomodoObject oraTranslator = verify(translators, "oracleOverride", VdbLexicon.Translator.TRANSLATOR);
        verifyProperty(oraTranslator, VdbLexicon.Translator.DESCRIPTION, "hello world");
        verifyProperty(oraTranslator, VdbLexicon.Translator.TYPE, "oracle");
        verifyProperty(oraTranslator, "my-property", "my-value");

        /*
         *      vdb:dataRoles
         *          @jcr:primaryType=vdb:dataRoles
         */
        KomodoObject dataRoles = verify(myVdbExample, VdbLexicon.Vdb.DATA_ROLES, VdbLexicon.Vdb.DATA_ROLES);

        /*
         *          roleOne
         *              @jcr:primaryType=vdb:dataRole
         *              @vdb:anyAuthenticated=false
         *              @vdb:grantAll=true
         *              @vdb:allowCreateTemporaryTables=true
         *              @vdb:description=roleOne described
         *              @vdb:mappedRoleNames=ROLE1, ROLE2
         */
        KomodoObject dataRole1 = verify(dataRoles, "roleOne", VdbLexicon.DataRole.DATA_ROLE);
        verifyProperty(dataRole1, VdbLexicon.Translator.DESCRIPTION, "roleOne described");
        verifyProperty(dataRole1, VdbLexicon.DataRole.ANY_AUTHENTICATED, Boolean.FALSE.toString());
        verifyProperty(dataRole1, VdbLexicon.DataRole.GRANT_ALL, Boolean.TRUE.toString());
        verifyProperty(dataRole1, VdbLexicon.DataRole.ALLOW_CREATE_TEMP_TABLES, Boolean.TRUE.toString());
        verifyProperty(dataRole1, VdbLexicon.DataRole.MAPPED_ROLE_NAMES, "ROLE1", "ROLE2");

        /*
         *              vdb:permissions
         *                  @jcr:primaryType=vdb:permissions
         */
        KomodoObject permissions = verify(dataRole1, VdbLexicon.DataRole.PERMISSIONS, VdbLexicon.DataRole.PERMISSIONS);

        /*
         *                  myTable.T1
         *                      @jcr.primaryType=vdb:permission
         *                      @allowRead=true
         */
        KomodoObject permission1 = verify(permissions, "myTable.T1", VdbLexicon.DataRole.Permission.PERMISSION);
        verifyProperty(permission1, VdbLexicon.DataRole.Permission.ALLOW_READ, Boolean.TRUE.toString());

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
        KomodoObject permission2 = verify(permissions, "myTable.T2", VdbLexicon.DataRole.Permission.PERMISSION);
        verifyProperty(permission2, VdbLexicon.DataRole.Permission.ALLOW_CREATE, Boolean.TRUE.toString());
        verifyProperty(permission2, VdbLexicon.DataRole.Permission.ALLOW_READ, Boolean.FALSE.toString());
        verifyProperty(permission2, VdbLexicon.DataRole.Permission.ALLOW_UPDATE, Boolean.TRUE.toString());
        verifyProperty(permission2, VdbLexicon.DataRole.Permission.ALLOW_DELETE, Boolean.TRUE.toString());
        verifyProperty(permission2, VdbLexicon.DataRole.Permission.ALLOW_EXECUTE, Boolean.TRUE.toString());
        verifyProperty(permission2, VdbLexicon.DataRole.Permission.ALLOW_ALTER, Boolean.TRUE.toString());

        /*
         *                      vdb:conditions
         *                          @jcr:primaryType=vdb:conditions
         */
        KomodoObject conditions = verify(permission2, VdbLexicon.DataRole.Permission.CONDITIONS, VdbLexicon.DataRole.Permission.CONDITIONS);

        /*
         *                          col1 = user()
         *                              @jcr:primaryType=vdb:condition
         *                              @vdb:constraint=false
         */
        KomodoObject condition = verify(conditions, "col1 = user()", VdbLexicon.DataRole.Permission.Condition.CONDITION);
        verifyProperty(condition, VdbLexicon.DataRole.Permission.Condition.CONSTRAINT, Boolean.FALSE.toString());

        /*
         *                  myTable.T2.col1
         *                      @jcr.primaryType=vdb:permission
         */
        KomodoObject permission3 = verify(permissions, "myTable.T2.col1", VdbLexicon.DataRole.Permission.PERMISSION);

        /*
         *                      vdb:masks
         *                          @jcr:primaryType=vdb:masks
         */
        KomodoObject masks = verify(permission3, VdbLexicon.DataRole.Permission.MASKS, VdbLexicon.DataRole.Permission.MASKS);

        /*
         *                          col2
         *                              @jcr:primaryType=vdb:mask
         *                              @vdb:order=1
         */
        KomodoObject mask = verify(masks, "col2", VdbLexicon.DataRole.Permission.Mask.MASK);
        verifyProperty(mask, VdbLexicon.DataRole.Permission.Mask.ORDER, Integer.toString(1));

        /*
         *                  javascript
         *                      @jcr.primaryType=vdb:permission
         *                      @allowLanguage=true
         */
        KomodoObject permission4 = verify(permissions, "javascript", VdbLexicon.DataRole.Permission.PERMISSION);
        verifyProperty(permission4, VdbLexicon.DataRole.Permission.ALLOW_LANGUAGE, Boolean.TRUE.toString());

        KomodoObject ddlStatements = verify(modelTwo, StandardDdlLexicon.STATEMENTS_CONTAINER);

        KomodoObject test = verify(ddlStatements, "Test");

        KomodoObject testQuery = verify(test, TeiidSqlLexicon.Query.ID);
        verify(testQuery, TeiidSqlLexicon.From.ID, JcrConstants.NT_UNSTRUCTURED, TeiidSqlLexicon.From.ID);
        
    }

    @Test
    public void testAllElementsVdbImport() throws Exception {
        InputStream vdbStream = setup(ALL_ELEMENTS_EXAMPLE_NAME);

        // Options for the import (default)
        ImportOptions importOptions = new ImportOptions();
        importOptions.setImportType(ImportType.VDB);
        importOptions.setOption(OptionKeys.NAME, ALL_ELEMENTS_EXAMPLE_NAME);

        // Saves Messages during import
        ImportMessages importMessages = new ImportMessages();

        KomodoObject vdbNode = executeImporter(vdbStream, importOptions, importMessages,
                                                                            ".*\\/ddl:statements/Test/tsql:query");

        // Test that a vdb was created
        assertNotNull("Failed - No Vdb Created ", vdbNode);

        // Test vdb name
        String vdbName = vdbNode.getName(null);
        assertEquals(importOptions.getOption(OptionKeys.NAME), vdbName);

        verifyAllElementsExampleNode(vdbNode);
    }
}

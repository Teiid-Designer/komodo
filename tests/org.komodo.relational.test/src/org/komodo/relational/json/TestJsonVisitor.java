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
package org.komodo.relational.json;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import org.junit.Test;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.test.utils.AbstractLocalRepositoryTest;
import org.komodo.test.utils.TestUtilities;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 *
 */
@SuppressWarnings({"nls", "javadoc"})
public class TestJsonVisitor extends AbstractLocalRepositoryTest {

    private void validateDefinition(String definition) {
        String testDefn = definition.replaceAll("[\\s]+", "");
        assertFalse(testDefn.contains("{}"));
        assertFalse(testDefn.contains(",,"));

        int braces = 0;
        int sqBrackets = 0;
        for (int i = 0; i < testDefn.length(); ++i) {
            char currC = testDefn.charAt(i);
            switch (currC) {
                case '{':
                    braces++;
                    break;
                case '}':
                    braces--;
                    break;
                case '[':
                    sqBrackets++;
                    break;
                case ']':
                    sqBrackets--;
                    break;
            }
        }
        assertEquals(0, braces);
        assertEquals(0, sqBrackets);

        JsonVerifyParser parser = new JsonVerifyParser();
        parser.verify(definition);
    }

    @Test(timeout=3000000)
    public void testJsonVisitor1() throws Exception {
//        createInitialTransaction();
        KomodoObject kWorkspace = _repo.komodoWorkspace(getTransaction());
        KomodoObject twitterExampleNode = TestUtilities.createTweetExampleNode(getTransaction(), kWorkspace);
        commit();

        traverse(getTransaction(), twitterExampleNode.getAbsolutePath());

        //
        // Create visitor and visit the objects
        //
        JsonVisitor visitor = new JsonVisitor();
        visitor.setFilter(KomodoType.UNKNOWN, KomodoType.TEIID);
        String definition = visitor.visit(getTransaction(), twitterExampleNode);
        System.out.println(definition);

        validateDefinition(definition);
    }

    @Test(timeout=3000000)
    public void testJsonVisitor2() throws Exception {
        VdbImporter importer = new VdbImporter(_repo);
//        createInitialTransaction();
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());

        ImportOptions importOptions = new ImportOptions();
        importOptions.setOption(OptionKeys.NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        ImportMessages importMessages = new ImportMessages();
        importer.importVdb(getTransaction(), TestUtilities.portfolioExample(), workspace, importOptions, importMessages);
        commit();

        traverse(getTransaction(), workspace.getAbsolutePath());

        KomodoObject vdbNode = workspace.getChild(getTransaction(), TestUtilities.PORTFOLIO_VDB_NAME, VdbLexicon.Vdb.VIRTUAL_DATABASE);
        assertNotNull("Failed - No Vdb Created ", vdbNode);

        traverse(getTransaction(), vdbNode.getAbsolutePath());

        //
        // Create visitor and visit the objects
        //
        JsonVisitor visitor = new JsonVisitor();
        visitor.setFilter(KomodoType.UNKNOWN, KomodoType.TEIID);
        String definition = visitor.visit(getTransaction(), vdbNode);
        System.out.println(definition);
        assertNotNull(definition);
        validateDefinition(definition);
    }

    @Test(timeout=3000000)
    public void testJsonVisitor3() throws Exception {
        VdbImporter importer = new VdbImporter(_repo);
//        createInitialTransaction();
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());

        ImportOptions importOptions = new ImportOptions();
        importOptions.setOption(OptionKeys.NAME, TestUtilities.PARTS_VDB_NAME);
        ImportMessages importMessages = new ImportMessages();
        importer.importVdb(getTransaction(), TestUtilities.partsExample(), workspace, importOptions, importMessages);
        commit();

        traverse(getTransaction(), workspace.getAbsolutePath());

        KomodoObject vdbNode = workspace.getChild(getTransaction(), TestUtilities.PARTS_VDB_NAME, VdbLexicon.Vdb.VIRTUAL_DATABASE);
        assertNotNull("Failed - No Vdb Created ", vdbNode);

        traverse(getTransaction(), vdbNode.getAbsolutePath());

        //
        // Create visitor and visit the objects
        //
        JsonVisitor visitor = new JsonVisitor();
        visitor.setFilter(KomodoType.UNKNOWN, KomodoType.TEIID);
        String definition = visitor.visit(getTransaction(), vdbNode);
        System.out.println(definition);
        assertNotNull(definition);
        validateDefinition(definition);
    }
}

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
package org.komodo.relational;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.modeshape.jcr.api.JcrConstants.JCR_MIXIN_TYPES;
import static org.modeshape.jcr.api.JcrConstants.JCR_PRIMARY_TYPE;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.test.utils.AbstractLocalRepositoryTest;
import org.komodo.utils.KLog;
import org.modeshape.jcr.JcrSession;
import org.modeshape.jcr.api.JcrConstants;


/**
 * AbstractImporterTest
 */
@SuppressWarnings( {"nls"} )
public abstract class AbstractImporterTest extends AbstractLocalRepositoryTest {

    protected static final String VDB_DIRECTORY = "vdb";

    protected static final String BOOKS_DIRECTORY = "books";

    protected static final String DDL_DIRECTORY = "ddl";

    protected static final String CONNECTION_DIRECTORY = "connection";

    protected abstract void runImporter(Repository repository,
                                                InputStream inputStream,
                                                KomodoObject parentObject,
                                                ImportOptions importOptions,
                                                ImportMessages importMessages) throws Exception;

    protected abstract void runImporter(Repository repository,
                                                File file,
                                                KomodoObject parentObject,
                                                ImportOptions importOptions,
                                                ImportMessages importMessages) throws Exception;

    protected void runImporter(Repository repository, Object content,
                                                                 KomodoObject parentObject, ImportOptions importOptions,
                                                                 ImportMessages importMessages) throws Exception {
        if (content instanceof File)
            runImporter(repository, (File) content, parentObject, importOptions, importMessages);
        else if (content instanceof InputStream)
            runImporter(repository, (InputStream) content, parentObject, importOptions, importMessages);

    }

    protected void executeImporter(Object content, KomodoObject parentObject,
                                                                        ImportOptions importOptions,
                                                                        ImportMessages importMessages)
                                                                        throws Exception {
        assertNotNull(_repo);
        assertNotNull(content);
        assertNotNull(parentObject);
        assertNotNull(importOptions);
        assertNotNull(importMessages);

        runImporter(_repo, content, parentObject, importOptions, importMessages);

        if (importMessages.hasError()) {
            KLog.getLogger().debug(importMessages.errorMessagesToString());
        }

        traverse(getTransaction(), parentObject.getAbsolutePath());

    }

    protected String enc(String input) throws Exception {
        return ( ( JcrSession )session( getTransaction() ) ).encode( input );
    }

    protected void verifyProperty(KomodoObject node, String propertyName, String... expectedValues) throws Exception {
        Property property = node.getRawProperty(getTransaction(), propertyName);
        assertNotNull(property);

        if ( property.isMultiple( getTransaction() ) ) {
            final List< String > values = Arrays.asList( property.getStringValues( getTransaction() ) );
            assertThat( values.size(), is( expectedValues.length ) );

            for ( String expectedValue : expectedValues ) {
                assertTrue( values.contains( expectedValue ) );
            }
        } else {
            assertThat( property.getStringValue( getTransaction() ), is( expectedValues[ 0 ] ) );
        }

    }

    protected void verifyPrimaryType(KomodoObject node, String expectedValue) throws Exception {
        verifyProperty(node, JCR_PRIMARY_TYPE, expectedValue);
    }

    protected void verifyMixinType(KomodoObject node, String... expectedValues) throws Exception {
        Property property = node.getRawProperty(getTransaction(), JCR_MIXIN_TYPES);
        assertNotNull(property);

        List<String> values;
        if (property.isMultiple(getTransaction()))
            values = Arrays.asList(property.getStringValues(getTransaction()));
        else {
            values = new ArrayList<>();
            values.add(property.getStringValue(getTransaction()));
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
        if (parentNode.hasChild(getTransaction(), relativePath)) {
            childNode = parentNode.getChild(getTransaction(), relativePath + indexExp);
        } else childNode = parentNode.getChild(getTransaction(), enc(relativePath) + indexExp);
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

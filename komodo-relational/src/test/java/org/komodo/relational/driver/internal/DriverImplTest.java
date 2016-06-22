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
package org.komodo.relational.driver.internal;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import java.io.InputStream;
import java.util.Properties;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.driver.Driver;
import org.komodo.spi.repository.KomodoType;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;

public class DriverImplTest extends RelationalModelTest {

    protected Driver driver;

    @Before
    public void init() throws Exception {
        this.driver = createDriver(TestUtilities.MYSQL_DRIVER_FILENAME);
    }

    @Test
    public void shouldSetContent() throws Exception {
        InputStream contentStream = TestUtilities.mySqlDriver();
        assertNotNull(contentStream);
        int contentLength = contentStream.available();
        byte[] content = FileUtils.write(contentStream);

        this.driver.setContent(getTransaction(), content);
        InputStream is = this.driver.getContent(getTransaction());
        assertEquals(contentLength, is.available());

        byte[] storedContent = FileUtils.write(is);
        assertEquals(content.length, storedContent.length);
        assertThat(storedContent, is(content));
    }

    @Test
    public void shouldHaveId() throws Exception {
        assertThat(this.driver.getName(getTransaction()), is(TestUtilities.MYSQL_DRIVER_FILENAME));
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat(this.driver.getPrimaryType(getTransaction()).getName(), is(KomodoLexicon.Driver.NODE_TYPE));
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.driver.getTypeIdentifier(getTransaction()), is(KomodoType.DRIVER));
    }

    @Test
    public void shouldExport() throws Exception {
        InputStream contentStream = TestUtilities.mySqlDriver();
        assertNotNull(contentStream);
        byte[] content = FileUtils.write(contentStream);
        long contentChkSum = TestUtilities.checksum(content);
        this.driver.setContent(getTransaction(), content);

        byte[] exportBytes = this.driver.export(getTransaction(), new Properties());
        long exportChkSum = TestUtilities.checksum(exportBytes);

        String suffix = this.driver.getDocumentType(getTransaction()).toString();
        assertEquals(JAR, suffix);
        assertEquals(contentChkSum, exportChkSum);
    }
}

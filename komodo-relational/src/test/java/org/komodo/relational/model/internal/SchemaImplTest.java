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
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.util.Properties;

import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Schema;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;

@SuppressWarnings( {"javadoc", "nls"} )
public class SchemaImplTest extends RelationalModelTest {

    private static final String DDL_VIEW = "CREATE VIEW G1 (" + NEW_LINE +
                                                                        TAB + "e1 integer," + NEW_LINE +
                                                                        TAB + "e2 varchar" + NEW_LINE +
                                                                        ") OPTIONS (\"CARDINALITY\" '1234567954432')" + NEW_LINE +
                                                                        "AS" + NEW_LINE +
                                                                        "SELECT e1, e2 FROM foo.bar;\n";

    private static final String NAME = "schema";

    private Schema schema;

    private KomodoObject workspace;

    @Before
    public void init() throws Exception {
        WorkspaceManager manager = WorkspaceManager.getInstance(_repo, getTransaction());
        workspace = _repo.komodoWorkspace(getTransaction());
        this.schema = manager.createSchema(getTransaction(), workspace, NAME);
        commit();
    }

    private void setRenditionValueAwaitSequencing( final String value ) throws Exception {
        this.schema.setRendition( getTransaction(), value );
        commit();

        traverse( getTransaction(), this.schema.getAbsolutePath() );
    }

    @Test
    public void shouldFailConstructionIfNotSchema() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new SchemaImpl(getTransaction(), _repo, workspace.getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldAllowEmptyRendition() throws Exception {
        this.schema.setRendition(getTransaction(), EMPTY_STRING);
        String rendition = this.schema.getRendition(getTransaction());
        assertThat(rendition, is(notNullValue()));
        assertThat(rendition.isEmpty(), is(true));
    }

    @Test
    public void shouldAllowNullRendition() throws Exception {
        this.schema.setRendition(getTransaction(), null);
        String rendition = this.schema.getRendition(getTransaction());
        assertThat(rendition, is(notNullValue()));
        assertThat(rendition.isEmpty(), is(true));
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.schema.rename( getTransaction(), newName );
        assertThat( this.schema.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldsetRendition() throws Exception {
        setRenditionValueAwaitSequencing(DDL_VIEW);
        assertThat(this.schema.getRendition(getTransaction()), is(DDL_VIEW));
    }

    @Test
    public void shouldExportEmptyDdl() throws Exception {
        byte[] fragmentBytes = this.schema.export(getTransaction(), new Properties());
        final String fragment = new String(fragmentBytes);
        assertThat(fragment, is(notNullValue()));
        assertThat(fragment.isEmpty(), is(true));
    }

    @Test
    public void shouldExportInvalidDdl() throws Exception {
        this.schema.setRendition( getTransaction(), "This is not ddl syntax" );

        // Invalid ddl
        commit(UnitOfWork.State.ERROR);

        traverse( getTransaction(), this.schema.getAbsolutePath() );

        byte[] fragmentBytes = this.schema.export(getTransaction(), new Properties());
        final String fragment = new String(fragmentBytes);
        assertThat(fragment, is(notNullValue()));
        assertThat(fragment.isEmpty(), is(true));
    }

    @Test
    public void shouldExportDdl() throws Exception {
        setRenditionValueAwaitSequencing(DDL_VIEW);

        // test
        byte[] fragmentBytes = this.schema.export(getTransaction(), new Properties());
        final String fragment = new String(fragmentBytes);
        assertThat(fragment, is(notNullValue()));
        assertThat(fragment.isEmpty(), is(false));
        assertEquals(DDL_VIEW, fragment);
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.schema.getTypeIdentifier( getTransaction() ), is(KomodoType.SCHEMA));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.schema.getPropertyNames( getTransaction() );
        final String[] rawProps = this.schema.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.schema.getPropertyNames( getTransaction() );
        final Filter[] filters = this.schema.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

}

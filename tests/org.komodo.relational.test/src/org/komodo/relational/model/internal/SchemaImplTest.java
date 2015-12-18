/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
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
                                                                        ") OPTIONS (CARDINALITY '1234567954432')" + NEW_LINE +
                                                                        "AS" + NEW_LINE +
                                                                        "SELECT e1, e2 FROM foo.bar;\n";

    private static final String NAME = "schema";

    private Schema schema;

    private KomodoObject workspace;

    @Before
    public void init() throws Exception {
        WorkspaceManager manager = WorkspaceManager.getInstance(_repo);
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
        final String fragment = this.schema.export(getTransaction(), new Properties());
        assertThat(fragment, is(notNullValue()));
        assertThat(fragment.isEmpty(), is(true));
    }

    @Test
    public void shouldExportInvalidDdl() throws Exception {
        this.schema.setRendition( getTransaction(), "This is not ddl syntax" );

        // Invalid ddl
        commit(UnitOfWork.State.ERROR);

        traverse( getTransaction(), this.schema.getAbsolutePath() );

        final String fragment = this.schema.export(getTransaction(), new Properties());
        assertThat(fragment, is(notNullValue()));
        assertThat(fragment.isEmpty(), is(true));
    }

    @Test
    public void shouldExportDdl() throws Exception {
        setRenditionValueAwaitSequencing(DDL_VIEW);

        // test
        final String fragment = this.schema.export(getTransaction(), new Properties());
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

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final String name = "blah";
        final KomodoObject kobject = Schema.RESOLVER.create( getTransaction(), _repo, null, name, null );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( Schema.class ) ) );
        assertThat( kobject.getName( getTransaction() ), is( name ) );
    }

}

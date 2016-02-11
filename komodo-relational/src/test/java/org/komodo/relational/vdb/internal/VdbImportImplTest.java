/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbImportImplTest extends RelationalModelTest {

    private VdbImport vdbImport;

    @Before
    public void init() throws Exception {
        final Vdb vdb = createVdb();
        this.vdbImport = vdb.addImport( getTransaction(), "vdbToImport" );
        commit();
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.vdbImport.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotVdbImport() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new VdbImportImpl( getTransaction(), _repo, this.vdbImport.getParent( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.vdbImport.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.ImportVdb.IMPORT_VDB ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.vdbImport.getTypeIdentifier( getTransaction() ), is(KomodoType.VDB_IMPORT));
    }

    @Test
    public void shouldHaveDefaultImportDataPoliciesAfterConstruction() throws Exception {
        assertThat( this.vdbImport.isImportDataPolicies( getTransaction() ), is( VdbImport.DEFAULT_IMPORT_DATA_POLICIES ) );
    }

    @Test
    public void shouldHaveDefaultImportDataPoliciesValueAfterConstruction() throws Exception {
        assertThat( this.vdbImport.isImportDataPolicies( getTransaction() ), is( VdbImport.DEFAULT_IMPORT_DATA_POLICIES ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.vdbImport.getPropertyNames( getTransaction() );
        final String[] rawProps = this.vdbImport.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentVdb() throws Exception {
        assertThat( this.vdbImport.getParent( getTransaction() ), is( instanceOf( Vdb.class ) ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.vdbImport.addChild( getTransaction(), "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.vdbImport.getPropertyNames( getTransaction() );
        final Filter[] filters = this.vdbImport.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.vdbImport.rename( getTransaction(), newName );
        assertThat( this.vdbImport.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetImportDataPoliciesValue() throws Exception {
        final boolean newValue = !VdbImport.DEFAULT_IMPORT_DATA_POLICIES;
        this.vdbImport.setImportDataPolicies( getTransaction(), newValue );
        assertThat( this.vdbImport.isImportDataPolicies( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetVersion() throws Exception {
        final int newValue = ( Vdb.DEFAULT_VERSION + 10 );
        this.vdbImport.setVersion( getTransaction(), newValue );
        assertThat( this.vdbImport.getVersion( getTransaction() ), is( newValue ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final String name = "blah";
        final KomodoObject kobject = VdbImport.RESOLVER.create( getTransaction(),
                                                                    _repo,
                                                                    this.vdbImport.getParent( getTransaction() ),
                                                                    name,
                                                                    null );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( VdbImport.class ) ) );
        assertThat( kobject.getName( getTransaction() ), is( name ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( getTransaction(), null, "bogus", null );
        VdbImport.RESOLVER.create( getTransaction(), _repo, bogusParent, "blah", null );
    }

}

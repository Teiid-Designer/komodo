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
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbImportImplTest extends RelationalModelTest {

    private VdbImport vdbImport;

    @Before
    public void init() throws Exception {
        final Vdb vdb = createVdb();
        this.vdbImport = vdb.addImport( this.uow, "vdbToImport" );
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
                new VdbImportImpl( this.uow, _repo, this.vdbImport.getParent( this.uow ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.vdbImport.getPrimaryType( this.uow ).getName(), is( VdbLexicon.ImportVdb.IMPORT_VDB ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.vdbImport.getTypeIdentifier( this.uow ), is(KomodoType.VDB_IMPORT));
    }

    @Test
    public void shouldHaveDefaultImportDataPoliciesAfterConstruction() throws Exception {
        assertThat( this.vdbImport.isImportDataPolicies( this.uow ), is( VdbImport.DEFAULT_IMPORT_DATA_POLICIES ) );
    }

    @Test
    public void shouldHaveDefaultImportDataPoliciesValueAfterConstruction() throws Exception {
        assertThat( this.vdbImport.isImportDataPolicies( this.uow ), is( VdbImport.DEFAULT_IMPORT_DATA_POLICIES ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.vdbImport.getPropertyNames( this.uow );
        final String[] rawProps = this.vdbImport.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentVdb() throws Exception {
        assertThat( this.vdbImport.getParent( this.uow ), is( instanceOf( Vdb.class ) ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.vdbImport.addChild( this.uow, "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.vdbImport.getPropertyNames( this.uow );
        final Filter[] filters = this.vdbImport.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldSetImportDataPoliciesValue() throws Exception {
        final boolean newValue = !VdbImport.DEFAULT_IMPORT_DATA_POLICIES;
        this.vdbImport.setImportDataPolicies( this.uow, newValue );
        assertThat( this.vdbImport.isImportDataPolicies( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetVersion() throws Exception {
        final int newValue = ( Vdb.DEFAULT_VERSION + 10 );
        this.vdbImport.setVersion( this.uow, newValue );
        assertThat( this.vdbImport.getVersion( this.uow ), is( newValue ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final String name = "blah";
        final KomodoObject kobject = VdbImport.RESOLVER.create( this.uow,
                                                                    _repo,
                                                                    this.vdbImport.getParent( this.uow ),
                                                                    name,
                                                                    null );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( VdbImport.class ) ) );
        assertThat( kobject.getName( this.uow ), is( name ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( this.uow, null, "bogus", null );
        VdbImport.RESOLVER.create( this.uow, _repo, bogusParent, "blah", null );
    }

}

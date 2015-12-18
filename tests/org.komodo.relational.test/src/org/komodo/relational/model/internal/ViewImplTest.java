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
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ViewImplTest extends RelationalModelTest {

    private Model model;
    private View view;

    @Before
    public void init() throws Exception {
        this.model = createModel();
        this.view = this.model.addView( getTransaction(), "view" );
        commit();
    }

    @Test
    public void shouldFailConstructionIfNotView() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new ViewImpl( getTransaction(), _repo, _repo.komodoLibrary( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenAddingForeignKey() throws KException {
        this.view.addForeignKey( getTransaction(), "blah", mock( Table.class ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenAddingUniqueConstraint() throws KException {
        this.view.addUniqueConstraint( getTransaction(), "blah" );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenRemovingForeignKey() throws KException {
        this.view.removeForeignKey( getTransaction(), "blah" );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenRemovingUniqueConstraint() throws KException {
        this.view.removeUniqueConstraint( getTransaction(), "blah" );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldFailWhenSettingPrimaryKey() throws KException {
        this.view.setPrimaryKey( getTransaction(), "blah" );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.view.getTypeIdentifier( getTransaction() ), is(KomodoType.VIEW));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.view.getPropertyNames( getTransaction() );
        final String[] rawProps = this.view.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentModel() throws Exception {
        assertThat( this.view.getParent( getTransaction() ), is( instanceOf( Model.class ) ) );
        assertThat( this.view.getParent( getTransaction() ), is( ( KomodoObject )this.model ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.view.getPropertyNames( getTransaction() );
        final Filter[] filters = this.view.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.view.rename( getTransaction(), newName );
        assertThat( this.view.getName( getTransaction() ), is( newName ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final String name = "blah";
        final KomodoObject kobject = View.RESOLVER.create( getTransaction(), _repo, this.model, name, null );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( View.class ) ) );
        assertThat( kobject.getName( getTransaction() ), is( name ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( getTransaction(), null, "bogus", null );
        View.RESOLVER.create( getTransaction(), _repo, bogusParent, "blah", null );
    }

}

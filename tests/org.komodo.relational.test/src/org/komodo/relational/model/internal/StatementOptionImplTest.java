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
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.RelationalProperty;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class StatementOptionImplTest extends RelationalModelTest {

    private static final String NAME = "statementoption";

    private StatementOption option;

    @Before
    public void init() throws Exception {
        final Table table = createTable();
        this.option = RelationalModelFactory.createStatementOption( this.uow, _repo, table, NAME, "initialValue" );
        commit();
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.option.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotStatementOption() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new StatementOptionImpl( this.uow, _repo, _repo.komodoLibrary( this.uow ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat( this.option.hasDescriptor( this.uow, StandardDdlLexicon.TYPE_STATEMENT_OPTION ), is( true ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.option.getTypeIdentifier( this.uow ), is(KomodoType.STATEMENT_OPTION));
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.option.getName( this.uow ), is( NAME ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.option.getPropertyNames( this.uow );
        final String[] rawProps = this.option.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.option.addChild( this.uow, "blah", null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotAllowEmptyOptionValueProperty() throws Exception {
        this.option.setOption( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotAllowNullOptionValueProperty() throws Exception {
        this.option.setOption( this.uow, null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.option.getPropertyNames( this.uow );
        final Filter[] filters = this.option.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldSetOptionValueProperty() throws Exception {
        final String value = "optionvalue";
        this.option.setOption( this.uow, value );
        assertThat( this.option.getOption( this.uow ), is( value ) );
        assertThat( this.option.getProperty( this.uow, StandardDdlLexicon.VALUE ).getStringValue( this.uow ), is( value ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final String name = "blah";

        final RelationalProperties props = new RelationalProperties();
        props.add( new RelationalProperty( StandardDdlLexicon.VALUE, "optionValue" ) );

        final KomodoObject kobject = StatementOptionImpl.RESOLVER.create( this.uow,
                                                                          _repo,
                                                                          this.option.getParent( this.uow ),
                                                                          name,
                                                                          props );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( StatementOption.class ) ) );
        assertThat( kobject.getName( this.uow ), is( name ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( this.uow, null, "bogus", null );
        StatementOptionImpl.RESOLVER.create( this.uow, _repo, bogusParent, "blah", new RelationalProperties() );
    }

}

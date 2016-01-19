/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.datasource.internal;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.datasource.Datasource;
import org.komodo.spi.repository.KomodoType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DatasourceImplTest extends RelationalModelTest {

    private static final String DS_NAME = "source";

    protected Datasource datasource;

    @Before
    public void init() throws Exception {
        this.datasource = createDatasource( DS_NAME );
    }

    @Test
    public void shouldSetJndiName() throws Exception {
        String newValue = "java:/jndiName";
        this.datasource.setJndiName( getTransaction(), newValue );
        assertThat( this.datasource.getJndiName( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetDriverName() throws Exception {
        String newValue = "oracle";
        this.datasource.setDriverName( getTransaction(), newValue );
        assertThat( this.datasource.getDriverName( getTransaction() ), is( newValue ) );
    }
        
    @Test
    public void shouldSetProfileName() throws Exception {
        String newValue = "myConnectionProfile";
        this.datasource.setProfileName( getTransaction(), newValue );
        assertThat( this.datasource.getProfileName( getTransaction() ), is( newValue ) );
    }
        
    @Test
    public void shouldSetJdbc() throws Exception {
        boolean isJdbc = false;
        this.datasource.setJdbc( getTransaction(), isJdbc );
        assertThat( this.datasource.isJdbc( getTransaction() ), is( isJdbc ) );
    }

    @Test
    public void shouldSetPreview() throws Exception {
        boolean isPreview = true;
        this.datasource.setPreview( getTransaction(), isPreview );
        assertThat( this.datasource.isPreview( getTransaction() ), is( isPreview ) );
    }

    @Test
    public void shouldSetClassname() throws Exception {
        String className = "myClass.name";
        this.datasource.setJdbc(getTransaction(), false);
        this.datasource.setClassName( getTransaction(), className );
        assertThat( this.datasource.getClassName( getTransaction() ), is( className ) );
    }
        
    @Test
    public void shouldHaveDefaultJdbc() throws Exception {
        assertThat( this.datasource.isJdbc( getTransaction() ), is( Datasource.DEFAULT_JDBC ) );
    }

    @Test
    public void shouldHaveDefaultPreview() throws Exception {
        assertThat( this.datasource.isPreview( getTransaction() ), is( Datasource.DEFAULT_PREVIEW ) );
    }

    @Test
    public void shouldHaveId() throws Exception {
        assertThat( this.datasource.getName( getTransaction() ), is( DS_NAME ) );
    }

    @Test
    public void shouldHaveJdbcProperty() throws Exception {
        String[] propNames = this.datasource.getPropertyNames( getTransaction() );
        assertThat( Arrays.asList(propNames).contains(KomodoLexicon.DataSource.JDBC), is( true ) );
    }
    
    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.datasource.getPropertyNames( getTransaction() );
        final String[] rawProps = this.datasource.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.datasource.getPropertyNames( getTransaction() );
        final Filter[] filters = this.datasource.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }
    
    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.datasource.getPrimaryType( getTransaction() ).getName(), is( KomodoLexicon.DataSource.NODE_TYPE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.datasource.getTypeIdentifier( getTransaction() ), is(KomodoType.DATASOURCE));
    }

}

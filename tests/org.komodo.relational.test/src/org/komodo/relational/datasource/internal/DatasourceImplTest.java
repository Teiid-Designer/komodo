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
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelTest;
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
    public void shouldHaveId() throws Exception {
        assertThat( this.datasource.getName( getTransaction() ), is( DS_NAME ) );
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

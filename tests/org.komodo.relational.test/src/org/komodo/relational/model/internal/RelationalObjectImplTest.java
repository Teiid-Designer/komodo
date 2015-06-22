/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.jcr.JcrMixLexicon;
import org.modeshape.jcr.ModeShapeLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RelationalObjectImplTest extends RelationalModelTest {

    class RelationalTestObject extends RelationalObjectImpl {

        RelationalTestObject( final UnitOfWork uow,
                              final Repository repository,
                              final String path ) throws KException {
            super( uow, repository, path );
        }

    }

    private RelationalObject robject;

    @Before
    public void init() throws Exception {
        final KomodoObject model = createModel();
        this.robject = new RelationalTestObject( this.uow, model.getRepository(), model.getAbsolutePath() );
        commit();
    }

    @Test
    public void shouldFilterJcrNamespace() throws Exception {
        this.robject.setFilters( new RelationalObject.Filter[] { RelationalObject.JCR_FILTER } );

        for ( final PropertyDescriptor descriptor : this.robject.getPropertyDescriptors( this.uow ) ) {
            if ( descriptor.getName().startsWith( JcrLexicon.Namespace.PREFIX ) ) {
                fail();
            }
        }
    }

    @Test
    public void shouldFilterMixNamespace() throws Exception {
        this.robject.addDescriptor( this.uow, JcrMixLexicon.CREATED.getString() );
        this.robject.setFilters( new RelationalObject.Filter[] { RelationalObject.MIX_FILTER } );

        for ( final Descriptor descriptor : this.robject.getDescriptors( this.uow ) ) {
            if ( descriptor.getName().startsWith( JcrMixLexicon.Namespace.PREFIX ) ) {
                fail();
            }
        }
    }

    @Test
    public void shouldFilterModeNamespace() throws Exception {
        this.robject.setFilters( new RelationalObject.Filter[] { RelationalObject.MODE_FILTER } );

        for ( final PropertyDescriptor descriptor : this.robject.getPropertyDescriptors( this.uow ) ) {
            if ( descriptor.getName().startsWith( ModeShapeLexicon.Namespace.PREFIX ) ) {
                fail();
            }
        }
    }

    @Test
    public void shouldFilterResidual() throws Exception {
        this.robject.setFilters( new RelationalObject.Filter[] { RelationalObject.RESIDUAL_FILTER } );

        for ( final PropertyDescriptor descriptor : this.robject.getPropertyDescriptors( this.uow ) ) {
            if ( "*".equals( descriptor.getName() ) ) {
                fail();
            }
        }
    }

}

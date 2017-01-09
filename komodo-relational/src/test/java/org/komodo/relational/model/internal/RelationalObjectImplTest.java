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
        this.robject = new RelationalTestObject( getTransaction(), model.getRepository(), model.getAbsolutePath() );
        commit();
    }

    @Test
    public void shouldFilterJcrNamespace() throws Exception {
        this.robject.setFilters( new RelationalObject.Filter[] { RelationalObject.JCR_FILTER } );

        for ( final PropertyDescriptor descriptor : this.robject.getPropertyDescriptors( getTransaction() ) ) {
            if ( descriptor.getName().startsWith( JcrLexicon.Namespace.PREFIX ) ) {
                fail();
            }
        }
    }

    @Test
    public void shouldFilterMixNamespace() throws Exception {
        this.robject.addDescriptor( getTransaction(), JcrMixLexicon.CREATED.getString() );
        this.robject.setFilters( new RelationalObject.Filter[] { RelationalObject.MIX_FILTER } );

        for ( final Descriptor descriptor : this.robject.getDescriptors( getTransaction() ) ) {
            if ( descriptor.getName().startsWith( JcrMixLexicon.Namespace.PREFIX ) ) {
                fail();
            }
        }
    }

    @Test
    public void shouldFilterModeNamespace() throws Exception {
        this.robject.setFilters( new RelationalObject.Filter[] { RelationalObject.MODE_FILTER } );

        for ( final PropertyDescriptor descriptor : this.robject.getPropertyDescriptors( getTransaction() ) ) {
            if ( descriptor.getName().startsWith( ModeShapeLexicon.Namespace.PREFIX ) ) {
                fail();
            }
        }
    }

    @Test
    public void shouldFilterResidual() throws Exception {
        this.robject.setFilters( new RelationalObject.Filter[] { RelationalObject.RESIDUAL_FILTER } );

        for ( final PropertyDescriptor descriptor : this.robject.getPropertyDescriptors( getTransaction() ) ) {
            if ( "*".equals( descriptor.getName() ) ) {
                fail();
            }
        }
    }

}

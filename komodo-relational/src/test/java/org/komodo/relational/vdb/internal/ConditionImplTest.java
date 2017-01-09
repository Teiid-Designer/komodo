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
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ConditionImplTest extends RelationalModelTest {

    private Condition condition;

    @Before
    public void init() throws Exception {
        final Vdb vdb = createVdb();
        final DataRole dataRole = vdb.addDataRole( getTransaction(), "dataRole" );
        final Permission permission = dataRole.addPermission( getTransaction(), "permission" );
        this.condition = permission.addCondition( getTransaction(), "condition" );
        commit();
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.condition.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotCondition() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new ConditionImpl( getTransaction(), _repo, this.condition.getParent( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveConstraintDefaultValueAfterConstruction() throws Exception {
        assertThat( this.condition.isConstraint( getTransaction() ), is( Condition.DEFAULT_CONSTRAINT ) );
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.condition.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.DataRole.Permission.Condition.CONDITION ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.condition.getTypeIdentifier( getTransaction() ), is(KomodoType.VDB_CONDITION));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.condition.getPropertyNames( getTransaction() );
        final String[] rawProps = this.condition.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentPermission() throws Exception {
        assertThat( this.condition.getParent( getTransaction() ), is( instanceOf( Permission.class ) ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.condition.addChild( getTransaction(), "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.condition.getPropertyNames( getTransaction() );
        final Filter[] filters = this.condition.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.condition.rename( getTransaction(), newName );
        assertThat( this.condition.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetConstraintValue() throws Exception {
        final boolean newValue = !Condition.DEFAULT_CONSTRAINT;
        this.condition.setConstraint( getTransaction(), newValue );
        assertThat( this.condition.isConstraint( getTransaction() ), is( newValue ) );
    }

}

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
package org.komodo.repository;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.komodo.spi.constants.StringConstants;
import org.komodo.test.utils.AbstractLocalRepositoryTest;

@SuppressWarnings( { "javadoc",
                     "nls" } )
public final class RepositoryImplTest extends AbstractLocalRepositoryTest {

    @Test
    public void shouldAllowEncryptingWhenValueIsEmpty() throws Exception {
        assertThat( _repo.encrypt( getTransaction(), this.name.getMethodName(), StringConstants.EMPTY_STRING ),
                    is( StringConstants.EMPTY_STRING ) );
    }

    @Test
    public void shouldAllowEncryptingWhenValueIsNull() throws Exception {
        assertThat( _repo.encrypt( getTransaction(), this.name.getMethodName(), null ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowDecryptionWhenValueIsEmpty() throws Exception {
        assertThat( _repo.decrypt( getTransaction(), this.name.getMethodName(), StringConstants.EMPTY_STRING ),
                    is( StringConstants.EMPTY_STRING ) );
    }

    @Test
    public void shouldAllowDecryptionWhenValueIsNull() throws Exception {
        assertThat( _repo.decrypt( getTransaction(), this.name.getMethodName(), null ), is( nullValue() ) );
    }

    @Test
    public void shouldEncryptValue() throws Exception {
        final String value = "blah";
        final String encrypted = _repo.encrypt( getTransaction(), this.name.getMethodName(), value );
        assertThat( encrypted, is( not( value ) ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailDecryptionWhenPropertyNameIsEmpty() throws Exception {
        _repo.decrypt( getTransaction(), StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailDecryptionWhenPropertyNameIsNull() throws Exception {
        _repo.decrypt( getTransaction(), null, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailEncryptionWhenPropertyIsEmpty() throws Exception {
        _repo.encrypt( getTransaction(), StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailEncryptionWhenPropertyIsNull() throws Exception {
        _repo.encrypt( getTransaction(), null, "blah" );
    }

    @Test
    public void shouldRoundTripEncryptingValue() throws Exception {
        final String value = "blah";
        final String encrypted = _repo.encrypt( getTransaction(), this.name.getMethodName(), value );
        final String decrypted = _repo.decrypt( getTransaction(), this.name.getMethodName(), encrypted );
        assertThat( decrypted, is( value ) );
    }

}

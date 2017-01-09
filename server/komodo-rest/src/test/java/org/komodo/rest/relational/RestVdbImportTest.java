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
package org.komodo.rest.relational;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.rest.relational.response.RestVdbImport;

@SuppressWarnings( {"javadoc", "nls"} )
public final class RestVdbImportTest {

    private static final String NAME = "MyVdbImport";
    private static final int VERSION = 2;
    private static final boolean IMPORT_DATA_POLICIES = true;

    private RestVdbImport vdbImport;

    @Before
    public void init() {
        this.vdbImport = new RestVdbImport();
        this.vdbImport.setName(NAME);
        this.vdbImport.setVersion(VERSION);
        this.vdbImport.setImportDataPolicies(IMPORT_DATA_POLICIES);
    }

    @Test
    public void shouldBeEqual() {
        final RestVdbImport thatImport = new RestVdbImport();
        thatImport.setName(this.vdbImport.getName());
        thatImport.setVersion(this.vdbImport.getVersion());
        thatImport.setImportDataPolicies(this.vdbImport.isImportDataPolicies());
        thatImport.setLinks(this.vdbImport.getLinks());
        thatImport.setProperties(this.vdbImport.getProperties());

        assertThat(this.vdbImport, is(thatImport));
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyEntries() {
        final RestVdbImport empty1 = new RestVdbImport();
        final RestVdbImport empty2 = new RestVdbImport();
        assertThat(empty1, is(empty2));
    }

    @Test
    public void shouldConstructEmptyImport() {
        final RestVdbImport empty = new RestVdbImport();
        assertThat(empty.getName(), is(nullValue()));
        assertThat(empty.getVersion(), is(Vdb.DEFAULT_VERSION));
        assertThat(empty.isImportDataPolicies(), is(VdbImport.DEFAULT_IMPORT_DATA_POLICIES));
        assertThat(empty.getProperties().isEmpty(), is(true));
        assertThat(empty.getLinks().size(), is(0));
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestVdbImport thatImport = new RestVdbImport();
        thatImport.setName(this.vdbImport.getName());
        thatImport.setVersion(this.vdbImport.getVersion());
        thatImport.setImportDataPolicies(this.vdbImport.isImportDataPolicies());
        thatImport.setLinks(this.vdbImport.getLinks());
        thatImport.setProperties(this.vdbImport.getProperties());

        assertThat(this.vdbImport.hashCode(), is(thatImport.hashCode()));
    }

    @Test
    public void shouldNotBeEqualWhenImportDataPoliciesIsDifferent() {
        final RestVdbImport thatImport = new RestVdbImport();
        thatImport.setName(this.vdbImport.getName());
        thatImport.setVersion(this.vdbImport.getVersion());
        thatImport.setImportDataPolicies(!this.vdbImport.isImportDataPolicies());
        thatImport.setLinks(this.vdbImport.getLinks());
        thatImport.setProperties(this.vdbImport.getProperties());

        assertThat(this.vdbImport.isImportDataPolicies(), is(not(thatImport.isImportDataPolicies())));
        assertThat(this.vdbImport, is(not(thatImport)));
    }

    @Test
    public void shouldNotBeEqualWhenNameIsDifferent() {
        final RestVdbImport thatImport = new RestVdbImport();
        thatImport.setName(this.vdbImport.getName() + "blah");
        thatImport.setVersion(this.vdbImport.getVersion());
        thatImport.setImportDataPolicies(this.vdbImport.isImportDataPolicies());
        thatImport.setLinks(this.vdbImport.getLinks());
        thatImport.setProperties(this.vdbImport.getProperties());

        assertThat(this.vdbImport.getName(), is(not(thatImport.getName())));
        assertThat(this.vdbImport, is(not(thatImport)));
    }

    @Test
    public void shouldNotBeEqualWhenVersionIsDifferent() {
        final RestVdbImport thatImport = new RestVdbImport();
        thatImport.setName(this.vdbImport.getName());
        thatImport.setVersion(this.vdbImport.getVersion() + 1);
        thatImport.setImportDataPolicies(this.vdbImport.isImportDataPolicies());
        thatImport.setLinks(this.vdbImport.getLinks());
        thatImport.setProperties(this.vdbImport.getProperties());

        assertThat(this.vdbImport.getVersion(), is(not(thatImport.getVersion())));
        assertThat(this.vdbImport, is(not(thatImport)));
    }

    @Test
    public void shouldSetImportDataPolicies() {
        final boolean newImportDataPolicies = !this.vdbImport.isImportDataPolicies();
        this.vdbImport.setImportDataPolicies(newImportDataPolicies);
        assertThat(this.vdbImport.isImportDataPolicies(), is(newImportDataPolicies));
    }

    @Test
    public void shouldSetName() {
        final String newName = "blah";
        this.vdbImport.setName(newName);
        assertThat(this.vdbImport.getName(), is(newName));
    }

    @Test
    public void shouldSetVersion() {
        final int newVersion = (this.vdbImport.getVersion() + 1);
        this.vdbImport.setVersion(newVersion);
        assertThat(this.vdbImport.getVersion(), is(newVersion));
    }

}

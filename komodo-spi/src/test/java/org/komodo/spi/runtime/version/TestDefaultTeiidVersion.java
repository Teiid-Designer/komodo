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
package org.komodo.spi.runtime.version;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;

public class TestDefaultTeiidVersion {

    @Test
    public void shouldReturnValidVersion() {
        TeiidVersion teiidVersion = DefaultTeiidVersion.Version.DEFAULT_TEIID_VERSION.get();
        System.out.println(teiidVersion);
        assertNotNull(teiidVersion);
    }

    private TeiidVersion version(String versionId) {
        return new DefaultTeiidVersion(versionId);
    }

    /**
     * Test {@link DefaultTeiidVersion#compareTo(TeiidVersion)}
     */
    @Test
    public void testCompareTo() {
        assertTrue(version("8.0.0").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").compareTo(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.0.0").compareTo(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.0.0").compareTo(version("8.x.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.x").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.x.0").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.x.x").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.0.0").compareTo(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").compareTo(version("8.1.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").compareTo(version("8.x.1"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").compareTo(version("9.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("7.0.0").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.1.0").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.x.1").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("9.0.0").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("9.1.1").compareTo(version("9.1.x")));
    }

    /**
     * Test {@link DefaultTeiidVersion#getMaximumVersion()}
     */
    @Test
    public void testGetMaximum() {
        assertEquals(version("8.0.0"), version("8.0.0").getMaximumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
        assertEquals(version("8.9.0"), version("8.x.0").getMaximumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
        assertEquals(version("8.0.9"), version("8.0.x").getMaximumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
        assertEquals(version("8.9.9"), version("8.x.x").getMaximumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * Test {@link DefaultTeiidVersion#getMinimumVersion()}
     */
    @Test
    public void testGetMinimum() {
        assertEquals(version("8.0.0"), version("8.0.0").getMinimumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
        assertEquals(version("8.0.0"), version("8.x.0").getMinimumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
        assertEquals(version("8.0.0"), version("8.0.x").getMinimumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
        assertEquals(version("8.0.0"), version("8.x.x").getMinimumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * Test {@link DefaultTeiidVersion#isGreaterThan(TeiidVersion)}
     */
    @Test
    public void testIsGreaterThan() {
        assertFalse(version("8.0.0").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").isGreaterThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.0").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").isGreaterThan(version("7.7.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.7.0").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").isGreaterThan(version("7.0.1"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.1").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").isGreaterThan(version("7.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.x").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").isGreaterThan(version("7.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.x.0").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").isGreaterThan(version("7.x.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.x.x").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.x.0").isGreaterThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.0").isGreaterThan(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.x").isGreaterThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.0").isGreaterThan(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.x.x").isGreaterThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.0").isGreaterThan(version("8.x.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.x").isGreaterThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.0").isGreaterThan(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.x.0").isGreaterThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.0").isGreaterThan(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.0.x").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isGreaterThan(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.x.0").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isGreaterThan(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        // silly micro version should be ignored since minor versions should be enough for the comparison
        assertTrue(version("8.1.extendedmicroversionid").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$//$NON-NLS-2$

        // same minor versions up until 1 and 2
        assertTrue(version("8.designer-2.0").isGreaterThan(version("8.designer-1.0"))); //$NON-NLS-1$//$NON-NLS-2$

        // Comparing 1 and 10
        assertTrue(version("8.designer-10.0").isGreaterThan(version("8.designer-1.0"))); //$NON-NLS-1$//$NON-NLS-2$

        // 20 < 18 but designer > teiidteiid
        assertTrue(version("8.teiidteiid-18.0").isGreaterThan(version("8.designer-20.0"))); //$NON-NLS-1$//$NON-NLS-2$
        assertFalse(version("8.designer-20.0").isGreaterThan(version("8.teiidteiid-18.0"))); //$NON-NLS-1$//$NON-NLS-2$

        assertTrue(version("8.11.0").isGreaterThan(version("8.8.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.10.0").isGreaterThan(version("8.8.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.10.1").isGreaterThan(version("8.10.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.10.1").isGreaterThan(version("8.11.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.7.1").isGreaterThan(version("8.7.x"))); //$NON-NLS-1$//$NON-NLS-2$
        assertTrue(version("8.12.4").isGreaterThan(version("8.9.0"))); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * Test {@link DefaultTeiidVersion#isLessThan(TeiidVersion)}
     */
    @Test
    public void testIsLessThan() {
        assertFalse(version("8.0.0").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.0").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isLessThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.7.0").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isLessThan(version("7.7.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.1").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isLessThan(version("7.0.1"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.x").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isLessThan(version("7.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.x.0").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isLessThan(version("7.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.x.x").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isLessThan(version("7.x.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.0").isLessThan(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.x.0").isLessThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.0").isLessThan(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.x").isLessThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.0").isLessThan(version("8.x.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.x.x").isLessThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.0").isLessThan(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.x").isLessThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.0").isLessThan(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.x.0").isLessThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.0.0").isLessThan(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.x").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.0.0").isLessThan(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.x.0").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        // silly micro version should be ignored since minor versions should be enough for the comparison
        assertTrue(version("8.0.0").isLessThan(version("8.1.extendedmicroversionid"))); //$NON-NLS-1$//$NON-NLS-2$

        // same minor versions up until 1 and 2
        assertTrue(version("8.designer-1.0").isLessThan(version("8.designer-2.0"))); //$NON-NLS-1$//$NON-NLS-2$

        // Comparing 1 and 10
        assertTrue(version("8.designer-1.0").isLessThan(version("8.designer-10.0"))); //$NON-NLS-1$//$NON-NLS-2$

        // 20 > 18 but designer < teiidteiid
        assertTrue(version("8.designer-20.0").isLessThan(version("8.teiidteiid-18.0"))); //$NON-NLS-1$//$NON-NLS-2$
        assertFalse(version("8.teiidteiid-18.0").isLessThan(version("8.designer-20.0"))); //$NON-NLS-1$//$NON-NLS-2$

        assertTrue(version("8.7.0").isLessThan(version("8.7.1"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.8.0").isLessThan(version("8.10.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.7.1").isLessThan(version("8.7.x"))); //$NON-NLS-1$//$NON-NLS-2$

    }

    @Test
    public void testIsLessThan2() {
        assertTrue(Version.TEIID_8_12_4.get().isLessThan(version("9.0.0")));

        assertFalse(Version.TEIID_8_12_4.get().isLessThan(version("8.6.0")));
    }
}

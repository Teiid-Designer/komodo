/*
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
*/
package org.teiid.query.sql.lang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import org.junit.Test;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.teiid.query.sql.lang.CriteriaOperator.Operator;

/**
 *
 */
public class TestCriteriaOperator {

    private class ExpectedResults {
        private final TeiidVersion version;
        private final int totalEnums;
        private final String[] missingSymbols;

        public ExpectedResults(TeiidVersion version, int totalEnums, String... missingSymbols) {
            this.version = version;
            this.totalEnums = totalEnums;
            this.missingSymbols = missingSymbols;
        }
    }

    private TeiidVersion version(String versionString) {
        return new DefaultTeiidVersion(versionString);
    }

    /**
     * Test the {@link Operator#getOperator(TeiidVersion, String)}
     * function to ensure that the correct number of enum values are returned
     * given a specific {@link DefaultTeiidVersion}.
     */
    @Test
    public void testGetOperator() {
        ExpectedResults[] expectedResults = new ExpectedResults[] {
            new ExpectedResults(version("7.0.0"), Operator.values().length), //$NON-NLS-1$
            new ExpectedResults(version("7.x.0"), Operator.values().length), //$NON-NLS-1$
            new ExpectedResults(version("8.0.0"), Operator.values().length - 5, "like", "in", "is", "is null", "between", ""), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
            new ExpectedResults(version("8.x.0"), Operator.values().length - 5, "like", "in", "is", "is null", "between", "") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
        };

        for (ExpectedResults expected : expectedResults) {
            int count = 0;
            for (Operator operator : Operator.values()) {
                try {
                    for (String symbol : operator.getSymbols()) {
                        Operator foundOperator = Operator.getOperator(expected.version, symbol);
                        assertNotNull(foundOperator);
                    }
                    count++;
                } catch (Throwable ex) {
                    // Assertion thrown so operator not found
                    for (String symbol : operator.getSymbols()) {
                        assertTrue(Arrays.asList(expected.missingSymbols).contains(symbol));
                    }
                }
            }
            assertEquals(expected.totalEnums, count);
        }
    }

}

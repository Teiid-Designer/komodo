/*************************************************************************************
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
 ************************************************************************************/
package org.komodo.spi.runtime.version;

import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;

/**
 * Parent marker interface for teiid instance version information
 */
public interface TeiidVersion {
    
    /**
     * dot
     */
    String DOT = ".";  //$NON-NLS-1$
    
    /**
     * wildcard character used in version strings
     */
    String WILDCARD = "x"; //$NON-NLS-1$
    
    /**
     * zero
     */
    String ZERO = "0"; //$NON-NLS-1$

    /**
     * one
     */
    String ONE = "1"; //$NON-NLS-1$

    /**
     * two
     */
    String TWO = "2"; //$NON-NLS-1$

    /**
     * three
     */
    String THREE = "3"; //$NON-NLS-1$

    /**
     * four
     */
    String FOUR = "4"; //$NON-NLS-1$

    /**
     * five
     */
    String FIVE = "5"; //$NON-NLS-1$

    /**
     * six
     */
    String SIX = "6"; //$NON-NLS-1$

    /**
     * seven
     */
    String SEVEN = "7"; //$NON-NLS-1$

    /**
     * eight
     */
    String EIGHT = "8"; //$NON-NLS-1$

    /**
     * nine
     */
    String NINE = "9"; //$NON-NLS-1$

    /**
     * Teiid id versions
     */
    enum VersionID {
        TEIID_7_7(SEVEN + DOT + SEVEN + DOT + ZERO),

        TEIID_8_0(EIGHT + DOT + ZERO + DOT + ZERO),

        TEIID_8_1(EIGHT + DOT + ONE + DOT + ZERO),

        TEIID_8_2(EIGHT + DOT + TWO + DOT + ZERO),

        TEIID_8_3(EIGHT + DOT + THREE + DOT + ZERO),

        TEIID_8_4(EIGHT + DOT + FOUR + DOT + ZERO),

        TEIID_8_5(EIGHT + DOT + FIVE + DOT + ZERO),

        TEIID_8_6(EIGHT + DOT + SIX + DOT + ZERO),

        TEIID_8_7(EIGHT + DOT + SEVEN + DOT + ZERO),

        TEIID_8_8(EIGHT + DOT + EIGHT + DOT + ZERO),

        TEIID_8_9(EIGHT + DOT + NINE + DOT + ZERO),

        TEIID_8_10(EIGHT + DOT + ONE + ZERO + DOT + ZERO),

        TEIID_8_11(EIGHT + DOT + ONE + ONE + DOT + ZERO),

        TEIID_8_11_5(EIGHT + DOT + ONE + ONE + DOT + FIVE),

        TEIID_8_12(EIGHT + DOT + ONE + TWO + DOT + ZERO),

        TEIID_8_12_4(EIGHT + DOT + ONE + TWO + DOT + FOUR),

        TEIID_8_12_7(EIGHT + DOT + ONE + TWO + DOT + SEVEN),

        TEIID_8_13(EIGHT + DOT + ONE + THREE + DOT + ZERO),

        TEIID_9_0(NINE + DOT + ZERO + DOT + ZERO),

        TEIID_9_1(NINE + DOT + ONE + DOT + ZERO),

        TEIID_9_1_1(NINE + DOT + ONE + DOT + ONE);

        private final String id;

        VersionID(String id) {
            this.id = id;
        }

        @Override
        public String toString() {
            return id;
        }
    }

    /**
     * Teiid version property constant
     */
    String TEIID_VERSION_PROPERTY = "org.teiid.version"; //$NON-NLS-1$

    /**
     * @return the major version segment
     */
    String getMajor();
    
    /**
     * @return the minor version segment
     */
    String getMinor();
    
    /**
     * @return the micro version segment 
     */
    String getMicro();

    /**
     * Test whether the minor or micro segments are wildcards '*'
     * 
     * @return true if there are wildcards. false otherwise
     */
    boolean hasWildCards();

    /**
     * @param otherVersion
     * 
     * @return true if the otherVersion is considered equivalent
     */
    boolean compareTo(TeiidVersion otherVersion);
    
    /**
     * Is this a 7 teiid instance?
     * 
     * @return true is version is 7
     */
    boolean isSevenInstance();

    /**
     * @return the minimum version that this version could be,
     *                 eg. 8.x.x will be 8.0.0 while 8.1.x will be 8.1.0 and
     *                       8.2.1 will always be 8.2.1
     */
    TeiidVersion getMinimumVersion();

    /**
     * @return the maximum version that this version could be,
     *                 eg. 8.x.x will be 8.9.9 while 8.1.x will be 8.1.9 and
     *                       8.2.1 will always be 8.2.1
     */
    TeiidVersion getMaximumVersion();

    /**
     * Is this version greater than the given version
     *
     * Wildcards will cause the result to return false since either
     * this or otherVersion could be the greater depending on the
     * value given to the wildcard.
     *
     * @param otherVersion
     *
     * @return true if this version is greater. False otherwise.
     */
    boolean isGreaterThan(TeiidVersion otherVersion);

    /**
     *
     * @see #isGreaterThan(TeiidVersion)
     *
     * @param otherVersion
     *
     * @return true if this version is greater. False otherwise.
     */
    boolean isGreaterThan(Version otherVersion);

    /**
     * Is this version less than the given version
     *
     * Wildcards will cause the result to return false since either
     * this or otherVersion could be the lesser depending on the
     * value given to the wildcard.
     *
     * @param otherVersion
     *
     * @return true if this version is less. False otherwise.
     */
    boolean isLessThan(TeiidVersion otherVersion);

    /**
     * @see #isLessThan(TeiidVersion)
     *
     * @param otherVersion
     *
     * @return true if this version is less. False otherwise.
     */
    boolean isLessThan(Version otherVersion);

    /**
     * Convenience that delegates to {@link #compareTo(TeiidVersion)}
     * and {@link #isGreaterThan(TeiidVersion)}.
     *
     * @param otherVersion
     *
     * @return this is greater than or equal to otherVersion
     */
    boolean isGreaterThanOrEqualTo(TeiidVersion otherVersion);

   /**
    *
    * @see #isGreaterThanOrEqualTo(TeiidVersion)
    *
    * @param otherVersion
    *
    * @return this is greater than or equal to otherVersion
    */
    boolean isGreaterThanOrEqualTo(Version otherVersion);

    /**
     * Convenience that delegates to {@link #compareTo(TeiidVersion)}
     * and {@link #isLessThan(TeiidVersion)}.
     *
     * @param otherVersion
     *
     * @return this is less than or equal to otherVersion
     */
    boolean isLessThanOrEqualTo(TeiidVersion otherVersion);

    /**
     * @see #isLessThanOrEqualTo(TeiidVersion)
     *
     * @param otherVersion
     *
     * @return this is less than or equal to otherVersion
     */
    boolean isLessThanOrEqualTo(Version otherVersion);
}

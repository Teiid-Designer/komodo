/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.komodo.spi.runtime.version;

/**
 * Parent marker interface for teiid instance version information
 */
public interface ITeiidVersion {
    
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

        TEIID_8_7(EIGHT + DOT + SEVEN + DOT + ZERO);

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
    boolean compareTo(ITeiidVersion otherVersion);
    
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
    ITeiidVersion getMinimumVersion();

    /**
     * @return the maximum version that this version could be,
     *                 eg. 8.x.x will be 8.9.9 while 8.1.x will be 8.1.9 and
     *                       8.2.1 will always be 8.2.1
     */
    ITeiidVersion getMaximumVersion();

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
    boolean isGreaterThan(ITeiidVersion otherVersion);

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
    boolean isLessThan(ITeiidVersion otherVersion);

    /**
     * Convenience that delegates to {@link #compareTo(ITeiidVersion)}
     * and {@link #isGreaterThan(ITeiidVersion)}.
     *
     * @param otherVersion
     *
     * @return this is greater than or equal to otherVersion
     */
    boolean isGreaterThanOrEqualTo(ITeiidVersion otherVersion);

    /**
     * Convenience that delegates to {@link #compareTo(ITeiidVersion)}
     * and {@link #isLessThan(ITeiidVersion)}.
     *
     * @param otherVersion
     *
     * @return this is less than or equal to otherVersion
     */
    boolean isLessThanOrEqualTo(ITeiidVersion otherVersion);
}

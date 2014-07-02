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

import org.komodo.spi.Messages;


/**
 * Teiid Instance version class 
 * 
 *
 */
public class TeiidVersion implements ITeiidVersion {

    /**
     * Version enumerator
     */
    public static enum Version {

        /**
         * The default preferred teiid instance
         */
        DEFAULT_TEIID_VERSION(VersionID.TEIID_8_6),

        /**
         * Teiid 7.7
         */
        TEIID_7_7(VersionID.TEIID_7_7),

        /**
         * Teiid 8.0
         */
        TEIID_8_0(VersionID.TEIID_8_0),

        /**
         * Teiid 8.1
         */
        TEIID_8_1(VersionID.TEIID_8_1),

        /**
         * Teiid 8.2
         */
        TEIID_8_2(VersionID.TEIID_8_2),

        /**
         * Teiid 8.3
         */
        TEIID_8_3(VersionID.TEIID_8_3),

        /**
         * Teiid 8.4
         */
        TEIID_8_4(VersionID.TEIID_8_4),

        /**
         * Teiid 8.5
         */
        TEIID_8_5(VersionID.TEIID_8_5),

        /**
         * Teiid 8.6
         */
        TEIID_8_6(VersionID.TEIID_8_6),

        /**
         * Teiid 8.7
         */
        TEIID_8_7(VersionID.TEIID_8_7);

        private final ITeiidVersion version;

        Version(VersionID id) {
            version = new TeiidVersion(id.toString());
        }

        /**
         * @return version model
         */
        public ITeiidVersion get() {
            return version;
        }
    }

    private String versionString = ZERO + DOT + ZERO + DOT + ZERO;

    private final String majorVersion;

    private String minorVersion = WILDCARD;

    private String microVersion = WILDCARD;

    /**
     * Create a new instance with the given version segments
     * 
     * @param major the major version
     * @param minor the minor version
     * @param micro the micro version
     */
    public TeiidVersion(String major, String minor, String micro) {
        if (major == null)
            throw new IllegalArgumentException(Messages.getString(Messages.SPI.valueCannotBeNull, "major")); //$NON-NLS-1$
        
        if (minor == null)
            throw new IllegalArgumentException(Messages.getString(Messages.SPI.valueCannotBeNull, "minor")); //$NON-NLS-1$
        
        if (micro == null)
            throw new IllegalArgumentException(Messages.getString(Messages.SPI.valueCannotBeNull, "micro")); //$NON-NLS-1$
        
        this.majorVersion = major;
        this.minorVersion = minor;
        this.microVersion = micro;
        this.versionString = major + DOT + minor + DOT + micro;
    }
    
    /**
     * Create a new instance with the given version string
     * 
     * @param versionString the version string
     */
    public TeiidVersion(String versionString) {
        this.versionString = versionString;

        String[] tokens = versionString.split("\\."); //$NON-NLS-1$

        if (tokens.length >= 3) {
            majorVersion = tokens[0];
            minorVersion = tokens[1];
            if(tokens[2]!=null) {
            	int dashIndex = tokens[2].indexOf('-');
            	if(dashIndex!=-1 && tokens[2].length()>0) {
            		microVersion = tokens[2].substring(0,dashIndex);
            	} else {
                    microVersion = tokens[2];
            	}
            }
        }
        else if(tokens.length == 2) {
            majorVersion = tokens[0];
            minorVersion = tokens[1];
        }
        else {
            majorVersion = tokens[0];
        }
        this.versionString = majorVersion + DOT + minorVersion + DOT + microVersion;
    }

    @Override
    public String toString() {
        return versionString;
    }

    @Override
    public String getMajor() {
        return majorVersion;
    }

    @Override
    public String getMinor() {
        return minorVersion;
    }

    @Override
    public String getMicro() {
        return microVersion;
    }
    
    @Override
    public boolean hasWildCards() {
        return majorVersion.equals(WILDCARD) || minorVersion.equals(WILDCARD) || microVersion.equals(WILDCARD);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.majorVersion == null) ? 0 : this.majorVersion.hashCode());
        result = prime * result + ((this.microVersion == null) ? 0 : this.microVersion.hashCode());
        result = prime * result + ((this.minorVersion == null) ? 0 : this.minorVersion.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        TeiidVersion other = (TeiidVersion)obj;
        if (this.majorVersion == null) {
            if (other.majorVersion != null) return false;
        } else if (!this.majorVersion.equals(other.majorVersion)) return false;
        if (this.microVersion == null) {
            if (other.microVersion != null) return false;
        } else if (!this.microVersion.equals(other.microVersion)) return false;
        if (this.minorVersion == null) {
            if (other.minorVersion != null) return false;
        } else if (!this.minorVersion.equals(other.minorVersion)) return false;
        return true;
    }

    @Override
    public boolean compareTo(ITeiidVersion otherVersion) {
        String entryMajor = otherVersion.getMajor();

        if (! getMajor().equals(entryMajor) && ! getMajor().equals(WILDCARD) && ! entryMajor.equals(WILDCARD))
            return false;
        
        String entryMinor = otherVersion.getMinor();
        
        if (! getMinor().equals(entryMinor) && ! getMinor().equals(WILDCARD) && ! entryMinor.equals(WILDCARD))
            return false;
        
        String entryMicro = otherVersion.getMicro();
        
        if (! getMicro().equals(entryMicro) && ! getMicro().equals(WILDCARD) && ! entryMicro.equals(WILDCARD))
            return false;
        
        /*
         *  Either this version or entry version contain sufficient wildcards
         *  to be considered a match
         */
        return true;
    }
    
    @Override
    public boolean isSevenInstance() {
        return ITeiidVersion.SEVEN.equals(getMajor());
    }

    @Override
    public ITeiidVersion getMinimumVersion() {
        if (! this.hasWildCards())
            return this;

        String major = getMajor().equals(WILDCARD) ? SEVEN : getMajor();
        String minor = getMinor().equals(WILDCARD) ? ZERO : getMinor();
        String micro = getMicro().equals(WILDCARD) ? ZERO : getMicro();

        return new TeiidVersion(major, minor, micro);
    }

    @Override
    public ITeiidVersion getMaximumVersion() {
        if (! this.hasWildCards())
            return this;

        String major = getMajor().equals(WILDCARD) ? NINE : getMajor();
        String minor = getMinor().equals(WILDCARD) ? NINE : getMinor();
        String micro = getMicro().equals(WILDCARD) ? NINE : getMicro();

        return new TeiidVersion(major, minor, micro);
    }

    @Override
    public boolean isGreaterThan(ITeiidVersion otherVersion) {
        ITeiidVersion myMaxVersion = getMaximumVersion();
        ITeiidVersion otherMinVersion = otherVersion.getMinimumVersion();

        int majCompResult = myMaxVersion.getMajor().compareTo(otherMinVersion.getMajor());
        if (majCompResult > 0)
            return true;
        
        int minCompResult = myMaxVersion.getMinor().compareTo(otherMinVersion.getMinor());
        if (majCompResult == 0 && minCompResult > 0)
            return true;

        int micCompResult = myMaxVersion.getMicro().compareTo(otherMinVersion.getMicro());
        if (majCompResult == 0 && minCompResult == 0 && micCompResult > 0)
            return true;
            
        return false;
    }

    @Override
    public boolean isLessThan(ITeiidVersion otherVersion) {
        ITeiidVersion myMaxVersion = getMaximumVersion();
        ITeiidVersion otherMinVersion = otherVersion.getMinimumVersion();

        int majCompResult = myMaxVersion.getMajor().compareTo(otherMinVersion.getMajor());
        if (majCompResult < 0)
            return true;

        int minCompResult = myMaxVersion.getMinor().compareTo(otherMinVersion.getMinor());
        if (majCompResult == 0 && minCompResult < 0)
            return true;

        int micCompResult = myMaxVersion.getMicro().compareTo(otherMinVersion.getMicro());
        if (majCompResult == 0 && minCompResult == 0 && micCompResult < 0)
            return true;
            
        return false;
    }

    @Override
    public boolean isGreaterThanOrEqualTo(ITeiidVersion otherVersion) {
        return this.compareTo(otherVersion) || this.isGreaterThan(otherVersion);
    }

    @Override
    public boolean isLessThanOrEqualTo(ITeiidVersion otherVersion) {
        return this.compareTo(otherVersion) || this.isLessThan(otherVersion);
    }
}

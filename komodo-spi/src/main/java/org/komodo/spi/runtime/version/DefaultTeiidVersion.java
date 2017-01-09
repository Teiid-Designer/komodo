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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import org.komodo.spi.Messages;


/**
 * Teiid Instance version class 
 * 
 *
 */
public class DefaultTeiidVersion implements TeiidVersion {

    private static TeiidVersion extractDefaultVersion() {
        InputStream fileStream = DefaultTeiidVersion.class.getClassLoader().getResourceAsStream("default-teiid-version.txt");
        BufferedReader reader = new BufferedReader(new InputStreamReader(fileStream));

        try {
            String line = reader.readLine();
            if (line == null)
                throw new RuntimeException("Programming error: The default version id cannot be null");

            DefaultTeiidVersion version = new DefaultTeiidVersion(line);
            return version;
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        } finally {
            try {
                reader.close();
            } catch (IOException e) {
                // Do Nothing
            }
        }
    }

    /**
     * Version enumerator
     */
    public static enum Version {

        /**
         * The default preferred teiid instance
         */
        DEFAULT_TEIID_VERSION(extractDefaultVersion()),

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
        TEIID_8_7(VersionID.TEIID_8_7),

        /**
         * Teiid 8.8
         */
        TEIID_8_8(VersionID.TEIID_8_8),

        /**
         * Teiid 8.9
         */
        TEIID_8_9(VersionID.TEIID_8_9),

        /**
         * Teiid 8.10
         */
        TEIID_8_10(VersionID.TEIID_8_10),

        /**
         * Teiid 8.11
         */
        TEIID_8_11(VersionID.TEIID_8_11),

        /**
         * Teiid 8.11.5
         */
        TEIID_8_11_5(VersionID.TEIID_8_11_5),

        /**
         * Teiid 8.12
         */
        TEIID_8_12(VersionID.TEIID_8_12),

        /**
         * Teiid 8.12.4
         */
        TEIID_8_12_4(VersionID.TEIID_8_12_4),

        /**
         * Teiid 8.12.7
         */
        TEIID_8_12_7(VersionID.TEIID_8_12_7),

        /**
         * Teiid 8.13
         */
        TEIID_8_13(VersionID.TEIID_8_13),

        /**
         * Teiid 9.0
         */
        TEIID_9_0(VersionID.TEIID_9_0),

        /**
         * Teiid 9.1
         */
        TEIID_9_1(VersionID.TEIID_9_1),

        /**
         * Teiid 9.1.1
         */
        TEIID_9_1_1(VersionID.TEIID_9_1_1);

        private final TeiidVersion version;

        Version(TeiidVersion version) {
            this.version = version;
        }

        Version(VersionID id) {
            version = new DefaultTeiidVersion(id.toString());
        }

        /**
         * @return version model
         */
        public TeiidVersion get() {
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
    public DefaultTeiidVersion(String major, String minor, String micro) {
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
    public DefaultTeiidVersion(String versionString) {
        this.versionString = versionString;

        String[] tokens = versionString.split("\\."); //$NON-NLS-1$

        if (tokens.length >= 3) {
            majorVersion = tokens[0];
            minorVersion = tokens[1];
            if (tokens[2] != null) {
                int dashIndex = tokens[2].indexOf('-');
                if (dashIndex != -1 && tokens[2].length() > 0) {
                    microVersion = tokens[2].substring(0, dashIndex);
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
        DefaultTeiidVersion other = (DefaultTeiidVersion)obj;
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
    public boolean compareTo(TeiidVersion otherVersion) {
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
        return TeiidVersion.SEVEN.equals(getMajor());
    }

    @Override
    public TeiidVersion getMinimumVersion() {
        if (! this.hasWildCards())
            return this;

        String major = getMajor().equals(WILDCARD) ? SEVEN : getMajor();
        String minor = getMinor().equals(WILDCARD) ? ZERO : getMinor();
        String micro = getMicro().equals(WILDCARD) ? ZERO : getMicro();

        return new DefaultTeiidVersion(major, minor, micro);
    }

    @Override
    public TeiidVersion getMaximumVersion() {
        if (! this.hasWildCards())
            return this;

        String major = getMajor().equals(WILDCARD) ? NINE : getMajor();
        String minor = getMinor().equals(WILDCARD) ? NINE : getMinor();
        String micro = getMicro().equals(WILDCARD) ? NINE : getMicro();

        return new DefaultTeiidVersion(major, minor, micro);
    }

    @Override
    public boolean isGreaterThan(TeiidVersion otherVersion) {
        TeiidVersion myMinVersion = getMinimumVersion();
        TeiidVersion otherMaxVersion = otherVersion.getMaximumVersion();

        int majCompResult = isOtherNumberGreaterThan(myMinVersion.getMajor(), otherMaxVersion.getMajor());
        if (majCompResult > 0)
            return true;
        
        int minCompResult = isOtherNumberGreaterThan(myMinVersion.getMinor(), otherMaxVersion.getMinor());
        if (majCompResult == 0 && minCompResult > 0)
            return true;

        int micCompResult = isOtherNumberGreaterThan(myMinVersion.getMicro(), otherMaxVersion.getMicro());
        if (majCompResult == 0 && minCompResult == 0 && micCompResult > 0)
            return true;

        return false;
    }

    @Override
    public boolean isGreaterThan(Version otherVersion) {
        return isGreaterThan(otherVersion.get());
    }

    @Override
    public boolean isLessThan(TeiidVersion otherVersion) {
        TeiidVersion myMaxVersion = getMaximumVersion();
        TeiidVersion otherMinVersion = otherVersion.getMinimumVersion();

        int majCompResult;
        try {
            int myMax = Integer.parseInt(myMaxVersion.getMajor());
            int otherMin = Integer.parseInt(otherMinVersion.getMajor());
            majCompResult = Integer.valueOf(myMax).compareTo(Integer.valueOf(otherMin));

        } catch (NumberFormatException ex) {
            // One or other is a string so compare lexographically
            majCompResult = myMaxVersion.getMajor().compareTo(otherMinVersion.getMajor());
        }

        if (majCompResult < 0)
            return true;

        int minCompResult;
        try {
            int myMax = Integer.parseInt(myMaxVersion.getMinor());
            int otherMin = Integer.parseInt(otherMinVersion.getMinor());
            minCompResult = Integer.valueOf(myMax).compareTo(Integer.valueOf(otherMin));
        } catch (NumberFormatException ex) {
            // One or other is a string so compare lexographically
            minCompResult = myMaxVersion.getMinor().compareTo(otherMinVersion.getMinor());
        }

        if (majCompResult == 0 && minCompResult < 0)
            return true;

        int micCompResult;
        try {
            int myMax = Integer.parseInt(myMaxVersion.getMicro());
            int otherMin = Integer.parseInt(otherMinVersion.getMicro());
            micCompResult = Integer.valueOf(myMax).compareTo(Integer.valueOf(otherMin));
        } catch (NumberFormatException ex) {
            // One or other is a string so compare lexographically
            micCompResult = myMaxVersion.getMicro().compareTo(otherMinVersion.getMicro());
        }

        if (majCompResult == 0 && minCompResult == 0 && micCompResult < 0)
            return true;
            
        return false;
    }

    @Override
    public boolean isLessThan(Version otherVersion) {
        return isLessThan(otherVersion.get());
    }

    @Override
    public boolean isGreaterThanOrEqualTo(TeiidVersion otherVersion) {
        return this.compareTo(otherVersion) || this.isGreaterThan(otherVersion);
    }

    @Override
    public boolean isGreaterThanOrEqualTo(Version otherVersion) {
        return isGreaterThanOrEqualTo(otherVersion.get());
    }

    @Override
    public boolean isLessThanOrEqualTo(TeiidVersion otherVersion) {
        return this.compareTo(otherVersion) || this.isLessThan(otherVersion);
    }

    @Override
    public boolean isLessThanOrEqualTo(Version otherVersion) {
        return isLessThan(otherVersion.get());
    }

    private int isOtherNumberGreaterThan(String myNumber, String otherNumber) {
        int myValue = -1;
        int otherValue = -1;

        try {
            myValue = Integer.parseInt(myNumber);
        } catch (NumberFormatException e) {
            myValue = -1;
        }

        try {
            otherValue = Integer.parseInt(otherNumber);
        } catch (NumberFormatException e) {
            otherValue = -1;
        }

        if (myValue < 0 || otherValue < 0) {
            return myNumber.compareTo(otherNumber);
        } else {
            return myValue - otherValue;
        }
    }
}

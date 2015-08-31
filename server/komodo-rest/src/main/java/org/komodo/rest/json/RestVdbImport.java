/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import java.util.Objects;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.utils.ArgCheck;
import com.google.gson.annotations.SerializedName;

/**
 * A VDB import that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * <code>
 * {
 *     "id" : "MyImportVdb",
 *     "version" : "1",
 *     "importDataPolicies" : "true"
 * }
 * </code>
 * </pre>
 */
public final class RestVdbImport extends KomodoRestEntity {

    /**
     * And empty array of VDB imports.
     */
    public static final RestVdbImport[] NO_IMPORTS = new RestVdbImport[ 0 ];

    @SerializedName( "id" )
    private String name;
    private int version = Vdb.DEFAULT_VERSION;
    private boolean importDataPolicies = VdbImport.DEFAULT_IMPORT_DATA_POLICIES;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbImport() {
        // nothing to do
    }

    /**
     * @param name
     *        the name of the VDB import (cannot be empty)
     * @param version
     *        the VDB version
     */
    public RestVdbImport( final String name,
                          final int version ) {
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        this.name = name;
        this.version = version;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object other ) {
        if ( !super.equals( other ) ) {
            return false;
        }

        assert( other != null );
        assert( getClass().equals( other.getClass() ) );

        final RestVdbImport that = ( RestVdbImport )other;

        // check name
        if ( this.name == null ) {
            if ( that.name != null ) {
                return false;
            }
        } else if ( !this.name.equals( that.name ) ) {
            return false;
        }

        // check version
        if ( this.version != that.version ) {
            return false;
        }

        // check importDataPolicies
        if ( this.importDataPolicies != that.importDataPolicies ) {
            return false;
        }

        return true;
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        return this.name;
    }

    /**
     * @return the VDB version
     */
    public int getVersion() {
        return this.version;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Objects.hash( this.name, this.version, this.importDataPolicies, super.hashCode() );
    }

    /**
     * @return <code>true</code> if importing data policies
     */
    public boolean isImportDataPolicies() {
        return this.importDataPolicies;
    }

    /**
     * @param newImportDataPolicies
     *        the new import data policies
     */
    public void setImportDataPolicies( final boolean newImportDataPolicies ) {
        this.importDataPolicies = newImportDataPolicies;
    }

    /**
     * @param newName
     *        the new VDB import name (can be empty)
     */
    public void setName( final String newName ) {
        this.name = newName;
    }

    /**
     * @param newVersion
     *        the new VDB import version
     */
    public void setVersion( final int newVersion ) {
        this.version = newVersion;
    }

}

/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import java.util.Arrays;
import java.util.Objects;
import org.komodo.rest.KomodoRestEntity;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * A VDB that can be used by GSON to build a JSON document representation.
 */
public final class RestVdb extends KomodoRestEntity {

    private String name;
    private String description;
    private String originalFilePath;
    private RestVdbDataRole[] dataRoles = RestVdbDataRole.NO_DATA_ROLES;
    private RestVdbEntry[] entries = RestVdbEntry.NO_ENTRIES;
    private RestVdbImport[] imports = RestVdbImport.NO_IMPORTS;
    private RestVdbTranslator[] translators = RestVdbTranslator.NO_TRANSLATORS;

    // models

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdb() {
        // nothing to do
    }

    /**
     * @param vdbName
     *        the name of the VDB (cannot be empty)
     */
    public RestVdb( final String vdbName ) {
        ArgCheck.isNotEmpty( vdbName, "vdbName" ); //$NON-NLS-1$
        this.name = vdbName;
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

        final RestVdb that = ( RestVdb )other;

        // check name
        if ( this.name == null ) {
            if ( that.name != null ) {
                return false;
            }
        } else if ( !this.name.equals( that.name ) ) {
            return false;
        }

        // check description
        if ( this.description == null ) {
            if ( that.description != null ) {
                return false;
            }
        } else if ( !this.description.equals( that.description ) ) {
            return false;
        }

        // check file path
        if ( this.originalFilePath == null ) {
            if ( that.originalFilePath != null ) {
                return false;
            }
        } else if ( !this.originalFilePath.equals( that.originalFilePath ) ) {
            return false;
        }

        // data roles
        if ( !Arrays.deepEquals( getDataRoles(), that.getDataRoles() ) ) {
            return false;
        }

        // entries
        if ( !Arrays.deepEquals( getEntries(), that.getEntries() ) ) {
            return false;
        }

        // imports
        if ( !Arrays.deepEquals( getImports(), that.getImports() ) ) {
            return false;
        }

        // translators
        if ( !Arrays.deepEquals( getTranslators(), that.getTranslators() ) ) {
            return false;
        }

        return true;
    }

    /**
     * @return the data roles (never <code>null</code> but can be empty)
     */
    public RestVdbDataRole[] getDataRoles() {
        return this.dataRoles;
    }

    /**
     * @return the VDB description (can be empty)
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * @return the entries (never <code>null</code> but can be empty)
     */
    public RestVdbEntry[] getEntries() {
        return this.entries;
    }

    /**
     * @return the VDB imports (never <code>null</code> but can be empty)
     */
    public RestVdbImport[] getImports() {
        return this.imports;
    }

    /**
     * @return the VDB name (can be empty)
     */
    public String getName() {
        return this.name;
    }

    /**
     * @return the external file path of the VDB (can be empty)
     */
    public String getOriginalFilePath() {
        return this.originalFilePath;
    }

    /**
     * @return the translators (never <code>null</code> but can be empty)
     */
    public RestVdbTranslator[] getTranslators() {
        return this.translators;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Objects.hash( this.name,
                             this.description,
                             this.originalFilePath,
                             Arrays.deepHashCode( this.dataRoles ),
                             Arrays.deepHashCode( this.entries ),
                             Arrays.deepHashCode( this.imports ),
                             Arrays.deepHashCode( this.translators ),
                             super.hashCode() );
    }

    /**
     * @param newDataRoles
     *        the new data roles (can be <code>null</code>)
     */
    public void setDataRoles( final RestVdbDataRole[] newDataRoles ) {
        this.dataRoles = ( ( newDataRoles == null ) ? RestVdbDataRole.NO_DATA_ROLES : newDataRoles );
    }

    /**
     * @param newDescription
     *        the new description (can be empty)
     */
    public void setDescription( final String newDescription ) {
        this.description = newDescription;
    }

    /**
     * @param newEntries
     *        the new entries (can be <code>null</code>)
     */
    public void setEntries( final RestVdbEntry[] newEntries ) {
        this.entries = ( ( newEntries == null ) ? RestVdbEntry.NO_ENTRIES : newEntries );
    }

    /**
     * @param newImports
     *        the new VDB imports (can be <code>null</code>)
     */
    public void setImports( final RestVdbImport[] newImports ) {
        this.imports = ( ( newImports == null ) ? RestVdbImport.NO_IMPORTS : newImports );
    }

    /**
     * @param newName
     *        the new VDB name (can be empty)
     */
    public void setName( final String newName ) {
        this.name = newName;
    }

    /**
     * @param newOriginalFilePath
     *        the new VDB external file path (can be empty)
     */
    public void setOriginalFilePath( final String newOriginalFilePath ) {
        this.originalFilePath = newOriginalFilePath;
    }

    /**
     * @param newTranslators
     *        the new translators (can be <code>null</code>)
     */
    public void setTranslators( final RestVdbTranslator[] newTranslators ) {
        this.translators = ( ( newTranslators == null ) ? RestVdbTranslator.NO_TRANSLATORS : newTranslators );
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder();
        builder.append( "VDB name = " ).append( this.name ); //$NON-NLS-1$
        builder.append( ", " ); //$NON-NLS-1$

        { // data roles
            final RestVdbDataRole[] dataRoles = getDataRoles();
            builder.append( "data roles = [" ); //$NON-NLS-1$

            if ( dataRoles.length != 0 ) {
                boolean firstTime = true;

                for ( final RestVdbDataRole dataRole : dataRoles ) {
                    if ( firstTime ) {
                        firstTime = false;
                    } else {
                        builder.append( ", " ); //$NON-NLS-1$
                    }

                    final String name = dataRole.getName();
                    builder.append( StringUtils.isBlank( name ) ? "<no name>" : name ); //$NON-NLS-1$
                }
            }

            builder.append( "]" ); //$NON-NLS-1$
        }

        { // entries
            final RestVdbEntry[] entries = getEntries();
            builder.append( "entries = [" ); //$NON-NLS-1$

            if ( entries.length != 0 ) {
                boolean firstTime = true;

                for ( final RestVdbEntry entry : entries ) {
                    if ( firstTime ) {
                        firstTime = false;
                    } else {
                        builder.append( ", " ); //$NON-NLS-1$
                    }

                    final String name = entry.getName();
                    builder.append( StringUtils.isBlank( name ) ? "<no name>" : name ); //$NON-NLS-1$
                }
            }

            builder.append( "]" ); //$NON-NLS-1$
        }

        { // VDB imports
            final RestVdbImport[] vdbImports = getImports();
            builder.append( "imports = [" ); //$NON-NLS-1$

            if ( this.imports.length != 0 ) {
                boolean firstTime = true;

                for ( final RestVdbImport vdbImport : vdbImports ) {
                    if ( firstTime ) {
                        firstTime = false;
                    } else {
                        builder.append( ", " ); //$NON-NLS-1$
                    }

                    final String name = vdbImport.getName();
                    builder.append( StringUtils.isBlank( name ) ? "<no name>" : name ); //$NON-NLS-1$
                }
            }

            builder.append( "]" ); //$NON-NLS-1$
        }

        { // translators
            final RestVdbTranslator[] translators = getTranslators();
            builder.append( "translators = [" ); //$NON-NLS-1$

            if ( translators.length != 0 ) {
                boolean firstTime = true;

                for ( final RestVdbTranslator translator : translators ) {
                    if ( firstTime ) {
                        firstTime = false;
                    } else {
                        builder.append( ", " ); //$NON-NLS-1$
                    }

                    final String name = translator.getName();
                    builder.append( StringUtils.isBlank( name ) ? "<no name>" : name ); //$NON-NLS-1$
                }
            }

            builder.append( "]" ); //$NON-NLS-1$
        }

        return builder.toString();
    }

}

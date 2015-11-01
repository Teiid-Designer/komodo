/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import java.net.URI;
import java.util.Arrays;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestEntity;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A data role that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * <code>
 * {
 *     "id" : "MyDataRole",
 *     "description" : "data role description goes here",
 *     "allowCreateTempTables" : true,
 *     "anyAuthenticated" : true,
 *     "grantAll" : true,
 *     "mappedRoles" : [
 *         "admin", "tester", "developer", "manager"
 *     ],
 *     "permissions" : [
 *         {
 *             "id" : "MyPermission",
 *             "allowAlter" : true,
 *             "allowCreate" : true,
 *             "allowDelete" : true,
 *             "allowExecute" : true,
 *             "allowLanguage" : true,
 *             "allowRead" : true,
 *             "allowUpdate" : true,
 *             "conditions" : {
 *                 "cant" : true,
 *                 "buy" : false,
 *                 "me" : true,
 *                 "love" : false
 *             },
 *             "masks" : {
 *                 "love" : "words",
 *                 "me" : "of",
 *                 "do" : "love"
 *             }
 *         },
 *         {
 *             "id" : "YourPermission",
 *             "allowAlter" : false,
 *             "allowCreate" : false,
 *             "allowDelete" : false,
 *             "allowExecute" : false,
 *             "allowLanguage" : false,
 *             "allowRead" : false,
 *             "allowUpdate" : false,
 *             "conditions" : {
 *                 "watching" : true,
 *                 "the" : true,
 *                 "detectives" : false
 *             },
 *             "masks" : {
 *                 "beatonthebrat" : "withabaseballbat"
 *             }
 *         }
 *     ]
 * }
 * </code>
 * </pre>
 */
public final class RestVdbDataRole extends KomodoRestEntity {

    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = KomodoService.encode(VdbLexicon.DataRole.DATA_ROLE);

    /**
     * Label used to describe description
     */
    public static final String DESCRIPTION_LABEL = KomodoService.encode(VdbLexicon.DataRole.DESCRIPTION);

    /**
     * Label used to describe allowCreateTempTables
     */
    public static final String ALLOW_CREATE_TEMP_TABLES_LABEL = KomodoService.encode(VdbLexicon.DataRole.ALLOW_CREATE_TEMP_TABLES);

    /**
     * Label used to describe anyAuthenticated
     */
    public static final String ANY_AUTHENTICATED_LABEL = KomodoService.encode(VdbLexicon.DataRole.ANY_AUTHENTICATED);

    /**
     * Label used to describe grantAll
     */
    public static final String GRANT_ALL_LABEL = KomodoService.encode(VdbLexicon.DataRole.GRANT_ALL);

    /**
     * Label used to describe mapped role names
     */
    public static final String MAPPED_ROLES_LABEL = KomodoService.encode(VdbLexicon.DataRole.MAPPED_ROLE_NAMES);

    /**
     * An empty array of data roles.
     */
    public static final RestVdbDataRole[] NO_DATA_ROLES = new RestVdbDataRole[ 0 ];

    private String name;
    private String description;
    private boolean allowCreateTempTables = DataRole.DEFAULT_ALLOW_CREATE_TEMP_TABLES;
    private boolean anyAuthenticated = DataRole.DEFAULT_ANY_AUTHENTICATED;
    private boolean grantAll = DataRole.DEFAULT_GRANT_ALL;
    private String[] mappedRoles = StringConstants.EMPTY_ARRAY;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbDataRole() {
        // nothing to do
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the vdb
     * @param id the id of this vdb
     * @param dataPath the data path of this vdb
     * @param kType the type of this vdb component
     * @param hasChildren true if vdb has children
     * @param parentVdb the name of the parent vdb
     */
    public RestVdbDataRole(URI baseUri, String id, String dataPath, KomodoType kType, boolean hasChildren, String parentVdb) {
        super(baseUri, id, dataPath, kType, hasChildren);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().buildVdbDataRoleUri(LinkType.SELF, parentVdb, id)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().buildVdbDataRoleUri(LinkType.PARENT, parentVdb, id)));
        addLink(new RestLink(LinkType.PERMISSIONS, getUriBuilder().buildVdbDataRoleUri(LinkType.PERMISSIONS, parentVdb, id)));
    }

    /**
     * @return the description (can be empty)
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * @return the mapped roles (never <code>null</code> but can be empty)
     */
    public String[] getMappedRoles() {
        return this.mappedRoles;
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        return this.name;
    }

    /**
     * @return <code>true</code> if allows creating temp tables
     */
    public boolean isAllowCreateTempTables() {
        return this.allowCreateTempTables;
    }

    /**
     * @return <code>true</code> if any authenticated
     */
    public boolean isAnyAuthenticated() {
        return this.anyAuthenticated;
    }

    /**
     * @return <code>true</code> if allowed grant all permissions
     */
    public boolean isGrantAll() {
        return this.grantAll;
    }

    /**
     * @param newAllowCreateTempTables
     *        <code>true</code> if allows creating temp tables
     */
    public void setAllowCreateTempTables( final boolean newAllowCreateTempTables ) {
        this.allowCreateTempTables = newAllowCreateTempTables;
    }

    /**
     * @param newAnyAuthenticated
     *        <code>true</code> if any authenticated
     */
    public void setAnyAuthenticated( final boolean newAnyAuthenticated ) {
        this.anyAuthenticated = newAnyAuthenticated;
    }

    /**
     * @param newDescription
     *        the new description (can be empty)
     */
    public void setDescription( final String newDescription ) {
        this.description = newDescription;
    }

    /**
     * @param newGrantAll
     *        <code>true</code> if allows grant all
     */
    public void setGrantAll( final boolean newGrantAll ) {
        this.grantAll = newGrantAll;
    }

    /**
     * @param newMappedRoles
     *        the new mapped roles (can be <code>null</code>)
     */
    public void setMappedRoles( final String[] newMappedRoles ) {
        this.mappedRoles = ( ( newMappedRoles == null ) ? StringConstants.EMPTY_ARRAY : newMappedRoles );
    }

    /**
     * @param newName
     *        the new translator name (can be empty)
     */
    public void setName( final String newName ) {
        this.name = newName;
    }

    /**
     * @param dataRole source data role
     * @param parentVdb vdb parent
     * @param baseUri base uri
     * @param uow the transaction
     * @return a new {@link RestVdbDataRole} based on the source {@link DataRole}
     * @throws KException if error occurs
     */
    public static RestVdbDataRole build(DataRole dataRole, Vdb parentVdb, URI baseUri, UnitOfWork uow) throws KException {
        final String vdbName = parentVdb.getName(uow);
        final RestVdbDataRole entity = new RestVdbDataRole(baseUri,
                                                                                       dataRole.getName(uow),
                                                                                       dataRole.getAbsolutePath(),
                                                                                       dataRole.getTypeIdentifier(uow),
                                                                                       dataRole.hasChildren(uow), vdbName);

        entity.setName(dataRole.getName(uow));
        entity.setDescription(dataRole.getDescription(uow));
        entity.setAllowCreateTempTables(dataRole.isAllowCreateTempTables(uow));
        entity.setAnyAuthenticated(dataRole.isAnyAuthenticated(uow));
        entity.setGrantAll(dataRole.isGrantAll(uow));

        String[] mappedRoles = dataRole.getMappedRoles(uow);
        if (mappedRoles != null) {
            entity.setMappedRoles(mappedRoles);
        }

        return entity;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.allowCreateTempTables ? 1231 : 1237);
        result = prime * result + (this.anyAuthenticated ? 1231 : 1237);
        result = prime * result + ((this.description == null) ? 0 : this.description.hashCode());
        result = prime * result + (this.grantAll ? 1231 : 1237);
        result = prime * result + Arrays.hashCode(this.mappedRoles);
        result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        RestVdbDataRole other = (RestVdbDataRole)obj;
        if (this.allowCreateTempTables != other.allowCreateTempTables)
            return false;
        if (this.anyAuthenticated != other.anyAuthenticated)
            return false;
        if (this.description == null) {
            if (other.description != null)
                return false;
        } else
            if (!this.description.equals(other.description))
                return false;
        if (this.grantAll != other.grantAll)
            return false;
        if (!Arrays.equals(this.mappedRoles, other.mappedRoles))
            return false;
        if (this.name == null) {
            if (other.name != null)
                return false;
        } else
            if (!this.name.equals(other.name))
                return false;
        return true;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "RestVdbDataRole [name=" + this.name + ", description=" + this.description + ", allowCreateTempTables=" + this.allowCreateTempTables + ", anyAuthenticated=" + this.anyAuthenticated + ", grantAll=" + this.grantAll + ", mappedRoles=" + Arrays.toString(this.mappedRoles) + ", id=" + this.id + ", dataPath=" + this.dataPath + ", kType=" + this.kType + ", hasChildren=" + this.hasChildren + ", properties=" + this.properties + ", links=" + this.links + "]";
    }

}

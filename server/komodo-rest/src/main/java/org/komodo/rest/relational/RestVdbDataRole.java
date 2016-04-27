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

import java.net.URI;
import java.util.Properties;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

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
public final class RestVdbDataRole extends RestBasicEntity {

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

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbDataRole() {
        setAllowCreateTempTables(DataRole.DEFAULT_ALLOW_CREATE_TEMP_TABLES);
        setAnyAuthenticated(DataRole.DEFAULT_ANY_AUTHENTICATED);
        setGrantAll(DataRole.DEFAULT_GRANT_ALL);
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param dataRole the data role
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestVdbDataRole(URI baseUri, DataRole dataRole, UnitOfWork uow) throws KException {
        super(baseUri, dataRole, uow, false);

        setName(dataRole.getName(uow));
        setDescription(dataRole.getDescription(uow));
        setAllowCreateTempTables(dataRole.isAllowCreateTempTables(uow));
        setAnyAuthenticated(dataRole.isAnyAuthenticated(uow));
        setGrantAll(dataRole.isGrantAll(uow));

        String[] mappedRoles = dataRole.getMappedRoles(uow);
        if (mappedRoles != null) {
            setMappedRoles(mappedRoles);
        }

        Vdb vdb = ancestor(dataRole, Vdb.class, uow);
        ArgCheck.isNotNull(vdb);
        String vdbName = vdb.getName(uow);

        Properties settings = getUriBuilder().createSettings(SettingNames.VDB_NAME, vdbName);
        getUriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, getUriBuilder().vdbParentUri(vdb, uow));
        getUriBuilder().addSetting(settings, SettingNames.DATA_ROLE_ID, getId());

        addLink(new RestLink(LinkType.SELF, getUriBuilder().vdbDataRoleUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().vdbDataRoleUri(LinkType.PARENT, settings)));
        createChildLink();
        addLink(new RestLink(LinkType.PERMISSIONS, getUriBuilder().vdbDataRoleUri(LinkType.PERMISSIONS, settings)));
    }

    /**
     * @return the description (can be empty)
     */
    public String getDescription() {
        Object description = tuples.get(DESCRIPTION_LABEL);
        return description != null ? description.toString() : null;
    }

    /**
     * @return the mapped roles (never <code>null</code> but can be empty)
     */
    public String[] getMappedRoles() {
        Object roles = tuples.get(MAPPED_ROLES_LABEL);
        return roles != null ? (String[]) roles : EMPTY_ARRAY;
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        Object name = tuples.get(NAME_LABEL);
        return name != null ? name.toString() : null;
    }

    /**
     * @return <code>true</code> if allows creating temp tables
     */
    public boolean isAllowCreateTempTables() {
        Object value = tuples.get(ALLOW_CREATE_TEMP_TABLES_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : DataRole.DEFAULT_ALLOW_CREATE_TEMP_TABLES;
    }

    /**
     * @return <code>true</code> if any authenticated
     */
    public boolean isAnyAuthenticated() {
        Object value = tuples.get(ANY_AUTHENTICATED_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : DataRole.DEFAULT_ANY_AUTHENTICATED;
    }

    /**
     * @return <code>true</code> if allowed grant all permissions
     */
    public boolean isGrantAll() {
        Object value = tuples.get(GRANT_ALL_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : DataRole.DEFAULT_GRANT_ALL;
    }

    /**
     * @param newAllowCreateTempTables
     *        <code>true</code> if allows creating temp tables
     */
    public void setAllowCreateTempTables( final boolean newAllowCreateTempTables ) {
        tuples.put(ALLOW_CREATE_TEMP_TABLES_LABEL, newAllowCreateTempTables);
    }

    /**
     * @param newAnyAuthenticated
     *        <code>true</code> if any authenticated
     */
    public void setAnyAuthenticated( final boolean newAnyAuthenticated ) {
        tuples.put(ANY_AUTHENTICATED_LABEL, newAnyAuthenticated);
    }

    /**
     * @param newDescription
     *        the new description (can be empty)
     */
    public void setDescription( final String newDescription ) {
        tuples.put(DESCRIPTION_LABEL, newDescription);
    }

    /**
     * @param newGrantAll
     *        <code>true</code> if allows grant all
     */
    public void setGrantAll( final boolean newGrantAll ) {
        tuples.put(GRANT_ALL_LABEL, newGrantAll);
    }

    /**
     * @param newMappedRoles
     *        the new mapped roles (can be <code>null</code>)
     */
    public void setMappedRoles( final String[] newMappedRoles ) {
        tuples.put(MAPPED_ROLES_LABEL, newMappedRoles);
    }

    /**
     * @param newName
     *        the new translator name (can be empty)
     */
    public void setName( final String newName ) {
        tuples.put(NAME_LABEL, newName);
    }
}

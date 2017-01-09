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
package org.komodo.rest.relational.response;

import java.net.URI;
import java.util.Properties;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
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
 *     "id" : "MyPermission",
 *     "allowAlter" : true,
 *     "allowCreate" : true,
 *     "allowDelete" : true,
 *     "allowExecute" : true,
 *     "allowLanguage" : true,
 *     "allowRead" : true,
 *     "allowUpdate" : true,
 *     "conditions" : {
 *         "cant" : true,
 *         "buy" : false,
 *         "me" : true,
 *         "love" : false
 *     },
 *     "masks" : {
 *         "love" : "words",
 *         "me" : "of",
 *         "do" : "love"
 *     }
 * }
 * </code>
 * </pre>
 */
public final class RestVdbPermission extends RestBasicEntity {

    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = KomodoService.protectPrefix(VdbLexicon.DataRole.Permission.PERMISSION);

    /**
     * Label used to describe allowAlter
     */
    public static final String ALLOW_ALTER_LABEL = KomodoService.protectPrefix(VdbLexicon.DataRole.Permission.ALLOW_ALTER);

    /**
     * Label used to describe allowCreate
     */
    public static final String ALLOW_CREATE_LABEL = KomodoService.protectPrefix(VdbLexicon.DataRole.Permission.ALLOW_CREATE);

    /**
     * Label used to describe allowDelete
     */
    public static final String ALLOW_DELETE_LABEL = KomodoService.protectPrefix(VdbLexicon.DataRole.Permission.ALLOW_DELETE);

    /**
     * Label used to describe allowExecute
     */
    public static final String ALLOW_EXECUTE_LABEL = KomodoService.protectPrefix(VdbLexicon.DataRole.Permission.ALLOW_EXECUTE);

    /**
     * Label used to describe allowLanguage
     */
    public static final String ALLOW_LANGUAGE_LABEL = KomodoService.protectPrefix(VdbLexicon.DataRole.Permission.ALLOW_LANGUAGE);

    /**
     * Label used to describe allowRead
     */
    public static final String ALLOW_READ_LABEL = KomodoService.protectPrefix(VdbLexicon.DataRole.Permission.ALLOW_READ);

    /**
     * Label used to describe allowUpdate
     */
    public static final String ALLOW_UPDATE_LABEL = KomodoService.protectPrefix(VdbLexicon.DataRole.Permission.ALLOW_UPDATE);

    /**
     * An empty array of permissions.
     */
    public static final RestVdbPermission[] NO_PERMISSIONS = new RestVdbPermission[ 0 ];

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbPermission() {
        setAllowAlter(Permission.DEFAULT_ALLOW_ALTER);
        setAllowCreate(Permission.DEFAULT_ALLOW_CREATE);
        setAllowDelete(Permission.DEFAULT_ALLOW_DELETE);
        setAllowExecute(Permission.DEFAULT_ALLOW_EXECUTE);
        setAllowLanguage(Permission.DEFAULT_ALLOW_LANGUAGE);
        setAllowRead(Permission.DEFAULT_ALLOW_READ);
        setAllowUpdate(Permission.DEFAULT_ALLOW_UPDATE);
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param permission the permission
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestVdbPermission(URI baseUri, Permission permission, UnitOfWork uow) throws KException {
        super(baseUri, permission, uow, false);

        setName(permission.getName(uow));
        setAllowAlter(permission.isAllowAlter(uow));
        setAllowCreate(permission.isAllowCreate(uow));
        setAllowDelete(permission.isAllowDelete(uow));
        setAllowExecute(permission.isAllowExecute(uow));
        setAllowLanguage(permission.isAllowLanguage(uow));
        setAllowRead(permission.isAllowRead(uow));
        setAllowUpdate(permission.isAllowUpdate(uow));

        DataRole dataRole = ancestor(permission, DataRole.class, uow);
        ArgCheck.isNotNull(dataRole);
        String dataRoleName = dataRole.getName(uow);

        Vdb vdb = ancestor(dataRole, Vdb.class, uow);
        ArgCheck.isNotNull(vdb);
        String vdbName = vdb.getName(uow);

        Properties settings = getUriBuilder().createSettings(SettingNames.VDB_NAME, vdbName);
        getUriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, getUriBuilder().vdbParentUri(vdb, uow));
        getUriBuilder().addSetting(settings, SettingNames.DATA_ROLE_ID, dataRoleName);
        getUriBuilder().addSetting(settings, SettingNames.PERMISSION_ID, getId());

        addLink(new RestLink(LinkType.SELF, getUriBuilder().vdbPermissionUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().vdbPermissionUri(LinkType.PARENT, settings)));
        createChildLink();
        addLink(new RestLink(LinkType.CONDITIONS, getUriBuilder().vdbPermissionUri(LinkType.CONDITIONS, settings)));
        addLink(new RestLink(LinkType.MASKS, getUriBuilder().vdbPermissionUri(LinkType.MASKS, settings)));
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        Object name = tuples.get(NAME_LABEL);
        return name != null ? name.toString() : null;
    }
    /**
     * @return <code>true</code> if allows alter
     */
    public boolean isAllowAlter() {
        Object value = tuples.get(ALLOW_ALTER_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : Permission.DEFAULT_ALLOW_ALTER;
    }

    /**
     * @return <code>true</code> if allows create
     */
    public boolean isAllowCreate() {
        Object value = tuples.get(ALLOW_CREATE_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : Permission.DEFAULT_ALLOW_CREATE;
    }

    /**
     * @return <code>true</code> if allows delete
     */
    public boolean isAllowDelete() {
        Object value = tuples.get(ALLOW_DELETE_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : Permission.DEFAULT_ALLOW_DELETE;
    }

    /**
     * @return <code>true</code> if allows execute
     */
    public boolean isAllowExecute() {
        Object value = tuples.get(ALLOW_EXECUTE_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : Permission.DEFAULT_ALLOW_EXECUTE;
    }

    /**
     * @return <code>true</code> if allows language
     */
    public boolean isAllowLanguage() {
        Object value = tuples.get(ALLOW_LANGUAGE_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : Permission.DEFAULT_ALLOW_LANGUAGE;
    }

    /**
     * @return <code>true</code> if allows read
     */
    public boolean isAllowRead() {
        Object value = tuples.get(ALLOW_READ_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : Permission.DEFAULT_ALLOW_READ;
    }

    /**
     * @return <code>true</code> if allows update
     */
    public boolean isAllowUpdate() {
        Object value = tuples.get(ALLOW_UPDATE_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : Permission.DEFAULT_ALLOW_UPDATE;
    }

    /**
     * @param newAllowAlter
     *        <code>true</code> if allows alter
     */
    public void setAllowAlter( final boolean newAllowAlter ) {
        tuples.put(ALLOW_ALTER_LABEL, newAllowAlter);
    }

    /**
     * @param newAllowCreate
     *        <code>true</code> if allows create
     */
    public void setAllowCreate( final boolean newAllowCreate ) {
        tuples.put(ALLOW_CREATE_LABEL, newAllowCreate);
    }

    /**
     * @param newAllowDelete
     *        <code>true</code> if allows delete
     */
    public void setAllowDelete( final boolean newAllowDelete ) {
        tuples.put(ALLOW_DELETE_LABEL, newAllowDelete);
    }

    /**
     * @param newAllowExecute
     *        <code>true</code> if allows execute
     */
    public void setAllowExecute( final boolean newAllowExecute ) {
        tuples.put(ALLOW_EXECUTE_LABEL, newAllowExecute);
    }

    /**
     * @param newAllowLanguage
     *        <code>true</code> if allows language
     */
    public void setAllowLanguage( final boolean newAllowLanguage ) {
        tuples.put(ALLOW_LANGUAGE_LABEL, newAllowLanguage);
    }

    /**
     * @param newAllowRead
     *        <code>true</code> if allows read
     */
    public void setAllowRead( final boolean newAllowRead ) {
        tuples.put(ALLOW_READ_LABEL, newAllowRead);
    }

    /**
     * @param newAllowUpdate
     *        <code>true</code> if allows update
     */
    public void setAllowUpdate( final boolean newAllowUpdate ) {
        tuples.put(ALLOW_UPDATE_LABEL, newAllowUpdate);
    }

    /**
     * @param newName
     *        the new translator name (can be empty)
     */
    public void setName( final String newName ) {
        tuples.put(NAME_LABEL, newName);
    }
}

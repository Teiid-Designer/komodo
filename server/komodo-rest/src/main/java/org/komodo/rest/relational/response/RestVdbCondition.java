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
import org.komodo.relational.vdb.Condition;
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
 * A condition that can be used by GSON to build a JSON document representation.
 */
public final class RestVdbCondition extends RestBasicEntity {

    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = KomodoService.protectPrefix(VdbLexicon.DataRole.Permission.Condition.CONDITION);

    /**
     * Label used to describe constraint
     */
    public static final String CONSTRAINT_LABEL = KomodoService.protectPrefix(VdbLexicon.DataRole.Permission.Condition.CONSTRAINT);

    /**
     * An empty array of conditions.
     */
    public static final RestVdbCondition[] NO_CONDITIONS = new RestVdbCondition[ 0 ];

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbCondition() {
        setConstraint(Condition.DEFAULT_CONSTRAINT);
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param condition the condition
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestVdbCondition(URI baseUri, Condition condition, UnitOfWork uow) throws KException {
        super(baseUri, condition, uow, false);

        setName(condition.getName(uow));
        setConstraint(condition.isConstraint(uow));

        Permission permission = ancestor(condition, Permission.class, uow);
        ArgCheck.isNotNull(permission);
        String permName = permission.getName(uow);

        DataRole dataRole = ancestor(permission, DataRole.class, uow);
        ArgCheck.isNotNull(dataRole);
        String dataRoleName = dataRole.getName(uow);

        Vdb vdb = ancestor(dataRole, Vdb.class, uow);
        ArgCheck.isNotNull(vdb);
        String vdbName = vdb.getName(uow);

        Properties settings = getUriBuilder().createSettings(SettingNames.VDB_NAME, vdbName);
        getUriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, getUriBuilder().vdbParentUri(vdb, uow));

        getUriBuilder().addSetting(settings, SettingNames.DATA_ROLE_ID, dataRoleName);
        getUriBuilder().addSetting(settings, SettingNames.PERMISSION_ID, permName);
        getUriBuilder().addSetting(settings, SettingNames.PERMISSION_CHILD_TYPE, LinkType.CONDITIONS.uriName());
        getUriBuilder().addSetting(settings, SettingNames.PERMISSION_CHILD_ID, getId());

        addLink(new RestLink(LinkType.SELF, getUriBuilder()
                             .vdbPermissionChildUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder()
                             .vdbPermissionChildUri(LinkType.PARENT, settings)));
        createChildLink();
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        Object name = tuples.get(NAME_LABEL);
        return name != null ? name.toString() : null;
    }

    /**
     * @param newName
     *        the new translator name (can be empty)
     */
    public void setName( final String newName ) {
        tuples.put(NAME_LABEL, newName);
    }

    /**
     * @return the constraint
     */
    public boolean isConstraint() {
        Object constraint = tuples.get(CONSTRAINT_LABEL);
        return constraint != null ? Boolean.parseBoolean(constraint.toString()) : Condition.DEFAULT_CONSTRAINT;
    }

    /**
     * @param constraint the constraint to set
     */
    public void setConstraint(boolean constraint) {
        tuples.put(CONSTRAINT_LABEL, constraint);
    }
}

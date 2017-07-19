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
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.template.Template;
import org.komodo.relational.template.TemplateEntry;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.connection.RestTemplate;
import org.komodo.rest.relational.connection.RestTemplateEntry;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.response.RestTeiid;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbCondition;
import org.komodo.rest.relational.response.RestVdbDataRole;
import org.komodo.rest.relational.response.RestVdbImport;
import org.komodo.rest.relational.response.RestVdbMask;
import org.komodo.rest.relational.response.RestVdbModel;
import org.komodo.rest.relational.response.RestVdbModelSource;
import org.komodo.rest.relational.response.RestVdbModelTable;
import org.komodo.rest.relational.response.RestVdbModelTableColumn;
import org.komodo.rest.relational.response.RestVdbPermission;
import org.komodo.rest.relational.response.RestVdbTranslator;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.KLog;

/**
 *
 */
public class RestEntityFactory implements V1Constants {

    private static final KLog LOGGER = KLog.getLogger();

    /**
     * @param kObject the object
     * @param baseUri the base uri
     * @param uow the transaction
     * @param properties extra properties
     * @return the rest object for the given kObject
     * @throws KException if error occurs
     */
    @SuppressWarnings( "unchecked" )
    public <T extends RestBasicEntity> T create(KomodoObject kObject, URI baseUri,
                                                                 UnitOfWork uow, KomodoProperties properties) throws KException {
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(kObject.getRepository(), uow);
        KomodoType kType = kObject.getTypeIdentifier(uow);

        switch (kType) {
            case VDB:
                Vdb vdb = wsMgr.resolve(uow, kObject, Vdb.class);
                Boolean exportXml = properties.getProperty(VDB_EXPORT_XML_PROPERTY, Boolean.FALSE);
                return (T) new RestVdb(baseUri, vdb, exportXml, uow);
            case VDB_CONDITION:
                Condition condition = wsMgr.resolve(uow, kObject, Condition.class);
                return (T) new RestVdbCondition(baseUri, condition, uow);
            case VDB_DATA_ROLE:
                DataRole dataRole = ( kObject instanceof DataRole ) ? ( DataRole )kObject : wsMgr.resolve(uow, kObject, DataRole.class);
                return (T) new RestVdbDataRole(baseUri, dataRole, uow);
            case VDB_IMPORT:
                VdbImport vdbImport = wsMgr.resolve(uow, kObject, VdbImport.class);
                return (T) new RestVdbImport(baseUri, vdbImport, uow);
            case VDB_MASK:
                Mask mask = wsMgr.resolve(uow, kObject, Mask.class);
                return (T) new RestVdbMask(baseUri, mask, uow);
            case MODEL:
                Model model = wsMgr.resolve(uow, kObject, Model.class);
                return (T) new RestVdbModel(baseUri, model, uow);
            case VDB_MODEL_SOURCE:
                ModelSource source = wsMgr.resolve(uow, kObject, ModelSource.class);
                return (T) new RestVdbModelSource(baseUri, source, uow);
            case TABLE:
                Table table = wsMgr.resolve(uow, kObject, Table.class);
                return (T) new RestVdbModelTable(baseUri, table, uow);
            case COLUMN:
                Column column = wsMgr.resolve(uow, kObject, Column.class);
                return (T) new RestVdbModelTableColumn(baseUri, column, uow);
            case VDB_PERMISSION:
                Permission permission = wsMgr.resolve(uow, kObject, Permission.class);
                return (T) new RestVdbPermission(baseUri, permission, uow);
            case VDB_TRANSLATOR:
                Translator translator = wsMgr.resolve(uow, kObject, Translator.class);
                return (T) new RestVdbTranslator(baseUri, translator, uow);
            case TEIID:
                Teiid teiid = wsMgr.resolve(uow, kObject, Teiid.class);
                return (T) new RestTeiid(baseUri, teiid, uow);
            case CONNECTION:
                Connection connection = wsMgr.resolve(uow, kObject, Connection.class);
                return (T) new RestConnection(baseUri, connection, uow);
            case TEMPLATE:
                Template template = wsMgr.resolve(uow, kObject, Template.class);
                return (T) new RestTemplate(baseUri, template, uow);
            case TEMPLATE_ENTRY:
                TemplateEntry entry = wsMgr.resolve(uow, kObject, TemplateEntry.class);
                return (T) new RestTemplateEntry(baseUri, entry, uow);
            case DATASERVICE:
                Dataservice dataService = wsMgr.resolve(uow, kObject, Dataservice.class);
                return (T) new RestDataservice(baseUri, dataService, false, uow);
            case UNKNOWN:
                return null;
            default:
                return (T) new RestBasicEntity(baseUri, kObject, uow);
        }
    }

    /**
     * @param kObject the object
     * @param baseUri the base uri
     * @param uow the transaction
     * @return the rest object for the given kObject
     * @throws KException if error occurs
     */
    public <T extends RestBasicEntity> T create(KomodoObject kObject, URI baseUri, UnitOfWork uow) throws KException {
        return create(kObject, baseUri, uow, new KomodoProperties());
    }

    /**
     * @param basicEntity the entity to be resolved
     * @param klazz the class to resolve it to
     * @return the resolved class or null
     */
    public static <T extends RestBasicEntity> T resolve(RestBasicEntity basicEntity, Class<T> klazz) {
        try {
            T instance = klazz.newInstance();
            basicEntity.clone(instance);

            return instance;
        } catch (Exception ex) {
            LOGGER.error("Failure to resolve entity", ex); //$NON-NLS-1$
            return null;
        }
    }
}

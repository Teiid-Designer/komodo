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
import org.komodo.relational.model.Model;
import org.komodo.relational.teiid.Teiid;
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
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(kObject.getRepository());
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
                DataRole dataRole = wsMgr.resolve(uow, kObject, DataRole.class);
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
            case VDB_PERMISSION:
                Permission permission = wsMgr.resolve(uow, kObject, Permission.class);
                return (T) new RestVdbPermission(baseUri, permission, uow);
            case VDB_TRANSLATOR:
                Translator translator = wsMgr.resolve(uow, kObject, Translator.class);
                return (T) new RestVdbTranslator(baseUri, translator, uow);
            case TEIID:
                Teiid teiid = wsMgr.resolve(uow, kObject, Teiid.class);
                return (T) new RestTeiid(baseUri, teiid, uow);
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

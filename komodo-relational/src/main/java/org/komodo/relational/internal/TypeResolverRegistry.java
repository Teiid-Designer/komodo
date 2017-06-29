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
package org.komodo.relational.internal;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.folder.Folder;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.relational.model.View;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.relational.model.internal.AccessPatternImpl;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.template.Template;
import org.komodo.relational.template.TemplateEntry;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.utils.KeyInValueHashMap;
import org.komodo.spi.utils.KeyInValueHashMap.KeyFromValueAdapter;

/**
 *
 */
public class TypeResolverRegistry {

    private class KTypeAdapter implements KeyFromValueAdapter<KomodoType, TypeResolver<?>> {
        @Override
        public KomodoType getKey(TypeResolver<?> value) {
            return value.identifier();
        }
    }

    private static TypeResolverRegistry instance;

    /**
     * @return singleton instance
     */
    public static TypeResolverRegistry getInstance() {
        if (instance == null)
            instance = new TypeResolverRegistry();

        return instance;
    }

    private KeyInValueHashMap<KomodoType, TypeResolver<?>> kTypeIndex =
                    new KeyInValueHashMap<KomodoType, TypeResolver<?>>(new KTypeAdapter());

    private Map<Class<? extends KomodoObject>, TypeResolver<?>> kClassIndex =
                    new HashMap<Class<? extends KomodoObject>, TypeResolver<?>>();

    private TypeResolverRegistry() {

        index(KomodoType.ACCESS_PATTERN, AccessPattern.RESOLVER);

        index(KomodoType.COLUMN, Column.RESOLVER);

        index(KomodoType.DATASERVICE, Dataservice.RESOLVER);

        index(KomodoType.CONNECTION, Connection.RESOLVER);

        index(KomodoType.DATA_TYPE_RESULT_SET, DataTypeResultSet.RESOLVER);

        index(KomodoType.DRIVER, Driver.RESOLVER);

        index(KomodoType.FOLDER, Folder.RESOLVER);

        index(KomodoType.FOREIGN_KEY, ForeignKey.RESOLVER);

        index(KomodoType.INDEX, Index.RESOLVER);

        index(KomodoType.MODEL, Model.RESOLVER);

        index(KomodoType.PARAMETER, Parameter.RESOLVER);

        index(KomodoType.PRIMARY_KEY, PrimaryKey.RESOLVER);

        index(KomodoType.PUSHDOWN_FUNCTION, PushdownFunction.RESOLVER);

        index(KomodoType.SCHEMA, Schema.RESOLVER);

        index(KomodoType.STATEMENT_OPTION, StatementOption.RESOLVER);

        index(KomodoType.STORED_PROCEDURE, StoredProcedure.RESOLVER);

        index(KomodoType.TABLE, Table.RESOLVER);

        index(KomodoType.TABULAR_RESULT_SET, TabularResultSet.RESOLVER);

        index(KomodoType.CACHED_TEIID, CachedTeiid.RESOLVER);

        index(KomodoType.TEMPLATE, Template.RESOLVER);

        index(KomodoType.TEMPLATE_ENTRY, TemplateEntry.RESOLVER);

        index(KomodoType.RESULT_SET_COLUMN, ResultSetColumn.RESOLVER);

        index(KomodoType.TEIID, Teiid.RESOLVER);

        index(KomodoType.UNIQUE_CONSTRAINT, UniqueConstraint.RESOLVER);

        index(KomodoType.USER_DEFINED_FUNCTION, UserDefinedFunction.RESOLVER);

        index(KomodoType.VIRTUAL_PROCEDURE, VirtualProcedure.RESOLVER);

        index(KomodoType.VDB, Vdb.RESOLVER);

        index(KomodoType.VDB_CONDITION, Condition.RESOLVER);

        index(KomodoType.VDB_DATA_ROLE, DataRole.RESOLVER);

        index(KomodoType.VDB_ENTRY, Entry.RESOLVER);

        index(KomodoType.VDB_IMPORT, VdbImport.RESOLVER);

        index(KomodoType.VDB_MASK, Mask.RESOLVER);

        index(KomodoType.VDB_MODEL_SOURCE, ModelSource.RESOLVER);

        index(KomodoType.VDB_PERMISSION, Permission.RESOLVER);

        index(KomodoType.VDB_TRANSLATOR, Translator.RESOLVER);

        index(KomodoType.VIEW, View.RESOLVER);
    }

    @SuppressWarnings( "unchecked" )
    private void index(KomodoType kType, TypeResolver<?> resolver) {
        kTypeIndex.add(resolver);

        // Indexes the impl class
        Class<? extends KomodoObject> owningClass = resolver.owningClass();
        kClassIndex.put(owningClass, resolver);

        // Indexes the interface class
        Class<?>[] interfaces = owningClass.getInterfaces();
        if (interfaces.length > 0) {
            for (Class<?> iface : interfaces) {
                if (KomodoObject.class.isAssignableFrom(iface))
                    kClassIndex.put((Class<? extends KomodoObject>) iface, resolver);
            }
        }
    }

    /**
     * @return all registered resolvers
     */
    public Collection<TypeResolver<?>> getResolvers() {
      return Collections.unmodifiableCollection(kClassIndex.values());
    }

    /**
     * @param kType the komodo type
     * @return the {@link TypeResolver} for the given komodo type
     */
    public TypeResolver<?> getResolver(KomodoType kType) {
        if (kType == null || KomodoType.UNKNOWN.equals(kType))
            return null;

        return kTypeIndex.get(kType);
    }

    /**
     * @param kClass the resolver owning class or its interface, eg. {@link AccessPatternImpl} or {@link AccessPattern}
     * @return the {@link TypeResolver} for the given komodo class
     */
    public TypeResolver<?> getResolver(Class<? extends KomodoObject> kClass) {
        TypeResolver<?> resolver = kClassIndex.get(kClass);
         return resolver;
    }
}

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
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.internal.AccessPatternImpl;
import org.komodo.relational.model.internal.ColumnImpl;
import org.komodo.relational.model.internal.DataTypeResultSetImpl;
import org.komodo.relational.model.internal.ForeignKeyImpl;
import org.komodo.relational.model.internal.IndexImpl;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.model.internal.ParameterImpl;
import org.komodo.relational.model.internal.PrimaryKeyImpl;
import org.komodo.relational.model.internal.PushdownFunctionImpl;
import org.komodo.relational.model.internal.ResultSetColumnImpl;
import org.komodo.relational.model.internal.SchemaImpl;
import org.komodo.relational.model.internal.StatementOptionImpl;
import org.komodo.relational.model.internal.StoredProcedureImpl;
import org.komodo.relational.model.internal.TableImpl;
import org.komodo.relational.model.internal.TabularResultSetImpl;
import org.komodo.relational.model.internal.UniqueConstraintImpl;
import org.komodo.relational.model.internal.UserDefinedFunctionImpl;
import org.komodo.relational.model.internal.ViewImpl;
import org.komodo.relational.model.internal.VirtualProcedureImpl;
import org.komodo.relational.teiid.internal.TeiidImpl;
import org.komodo.relational.vdb.internal.ConditionImpl;
import org.komodo.relational.vdb.internal.DataRoleImpl;
import org.komodo.relational.vdb.internal.EntryImpl;
import org.komodo.relational.vdb.internal.MaskImpl;
import org.komodo.relational.vdb.internal.ModelSourceImpl;
import org.komodo.relational.vdb.internal.PermissionImpl;
import org.komodo.relational.vdb.internal.TranslatorImpl;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.relational.vdb.internal.VdbImportImpl;
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

        index(KomodoType.ACCESS_PATTERN, AccessPatternImpl.RESOLVER);

        index(KomodoType.COLUMN, ColumnImpl.RESOLVER);

        index(KomodoType.DATA_TYPE_RESULT_SET, DataTypeResultSetImpl.RESOLVER);

        index(KomodoType.FOREIGN_KEY, ForeignKeyImpl.RESOLVER);

        index(KomodoType.INDEX, IndexImpl.RESOLVER);

        index(KomodoType.MODEL, ModelImpl.RESOLVER);

        index(KomodoType.PARAMETER, ParameterImpl.RESOLVER);

        index(KomodoType.PRIMARY_KEY, PrimaryKeyImpl.RESOLVER);

        index(KomodoType.PUSHDOWN_FUNCTION, PushdownFunctionImpl.RESOLVER);

        index(KomodoType.SCHEMA, SchemaImpl.RESOLVER);

        index(KomodoType.STATEMENT_OPTION, StatementOptionImpl.RESOLVER);

        index(KomodoType.STORED_PROCEDURE, StoredProcedureImpl.RESOLVER);

        index(KomodoType.TABLE, TableImpl.RESOLVER);

        index(KomodoType.TABULAR_RESULT_SET, TabularResultSetImpl.RESOLVER);

        index(KomodoType.RESULT_SET_COLUMN, ResultSetColumnImpl.RESOLVER);

        index(KomodoType.TEIID, TeiidImpl.RESOLVER);

        index(KomodoType.UNIQUE_CONSTRAINT, UniqueConstraintImpl.RESOLVER);

        index(KomodoType.USER_DEFINED_FUNCTION, UserDefinedFunctionImpl.RESOLVER);

        index(KomodoType.VIRTUAL_PROCEDURE, VirtualProcedureImpl.RESOLVER);

        index(KomodoType.VDB, VdbImpl.RESOLVER);

        index(KomodoType.VDB_CONDITION, ConditionImpl.RESOLVER);

        index(KomodoType.VDB_DATA_ROLE, DataRoleImpl.RESOLVER);

        index(KomodoType.VDB_ENTRY, EntryImpl.RESOLVER);

        index(KomodoType.VDB_IMPORT, VdbImportImpl.RESOLVER);

        index(KomodoType.VDB_MASK, MaskImpl.RESOLVER);

        index(KomodoType.VDB_MODEL_SOURCE, ModelSourceImpl.RESOLVER);

        index(KomodoType.VDB_PERMISSION, PermissionImpl.RESOLVER);

        index(KomodoType.VDB_TRANSLATOR, TranslatorImpl.RESOLVER);

        index(KomodoType.VIEW, ViewImpl.RESOLVER);
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

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
package org.komodo.repository;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.komodo.core.KomodoLexicon;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.utils.KeyInValueHashMap;
import org.komodo.spi.utils.KeyInValueHashMap.KeyFromValueAdapter;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 *
 */
public class KomodoTypeRegistry implements StringConstants {

    /**
     * Identifier representation of a komodo relational type
     */
    public static class TypeIdentifier {

        private final KomodoType kType;

        private final String lexiconType;

        /**
         * Construct new instance
         *
         * @param kType the komodo type
         * @param lexiconType the node id
         */
        public TypeIdentifier(KomodoType kType, String lexiconType) {
            ArgCheck.isNotNull(kType);
            ArgCheck.isNotNull(lexiconType);

            this.kType = kType;
            this.lexiconType = lexiconType;
        }

        /**
         * @return the komodo type
         */
        public KomodoType getKomodoType() {
            return kType;
        }

        /**
         * @return the komodo type id
         */
        public String getKomodoTypeId() {
            return kType.getType();
        }

        /**
         * @return the lexiconType
         */
        public String getLexiconType() {
            return this.lexiconType;
        }

        @Override
        public String toString() {
            return "TypeIdentifier [kType=" + this.kType + ", lexiconType=" + this.lexiconType + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
    }

    private class KTypeAdapter implements KeyFromValueAdapter<KomodoType, TypeIdentifier> {
        @Override
        public KomodoType getKey(TypeIdentifier value) {
            return value.getKomodoType();
        }
    }

    private static KomodoTypeRegistry instance;

    /**
     * @return singleton instance
     */
    public static KomodoTypeRegistry getInstance() {
        if (instance == null)
            instance = new KomodoTypeRegistry();

        return instance;
    }

    private KeyInValueHashMap<KomodoType, TypeIdentifier> kTypeIndex =
                    new KeyInValueHashMap<>(new KTypeAdapter());

    private KomodoTypeRegistry() {

        index(KomodoType.ACCESS_PATTERN, TeiidDdlLexicon.Constraint.TABLE_ELEMENT);

        index(KomodoType.COLUMN, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        index(KomodoType.DATA_TYPE_RESULT_SET, TeiidDdlLexicon.CreateProcedure.RESULT_DATA_TYPE);

        index(KomodoType.DATASERVICE, DataVirtLexicon.DataService.NODE_TYPE);

        index(KomodoType.CONNECTION, DataVirtLexicon.Connection.NODE_TYPE);

        index(KomodoType.DRIVER, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE);
        index(KomodoType.DRIVER_ENTRY, DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE);

        index(KomodoType.UDF_FILE, DataVirtLexicon.ResourceFile.UDF_FILE_NODE_TYPE);
        index(KomodoType.UDF_ENTRY, DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE);

        index(KomodoType.DDL_FILE, DataVirtLexicon.ResourceFile.DDL_FILE_NODE_TYPE);
        index(KomodoType.DDL_FILE, DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE);

        index(KomodoType.RESOURCE, DataVirtLexicon.ResourceFile.NODE_TYPE);
        index(KomodoType.RESOURCE_ENTRY, DataVirtLexicon.ResourceEntry.NODE_TYPE);

        index(KomodoType.FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        index(KomodoType.FOREIGN_KEY, TeiidDdlLexicon.Constraint.FOREIGN_KEY_CONSTRAINT);

        index(KomodoType.INDEX, TeiidDdlLexicon.Constraint.INDEX_CONSTRAINT);

        index(KomodoType.MODEL, VdbLexicon.Vdb.DECLARATIVE_MODEL);

        index(KomodoType.PARAMETER, TeiidDdlLexicon.CreateProcedure.PARAMETER);

        index(KomodoType.PRIMARY_KEY, TeiidDdlLexicon.Constraint.TABLE_ELEMENT);

        index(KomodoType.PUSHDOWN_FUNCTION, TeiidDdlLexicon.CreateProcedure.FUNCTION_STATEMENT);

        index(KomodoType.RESULT_SET_COLUMN, TeiidDdlLexicon.CreateProcedure.RESULT_COLUMN);

        index(KomodoType.SCHEMA, KomodoLexicon.Schema.NODE_TYPE);

        index(KomodoType.STATEMENT_OPTION, StandardDdlLexicon.TYPE_STATEMENT_OPTION);

        index(KomodoType.STORED_PROCEDURE, TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT);

        index(KomodoType.TABLE, TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);

        index(KomodoType.TABULAR_RESULT_SET, TeiidDdlLexicon.CreateProcedure.RESULT_COLUMNS);

        index(KomodoType.TEIID, KomodoLexicon.Teiid.NODE_TYPE);

        index(KomodoType.TEMPLATE, DataVirtLexicon.Template.NODE_TYPE);

        index(KomodoType.TEMPLATE_ENTRY, DataVirtLexicon.TemplateEntry.NODE_TYPE);

        index(KomodoType.CACHED_TEIID, KomodoLexicon.CachedTeiid.NODE_TYPE);

        index(KomodoType.UNIQUE_CONSTRAINT, TeiidDdlLexicon.Constraint.TABLE_ELEMENT);

        index(KomodoType.USER_DEFINED_FUNCTION, TeiidDdlLexicon.CreateProcedure.FUNCTION_STATEMENT);

        index(KomodoType.VIRTUAL_PROCEDURE, TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT);

        index(KomodoType.VDB, VdbLexicon.Vdb.VIRTUAL_DATABASE);

        index(KomodoType.VDB_CONDITION, VdbLexicon.DataRole.Permission.Condition.CONDITION);

        index(KomodoType.VDB_DATA_ROLE, VdbLexicon.DataRole.DATA_ROLE);

        index(KomodoType.VDB_ENTRY, VdbLexicon.Entry.ENTRY);

        index(KomodoType.VDB_IMPORT, VdbLexicon.ImportVdb.IMPORT_VDB);

        index(KomodoType.VDB_MASK, VdbLexicon.DataRole.Permission.Mask.MASK);

        index(KomodoType.VDB_MODEL_SOURCE, VdbLexicon.Source.SOURCE);

        index(KomodoType.VDB_PERMISSION, VdbLexicon.DataRole.Permission.PERMISSION);

        index(KomodoType.VDB_TRANSLATOR, VdbLexicon.Translator.TRANSLATOR);

        index(KomodoType.VIEW, TeiidDdlLexicon.CreateTable.VIEW_STATEMENT);

        index(KomodoType.DDL_SCHEMA, TeiidDdlLexicon.Namespace.PREFIX);

        index(KomodoType.TSQL_SCHEMA, TeiidSqlLexicon.Namespace.PREFIX);

        index(KomodoType.VDB_SCHEMA, VdbLexicon.Namespace.PREFIX);
    }

    private void index(KomodoType kType, String lexiconType) {
        TypeIdentifier identifier = new TypeIdentifier(kType, lexiconType);
        kTypeIndex.add(identifier);
    }

    /**
     * @param kType the komodo type
     * @return the {@link TypeIdentifier} for the given komodo type
     */
    public TypeIdentifier getIdentifier(KomodoType kType) {
        return kTypeIndex.get(kType);
    }

    /**
     * @param lexiconType the lexicon identified type
     * @return all the type identifiers with the given lexicon type
     */
    public Set<TypeIdentifier> getIdentifiers(String lexiconType) {
        if (lexiconType == null)
            return Collections.emptySet();

        Set<TypeIdentifier> identifiers = new HashSet<>();
        for (TypeIdentifier identifier : kTypeIndex.values()) {
            if (identifier.getLexiconType().equals(lexiconType))
                identifiers.add(identifier);
        }

        if (! identifiers.isEmpty())
            return identifiers;

        //
        // We want to return TSQL for Teiid SQL nodes
        // but do not want to index all of them.
        //
        if (lexiconType.startsWith(TeiidSqlLexicon.Namespace.PREFIX)) {
            identifiers.add(kTypeIndex.get(KomodoType.TSQL_SCHEMA));
            return identifiers;
        }

        //
        // We want to return DDL for ddl nodes that do not have explicit types
        //
        if (lexiconType.startsWith(TeiidDdlLexicon.Namespace.PREFIX)) {
            identifiers.add(kTypeIndex.get(KomodoType.DDL_SCHEMA));
            return identifiers;
        }

        //
        // We want to return VDB for vdb nodes that do not have explicit types
        //
        if (lexiconType.startsWith(VdbLexicon.Namespace.PREFIX)) {
            identifiers.add(kTypeIndex.get(KomodoType.VDB_SCHEMA));
            return identifiers;
        }

        return identifiers;
    }
}

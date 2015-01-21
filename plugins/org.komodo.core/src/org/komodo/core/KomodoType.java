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
package org.komodo.core;

import java.util.ArrayList;
import java.util.List;
import org.komodo.core.KomodoLexicon.DataSource;
import org.komodo.core.KomodoLexicon.Namespace;
import org.komodo.core.KomodoLexicon.Repository;
import org.komodo.core.KomodoLexicon.Schema;
import org.komodo.core.KomodoLexicon.Teiid;
import org.komodo.core.KomodoLexicon.Vdb;
import org.komodo.core.KomodoLexicon.VdbEntry;
import org.komodo.core.KomodoLexicon.VdbImport;
import org.komodo.core.KomodoLexicon.VdbModel;
import org.komodo.core.KomodoLexicon.VdbModelSource;
import org.komodo.core.KomodoLexicon.VdbTranslator;
import org.komodo.spi.constants.StringConstants;

/**
 * Enum of all the Komodo types
 */
public enum KomodoType {

    DATA_SOURCE(DataSource.NODE_TYPE),

    REPOSITORY(Repository.NODE_TYPE),

    SCHEMA(Schema.NODE_TYPE),

    TEIID(Teiid.NODE_TYPE),

    VDB(Vdb.NODE_TYPE),

    VDB_ENTRY(VdbEntry.NODE_TYPE),

    VDB_IMPORT(VdbImport.NODE_TYPE),

    VDB_MODEL(VdbModel.NODE_TYPE),

    VDB_MODEL_SOURCE(VdbModelSource.NODE_TYPE),

    VDB_TRANSLATOR(VdbTranslator.NODE_TYPE);

    private String type;

    private KomodoType(String type) {
        this.type = type;
    }

    /**
     * @return actual type
     */
    public String getType() {
        return type;
    }

    @Override
    public String toString() {
        return type.replaceAll(Namespace.PREFIX + KomodoLexicon.COLON, StringConstants.EMPTY_STRING);
    }

    public static KomodoType getKType(String aType) {
        if (aType == null)
            return null;

        if (! aType.startsWith(Namespace.PREFIX + KomodoLexicon.COLON))
            aType = Namespace.PREFIX + KomodoLexicon.COLON + aType;

        for (KomodoType kType : values()) {
            if (kType.type.equalsIgnoreCase(aType))
                return kType;
        }

        return null;
    }

    public static List<String> getAllTypeNames() {
        List<String> names = new ArrayList<String>();
        for (KomodoType kType : values()) {
            names.add(kType.toString());
        }

        return names;
    }
}
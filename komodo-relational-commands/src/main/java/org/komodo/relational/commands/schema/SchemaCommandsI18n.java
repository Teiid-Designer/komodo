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
package org.komodo.relational.commands.schema;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.schema}.
 */
@SuppressWarnings( "javadoc" )
public final class SchemaCommandsI18n extends I18n {

    public static String exportExamples;
    public static String exportHelp;
    public static String exportUsage;

    public static String setSchemaPropertyExamples;
    public static String setSchemaPropertyHelp;
    public static String setSchemaPropertyUsage;

    public static String unsetSchemaPropertyExamples;
    public static String unsetSchemaPropertyHelp;
    public static String unsetSchemaPropertyUsage;

    public static String ddlExported;

    static {
        final SchemaCommandsI18n i18n = new SchemaCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private SchemaCommandsI18n() {
        // nothing to do
    }

}

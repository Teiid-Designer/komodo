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
package org.komodo.relational.commands;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands}.
 */
@SuppressWarnings( "javadoc" )
public final class RelationalCommandsI18n extends I18n {

    public static String findExamples;
    public static String findHelp;
    public static String findUsage;

    public static String setCustomPropertyHelp;
    public static String setCustomPropertyExamples;
    public static String setCustomPropertyUsage;

    public static String setCustomOptionHelp;
    public static String setCustomOptionExamples;
    public static String setCustomOptionUsage;

    public static String unsetCustomOptionHelp;
    public static String unsetCustomOptionExamples;
    public static String unsetCustomOptionUsage;

    public static String unsetCustomPropertyExamples;
    public static String unsetCustomPropertyHelp;
    public static String unsetCustomPropertyUsage;
    
    public static String cannotAddChildAlreadyExistsError;
    public static String cannotCreateChildAlreadyExistsError;
    public static String invalidType;
    public static String missingPropertyNameValue;
    public static String missingOptionNameValue;
    public static String missingTypeName;
    public static String noObjectsFound;
    public static String noObjectsFoundForPattern;
    public static String relationalCommandCategory;
    public static String setPropertySuccess;
    public static String setCustomOptionSuccess;
    public static String typeHeader;
    public static String typeHeaderForPattern;
    public static String unsetMissingPropertyName;
    public static String unsetMissingOptionName;
    public static String unsetPropertySuccess;
    public static String unsetCustomOptionSuccess;
    public static String useSetPropertyCommand;

    static {
        final RelationalCommandsI18n i18n = new RelationalCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private RelationalCommandsI18n() {
        // nothing to do
    }

}

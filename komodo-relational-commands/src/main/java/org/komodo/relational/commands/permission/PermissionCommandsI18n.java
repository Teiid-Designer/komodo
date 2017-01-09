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
package org.komodo.relational.commands.permission;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.permission}.
 */
@SuppressWarnings( "javadoc" )
public final class PermissionCommandsI18n extends I18n {

    public static String addConditionExamples;
    public static String addConditionHelp;
    public static String addConditionUsage;

    public static String addMaskExamples;
    public static String addMaskHelp;
    public static String addMaskUsage;

    public static String deleteConditionExamples;
    public static String deleteConditionHelp;
    public static String deleteConditionUsage;

    public static String deleteMaskHelp;
    public static String deleteMaskExamples;
    public static String deleteMaskUsage;

    public static String setPermissionPropertyExamples;
    public static String setPermissionPropertyHelp;
    public static String setPermissionPropertyUsage;

    public static String showConditionsExamples;
    public static String showConditionsHelp;
    public static String showConditionsUsage;

    public static String showMasksExamples;
    public static String showMasksHelp;
    public static String showMasksUsage;

    public static String unsetPermissionPropertyExamples;
    public static String unsetPermissionPropertyHelp;
    public static String unsetPermissionPropertyUsage;

    public static String conditionAdded;
    public static String conditionDeleted;
    public static String conditionsHeader;
    public static String maskAdded;
    public static String maskDeleted;
    public static String masksHeader;
    public static String matchedConditionsHeader;
    public static String matchedMasksHeader;
    public static String missingConditionName;
    public static String missingMaskName;
    public static String noConditions;
    public static String noMasks;
    public static String noMatchedConditions;
    public static String noMatchedMasks;

    static {
        final PermissionCommandsI18n i18n = new PermissionCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private PermissionCommandsI18n() {
        // nothing to do
    }

}

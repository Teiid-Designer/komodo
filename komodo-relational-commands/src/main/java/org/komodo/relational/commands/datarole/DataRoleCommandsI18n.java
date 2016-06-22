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
package org.komodo.relational.commands.datarole;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.datarole} package.
 */
@SuppressWarnings( "javadoc" )
public final class DataRoleCommandsI18n extends I18n {

    public static String addMappedRoleUsage;
    public static String addMappedRoleHelp;
    public static String addMappedRoleExamples;

    public static String addPermissionUsage;
    public static String addPermissionHelp;
    public static String addPermissionExamples;

    public static String deleteMappedRoleUsage;
    public static String deleteMappedRoleHelp;
    public static String deleteMappedRoleExamples;
    public static String deleteMappedRoleNoRolesError;
    public static String deleteMappedRoleNoMatchingRoleError;

    public static String deletePermissionUsage;
    public static String deletePermissionHelp;
    public static String deletePermissionExamples;

    public static String setDataRolePropertyUsage;
    public static String setDataRolePropertyHelp;
    public static String setDataRolePropertyExamples;

    public static String showMappedRolesUsage;
    public static String showMappedRolesHelp;
    public static String showMappedRolesExamples;

    public static String showPermissionsUsage;
    public static String showPermissionsHelp;
    public static String showPermissionsExamples;

    public static String unsetDataRolePropertyExamples;
    public static String unsetDataRolePropertyUsage;
    public static String unsetDataRolePropertyHelp;

    public static String mappedRoleAdded;
    public static String mappedRoleDeleted;
    public static String mappedRolesHeader;
    public static String matchingMappedRolesHeader;
    public static String matchingPermissionsHeader;
    public static String missingMappedRoleName;
    public static String missingPermissionName;
    public static String noMappedRoles;
    public static String noMatchingMappedRoles;
    public static String noMatchingPermissions;
    public static String noPermissions;
    public static String permissionAdded;
    public static String permissionDeleted;
    public static String permissionsHeader;

    static {
        final DataRoleCommandsI18n i18n = new DataRoleCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private DataRoleCommandsI18n() {
        // nothing to do
    }

}

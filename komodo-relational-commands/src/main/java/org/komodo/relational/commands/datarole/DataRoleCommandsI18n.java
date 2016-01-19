/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
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

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
package org.komodo.relational.commands.model;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.model}.
 */
@SuppressWarnings( "javadoc" )
public final class ModelCommandsI18n extends I18n {

    public static String addPushdownFunctionExamples;
    public static String addPushdownFunctionHelp;
    public static String addPushdownFunctionUsage;

    public static String addSourceExamples;
    public static String addSourceHelp;
    public static String addSourceUsage;

    public static String addStoredProcedureExamples;
    public static String addStoredProcedureHelp;
    public static String addStoredProcedureUsage;

    public static String addTableExamples;
    public static String addTableHelp;
    public static String addTableUsage;

    public static String addUserDefinedFunctionExamples;
    public static String addUserDefinedFunctionHelp;
    public static String addUserDefinedFunctionUsage;

    public static String addViewExamples;
    public static String addViewHelp;
    public static String addViewUsage;

    public static String addVirtualProcedureExamples;
    public static String addVirtualProcedureHelp;
    public static String addVirtualProcedureUsage;

    public static String deletePushdownFunctionExamples;
    public static String deletePushdownFunctionHelp;
    public static String deletePushdownFunctionUsage;

    public static String deleteSourceExamples;
    public static String deleteSourceHelp;
    public static String deleteSourceUsage;

    public static String deleteStoredProcedureExamples;
    public static String deleteStoredProcedureHelp;
    public static String deleteStoredProcedureUsage;

    public static String deleteTableExamples;
    public static String deleteTableHelp;
    public static String deleteTableUsage;

    public static String deleteUserDefinedFunctionExamples;
    public static String deleteUserDefinedFunctionHelp;
    public static String deleteUserDefinedFunctionUsage;

    public static String deleteViewExamples;
    public static String deleteViewHelp;
    public static String deleteViewUsage;

    public static String deleteVirtualProcedureExamples;
    public static String deleteVirtualProcedureHelp;
    public static String deleteVirtualProcedureUsage;

    public static String exportExamples;
    public static String exportHelp;
    public static String exportUsage;

    public static String importExamples;
    public static String importHelp;
    public static String importUsage;

    public static String setModelPropertyExamples;
    public static String setModelPropertyHelp;
    public static String setModelPropertyUsage;

    public static String showPushdownFunctionsExamples;
    public static String showPushdownFunctionsHelp;
    public static String showPushdownFunctionsUsage;

    public static String showSourcesExamples;
    public static String showSourcesHelp;
    public static String showSourcesUsage;

    public static String showStoredProceduresExamples;
    public static String showStoredProceduresHelp;
    public static String showStoredProceduresUsage;

    public static String showTablesExamples;
    public static String showTablesHelp;
    public static String showTablesUsage;

    public static String showUserDefinedFunctionsExamples;
    public static String showUserDefinedFunctionsHelp;
    public static String showUserDefinedFunctionsUsage;

    public static String showViewsExamples;
    public static String showViewsHelp;
    public static String showViewsUsage;

    public static String showVirtualProceduresExamples;
    public static String showVirtualProceduresHelp;
    public static String showVirtualProceduresUsage;

    public static String unsetModelPropertyExamples;
    public static String unsetModelPropertyHelp;
    public static String unsetModelPropertyUsage;

    public static String cannotImportWouldCreateDuplicate;
    public static String childTypeNotAllowed;
    public static String ddlExported;
    public static String ddlImported;
    public static String ddlImportInProgressMsg;
    public static String ddlImportSuccessMsg;
    public static String deleteTempContextFailedMsg;
    public static String errorCreatingTempNode;
    public static String importFailedMsg;
    public static String invalidDDLParentType;
    public static String invalidModelTypePropertyValue;
    public static String matchedPushdownFunctionsHeader;
    public static String matchedSourcesHeader;
    public static String matchedStoredProceduresHeader;
    public static String matchedTablesHeader;
    public static String matchedUserDefinedFunctionsHeader;
    public static String matchedViewsHeader;
    public static String matchedVirtualProceduresHeader;
    public static String missingPushdownFunctionName;
    public static String missingSourceName;
    public static String missingStoredProcedureName;
    public static String missingTableName;
    public static String missingUserDefinedFunctionName;
    public static String missingViewName;
    public static String missingVirtualProcedureName;
    public static String noMatchingPushdownFunctions;
    public static String noMatchingSources;
    public static String noMatchingStoredProcedures;
    public static String noMatchingTables;
    public static String noMatchingUserDefinedFunctions;
    public static String noMatchingViews;
    public static String noMatchingVirtualProcedures;
    public static String noPushdownFunctions;
    public static String noSources;
    public static String noStoredProcedures;
    public static String noTables;
    public static String noUserDefinedFunctions;
    public static String noViews;
    public static String noVirtualProcedures;
    public static String pushdownFunctionAdded;
    public static String pushdownFunctionDeleted;
    public static String pushdownsHeader;
    public static String sourceDeleted;
    public static String sourceAdded;
    public static String sourcesHeader;
    public static String storedProcedureAdded;
    public static String storedProcedureDeleted;
    public static String storedProceduresHeader;
    public static String tableAdded;
    public static String tableDeleted;
    public static String tablesHeader;
    public static String userDefinedFunctionAdded;
    public static String userDefinedFunctionDeleted;
    public static String userDefinedFunctionsHeader;
    public static String viewAdded;
    public static String viewDeleted;
    public static String viewsHeader;
    public static String virtualProcedureAdded;
    public static String virtualProcedureDeleted;
    public static String virtualProceduresHeader;

    static {
        final ModelCommandsI18n i18n = new ModelCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private ModelCommandsI18n() {
        // nothing to do
    }

}

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
package org.komodo.relational.commands.server;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.server}.
 */
@SuppressWarnings( "javadoc" )
public final class ServerCommandsI18n extends I18n {

    public static String serverConnectExamples;
    public static String serverConnectHelp;
    public static String serverConnectUsage;

    public static String serverDatasourceExamples;
    public static String serverDatasourceHelp;
    public static String serverDatasourceUsage;

    public static String serverDatasourcesExamples;
    public static String serverDatasourcesHelp;
    public static String serverDatasourcesUsage;

    public static String serverDatasourceTypeExamples;
    public static String serverDatasourceTypeHelp;
    public static String serverDatasourceTypeUsage;

    public static String serverDatasourceTypesExamples;
    public static String serverDatasourceTypesHelp;
    public static String serverDatasourceTypesUsage;

    public static String serverDeployDatasourceExamples;
    public static String serverDeployDatasourceHelp;
    public static String serverDeployDatasourceUsage;

    public static String serverDeployDriverExamples;
    public static String serverDeployDriverHelp;
    public static String serverDeployDriverUsage;

    public static String serverDeployVdbExamples;
    public static String serverDeployVdbHelp;
    public static String serverDeployVdbUsage;

    public static String serverDisconnectExamples;
    public static String serverDisconnectHelp;
    public static String serverDisconnectUsage;

    public static String serverGetDatasourceExamples;
    public static String serverGetDatasourceHelp;
    public static String serverGetDatasourceUsage;

    public static String serverGetVdbExamples;
    public static String serverGetVdbHelp;
    public static String serverGetVdbUsage;

    public static String serverShowPropertiesExamples;
    public static String serverShowPropertiesHelp;
    public static String serverShowPropertiesUsage;

    public static String serverTranslatorExamples;
    public static String serverTranslatorHelp;
    public static String serverTranslatorUsage;

    public static String serverTranslatorsExamples;
    public static String serverTranslatorsHelp;
    public static String serverTranslatorsUsage;

    public static String serverUndeployDatasourceExamples;
    public static String serverUndeployDatasourceHelp;
    public static String serverUndeployDatasourceUsage;

    public static String serverUndeployVdbExamples;
    public static String serverUndeployVdbHelp;
    public static String serverUndeployVdbUsage;

    public static String serverVdbExamples;
    public static String serverVdbHelp;
    public static String serverVdbUsage;

    public static String serverVdbsExamples;
    public static String serverVdbsHelp;
    public static String serverVdbsUsage;

    public static String setServerPropertyExamples;
    public static String setServerPropertyHelp;
    public static String setServerPropertyUsage;

    public static String unsetServerPropertyExamples;
    public static String unsetServerPropertyHelp;
    public static String unsetServerPropertyUsage;

    public static String attemptingToConnect;
    public static String attemptingToDisconnect;
    public static String canOnlyCopyDynamicVDBs;
    public static String commandCategory;
    public static String connected;
    public static String connectionError;
    public static String connectionErrorWillDisconnect;
    public static String currentServer;
    public static String datasourceCopyToRepoFinished;
    public static String datasourceDeployFinished;
    public static String datasourceDeploymentError;
    public static String datasourceDeploymentOverwriteDisabled;
    public static String datasourceDeploymentTypeNotFound;
    public static String datasourceOverwriteNotEnabled;
    public static String datasourcePropertiesError;
    public static String datasourceTypeDefaultValueLabel;
    public static String datasourceTypeNameLabel;
    public static String datasourceTypePropertiesHeader;
    public static String datasourceUnDeployFinished;
    public static String disconnectSuccessMsg;
    public static String driverDeploymentError;
    public static String driverDeployErrorServerHasMatch;
    public static String driverDeployFinished;
    public static String errorConnectingToServerOnStartup;
    public static String infoMessageDatasource;
    public static String infoMessageDatasources;
    public static String infoMessageDatasourceType;
    public static String infoMessageDatasourceTypes;
    public static String infoMessageTranslator;
    public static String infoMessageTranslators;
    public static String infoMessageVdb;
    public static String infoMessageVdbs;
    public static String missingVdbName;
    public static String missingDatasourceName;
    public static String missingDatasourceTypeName;
    public static String missingDriverNameForDeployment;
    public static String missingInputDriverFilePath;
    public static String missingTranslatorName;
    public static String notConnected;
    public static String noServerToDisconnectMsg;
    public static String noDatasourcesMsg;
    public static String noDatasourceTypesMsg;
    public static String noTranslatorsMsg;
    public static String noVdbsMsg;
    public static String overwriteArgInvalid;
    public static String repoDatasourceWithNameExists;
    public static String repoVdbWithNameExists;
    public static String serverDatasourceNotFound;
    public static String serverDatasourceTypeNotFound;
    public static String serverDisconnectError;
    public static String serverGetVdbError;
    public static String serverNotConnected;
    public static String serverStatusText;
    public static String serverTranslatorNotFound;
    public static String serverVdbNotFound;
    public static String teiidStatus;
    public static String vdbDeploymentError;
    public static String vdbExportFailed;
    public static String vdbDeployFailedMissingSourceJndi;
    public static String vdbDeployFinished;
    public static String vdbDeploymentOverwriteDisabled;
    public static String vdbCopyToRepoFinished;
    public static String vdbOverwriteNotEnabled;
    public static String vdbUnDeployFinished;
    public static String workspaceDatasourceNotFound;
    public static String workspaceVdbNotFound;

    static {
        final ServerCommandsI18n i18n = new ServerCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private ServerCommandsI18n() {
        // nothing to do
    }

}

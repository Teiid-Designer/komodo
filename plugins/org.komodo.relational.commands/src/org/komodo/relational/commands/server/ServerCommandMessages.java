/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import java.util.ResourceBundle;
import org.komodo.relational.vdb.DataRole;
import org.komodo.spi.constants.StringConstants;

/**
 * Localized messages for {@link DataRole}-related shell commands.
 */
public class ServerCommandMessages implements StringConstants {

    private static final String BUNDLE_NAME = ( ServerCommandMessages.class.getPackage().getName() + DOT + ServerCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    @SuppressWarnings( "javadoc" )
    public enum Common {
        CommandCategory,
        NoTeiidDefined,
        ServerNotConnected,
        Connected,
        NotConnected,
        serverStatusText,
        CurrentTeiid,
        MissingVdbName,
        MissingDatasourceName,
        MissingDatasourceTypeName,
        MissingTranslatorName,
        ServerDatasourceNotFound,
        ServerDatasourceTypeNotFound,
        ServerTranslatorNotFound,
        ServerVdbNotFound,
        WorkspaceVdbNotFound;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerConnectCommand {
        AttemptingToConnect,
        ConnectionError,
        TeiidStatus,
        Connected,
        NotConnected;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerDisconnectCommand {
        AttemptingToDisconnect,
        DisconnectSuccessMsg,
        NoServerToDisconnectMsg;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerSetCommand {
        ServerDoesNotExist,
        MissingServerNameArg,
        ServerSetSuccess;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerVdbsCommand {
        InfoMessage,
        ListHeader;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerTranslatorsCommand {
        InfoMessage,
        ListHeader;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerDatasourcesCommand {
        InfoMessage,
        ListHeader;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerDatasourceTypesCommand {
        InfoMessage,
        ListHeader;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerVdbCommand {
        InfoMessage;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerTranslatorCommand {
        InfoMessage;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerDatasourceCommand {
        InfoMessage;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerDatasourceTypeCommand {
        InfoMessage;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerDeployVdbCommand {
        VdbExportFailed,
        OverwriteArgInvalid,
        VdbDeploymentOverwriteDisabled,
        VdbDeployFinished;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerUndeployVdbCommand {
        VdbUnDeployFinished;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum ServerGetVdbCommand {
        VdbCopyToRepoFinished,
        CanOnlyCopyDynamicVDBs;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    private static String getEnumName(Enum<?> enumValue) {
        String className = enumValue.getClass().getName();
        String[] components = className.split("\\$"); //$NON-NLS-1$
        return components[components.length - 1];
    }

}
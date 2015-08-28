/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.shell;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import org.komodo.spi.constants.StringConstants;

/**
 * Localized messages for the Shell plugin.
 */
@SuppressWarnings( "javadoc" )
public class Messages implements StringConstants {

    private static final String BUNDLE_NAME = Messages.class.getPackage().getName()
    																					+ DOT
    																					+ Messages.class.getSimpleName().toLowerCase();

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

    public enum SHELL {
        ERROR_LOADING_PROPERTIES,
        INVALID_GLOBAL_PROPERTY_NAME,
        INVALID_BOOLEAN_GLOBAL_PROPERTY_VALUE,
        PROMPT,
        PROMPT_WITH_TYPE,
        CHILD_NAME_HEADER,
        CHILD_TYPE_HEADER,
        PATH_NOT_FOUND,
        PROPERTY_NAME_HEADER,
        PROPERTY_VALUE_HEADER,
        NO_PROPERTY_VALUE,
        TRANSACTION_COMMIT_ERROR,
        TRANSACTION_ROLLBACK_ERROR,
        TRANSACTION_TIMEOUT,
        COMPONENT_STARTED,
        COMPONENT_FAILED,
        ENGINE_STARTING,
        LOCAL_REPOSITORY_STARTING,
        LOCAL_REPOSITORY_TIMEOUT_ERROR,
    	COMMAND_NOT_FOUND,
    	GOOD_BYE,
    	Help_COMMAND_LIST_MSG,
    	Help_INVALID_COMMAND,
    	Help_USAGE,
    	Help_GET_HELP_1,
    	Help_GET_HELP_2,
    	EXITING,
    	INVALID_ARG,
    	USAGE,
    	SHUTTING_DOWN,
    	DONE,
    	InvalidArgMsg_EntryPath,
    	ENTRY_PATH,
    	ENTRY_LIST_SUMMARY,
    	ENTRY,
    	InvalidArgMsg_ArtifactId,
    	Property_InvalidArgMsg_SubCommand,
    	Property_InvalidArgMsg_PropertyName,
    	Property_InvalidArgMsg_PropertyValue,
    	Property_PropertySet,
    	Property_PropertyUnset,
    	Property_InvalidSubCommand,
    	Property_Failure,
    	NoPropertiesMsg,
    	PropertiesHeader,
    	noChildrenMsg,
    	ChildrenHeader,
    	PropertyHeader,
    	CommandFailure,
    	InvalidArgMsg_propertiesFile_not_exist,
    	InvalidArgMsg_property_not_correct_format,
    	InvalidArgMsg_propertiesFile_error_reading,
    	InvalidArgMsg_propertiesFile_error_reading_line,
    	InvalidArgMsg_PropertyName,
    	InvalidArgMsg_PropertyValue,
    	TOO_MANY_ARGS,
    	FileShellCommandReader_NoConsole,
        HelpNoAliases,
        HelpAliasesHeading,
        HelpUsageHeading,
        HelpDescription,
        HelpDescriptionHeading,
        HelpExamplesHeading,
        WelcomeMessage,
        locationArg_noContextWithThisName,
        propertyArg_noPropertyWithThisName,
        locationArg_empty,
        typeArg_childTypeNotAllowed,
        objectNameNotValid,
        FileNotFound,
        FileArgNotAFile,
        FileCannotRead,
        RecordingFileOutputError,
        RecordingFileCannotWrite,
        RecordingFileNotDefined;

    	@Override
    	public String toString() {
    		return getEnumName(this) + DOT + name();
    	}
    }

//    public enum CreateCommand {
//        DEFAULT_VDB_FILE_PATH,
//        FAILURE,
//        MISSING_ENTRY_PATH,
//        MISSING_FOREIGN_KEY_TABLE_REF,
//        MISSING_OBJ_NAME,
//        MISSING_OBJ_TYPE,
//        MISSING_OPTION_VALUE,
//        MISSING_PROPERTY_NAME,
//        MISSING_PROPERTY_VALUE,
//        PROPERTY_ALREADY_EXISTS,
//        MISSING_TRANSLATOR_TYPE,
//        NO_DUPLICATES_ALLOWED,
//        OBJECT_CREATED,
//        PROPERTY_CREATED,
//        PATH_NOT_FOUND,
//        TOO_MANY_ARGS,
//        TYPE_NOT_VALID;
//
//        @Override
//        public String toString() {
//            return getEnumName(this) + DOT + name();
//        }
//    }
//    
//    public enum DeleteCommand {
//        InvalidArgMsg_ObjectPath,
//        ObjectDeleted,
//        Failure,
//        cannotDelete_objectDoesNotExist,
//        contextMustBeBelowCurrent, 
//        cantDeleteReserved,
//        cannotRename_objectDoesNotExist;
//
//        @Override
//        public String toString() {
//            return getEnumName(this) + DOT + name();
//        }
//    }

    public enum StatusCommand {
        Separator,
        Connected,
        NotConnected,
        PingOk,
        PingFail;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }
    
    public enum PlayCommand {
        InvalidArgMsg_FileName,
        fileExecuted,
        Failure,
        CommandFailure;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum ListCommand {
        noChildrenMsg;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }
    
    public enum SetRecordCommand {
        onOffArg_empty,
        onOffArg_invalid,
        setRecordingStateMsg,
        recordingFileNotSet,
        recordingFileProblem;
        
      @Override
      public String toString() {
          return getEnumName(this) + DOT + name();
      }
    }

    public enum SetGlobalPropertyCommand {
        InvalidArgMsg_GlobalPropertyName,
        InvalidGlobalProperty,
        InvalidArgMsg_PropertyValue,
        GlobalPropertySet;
        
      @Override
      public String toString() {
          return getEnumName(this) + DOT + name();
      }
    }

    public enum SetPropertyCommand {
        PropertySet;
        
      @Override
      public String toString() {
          return getEnumName(this) + DOT + name();
      }
    }

    public enum UnsetPropertyCommand {
        InvalidArgMsg_GlobalPropertyName,
        InvalidGlobalProperty,
        InvalidArgMsg_PropertyValue,
        GlobalPropertySet;
        
      @Override
      public String toString() {
          return getEnumName(this) + DOT + name();
      }
    }

//    public enum SetCommand {
//
//        ADD_TABLE_CONSTRAINT_COLUMN_FAILED,
//        COLUMN_PATH_NOT_FOUND,
//        INVALID_TABLE_REF_COLUMN_PATH,
//        TABLE_COLUMNS_CANNOT_BE_SET,
//        TABLE_COLUMNS_SET,
//        TABLE_PATH_NOT_FOUND,
//        TABLE_REF_CANNOT_BE_SET,
//        TABLE_REF_COLUMNS_SET,
//        TABLE_REF_REFS_CANNOT_BE_SET,
//        TABLE_REF_SET,
//        TOO_MANY_ARGS,
//        UNSET_TABLE_CONSTRAINT_COLUMN_FAILED,
//        UNSET_TABLE_REF_COLUMN_FAILED,
//        InvalidArgMsg_SubCommand,
//        InvalidArgMsg_PropertyName,
//        InvalidArgMsg_PropertyValue,
//        InvalidArgMsg_GlobalPropertyName,
//        InvalidGlobalProperty,
//        PropertySet,
//        GlobalPropertySet,
//        resetGlobalProperties,
//        InvalidSubCommand,
//        Failure,
//        onOffArg_empty,
//        onOffArg_invalid,
//        setRecordingStateMsg,
//        recordingFileNotSet,
//        recordingFileNotWriteable,
//        invalidTeiidName;
//        @Override
//        public String toString() {
//            return getEnumName(this) + DOT + name();
//        }
//    }
//
//    public enum ShowCommand {
//        InvalidArgMsg_SubCommand,
//        InvalidArgMsg_PropertyName,
//        InvalidArgMsg_ServerObjName,
//        InvalidSubCommand,
//        Failure,
//        NoPropertiesMsg,
//        PropertiesHeader,
//        PropertyHeader,
//        ChildrenHeader,
//        CurrentRepoUrl,
//        CurrentRepoName,
//        NoCurrentTeiid,
//        CurrentTeiid,
//        CurrentTeiidJdbc,
//        CurrentContext,
//        GlobalPropertiesHeader,
//        noChildrenMsg,
//        serverStatusText,
//        ServerNotConnected,
//        ServerTypeHeader,
//        Connected,
//        NotConnected,
//        PingOk,
//        PingFail;
//        
//        @Override
//        public String toString() {
//            return getEnumName(this) + DOT + name();
//        }
//    }
    
    public enum ShowStatusCommand {
        CurrentRepoUrl,
        CurrentRepoName,
        NoCurrentTeiid,
        CurrentTeiid,
        CurrentContext;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }
    
    public enum ShowGlobalCommand {
        GlobalPropertiesHeader;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }
    
//    public enum ShowChildrenCommand {
//        noChildrenMsg,
//        ChildrenHeader,
//        Failure;
//        
//        @Override
//        public String toString() {
//            return getEnumName(this) + DOT + name();
//        }
//    }

    public enum ShowPropertyCommand {
        InvalidArgMsg_PropertyName;
        
        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }
    
    public enum ServerCommand {
        InvalidArgMsg_SubCommand,
        InvalidArgMsg_ServerObjType,
        InvalidArgMsg_ServerObjName,
        InvalidArgMsg_DeployServerObjType,
        InvalidArgMsg_DeployServerObjName,
        InvalidSubCommand,
        InvalidServerObjectType,
        InvalidServerDeployObjectType,
        Failure,
        NoTeiidDefined,
        PropertiesHeader,
        PropertyHeader,
        ServerNotConnected,
        ServerTypeHeader,
        ServerObjDetailsHeader,
        ObjectNameHeader,
        AttemptingToConnect,
        ConnectionError,
        TeiidStatus,
        Connected,
        NotConnected,
        AttemptingToDisconnect,
        DisconnectSuccessMsg,
        NoServerToDisconnectMsg,
        CouldNotResolve,
        ServerItemNotFound,
        CanOnlyCopyDynamicVDBs,
        VdbExportFailed,
        VdbDeployFinished,
        VdbUnDeployFinished,
        VdbCopyToRepoFinished,
        InvalidArgMsg_ServerName,
        noTeiidInstancesDefined,
        teiidSetOk,
        noTeiidWithName;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }
    
    public enum ImportCommand {
    	InvalidArgMsg_SubCommand,
    	InvalidArgMsg_FileName,
    	InvalidArgMsg_ModelName,
    	InvalidTargetPath,
    	DdlImportInProgressMsg,
    	VdbImportInProgressMsg,
    	DdlImportSuccessMsg,
    	VdbImportSuccessMsg,
    	InvalidSubCommand,
    	ImportFailedMsg,
    	childTypeNotAllowed,
    	InvalidDDLParentType,
    	ErrorCreatingTempNode,
    	DeleteTempContextFailedMsg,
    	cannotImport_wouldCreateDuplicate;

    	@Override
    	public String toString() {
    		return getEnumName(this) + DOT + name();
    	}
    }

    public enum ExportCommand {
    	InvalidArgMsgSubCommand,
    	InvalidArgMsgObjectName,
        InvalidArgMsgOutputFileName,
        Failure,
        ObjectExported,
        NoContentExported,
        ObjectNotAVdb,
        CannotExportObjectDoesNotExist,
        CannotExportObjectNotExportable,
        CannotExportFileAlreadyExists,
        CannotExportProblemWithVdb,
        CannotExportProblemWithModel,
        CannotExportProblemWithSchema,
        ExportOfTypeNotSupported;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }
    
    public enum FindCommand {
        helpTypesHeading,
        Failure,
        MissingTypeArg,
        InvalidType,
        NoObjectsFound,
        TypeHeader;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }
    
    /**
     * Localized messages of the {@link AddConstraintColumnCommand}.
     */
    public enum AddConstraintColumnCommand {

        columnRefAdded,
        columnPathNotFound,
        error,
        invalidColumnPath,
        invalidColumn,
        missingColumnPathArg;

        /**
         * {@inheritDoc}
         *
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return getEnumName( this ) + DOT + name();
        }
    }
    
    /**
     * Localized messages of the {@link RemoveConstraintColumnCommand}.
     */
    public enum RemoveConstraintColumnCommand {

        COLUMN_REF_REMOVED,
        COLUMN_PATH_NOT_FOUND,
        ERROR,
        INVALID_COLUMN_PATH,
        MISSING_COLUMN_PATH_ARG;

        /**
         * {@inheritDoc}
         *
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return getEnumName( this ) + DOT + name();
        }
    }
    
    /**
     * Localized messages of the {@link RenameCommand}.
     */
    public enum RenameCommand {
        InvalidArgMsg_ObjectName,
        InvalidArgMsg_NewName,
        ObjectRenamed,
        Failure,
        cannotRename_wouldCreateDuplicate,
        cannotRename_objectDoesNotExist,
        cannotRename_targetContextDoesNotExist;

        /**
         * {@inheritDoc}
         *
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return getEnumName( this ) + DOT + name();
        }
    }

    private static String getEnumName(Enum<?> enumValue) {
        String className = enumValue.getClass().getName();
        String[] components = className.split("\\$"); //$NON-NLS-1$
        return components[components.length - 1];
    }

    private Messages() {
    }

    /**
     * Get message string
     *
     * @param key
     *
     * @return i18n string
     */
    private static String getString(Enum<?> key) {
        try {
            return RESOURCE_BUNDLE.getString(key.toString());
        } catch (final Exception err) {
            String msg;

            if (err instanceof NullPointerException) {
                msg = "<No message available>"; //$NON-NLS-1$
            } else if (err instanceof MissingResourceException) {
                msg = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
                msg = err.getLocalizedMessage();
            }

            return msg;
        }
    }

    /**
     * Get message string with parameters
     *
     * @param key the enum key
     * @param parameters parameters
     *
     * @return i18n string
     */
    public static String getString(Enum<?> key, Object... parameters) {
        String text = getString(key);

        // Check the trivial cases ...
        if (text == null) {
            return OPEN_ANGLE_BRACKET + key.toString() + CLOSE_ANGLE_BRACKET;
        }
        if (parameters == null || parameters.length == 0) {
            return text;
        }

        return MessageFormat.format(text, parameters);
    }

    /**
     * Look up a message in the i18n resource message bundle by key, then format the
     * message with the given params and return the result.
     * @param key the message key
     * @param parameters the parameters
     * @return the message
     */
    public static String getString(String key, Object ... parameters) {
    	String text = null;
        try {
        	text = RESOURCE_BUNDLE.getString(key);
        } catch (final Exception err) {
            if (err instanceof NullPointerException) {
            	text = "<No message available>"; //$NON-NLS-1$
            } else if (err instanceof MissingResourceException) {
            	text = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
            	text = err.getLocalizedMessage();
            }
        }

        // Check the trivial cases ...
        if (text == null) {
            return OPEN_ANGLE_BRACKET + key + CLOSE_ANGLE_BRACKET;
        }
        if (parameters == null || parameters.length == 0) {
            return text;
        }

        return MessageFormat.format(text, parameters);
    }
}

/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.workspace;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.workspace}.
 */
@SuppressWarnings( "javadoc" )
public final class WorkspaceCommandsI18n extends I18n {

    public static String createSchemaExamples;
    public static String createSchemaHelp;
    public static String createSchemaUsage;

    public static String createTeiidExamples;
    public static String createTeiidHelp;
    public static String createTeiidUsage;

    public static String createVdbExamples;
    public static String createVdbHelp;
    public static String createVdbUsage;

    public static String deleteSchemaExamples;
    public static String deleteSchemaHelp;
    public static String deleteSchemaUsage;

    public static String deleteTeiidExamples;
    public static String deleteTeiidHelp;
    public static String deleteTeiidUsage;

    public static String deleteVdbExamples;
    public static String deleteVdbHelp;
    public static String deleteVdbUsage;

    public static String exportVdbExamples;
    public static String exportVdbHelp;
    public static String exportVdbUsage;

    public static String importVdbExamples;
    public static String importVdbHelp;
    public static String importVdbUsage;

    public static String uploadVdbExamples;
    public static String uploadVdbHelp;
    public static String uploadVdbUsage;

    public static String cannotImportWouldCreateDuplicate;
    public static String deleteSchemaError;
    public static String deleteTeiidError;
    public static String deleteVdbError;
    public static String deleteTempVdbFailedMsg;
    public static String errorDdlEmpty;
    public static String errorWritingFile;
    public static String fileExistsOverwriteDisabled;
    public static String importFailedMsg;
    public static String inputFileError;
    public static String invalidBooleanPropertyValue;
    public static String invalidIntegerPropertyValue;
    public static String invalidNullablePropertyValue;
    public static String invalidObjectType;
    public static String invalidPropertyName;
    public static String missingInputFileName;
    public static String missingInputVdbFilePath;
    public static String missingOutputFileName;
    public static String missingPropertyNameValue;
    public static String missingSchemaName;
    public static String missingTeiidName;
    public static String missingVdbName;
    public static String missingVdbNameForUpload;
    public static String outputFileError;
    public static String overwriteArgInvalid;
    public static String printRelationalObject;
    public static String schemaCreated;
    public static String schemaDeleted;
    public static String schemaNotFound;
    public static String setPropertySuccess;
    public static String teiidCreated;
    public static String teiidDeleted;
    public static String teiidNotFound;
    public static String unsetMissingPropertyName;
    public static String unsetPropertySuccess;
    public static String vdbCreated;
    public static String vdbDeleted;
    public static String vdbExported;
    public static String vdbInputFileIsEmpty;
    public static String vdbNotFound;
    public static String vdbOverwriteDisabled;
    public static String vdbUploaded;
    public static String vdbImportInProgressMsg;
    public static String vdbImportSuccessMsg;

    static {
        final WorkspaceCommandsI18n i18n = new WorkspaceCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    public WorkspaceCommandsI18n() {
        // nothing to do
    }

}

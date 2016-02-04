/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.workspace;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.datasource.internal.DatasourceParser;
import org.komodo.relational.datasource.internal.DatasourceValidationParser;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * Loads a {@link Datasource DS} from a local file.
 */
public final class UploadDatasourceCommand extends WorkspaceShellCommand {

    static final String NAME = "upload-datasource"; //$NON-NLS-1$

    private static final List< String > VALID_OVERWRITE_ARGS = Arrays.asList( new String[] { "-o", "--overwrite" } ); //$NON-NLS-1$ //$NON-NLS-2$;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public UploadDatasourceCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final String fileName = requiredArgument( 0, I18n.bind( WorkspaceCommandsI18n.missingInputDatasourceFilePath ) );
            final String overwriteArg = optionalArgument( 1, null );
            final boolean overwrite = !StringUtils.isBlank( overwriteArg );

            // make sure overwrite arg is valid
            if ( overwrite && !VALID_OVERWRITE_ARGS.contains( overwriteArg ) ) {
                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.overwriteArgInvalid, overwriteArg ), null );
            }

            { // Validates the supplied fileNameArg is a valid, readable file, and has property extension
                final String validationResult = validateReadableFileArg( fileName );

                if ( !CompletionConstants.OK.equals( validationResult ) ) {
                    return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.inputFileError, fileName, validationResult ), null );
                }
            }

            // read file
            final String content = new String( Files.readAllBytes( Paths.get( fileName ) ) );

            if ( StringUtils.isEmpty( content ) ) {
                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.datasourceInputFileIsEmpty, fileName ), null );
            }

            // Parser validates xml file and obtains dsNames
            DatasourceValidationParser datasourceValidationParser = new DatasourceValidationParser( );
            
            File dsXmlFile = new File(fileName);
            // Validates XML file is error-free.
            String[] dsNames = datasourceValidationParser.parse(getTransaction(), dsXmlFile);
            List<String> errors = datasourceValidationParser.getErrors();
            if(errors.size()>0) {
                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.datasourceParserErrors, fileName ), null );
            }

            // make sure we can overwrite if any datasources that already exist
            for( String dsName : dsNames ) {
                boolean hasSource = getWorkspaceManager().hasChild(getTransaction(), dsName, KomodoLexicon.DataSource.NODE_TYPE);
                if ( hasSource && !overwrite ) {
                    return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.datasourceOverwriteDisabled, fileName, dsName ), null );
                }
            }

            // Datasource parser creates the sources
            DatasourceParser datasourceParser = new DatasourceParser( getRepository(), overwrite );
            datasourceParser.parse(getTransaction(), dsXmlFile);

            return new CommandResultImpl( I18n.bind( WorkspaceCommandsI18n.datasourcesUploaded ) );
        } catch ( final Exception e ) {
            return new CommandResultImpl( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 2;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.uploadDatasourceHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.uploadDatasourceExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.uploadDatasourceUsage ) );
    }

}

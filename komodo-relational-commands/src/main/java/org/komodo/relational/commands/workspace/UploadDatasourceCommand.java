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
package org.komodo.relational.commands.workspace;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.datasource.internal.DatasourceParser;
import org.komodo.relational.datasource.internal.DatasourceParser.ParserOption;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.KLog;
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
            DatasourceParser datasourceParser = new DatasourceParser();
            
            File dsXmlFile = new File(fileName);
            // Validate the data source XML file and get names
            String[] dsNames = datasourceParser.validate(dsXmlFile);
            // Collect fatalErrors and Errors
            List<String> parseErrors = datasourceParser.getFatalErrors();
            if(parseErrors.isEmpty()) {
                parseErrors.addAll(datasourceParser.getErrors());
            }
            // If any error encountered, log first one and return.
            if( !parseErrors.isEmpty() ) {
                KLog.getLogger().error( "Datasource file parsing error encountered : ", parseErrors.get(0) ); //$NON-NLS-1$
                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.datasourceParserErrors, fileName ), null );
            }

            // If any data sources already exist, overwrite must be specified.
            for( String dsName : dsNames ) {
                boolean hasSource = getWorkspaceManager(getTransaction()).hasChild(getTransaction(), dsName, KomodoLexicon.DataSource.NODE_TYPE);
                if ( hasSource && !overwrite ) {
                    return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.datasourceOverwriteDisabled, fileName, dsName ), null );
                }
            }

            EnumSet<ParserOption> options = EnumSet.of(ParserOption.CREATE_REPO_SOURCES);
            if (overwrite)
                options.add(ParserOption.REPLACE_REPO_SOURCE);

            // Parse creates the sources in the repo.
            datasourceParser.parse(getTransaction(),
                                                       getRepository().komodoWorkspace(getTransaction()),
                                                       dsXmlFile, options);

            // Check again for parse errors
            parseErrors = datasourceParser.getFatalErrors();
            if(parseErrors.isEmpty()) {
                parseErrors.addAll(datasourceParser.getErrors());
            }
            // If any error encountered, log first one and return.
            if( !parseErrors.isEmpty() ) {
                KLog.getLogger().error( "Datasource file parsing error encountered : ", parseErrors.get(0) ); //$NON-NLS-1$
                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.datasourceParserErrors, fileName ), null );
            }

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

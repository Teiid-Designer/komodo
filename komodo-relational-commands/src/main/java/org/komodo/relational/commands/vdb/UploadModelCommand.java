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
package org.komodo.relational.commands.vdb;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlParser;

/**
 * Loads a {@link Model MODEL} from a local file.
 */
public final class UploadModelCommand extends VdbShellCommand {

    static final String NAME = "upload-model"; //$NON-NLS-1$

    private static final List< String > VALID_ARGS = Arrays.asList( new String[] { "-o", "--overwrite" } ); //$NON-NLS-1$ //$NON-NLS-2$;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public UploadModelCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final String modelName = requiredArgument( 0, I18n.bind( VdbCommandsI18n.missingModelNameForUpload ) );
            final String modelType = requiredArgument( 1, I18n.bind( VdbCommandsI18n.missingModelType ) );
            final String fileName = requiredArgument( 2, I18n.bind( VdbCommandsI18n.missingInputModelFilePath ) );
            final String overwriteArg = optionalArgument( 3, null );
            final boolean overwrite = !StringUtils.isBlank( overwriteArg );

            // make sure ModelType arg is valid
            if(!modelType.equals(Model.Type.PHYSICAL.name()) && !modelType.equals(Model.Type.VIRTUAL.name())) {
                return new CommandResultImpl( false, I18n.bind( VdbCommandsI18n.modelTypeError, modelType ), null );
            }

            // make sure overwrite arg is valid
            if ( overwrite && !VALID_ARGS.contains( overwriteArg ) ) {
                return new CommandResultImpl( false, I18n.bind( VdbCommandsI18n.invalidOverwriteArg, overwriteArg ), null );
            }

            { // Validates the supplied fileNameArg is a valid, readable file, and has property extension
                final String validationResult = validateReadableFileArg( fileName );

                if ( !CompletionConstants.OK.equals( validationResult ) ) {
                    return new CommandResultImpl( false,
                                                  I18n.bind( WorkspaceCommandsI18n.inputFileError, fileName, validationResult ),
                                                  null );
                }
            }

            // read file
            final String content = new String( Files.readAllBytes( Paths.get( fileName ) ) );

            if ( StringUtils.isEmpty( content ) ) {
                return new CommandResultImpl( false, I18n.bind( VdbCommandsI18n.modelInputFileIsEmpty, fileName ), null );
            }

            final Repository.UnitOfWork uow = getTransaction();

            // make sure we can overwrite
            Vdb vdbContext = (Vdb)getContext();
            Model[] allModels = vdbContext.getModels(uow);
            boolean hasModel = false;
            for(Model theModel : allModels) {
                if(modelName.equals(theModel.getName(uow))) {
                    hasModel = true;
                    break;
                }
            }
            if ( hasModel && !overwrite ) {
                return new CommandResultImpl( false, I18n.bind( VdbCommandsI18n.modelOverwriteDisabled, fileName ), null );
            }

            // create Model
            final Model model = vdbContext.addModel( uow, modelName );

            model.setModelType(uow, Model.Type.valueOf(modelType));
            model.setModelDefinition(uow, content);
            model.setProperty(uow, StandardDdlLexicon.PARSER_ID, TeiidDdlParser.ID);

            return new CommandResultImpl( I18n.bind( VdbCommandsI18n.modelUploaded, modelName ) );
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
        return 4;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( VdbCommandsI18n.uploadModelHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( VdbCommandsI18n.uploadModelExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( VdbCommandsI18n.uploadModelUsage ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        if ( ( args.size() == 1 ) ) {
            if(lastArgument ==null) {
                candidates.add( Model.Type.PHYSICAL.name() );
                candidates.add( Model.Type.VIRTUAL.name() );
            } else {
                if( Model.Type.PHYSICAL.name().toUpperCase().startsWith(lastArgument.toUpperCase()) ) {
                    candidates.add(Model.Type.PHYSICAL.name());
                } else if ( Model.Type.VIRTUAL.name().toUpperCase().startsWith(lastArgument.toUpperCase()) ) {
                    candidates.add(Model.Type.VIRTUAL.name());
                }
            }
        }
        return TabCompletionModifier.AUTO;
    }

}

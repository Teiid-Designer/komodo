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
package org.komodo.relational;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.internal.TypeResolverRegistry;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.utils.TextFormat;
import org.komodo.ui.DefaultLabelProvider;
import org.komodo.utils.KLog;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * A label provider for relational objects.
 */
public class RelationalLabelProvider extends DefaultLabelProvider {

    /**
     * A collection of grouping node names that should be removed from the display paths.
     */
    private static final List< String > GROUPING_NODES = Arrays.asList( new String[] { VdbLexicon.DataRole.PERMISSIONS,
                                                                                       VdbLexicon.DataRole.Permission.CONDITIONS,
                                                                                       VdbLexicon.DataRole.Permission.MASKS,
                                                                                       VdbLexicon.Vdb.DATA_ROLES,
                                                                                       VdbLexicon.Vdb.TRANSLATORS,
                                                                                       VdbLexicon.Vdb.SOURCES,
                                                                                       VdbLexicon.Vdb.ENTRIES,
                                                                                       VdbLexicon.Vdb.IMPORT_VDBS } );

    /**
     * Constructs a command provider for workspace shell commands.
     */
    public RelationalLabelProvider() {
        // nothing to do
    }

    private void configurePathFormat( final KomodoObject kobject,
                                      final TextFormat format ) {
        if ( format != null ) {
            format.bold();
//
//            if ( kobject instanceof DataRole ) {
//                format.setForeground( Rgb.GREEN );
//            } else if ( kobject instanceof Model ) {
//                format.setForeground( Rgb.YELLOW );
//            } else if ( kobject instanceof Translator ) {
//                format.setForeground( Rgb.BLUE );
//            } else if ( kobject instanceof Vdb ) {
//                format.setForeground( Rgb.MAGENTA );
//            } else if ( kobject instanceof VdbImport ) {
//                format.setForeground( Rgb.CYAN );
//            }
        }
    }

    private void configureTypeFormat( final KomodoObject kobject,
                                      final TextFormat format ) {
        if ( format != null ) {
//            format.italic();
//            format.setBackground( Rgb.WHITE );
//            format.setForeground( Rgb.BLACK );
//            format.setTextCase( TextCase.UPPER );
            format.unBold();
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.ui.KomodoObjectLabelProvider#skippedPathSegmentNames()
     */
    @Override
    public List< String > skippedPathSegmentNames() {
        return GROUPING_NODES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.ui.DefaultLabelProvider#getDisplayName(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject, org.komodo.spi.utils.TextFormat)
     */
    @Override
    public String getDisplayName( final UnitOfWork transaction,
                                  final KomodoObject kobject,
                                  final TextFormat format ) {
        final String displayName = super.getDisplayName( transaction, kobject, format );
        configurePathFormat( kobject, format );
        return displayName;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.ui.DefaultLabelProvider#getDisplayPath(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject, org.komodo.spi.utils.TextFormat)
     */
    @Override
    public String getDisplayPath( final UnitOfWork transaction,
                                  final KomodoObject kobject,
                                  final TextFormat format ) {
        final String displayPath = super.getDisplayPath( transaction, kobject, format );
        configurePathFormat( kobject, format );
        return displayPath;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.ui.DefaultLabelProvider#getTypeDisplay(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject, org.komodo.spi.utils.TextFormat)
     */
    @Override
    public String getTypeDisplay( final UnitOfWork uow,
                                  final KomodoObject kobject,
                                  final TextFormat format ) {
        if ( kobject instanceof RelationalObject ) {
            configureTypeFormat( kobject, format );
            return getTypeFromObject( ( RelationalObject )kobject );
        }

        try {
            TypeResolver< ? > resolver = TypeResolverRegistry.getInstance().getResolver( kobject.getTypeIdentifier( uow ) );
            if ( resolver != null ) {
                KomodoObject resolvedObject = resolver.resolve( uow, kobject );
                if ( resolvedObject instanceof RelationalObject ) {
                    return getTypeFromObject( ( RelationalObject )resolvedObject );
                }
            }
        } catch ( KException e ) {
            KLog.getLogger().error(e.getLocalizedMessage(), e);
        }

        return null;
    }

    private String getTypeFromObject( RelationalObject relObject ) {
        return org.komodo.relational.Messages.getString( relObject.getClass().getSimpleName() + ".typeName" ); //$NON-NLS-1$
    }

}

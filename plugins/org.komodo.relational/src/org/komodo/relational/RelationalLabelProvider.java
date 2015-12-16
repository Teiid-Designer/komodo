/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational;

import java.util.Arrays;
import java.util.List;

import org.komodo.relational.internal.TypeResolverRegistry;
import org.komodo.shell.DefaultLabelProvider;
import org.komodo.shell.ShellI18n;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.KLog;
import org.komodo.utils.i18n.I18n;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

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
                                                                                       VdbLexicon.Vdb.IMPORT_VDBS} );

    /**
     * Constructs a command provider for workspace shell commands.
     */
    public RelationalLabelProvider() {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.KomodoObjectLabelProvider#skippedPathSegmentNames(java.lang.String)
     */
	@Override
	public List<String> skippedPathSegmentNames(){
		return GROUPING_NODES;
	}

	/**
	 * {@inheritDoc}
	 *
	 *
	 * @see org.komodo.shell.api.KomodoObjectLabelProvider#getTypeDisplay(org.komodo.spi.repository.Repository.UnitOfWork,
	 *      org.komodo.spi.repository.KomodoObject)
	 */
	@Override
	public String getTypeDisplay(UnitOfWork uow, KomodoObject kobject) {
		if (kobject instanceof RelationalObject) {
			return getTypeFromObject((RelationalObject) kobject);
		}

		try {
			TypeResolver<?> resolver = TypeResolverRegistry.getInstance().getResolver(kobject.getTypeIdentifier(uow));
			if (resolver != null) {
				KomodoObject resolvedObject = resolver.resolve(uow, kobject);
				if (resolvedObject instanceof RelationalObject) {
					return getTypeFromObject((RelationalObject) resolvedObject);
				}
			}
		} catch (KException e) {
			KLog.getLogger().error(I18n.bind(ShellI18n.internalError), e);
		}

		return super.getTypeDisplay(uow, kobject);
	}

	private String getTypeFromObject(RelationalObject relObject) {
		return org.komodo.relational.Messages.getString(relObject.getClass().getSimpleName() + ".typeName"); //$NON-NLS-1$
	}

}

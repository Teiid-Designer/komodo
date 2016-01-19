/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational;

import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.utils.ArgCheck;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.jcr.JcrMixLexicon;
import org.modeshape.jcr.JcrNtLexicon;
import org.modeshape.jcr.ModeShapeLexicon;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;

/**
 * A {@link KomodoObject} related to a relational model.
 */
public interface RelationalObject extends KomodoObject {

    /**
     * A filter to use when deciding which properties and descriptors apply to an object.
     */
    public interface Filter {

        /**
         * @param descriptorName
         *        the name of the descriptor being checked (cannot be empty)
         * @return <code>true</code> if the descriptor should be reject
         */
        boolean rejectDescriptor( final String descriptorName );

        /**
         * @param propName
         *        the name of the property being checked (cannot be empty)
         * @return <code>true</code> if this property should be rejected
         */
        boolean rejectProperty( final String propName );

    }

    /**
     * A filter to exclude specific DDL-namespaced properties and type descriptors.
     */
    Filter DDL_QNAMES_FILTER = new ExcludeQNamesFilter( StandardDdlLexicon.DDL_EXPRESSION,
                                                        StandardDdlLexicon.DDL_LENGTH,
                                                        StandardDdlLexicon.DEFAULT_PRECISION,
                                                        StandardDdlLexicon.DEFAULT_OPTION,
                                                        StandardDdlLexicon.DDL_ORIGINAL_EXPRESSION,
                                                        StandardDdlLexicon.DDL_START_CHAR_INDEX,
                                                        StandardDdlLexicon.DDL_START_COLUMN_NUMBER,
                                                        StandardDdlLexicon.DDL_START_LINE_NUMBER );

    /**
     * A filter to exclude JCR-namespaced properties and type descriptors.
     */
    Filter JCR_FILTER = new ExcludeNamespaceFilter( JcrLexicon.Namespace.PREFIX, JcrLexicon.Namespace.URI );

    /**
     * A filter to exclude MIX-namespaced properties and type descriptors.
     */
    Filter MIX_FILTER = new ExcludeNamespaceFilter( JcrMixLexicon.Namespace.PREFIX, JcrMixLexicon.Namespace.URI );

    /**
     * A filter to exclude MODE-namespaced properties and type descriptors.
     */
    Filter MODE_FILTER = new ExcludeNamespaceFilter( ModeShapeLexicon.Namespace.PREFIX, ModeShapeLexicon.Namespace.URI );

    /**
     * An empty collection of filters.
     */
    Filter[] NO_FILTERS = new Filter[ 0 ];

    /**
     * A filter to exclude NT-namespaced properties and type descriptors.
     */
    Filter NT_FILTER = new ExcludeNamespaceFilter( JcrNtLexicon.Namespace.PREFIX, JcrNtLexicon.Namespace.URI );

    /**
     * A filter to exclude residual properties and type descriptors.
     */
    Filter RESIDUAL_FILTER = new Filter() {

        private String NAME = "*"; //$NON-NLS-1$

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.RelationalObject.Filter#rejectProperty(java.lang.String)
         */
        @Override
        public boolean rejectProperty( final String propName ) {
            ArgCheck.isNotEmpty( propName, "propName" ); //$NON-NLS-1$
            return NAME.equals( propName );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.RelationalObject.Filter#rejectDescriptor(java.lang.String)
         */
        @Override
        public boolean rejectDescriptor( final String descriptorName ) {
            ArgCheck.isNotEmpty( descriptorName, "descriptorName" ); //$NON-NLS-1$
            return NAME.equals( descriptorName );
        }

    };

    /**
     * The default set of filters for restricting which properties and descriptors apply to relational objects.
     */
    Filter[] DEFAULT_FILTERS = new Filter[] { DDL_QNAMES_FILTER, JCR_FILTER, MIX_FILTER, MODE_FILTER, NT_FILTER, RESIDUAL_FILTER };

    /**
     * @return the filters to use when deciding which {@link PropertyDescriptor properties} and {@link Descriptor descriptors} are
     *         valid for this object (never <code>null</code> but can be empty)
     */
    Filter[] getFilters();

    /**
     * @param newFilters
     *        the new set of filters to use when deciding which {@link PropertyDescriptor properties} and {@link Descriptor
     *        descriptors} are valid for this object (can be <code>null</code>)
     */
    void setFilters( final Filter[] newFilters );

}

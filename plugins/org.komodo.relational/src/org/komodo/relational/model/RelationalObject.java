/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.relational.ExcludeNamespaceFilter;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.PropertyDescriptor;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.jcr.JcrMixLexicon;
import org.modeshape.jcr.JcrNtLexicon;
import org.modeshape.jcr.ModeShapeLexicon;

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
     * The default set of filters for restricting which properties and descriptors apply to relational objects.
     */
    Filter[] DEFAULT_FILTERS = new Filter[] { JCR_FILTER, MIX_FILTER, MODE_FILTER, NT_FILTER };

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

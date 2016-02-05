/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational;

import java.util.ArrayList;
import java.util.List;
import javax.xml.namespace.QName;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.utils.ArgCheck;

/**
 * A filter for excluding specific {@link QName}s.
 */
public class ExcludeQNamesFilter implements Filter {

    private final List< String > qnames;

    /**
     * Constructs a filter.
     *
     * @param namesToExclude
     *        a collection of names being excluded (cannot be <code>null</code> or empty)
     */
    public ExcludeQNamesFilter( final String... namesToExclude ) {
        ArgCheck.isNotEmpty( namesToExclude, "qnames" ); //$NON-NLS-1$
        this.qnames = new ArrayList< String >( namesToExclude.length );

        for ( final String name : namesToExclude ) {
            this.qnames.add( name );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject.Filter#rejectDescriptor(java.lang.String)
     */
    @Override
    public boolean rejectDescriptor( final String descriptorName ) {
        ArgCheck.isNotEmpty( descriptorName, "descriptorName" ); //$NON-NLS-1$
        return this.qnames.contains( descriptorName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject.Filter#rejectProperty(java.lang.String)
     */
    @Override
    public boolean rejectProperty( final String propName ) {
        ArgCheck.isNotEmpty( propName, "propName" ); //$NON-NLS-1$
        return this.qnames.contains( propName );
    }

}
